#' =============================================================================
#' STEP 07: Bayesian Bootstrap (Standard Errors)
#' =============================================================================
#' Reweights the estimation sample with Dirichlet (exponential, normalized)
#' location-level weights and re-runs the same shared estimation pipeline used by
#' 06_estimation.R.
#'
#' Inputs:
#'   - mkdata/data/04_estimation_sample.rds
#'   - results/data/06_parameters.rds  (point estimates as warm start)
#'
#' Outputs:
#'   - results/data/07_boot_weights.rds
#'   - results/data/bootstrap_reps/boot_res_<iteration>.rds
#'   - results/data/07_bootstrap.rds
#' =============================================================================

library("data.table")

source("config.R")
source("preamble.R")

script_start <- Sys.time()

estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
parameters_path <- file.path("results", "data", "06_parameters.rds")
weights_path <- file.path("results", "data", "07_boot_weights.rds")
reps_dir <- CONFIG$bootstrap_results_dir
assert_required_files(c(estimation_sample_path, parameters_path))
ensure_directory(file.path("results", "data"))
ensure_directory(reps_dir)

estimation_sample <- readRDS(estimation_sample_path)
working_data <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix
min_wage_levels <- data.table(estimation_sample$min_wage_levels)
point_parameters <- readRDS(parameters_path)

resolve_bootstrap_iterations <- function(config = CONFIG) {
  if (!is.na(config$slurm_array_task_id)) {
    return(config$slurm_array_task_id)
  }
  if (!is.na(config$bootstrap_iteration)) {
    return(config$bootstrap_iteration)
  }
  if (!is.na(config$bootstrap_iteration_start) || !is.na(config$bootstrap_iteration_end)) {
    first_iter <- if (is.na(config$bootstrap_iteration_start)) 1L else config$bootstrap_iteration_start
    last_iter <- if (is.na(config$bootstrap_iteration_end)) config$bootstrap_reps else config$bootstrap_iteration_end
    return(seq.int(first_iter, last_iter))
  }
  seq_len(config$bootstrap_reps)
}

build_bootstrap_weights <- function(data, config = CONFIG) {
  location_ids <- unique(data$location_id)
  set.seed(config$bootstrap_seed)
  rbindlist(lapply(seq_len(config$bootstrap_reps), function(iter) {
    temp_weight <- stats::rexp(length(location_ids), 1)
    temp_weight <- temp_weight / sum(temp_weight)
    data.table(iteration = iter, location_id = location_ids, bweight = temp_weight)
  }))
}

load_or_create_bootstrap_weights <- function(data, path, config = CONFIG, persist = TRUE) {
  if (file.exists(path)) {
    weights <- readRDS(path)
    if (max(weights$iteration) >= config$bootstrap_reps) {
      return(data.table(weights))
    }
    message("Existing bootstrap weights do not cover all configured reps; regenerating.")
  }

  weights <- build_bootstrap_weights(data, config)
  if (isTRUE(persist)) {
    saveRDS(weights, path)
  }
  weights
}

bootstrap_row_weights <- function(iter, data, boot_weight_all) {
  current_weights <- boot_weight_all[iteration == iter]
  if (nrow(current_weights) == 0) {
    stop("No bootstrap weights found for iteration ", iter, ".")
  }

  weights <- current_weights$bweight[match(data$location_id, current_weights$location_id)]
  if (anyNA(weights)) {
    stop("Missing bootstrap weights for one or more locations in iteration ", iter, ".")
  }
  weights
}

bootstrap_result_row <- function(iter, parameter_table) {
  values <- parameter_table$coefficients
  names(values) <- parameter_table$parm_name
  as.data.table(t(c(iteration = iter, values)))
}

combine_bootstrap_results <- function(iterations, reps_dir, output_path) {
  rep_paths <- file.path(reps_dir, paste0("boot_res_", iterations, ".rds"))
  existing <- file.exists(rep_paths)
  if (!any(existing)) {
    warning("No bootstrap replication files found to combine.")
    return(invisible(NULL))
  }
  if (!all(existing)) {
    warning("Missing bootstrap replication files for iterations: ",
            paste(iterations[!existing], collapse = ", "))
  }

  combined <- rbindlist(lapply(rep_paths[existing], readRDS), fill = TRUE)
  setorder(combined, iteration)
  saveRDS(combined, output_path)
  invisible(combined)
}

run_bootstrap_iteration <- function(iter, config = CONFIG) {
  message("\n--- Starting bootstrap iteration ", iter, " at ", Sys.time(), " ---")
  weights <- bootstrap_row_weights(iter, working_data, boot_weight_all)
  clust <- NULL
  if (identical(get_os(), "windows") && isTRUE(config$pl_on)) {
    clust <- make_windows_solver_cluster(config)
    on.exit(parallel::stopCluster(clust), add = TRUE)
  }

  result <- estimate_structural_parameters(
    working_data,
    estim_matrix,
    min_wage_levels,
    config = config,
    clust = clust,
    weights = weights,
    starting_parameters = point_parameters,
    skip_structural_optimizer = FALSE
  )

  if (!is.null(result$wage_result$convergence) && result$wage_result$convergence != 0) {
    warning("Bootstrap iteration ", iter, " wage solve convergence code: ",
            result$wage_result$convergence)
  }
  if (result$price_result$result$convergence != 0) {
    warning("Bootstrap iteration ", iter, " price solve convergence code: ",
            result$price_result$result$convergence)
  }

  this_res <- bootstrap_result_row(iter, result$parameter_table)
  saveRDS(this_res, file.path(reps_dir, paste0("boot_res_", iter, ".rds")))
  message("--- Completed bootstrap iteration ", iter, " at ", Sys.time(), " ---")
  this_res
}

resolve_bootstrap_backend <- function(iterations, config = CONFIG) {
  backend <- tolower(config$bootstrap_backend)
  if (!identical(backend, "auto")) {
    return(backend)
  }
  if (!is.na(config$slurm_array_task_id) || length(iterations) == 1L) {
    return("serial")
  }
  if (identical(get_os(), "windows")) {
    return("windows")
  }
  "multicore"
}

run_bootstrap_iterations <- function(iterations, config = CONFIG) {
  backend <- resolve_bootstrap_backend(iterations, config)
  message("Bootstrap backend: ", backend, "; iterations: ", paste(iterations, collapse = ", "))

  if (identical(backend, "serial")) {
    return(rbindlist(lapply(iterations, run_bootstrap_iteration, config = config), fill = TRUE))
  }

  worker_count <- if (is.na(config$bootstrap_workers)) {
    min(length(iterations), get_core_count(config))
  } else {
    min(length(iterations), config$bootstrap_workers)
  }

  rep_config <- config
  rep_config$pl_on <- FALSE

  if (identical(backend, "windows")) {
    clust <- parallel::makeCluster(worker_count)
    on.exit(parallel::stopCluster(clust), add = TRUE)
    invisible(parallel::clusterEvalQ(clust, {
      library("data.table")
      source("config.R")
      source("preamble.R")
      NULL
    }))
    parallel::clusterExport(
      clust,
      c("working_data", "estim_matrix", "min_wage_levels", "point_parameters",
        "boot_weight_all", "reps_dir", "bootstrap_row_weights",
        "bootstrap_result_row", "run_bootstrap_iteration"),
      envir = .GlobalEnv
    )
    return(rbindlist(parallel::parLapply(clust, iterations, run_bootstrap_iteration,
                                         config = rep_config), fill = TRUE))
  }

  if (identical(backend, "multicore")) {
    return(rbindlist(parallel::mclapply(
      iterations,
      run_bootstrap_iteration,
      config = rep_config,
      mc.cores = worker_count
    ), fill = TRUE))
  }

  stop("Unknown bootstrap backend: ", backend,
       ". Use auto, serial, windows, or multicore.")
}

iters_to_run <- resolve_bootstrap_iterations(CONFIG)
if (any(iters_to_run < 1L | iters_to_run > CONFIG$bootstrap_reps)) {
  stop("Bootstrap iterations must be between 1 and ", CONFIG$bootstrap_reps, ".")
}

array_task_mode <- !is.na(CONFIG$slurm_array_task_id)
persist_weights <- !array_task_mode || isTRUE(CONFIG$bootstrap_combine_only)
boot_weight_all <- load_or_create_bootstrap_weights(
  working_data,
  weights_path,
  CONFIG,
  persist = persist_weights
)

if (!isTRUE(CONFIG$bootstrap_combine_only)) {
  run_bootstrap_iterations(iters_to_run, CONFIG)
}

target_iter <- if (!is.na(CONFIG$slurm_array_task_id)) {
  CONFIG$slurm_array_task_id
} else {
  CONFIG$bootstrap_iteration
}

if (is.na(target_iter) || isTRUE(CONFIG$bootstrap_combine_only)) {
  combine_bootstrap_results(
    seq_len(CONFIG$bootstrap_reps),
    reps_dir,
    file.path("results", "data", "07_bootstrap.rds")
  )
  message("Saved combined bootstrap results.")
} else {
  message("Single iteration run complete. Combine later with JMP_BOOTSTRAP_COMBINE_ONLY=true.")
}

script_end <- Sys.time()
message("Script finished at ", script_end, " (Duration: ",
        round(difftime(script_end, script_start, units = "mins"), 2), " minutes)")
