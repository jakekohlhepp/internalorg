#' =============================================================================
#' STEP 07: Standard Errors via First-Stage Parameter Draws (Petrin-Train)
#' =============================================================================
#' Two-step inference following Petrin & Train, "Omitted Product Attributes in
#' Discrete Choice Models" (NBER WP 9452, 2003; published as "A Control
#' Function Approach to Endogeneity in Consumer Choice Models", JMR 2010).
#' See docs/bootstrap_petrin_train.md for the full procedure write-up.
#'
#'   1. FIRST STAGE (demand-side 2SLS; the demand == TRUE rows of the
#'      parameter table): inference is analytical. We compute the asymptotic
#'      cluster-robust (CR1) variance-covariance matrix of the 2SLS estimator,
#'      clustered at location_id, and its SEs are the reported first-stage
#'      standard errors. Saved to results/data/07_first_stage_vcov.rds.
#'   2. SECOND STAGE (structural wage + price parameters): each replication r
#'      draws beta^(r) ~ N(beta_hat, V_clustered) and re-runs the second-stage
#'      estimation (wage solver + price L-BFGS-B step) on the ORIGINAL,
#'      unweighted sample, conditioning on beta^(r). The reported second-stage
#'      SE (computed in 08_display_estimates.R) is the standard deviation of
#'      each parameter across replications.
#'
#' Inputs:
#'   - mkdata/data/04_estimation_sample.rds
#'   - results/data/06_parameters.rds  (canonical 06 output; wage warm start is
#'     pulled from the parameter_table rows whose parm_name matches the
#'     :avg_labor:E_raw_* pattern)
#'
#' Outputs:
#'   - results/data/07_first_stage_vcov.rds
#'   - results/data/07_boot_draws.rds
#'   - results/data/bootstrap_reps/boot_res_<iteration>.rds
#'   - results/data/07_bootstrap.rds
#' =============================================================================

library("data.table")

source("config.R")
source("preamble.R")

script_start <- Sys.time()

estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
parameters_06_path <- file.path("results", "data", "06_parameters.rds")
first_stage_path <- file.path("results", "data", "07_first_stage_vcov.rds")
draws_path <- file.path("results", "data", "07_boot_draws.rds")
reps_dir <- CONFIG$bootstrap_results_dir
assert_required_files(c(estimation_sample_path, parameters_06_path))
ensure_directory(file.path("results", "data"))
ensure_directory(reps_dir)

estimation_sample <- readRDS(estimation_sample_path)
working_data <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix
min_wage_levels <- data.table(estimation_sample$min_wage_levels)

## The analytical first-stage vcov below is for the UNCONSTRAINED 2SLS demand
## estimator. Under the workers_rows monotonicity restriction beta solves a
## constrained QP, whose sampling distribution is not the 2SLS sandwich.
if (!identical(CONFIG$skill_monotone_orientation, "none")) {
  stop("07_bootstrap.R (Petrin-Train) requires skill_monotone_orientation = ",
       "'none'; got '", CONFIG$skill_monotone_orientation, "'. The analytical ",
       "2SLS vcov does not apply to the constrained demand estimator.")
}

## ---------------------------------------------------------------------------
## First stage: 2SLS point estimates + analytical clustered vcov.
## beta_hat re-derives the exact 06 demand coefficients (same rank-aware 2SLS
## on the same sample); cluster_vcov_2sls() gives the CR1 sandwich clustered
## at location_id.
## ---------------------------------------------------------------------------
estimation_objects <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta_hat <- estimation_objects$beta
beta_2 <- estimation_objects$beta_2

first_stage <- cluster_vcov_2sls(
  x = estimation_objects$mm_1,
  z = estimation_objects$z_mm_1,
  y = estim_matrix[, "log_rel_mkt"],
  beta = beta_hat,
  cluster = estim_matrix[, "location_id"],
  context = "demand IV clustered vcov"
)
rm(estimation_objects)

first_stage_record <- list(
  beta = beta_hat,
  vcov = first_stage$vcov,
  se = first_stage$se,
  cluster_variable = "location_id",
  n_clusters = first_stage$n_clusters,
  n_obs = first_stage$n_obs,
  rank = first_stage$rank,
  small_sample_adjustment = first_stage$adjustment,
  type = "CR1",
  estimator = "rank_aware_2sls (demand IV)"
)
message("First stage: ", nrow(beta_hat), " demand parameters, ",
        first_stage$n_clusters, " location_id clusters, ",
        first_stage$n_obs, " observations; median analytical SE ",
        signif(stats::median(first_stage$se), 4))

## Wage warm start. Pull the wage coefficients straight from 06_parameters.rds.
## In utils/estimation_pipeline.R, the parameter_table's wage rows are written
## with parm_name = names(wage_coefs) = names(beta_2_subset) = the
## :avg_labor:E_raw_* names off beta_2's rownames; so we filter on that pattern
## and reorder to match rownames(beta_2)[wage_idx], which is the order
## extract_wage_start expects.
wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
wage_names <- rownames(beta_2)[wage_idx]

parameters_06 <- as.data.table(readRDS(parameters_06_path))
wage_rows <- parameters_06[parm_name %in% wage_names]
stopifnot(nrow(wage_rows) == length(wage_names),
          length(wage_names) > 0,
          setequal(wage_rows$parm_name, wage_names))
beta_2_subset <- setNames(
  wage_rows$coefficients[match(wage_names, wage_rows$parm_name)],
  wage_names
)
stopifnot(length(beta_2_subset) == sum(wage_idx),
          all(is.finite(beta_2_subset)))

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

## All replications' first-stage draws are generated in one block from
## config$bootstrap_seed, so every concurrent array task regenerates the
## identical draw matrix and indexes its own row -- same determinism contract
## the Dirichlet weights had before the Petrin-Train rewrite.
build_bootstrap_parameter_draws <- function(beta, vcov, config = CONFIG) {
  set.seed(config$bootstrap_seed)
  draw_first_stage_parameters(beta, vcov, n_draws = config$bootstrap_reps)
}

load_or_create_parameter_draws <- function(beta, vcov, path, config = CONFIG,
                                           persist = TRUE) {
  if (file.exists(path)) {
    stored <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.list(stored) &&
        identical(stored$seed, config$bootstrap_seed) &&
        is.matrix(stored$draws) &&
        nrow(stored$draws) >= config$bootstrap_reps &&
        identical(colnames(stored$draws), rownames(beta)) &&
        isTRUE(max(abs(stored$beta - as.numeric(beta))) <= 1e-8)) {
      return(stored$draws)
    }
    message("Existing first-stage draws do not match the current seed, reps, ",
            "or point estimates; regenerating.")
  }

  draws <- build_bootstrap_parameter_draws(beta, vcov, config)
  if (isTRUE(persist)) {
    saveRDS(list(seed = config$bootstrap_seed,
                 beta = as.numeric(beta),
                 parm_names = rownames(beta),
                 draws = draws),
            path)
  }
  draws
}

## One replication's first-stage parameter vector, shaped exactly like the
## rank_aware_2sls output (1-column matrix with named rows) so every
## downstream consumer (build_cost_matrix, add_price_adjustment, the wage
## moment objectives) indexes it identically to the 06 point estimates.
bootstrap_draw_beta <- function(iter, draws) {
  if (iter < 1L || iter > nrow(draws)) {
    stop("No first-stage draw found for iteration ", iter, ".")
  }
  values <- draws[iter, ]
  if (anyNA(values) || any(!is.finite(values))) {
    stop("Non-finite first-stage draw for iteration ", iter, ".")
  }
  matrix(values, ncol = 1L, dimnames = list(colnames(draws), NULL))
}

## Stamped into every rep row. Distinguishes Petrin-Train first-stage-draw
## reps from the pre-2026-06 Dirichlet-reweighting reps so the resume logic
## never silently reuses a stale rep and the combine pass never mixes the two
## procedures in one SE distribution.
BOOTSTRAP_PROCEDURE <- "petrin_train_draws"

bootstrap_result_row <- function(iter, parameter_table,
                                 wage_convergence = NA_integer_,
                                 price_convergence = NA_integer_,
                                 status = "ok",
                                 error_message = NA_character_) {
  if (is.null(parameter_table) || nrow(parameter_table) == 0L) {
    out <- data.table(iteration = iter)
  } else {
    values <- parameter_table$coefficients
    names(values) <- parameter_table$parm_name
    out <- as.data.table(t(c(iteration = iter, values)))
  }
  out[, `:=`(
    wage_convergence  = as.integer(wage_convergence),
    price_convergence = as.integer(price_convergence),
    status            = as.character(status),
    error_message     = as.character(error_message),
    procedure         = BOOTSTRAP_PROCEDURE
  )]
  out
}

validate_bootstrap_results <- function(results, context = "bootstrap results") {
  if (is.null(results) || nrow(results) == 0L) {
    stop(context, " are empty.", call. = FALSE)
  }

  ## Only status="error" rejects the rep -- those rows have no parameter
  ## columns, so they cannot enter the bootstrap distribution. The
  ## soft-revert statuses (wage_nonconverged / price_nonconverged /
  ## wage+price_nonconverged) keep the rep: per docs/bootstrap_slurm.md
  ## "Wage-stage convergence and fallback semantics", these reps have a
  ## full parameter table (some county/columns may be 06-warm-start
  ## fallbacks for wage, or L-BFGS-B final iterate for price). The
  ## downstream filter in 08_display_estimates.R is the line of defense
  ## that decides whether to drop or flag them for the final std errors.
  if ("status" %in% names(results)) {
    bad_status <- results[!is.na(status) & status == "error"]
    if (nrow(bad_status) > 0L) {
      stop(context, " contain status=error replications (no parameters): ",
           paste(head(paste0("iteration ", bad_status$iteration), 10L),
                 collapse = ", "),
           call. = FALSE)
    }
    soft_status <- results[!is.na(status) & status != "ok" & status != "error"]
    if (nrow(soft_status) > 0L) {
      message(context, ": ", nrow(soft_status), " soft-revert replication(s): ",
              paste(head(paste0("iteration ", soft_status$iteration,
                                " (", soft_status$status, ")"), 10L),
                    collapse = ", "),
              if (nrow(soft_status) > 10L) ", ..." else "")
    }
  }

  for (conv_col in intersect(c("wage_convergence", "price_convergence"), names(results))) {
    bad_conv <- results[!is.na(get(conv_col)) & get(conv_col) != 0L]
    if (nrow(bad_conv) > 0L) {
      message(context, ": ", nrow(bad_conv), " replication(s) with ",
              conv_col, " != 0: ",
              paste(head(paste0("iteration ", bad_conv$iteration,
                                " (code=", bad_conv[[conv_col]], ")"), 10L),
                    collapse = ", "),
              if (nrow(bad_conv) > 10L) ", ..." else "")
    }
  }

  invisible(TRUE)
}

combine_bootstrap_results <- function(iterations, reps_dir, output_path) {
  rep_paths <- file.path(reps_dir, paste0("boot_res_", iterations, ".rds"))
  existing <- file.exists(rep_paths)
  if (!any(existing)) {
    stop("No bootstrap replication files found to combine.", call. = FALSE)
  }
  if (!all(existing)) {
    stop("Missing bootstrap replication files for iterations: ",
         paste(iterations[!existing], collapse = ", "),
         call. = FALSE)
  }

  combined <- rbindlist(lapply(rep_paths, readRDS), fill = TRUE)
  setorder(combined, iteration)
  stale <- if (!"procedure" %in% names(combined)) {
    combined$iteration
  } else {
    combined[is.na(procedure) | procedure != BOOTSTRAP_PROCEDURE, iteration]
  }
  if (length(stale) > 0L) {
    stop("Refusing to combine: ", length(stale), " replication file(s) predate ",
         "the Petrin-Train procedure (no procedure='", BOOTSTRAP_PROCEDURE,
         "' stamp), e.g. iterations ",
         paste(head(stale, 10L), collapse = ", "),
         ". Delete or archive the stale files under ", reps_dir,
         " and re-run the array.", call. = FALSE)
  }
  validate_bootstrap_results(combined, "Combined bootstrap results")
  saveRDS(combined, output_path)
  invisible(combined)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
.boot_iter_traceback <- NULL

run_bootstrap_iteration <- function(iter, config = CONFIG) {
  .boot_iter_traceback <<- NULL
  ## Skip if an "ok" .rds already exists. Lets us cancel + resubmit the
  ## full array without redoing reps that already passed the convergence
  ## gate, while still rerunning any wage_nonconverged / error stubs (the
  ## user must delete bad stubs to trigger a rerun).
  out_path <- file.path(reps_dir, paste0("boot_res_", iter, ".rds"))
  if (file.exists(out_path)) {
    existing <- tryCatch(readRDS(out_path), error = function(e) NULL)
    if (!is.null(existing) &&
        identical(as.character(existing$status[[1L]]), "ok") &&
        "procedure" %in% names(existing) &&
        identical(as.character(existing$procedure[[1L]]), BOOTSTRAP_PROCEDURE)) {
      message("--- Skipping bootstrap iteration ", iter,
              " (existing rep has status=ok at ", out_path, ") ---")
      return(existing)
    }
    if (!is.null(existing) &&
        (!"procedure" %in% names(existing) ||
         !identical(as.character(existing$procedure[[1L]]), BOOTSTRAP_PROCEDURE))) {
      message("--- Re-running bootstrap iteration ", iter,
              " (existing rep at ", out_path,
              " predates the Petrin-Train procedure) ---")
    }
  }
  message("\n--- Starting bootstrap iteration ", iter, " at ", Sys.time(), " ---")
  beta_draw <- bootstrap_draw_beta(iter, boot_param_draws)
  iter_config <- config
  iter_config$bootstrap_iteration <- iter
  clust <- NULL
  if (identical(get_os(), "windows") && isTRUE(iter_config$pl_on)) {
    clust <- make_windows_solver_cluster(iter_config)
    on.exit(parallel::stopCluster(clust), add = TRUE)
  }

  result <- tryCatch(
    withCallingHandlers(
      estimate_structural_parameters(
        working_data,
        estim_matrix,
        min_wage_levels,
        config = iter_config,
        clust = clust,
        weights = NULL,
        beta = beta_draw,
        beta_2_subset = beta_2_subset,
        skip_structural_optimizer = isTRUE(iter_config$skip_structural_optimizer)
      ),
      error = function(e) {
        .boot_iter_traceback <<- vapply(sys.calls(), function(c) {
          s <- tryCatch(deparse(c, nlines = 1L)[1L], error = function(...) "<undeparsable>")
          if (nchar(s) > 200L) paste0(substr(s, 1L, 200L), "...") else s
        }, character(1))
      }
    ),
    error = function(e) {
      out_path <- file.path(reps_dir, paste0("boot_res_", iter, ".rds"))
      tb <- .boot_iter_traceback %||% character(0)
      this_res <- bootstrap_result_row(
        iter,
        parameter_table = NULL,
        status = "error",
        error_message = paste0(conditionMessage(e),
                               if (length(tb)) paste0(" || traceback: ",
                                                      paste(rev(tb), collapse = " <- "))
                               else "")
      )
      saveRDS(this_res, out_path)
      message("--- TRACEBACK for bootstrap iteration ", iter, " ---")
      for (i in seq_along(tb)) message("  ", length(tb) - i + 1L, ": ", tb[length(tb) - i + 1L])
      stop("Bootstrap iteration ", iter, " errored: ", conditionMessage(e),
           ". Saved diagnostic row at ", out_path, ".", call. = FALSE)
    }
  )

  wage_conv  <- result$wage_result$convergence
  price_conv <- if (is.list(result$price_result)) result$price_result$result$convergence else NA_integer_

  wage_nonconv <- !isTRUE(iter_config$skip_structural_optimizer) &&
    isTRUE(!is.null(wage_conv) && !is.na(wage_conv) && wage_conv != 0)
  price_nonconv <- isTRUE(!is.null(price_conv) && !is.na(price_conv) && price_conv != 0)

  if (wage_nonconv) {
    message("Bootstrap iteration ", iter, " wage solve non-converged: code ", wage_conv)
  }
  if (price_nonconv) {
    message("Bootstrap iteration ", iter, " price solve non-converged: code ", price_conv)
  }

  status <- if (wage_nonconv && price_nonconv) "wage+price_nonconverged"
            else if (wage_nonconv)             "wage_nonconverged"
            else if (price_nonconv)            "price_nonconverged"
            else                               "ok"

  this_res <- bootstrap_result_row(
    iter, result$parameter_table,
    wage_convergence  = if (is.null(wage_conv))  NA_integer_ else wage_conv,
    price_convergence = if (is.null(price_conv)) NA_integer_ else price_conv,
    status            = status
  )

  out_path <- file.path(reps_dir, paste0("boot_res_", iter, ".rds"))
  saveRDS(this_res, out_path)
  message("--- Completed bootstrap iteration ", iter, " at ", Sys.time(),
          " (status=", this_res$status, ") ---")
  validate_bootstrap_results(this_res, paste0("Bootstrap iteration ", iter))
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
      c("working_data", "estim_matrix", "min_wage_levels", "beta_2_subset",
        "boot_param_draws", "reps_dir", "bootstrap_draw_beta",
        "bootstrap_result_row", "validate_bootstrap_results", "run_bootstrap_iteration"),
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

## In array mode every task derives the first stage + draws deterministically
## in memory; only non-array runs (and the combine-only pass) persist them, so
## 1100 concurrent tasks never race on the same output files.
array_task_mode <- !is.na(CONFIG$slurm_array_task_id)
persist_first_stage <- !array_task_mode || isTRUE(CONFIG$bootstrap_combine_only)
if (persist_first_stage) {
  saveRDS(first_stage_record, first_stage_path)
  message("Saved first-stage clustered vcov to ", first_stage_path)
}
boot_param_draws <- load_or_create_parameter_draws(
  beta_hat,
  first_stage_record$vcov,
  draws_path,
  CONFIG,
  persist = persist_first_stage
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
