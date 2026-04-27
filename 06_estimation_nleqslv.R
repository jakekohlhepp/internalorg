#' =============================================================================
#' STEP 06 (nleqslv variant): Structural Estimation via Broyden / nleqslv
#' =============================================================================
#' Identical to 06_estimation.R except the wage stage is solved per-county
#' with nleqslv::nleqslv (Broyden's method with numerical Jacobian) instead
#' of BB::BBsolve (dfsane).
#'
#' Inputs:
#'   - mkdata/data/04_estimation_sample.rds
#'   - mkdata/data/seeit_bb.rds   (starting point only; not overwritten)
#'
#' Outputs (separate from BBsolve run so they can be compared side-by-side):
#'   - mkdata/data/seeit_bb_nleqslv.rds
#'   - mkdata/data/06_nleqslv_warmstart.rds
#'   - results/data/06_parameters_nleqslv.rds
#' =============================================================================

library("data.table")
set.seed(4459665)

source("config.R")

CONFIG$innertol <- 1e-10
CONFIG$outertol <- 1e-8
CONFIG$obj_tol  <- 1e-6
CONFIG$wage_optimizer_mode <- "county"
message("Tolerances: innertol=", CONFIG$innertol,
        ", outertol=", CONFIG$outertol, ", obj_tol=", CONFIG$obj_tol,
        " | staged: coarse=", CONFIG$coarse_innertol, "/", CONFIG$coarse_outertol,
        " switch_norm=", CONFIG$staged_tolerance_switch_norm,
        " | solver=nleqslv (Broyden)")

estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
assert_required_files(estimation_sample_path)

estimation_sample <- readRDS(estimation_sample_path)
working_data <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix
if (!"min_wage_levels" %in% names(estimation_sample)) {
  min_wage_levels <- unique(working_data[, .(county, quarter_year, min_wage)])
} else {
  min_wage_levels <- data.table(estimation_sample$min_wage_levels)
}
stopifnot(uniqueN(min_wage_levels[, .(county, quarter_year)]) == nrow(min_wage_levels))

source("preamble.R")
estimation_objects <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta <- estimation_objects$beta
beta_2 <- estimation_objects$beta_2
rm(estimation_objects)

## Starting point identical to BBsolve run.
beta_2_subset <- readRDS(file.path(CONFIG$prep_output_dir, "seeit_bb.rds"))
names(beta_2_subset) <- rownames(beta_2)[grep(
  "avg_labor:E_raw_[2-9]{1}\\d{0,3}",
  rownames(beta_2)
)]

## Separate warmstart path so the two concurrent runs do not clobber each other.
nleqslv_warmstart_path <- file.path(CONFIG$prep_output_dir, "06_nleqslv_warmstart.rds")
if (file.exists(nleqslv_warmstart_path)) {
  nleqslv_warmstart <- tryCatch(readRDS(nleqslv_warmstart_path), error = function(e) NULL)
  if (!is.null(nleqslv_warmstart) && length(nleqslv_warmstart) == length(beta_2_subset)) {
    message("nleqslv warm-start: loading ", length(nleqslv_warmstart),
            "-element checkpoint from ", nleqslv_warmstart_path)
    beta_2_subset <- setNames(as.numeric(nleqslv_warmstart), names(beta_2_subset))
  }
}

.orig_objective_gmm <- objective_gmm
objective_gmm <- function(theta, ...) {
  tryCatch(saveRDS(theta, nleqslv_warmstart_path),
           error = function(e) warning("nleqslv checkpoint write failed: ", e$message))
  .orig_objective_gmm(theta, ...)
}

clust <- make_windows_solver_cluster(CONFIG)
if (!is.null(clust)) {
  on.exit(parallel::stopCluster(clust), add = TRUE)
}

if (isTRUE(CONFIG$check_structural_start)) {
  starting_moments <- weighted_col_means(objective_gmm(
    theta = beta_2_subset,
    x = estim_matrix,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = CONFIG,
    clust = clust
  ))
  message("Starting wage moment squared norm: ", signif(sum(starting_moments^2), 6))
}

## ---------------------------------------------------------------------------
## Override estimate_wage_parameters with a per-county nleqslv solver. This
## shadows the version sourced from utils/structural_solver.R so the rest of
## the pipeline (estimate_structural_parameters) picks it up unchanged.
## ---------------------------------------------------------------------------
estimate_wage_parameters <- function(start, x, beta, beta_2_subset, config = CONFIG,
                                     clust = NULL, solver_state = NULL,
                                     moment_weights = NULL) {
  if (is.null(solver_state) && solver_flag(config, "use_solver_warm_starts", TRUE)) {
    solver_state <- get_default_solver_state(reset = TRUE)
  }

  data <- as.data.frame(x)
  full_par <- start
  names(full_par) <- names(beta_2_subset)
  county_results <- list()

  start_time <- Sys.time()
  for (cnty in config$counties) {
    county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
    par_idx <- grepl(county_pattern, names(full_par), fixed = TRUE)
    row_idx <- as.character(data$county) == cnty
    if (!any(par_idx) || !any(row_idx)) next

    x_county <- data[row_idx, , drop = FALSE]
    objective_county <- function(parms) {
      candidate <- full_par
      candidate[par_idx] <- parms
      moments <- weighted_col_means(objective_gmm(
        theta = candidate,
        x = x_county,
        beta = beta,
        beta_2_subset = beta_2_subset,
        config = config,
        clust = clust,
        solver_state = solver_state
      ), weights = moment_weights)
      county_moments <- grepl(paste0("county", cnty, ":E_"),
                              names(moments), fixed = TRUE)
      as.numeric(moments[county_moments])
    }

    cat("[nleqslv] solving county", cnty, "starting from",
        round(full_par[par_idx], 3), "\n")
    result <- nleqslv::nleqslv(
      x      = full_par[par_idx],
      fn     = objective_county,
      method = "Broyden",
      global = "dbldog",
      control = list(
        xtol  = config$obj_tol,
        ftol  = config$obj_tol,
        maxit = 200L,
        trace = 1L
      )
    )
    cat("[nleqslv] county", cnty, "done. termcd=", result$termcd,
        "  message=", result$message,
        "  ||fvec||=", sqrt(sum(result$fvec^2)),
        "  iter=", result$iter, "\n")
    full_par[par_idx] <- result$x
    county_results[[as.character(cnty)]] <- result
  }
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("[nleqslv] all counties solved in", round(elapsed, 1), "s\n")

  final_moments <- weighted_col_means(objective_gmm(
    theta = full_par,
    x = x,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = config,
    clust = clust,
    solver_state = solver_state
  ), weights = moment_weights)

  list(
    par = full_par,
    convergence = if (all(sapply(county_results, function(r) r$termcd == 1))) 0L else 1L,
    county_results = county_results,
    final_moments = final_moments,
    objective = sum(final_moments^2),
    mode = "nleqslv-county"
  )
}

estimation_result <- estimate_structural_parameters(
  working_data,
  estim_matrix,
  min_wage_levels,
  config = CONFIG,
  clust = clust,
  beta = beta,
  beta_2_subset = beta_2_subset,
  skip_structural_optimizer = CONFIG$skip_structural_optimizer
)

print(estimation_result$wage_result)
if (!isTRUE(CONFIG$skip_structural_optimizer) &&
    !is.null(estimation_result$wage_result$convergence) &&
    estimation_result$wage_result$convergence != 0) {
  warning("nleqslv did not converge in at least one county.")
}

coef_vect <- estimation_result$wage_coefficients
seeit_bb_path <- file.path(CONFIG$prep_output_dir, "seeit_bb_nleqslv.rds")
if (file.exists(seeit_bb_path)) {
  backup_path <- file.path(
    CONFIG$prep_output_dir,
    paste0("seeit_bb_nleqslv_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  )
  file.copy(seeit_bb_path, backup_path, overwrite = FALSE)
}
saveRDS(unname(coef_vect), seeit_bb_path)
message("Wrote new seeit_bb_nleqslv.rds (length ", length(coef_vect), ").")

print(estimation_result$price_result$result)
if (estimation_result$price_result$result$convergence != 0) {
  warning("L-BFGS-B did not converge. Convergence code: ",
          estimation_result$price_result$result$convergence,
          "\nMessage: ", estimation_result$price_result$result$message)
}

ensure_directory(file.path("results", "data"))
saveRDS(estimation_result$parameter_table,
        file.path("results", "data", "06_parameters_nleqslv.rds"))
message("Wrote results/data/06_parameters_nleqslv.rds")
