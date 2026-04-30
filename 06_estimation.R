#' =============================================================================
#' STEP 06: Structural Estimation
#' =============================================================================
#' Estimates the structural model on the estimation-ready sample assembled by
#' 04_estimation_sample.R.
#'
#' Inputs:
#'   - mkdata/data/04_estimation_sample.rds
#'   - mkdata/data/seeit_bb.rds
#'
#' Output:
#'   - results/data/06_parameters.rds
#' =============================================================================

library("data.table")
set.seed(4459665)

source("config.R")

message("Tolerances: innertol=", CONFIG$innertol,
        ", outertol=", CONFIG$outertol, ", obj_tol=", CONFIG$obj_tol,
        " | staged: coarse=", CONFIG$coarse_innertol, "/", CONFIG$coarse_outertol,
        " switch_norm=", CONFIG$staged_tolerance_switch_norm,
        " | wage_optimizer_mode=", CONFIG$wage_optimizer_mode)

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

## get starting point.
beta_2_subset <- readRDS(file.path(CONFIG$prep_output_dir, "seeit_bb.rds"))
wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
names(beta_2_subset) <- rownames(beta_2)[wage_idx]
stopifnot(length(beta_2_subset) == sum(wage_idx),
          length(beta_2_subset) > 0)

## ---------------------------------------------------------------------------
## BBsolve warm-restart checkpoint
## ---------------------------------------------------------------------------
bb_warmstart_path <- file.path(CONFIG$prep_output_dir, "06_bb_warmstart.rds")
if (file.exists(bb_warmstart_path)) {
  bb_warmstart <- tryCatch(readRDS(bb_warmstart_path), error = function(e) NULL)
  if (!is.null(bb_warmstart) && length(bb_warmstart) == length(beta_2_subset)) {
    message("BBsolve warm-start: loading ", length(bb_warmstart),
            "-element checkpoint from ", bb_warmstart_path)
    beta_2_subset <- setNames(as.numeric(bb_warmstart), names(beta_2_subset))
  } else {
    warning("BBsolve warm-start file exists at ", bb_warmstart_path,
            " but is unusable (NULL or wrong length); falling back to seeit_bb.rds")
  }
}

if (isTRUE(CONFIG$bb_checkpoint_enabled) &&
    tolower(CONFIG$wage_optimizer_mode) %in% c("joint", "county")) {
  .bb_orig_objective_gmm <- objective_gmm
  .bb_checkpoint_call_count <- 0L
  .bb_checkpoint_every <- max(1L, as.integer(CONFIG$bb_checkpoint_every))
  objective_gmm <- function(theta, ...) {
    .bb_checkpoint_call_count <<- .bb_checkpoint_call_count + 1L
    if (.bb_checkpoint_call_count %% .bb_checkpoint_every == 1L) {
      tryCatch({
        tmp_path <- paste0(bb_warmstart_path, ".tmp")
        saveRDS(theta, tmp_path)
        file.rename(tmp_path, bb_warmstart_path)
      }, error = function(e) warning("BBsolve checkpoint write failed: ", e$message))
    }
    .bb_orig_objective_gmm(theta, ...)
  }
  message("BBsolve checkpoint enabled: writing every ",
          .bb_checkpoint_every, " call(s) to ", bb_warmstart_path)
}

message("Starting Windows solver cluster...")
clust <- make_windows_solver_cluster(CONFIG)
message("Windows solver cluster ready.")
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

if (isTRUE(CONFIG$skip_structural_optimizer)) {
  message(
    "Skipping BBsolve and reusing mkdata/data/seeit_bb.rds because ",
    "JMP_SKIP_STRUCTURAL_OPTIMIZER=true."
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
  warning("BBsolve did not converge. Convergence code: ",
          estimation_result$wage_result$convergence,
          "\nResults may be unreliable. Consider adjusting tolerances or starting values.")
}

coef_vect <- estimation_result$wage_coefficients
seeit_bb_path <- file.path(CONFIG$prep_output_dir, "seeit_bb.rds")
if (file.exists(seeit_bb_path)) {
  backup_path <- file.path(
    CONFIG$prep_output_dir,
    paste0("seeit_bb_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  )
  file.copy(seeit_bb_path, backup_path, overwrite = FALSE)
  message("Backed up previous seeit_bb.rds to ", backup_path)
}
saveRDS(unname(coef_vect), seeit_bb_path)
message("Wrote new seeit_bb.rds starting vector (length ", length(coef_vect), ").")

print(estimation_result$price_result$result)
if (estimation_result$price_result$result$convergence != 0) {
  warning("L-BFGS-B did not converge. Convergence code: ",
          estimation_result$price_result$result$convergence,
          "\nMessage: ", estimation_result$price_result$result$message,
          "\nResults may be unreliable.")
}

ensure_directory(file.path("results", "data"))
saveRDS(estimation_result$parameter_table, file.path("results", "data", "06_parameters.rds"))
