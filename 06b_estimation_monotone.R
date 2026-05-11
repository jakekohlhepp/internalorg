#' =============================================================================
#' STEP 06b: Structural Estimation under workers-as-rows monotonicity
#' =============================================================================
#' Mirrors 06_estimation.R but imposes the workers-as-rows monotonicity
#' restriction on each county's skill matrix B[task, worker]. The demand-IV
#' step is replaced by a constrained 2SLS QP that searches all n_w!
#' permutations of worker types per county and picks the one minimizing the
#' constrained 2SLS criterion (utils/constrained_demand_iv.R). The wage and
#' price stages downstream consume the constrained beta unchanged.
#'
#' Inputs:
#'   - mkdata/data/04_estimation_sample.rds
#'   - mkdata/data/seeit_bb.rds  (or mkdata/data/06b_seeit_bb.rds if present)
#'
#' Outputs:
#'   - results/data/06b_parameters_monotone.rds
#'   - results/data/06b_perms.rds
#'   - results/data/06b_qp_diagnostics.rds
#'   - mkdata/data/06b_seeit_bb.rds
#' =============================================================================

library("data.table")
set.seed(4459665)

source("config.R")
CONFIG$skill_monotone_orientation <- "workers_rows"

message("Tolerances: innertol=", CONFIG$innertol,
        ", outertol=", CONFIG$outertol, ", obj_tol=", CONFIG$obj_tol,
        " | staged: coarse=", CONFIG$coarse_innertol, "/", CONFIG$coarse_outertol,
        " switch_norm=", CONFIG$staged_tolerance_switch_norm,
        " | wage_optimizer_mode=", CONFIG$wage_optimizer_mode,
        " | skill_monotone=", CONFIG$skill_monotone_orientation)

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
monotone_fit <- estimation_objects$skill_monotone_fit
stopifnot(!is.null(monotone_fit))
rm(estimation_objects)

## persist the QP search diagnostics now (before any solver risk).
ensure_directory(file.path("results", "data"))
saveRDS(
  list(perms_by_county = monotone_fit$perms_by_county),
  file.path("results", "data", "06b_perms.rds")
)
saveRDS(
  list(
    obj_by_perm_by_county = monotone_fit$obj_by_perm_by_county,
    obj_by_county         = monotone_fit$obj_by_county,
    status_by_county      = monotone_fit$status_by_county,
    ridge_by_county       = monotone_fit$ridge_by_county,
    best_idx_by_county    = monotone_fit$best_idx_by_county,
    perms                 = monotone_fit$perms
  ),
  file.path("results", "data", "06b_qp_diagnostics.rds")
)

## Permutation-search readout: chosen pi per county and the gap to the
## next-best permutation. A near-tie (<1% gap) flags weak identification of
## the worker-skill ranking and is reported as a warning, not silently
## resolved.
for (cnty in CONFIG$counties) {
  obj_vec <- monotone_fit$obj_by_perm_by_county[[as.character(cnty)]]
  best_idx <- monotone_fit$best_idx_by_county[[as.character(cnty)]]
  best_obj <- obj_vec[best_idx]
  next_obj <- min(obj_vec[-best_idx], na.rm = TRUE)
  gap <- next_obj - best_obj
  rel_gap <- if (best_obj > 0) gap / best_obj else gap
  message(sprintf(
    "[06b] county %s: chosen perm = (%s); chosen obj = %.6g; next-best obj = %.6g; gap = %.6g (relative %.4f)",
    cnty,
    paste(monotone_fit$perms_by_county[[as.character(cnty)]], collapse = " -> "),
    best_obj, next_obj, gap, rel_gap
  ))
  if (is.finite(rel_gap) && rel_gap < 0.01) {
    warning(
      "[06b] county ", cnty,
      ": chosen worker-skill ordering is weakly identified (relative QP-objective gap to next-best perm = ",
      signif(rel_gap, 3), "). Reported permutation may be noise-sensitive.",
      call. = FALSE
    )
  }
}

## get starting point for wage solver. Prefer 06b's own warm-start file if
## present; otherwise fall back to seeit_bb.rds from the unconstrained run.
seeit_bb_06b_path <- file.path(CONFIG$prep_output_dir, "06b_seeit_bb.rds")
if (file.exists(seeit_bb_06b_path)) {
  message("Loading wage starting values from ", seeit_bb_06b_path,
          " (06b warm-restart).")
  beta_2_subset <- readRDS(seeit_bb_06b_path)
} else {
  fallback_path <- file.path(CONFIG$prep_output_dir, "seeit_bb.rds")
  message("No 06b_seeit_bb.rds yet; bootstrapping wage starting values from ",
          fallback_path)
  beta_2_subset <- readRDS(fallback_path)
}
wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
names(beta_2_subset) <- rownames(beta_2)[wage_idx]
stopifnot(length(beta_2_subset) == sum(wage_idx),
          length(beta_2_subset) > 0)

## ---------------------------------------------------------------------------
## BBsolve warm-restart checkpoint (06b-specific path).
## ---------------------------------------------------------------------------
bb_warmstart_path <- file.path(CONFIG$prep_output_dir, "06b_bb_warmstart.rds")
if (file.exists(bb_warmstart_path)) {
  bb_warmstart <- tryCatch(readRDS(bb_warmstart_path), error = function(e) NULL)
  if (!is.null(bb_warmstart) && length(bb_warmstart) == length(beta_2_subset)) {
    message("BBsolve warm-start: loading ", length(bb_warmstart),
            "-element checkpoint from ", bb_warmstart_path)
    beta_2_subset <- setNames(as.numeric(bb_warmstart), names(beta_2_subset))
  } else {
    warning("BBsolve warm-start file exists at ", bb_warmstart_path,
            " but is unusable (NULL or wrong length); falling back.",
            call. = FALSE)
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
    "Skipping wage solver (mode=", CONFIG$wage_optimizer_mode,
    ") because JMP_SKIP_STRUCTURAL_OPTIMIZER=true."
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
  warning("[06b] Wage solver (mode=", CONFIG$wage_optimizer_mode,
          ") did not converge. Convergence code: ",
          estimation_result$wage_result$convergence,
          "\nResults may be unreliable.",
          call. = FALSE)
}

coef_vect <- estimation_result$wage_coefficients
if (file.exists(seeit_bb_06b_path)) {
  backup_path <- file.path(
    CONFIG$prep_output_dir,
    paste0("06b_seeit_bb_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  )
  file.copy(seeit_bb_06b_path, backup_path, overwrite = FALSE)
  message("Backed up previous 06b_seeit_bb.rds to ", backup_path)
}
saveRDS(unname(coef_vect), seeit_bb_06b_path)
message("Wrote 06b wage starting vector (length ", length(coef_vect), ") to ",
        seeit_bb_06b_path)

print(estimation_result$price_result$result)
if (estimation_result$price_result$result$convergence != 0) {
  warning("[06b] L-BFGS-B price step did not converge. Convergence code: ",
          estimation_result$price_result$result$convergence,
          "\nMessage: ", estimation_result$price_result$result$message,
          "\nResults may be unreliable.",
          call. = FALSE)
}

saveRDS(
  estimation_result$parameter_table,
  file.path("results", "data", "06b_parameters_monotone.rds")
)

## --------------------------------------------------------------------------
## Comparison readout: per-county Frobenius distance from the unconstrained
## skill matrix to the constrained one. Useful for diagnosing how much each
## county's productivity matrix moved under the restriction.
## --------------------------------------------------------------------------
unc_path <- file.path("results", "data", "06_parameters.rds")
if (file.exists(unc_path)) {
  unc_params <- readRDS(unc_path)
  unc_table  <- data.table::as.data.table(unc_params)
  con_table  <- data.table::as.data.table(estimation_result$parameter_table)

  for (cnty in CONFIG$counties) {
    pat <- paste0("factor\\(county\\)", cnty, ":avg_labor:B_raw_")
    unc_b <- unc_table[grep(pat, parm_name)]
    con_b <- con_table[grep(pat, parm_name)]
    setkey(unc_b, parm_name); setkey(con_b, parm_name)
    merged <- merge(unc_b[, .(parm_name, unc = coefficients)],
                    con_b[, .(parm_name, con = coefficients)],
                    by = "parm_name")
    fro_unc <- sqrt(sum(merged$unc^2))
    fro_diff <- sqrt(sum((merged$unc - merged$con)^2))
    rel <- if (fro_unc > 0) fro_diff / fro_unc else NA_real_
    message(sprintf(
      "[06b] county %s: ||B_unc - B_con||_F = %.4f; ||B_unc||_F = %.4f; relative = %.4f",
      cnty, fro_diff, fro_unc, rel
    ))
  }
} else {
  message("[06b] Skipping unconstrained-vs-constrained comparison: ",
          unc_path, " not found.")
}
