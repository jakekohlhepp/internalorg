#' =============================================================================
#' Bit-identity verification for the wage-stage scheduling changes
#' (county-parallel PSO, parallel L5 joint multistart, L4 polish-only).
#'
#' Runs the FULL wage stage (estimate_wage_parameters: dispatcher + fallback
#' ladder) on a row-subset of the real estimation sample with a scaled-down
#' PSO, then saves everything the scheduling changes could plausibly perturb:
#'   - the wage result object (parameters, objectives, per-county details,
#'     fallback-layer reports)
#'   - the global solver warm-start cache (the price stage consumes it via
#'     get_gammas(), so it must match bit-for-bit too)
#'   - the final .Random.seed
#'
#' Usage:
#'   Rscript diagnostics/verify_wage_stage_bitident.R <tag>
#'
#' Env:
#'   BASELINE_OVERLAY=<dir>  source pre-change copies of structural_solver.R
#'                           and wage_fallbacks.R over the current tree, i.e.
#'                           run the old solver code.
#'   JMP_SMOKE_N_ROWS        rows kept per county (default 8).
#'   plus the usual JMP_* solver knobs -- set IDENTICALLY across compared runs
#'   (particles, iters, fallback maxits, L5 K, checkpoint path "none").
#'
#' Compare two tags byte-for-byte with:
#'   Rscript -e 'a <- readRDS("diagnostics/out/bitident_A.rds");
#'               b <- readRDS("diagnostics/out/bitident_B.rds");
#'               stopifnot(identical(serialize(a, NULL), serialize(b, NULL)))'
#' =============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("usage: verify_wage_stage_bitident.R <tag>")
tag <- args[[1]]

library("data.table")
set.seed(4459665)  # mirror 06_estimation.R

source("config.R")

estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
estimation_sample <- readRDS(estimation_sample_path)
working_data <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix

source("preamble.R")

overlay <- Sys.getenv("BASELINE_OVERLAY", unset = "")
if (nzchar(overlay)) {
  message("Sourcing baseline (pre-change) solver overlay from ", overlay)
  source(file.path(overlay, "structural_solver.R"))
  source(file.path(overlay, "wage_fallbacks.R"))
}

estimation_objects <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta <- estimation_objects$beta
beta_2 <- estimation_objects$beta_2
rm(estimation_objects)

beta_2_subset <- readRDS(file.path(CONFIG$prep_output_dir, "seeit_bb.rds"))
wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
names(beta_2_subset) <- rownames(beta_2)[wage_idx]
stopifnot(length(beta_2_subset) == sum(wage_idx), length(beta_2_subset) > 0)

## Row subset: first N rows per county, original order. Small enough that a
## full L1-L5 ladder pass runs in minutes; real data so every code path
## (staged tolerances, interior penalty, bound guard) is exercised.
n_keep <- as.integer(Sys.getenv("JMP_SMOKE_N_ROWS", unset = "8"))
county_vec <- as.character(as.data.frame(estim_matrix)$county)
keep <- sort(unlist(lapply(split(seq_along(county_vec), county_vec), head, n_keep),
             use.names = FALSE))
x_sub <- estim_matrix[keep, , drop = FALSE]
message("Smoke sample: ", length(keep), " of ", length(county_vec),
        " rows (", n_keep, " per county); mode=", CONFIG$wage_optimizer_mode,
        "; county_parallel=", isTRUE(CONFIG$wage_pso_county_parallel),
        "; l5_parallel=", isTRUE(CONFIG$wage_fallback_joint_multistart_parallel),
        "; l4_polish_only=", isTRUE(CONFIG$wage_fallback_repso_polish_only),
        "; cores=", get_core_count(CONFIG))

clust <- make_windows_solver_cluster(CONFIG)
if (!is.null(clust)) on.exit(parallel::stopCluster(clust), add = TRUE)

t0 <- Sys.time()
wage_result <- estimate_wage_parameters(
  beta_2_subset, x_sub, beta, beta_2_subset,
  config = CONFIG, clust = clust
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
message(sprintf("Wage stage done in %.1f s; objective=%.15g convergence=%d",
                elapsed, wage_result$objective, wage_result$convergence))

## Global solver cache snapshot. Sort keys: environment listing order is not
## stable, and key SETS legitimately differ across scheduling (a serial run's
## cache holds every county's entries; so must a parallel run's after the
## merge). Values are the bit-level payload the price stage warm-starts from.
st <- get_default_solver_state()
gamma_cache <- as.list(st$gamma_by_key)
gamma_cache <- gamma_cache[order(names(gamma_cache))]
E_cache <- as.list(st$E_by_key)
E_cache <- E_cache[order(names(E_cache))]
state_snapshot <- list(
  gamma = gamma_cache,
  E = E_cache,
  last_moment_norm = st$last_moment_norm,
  n_objective_calls = st$n_objective_calls
)

rng_final <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
  get(".Random.seed", envir = globalenv(), inherits = FALSE)
} else {
  NULL
}

dir.create(file.path("diagnostics", "out"), showWarnings = FALSE, recursive = TRUE)
out_path <- file.path("diagnostics", "out", paste0("bitident_", tag, ".rds"))
saveRDS(list(result = wage_result, state = state_snapshot, rng = rng_final),
        out_path)
message("Saved ", out_path)
