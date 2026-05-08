#' Drill-down: hold wage_coefs fixed and ask whether get_gammas() output is
#' deterministic with respect to solver_state across realistic state contents.
#'
#' Three runs, identical inputs except solver_state:
#'   (1) state = NULL                       -> default solver_state, fine tols
#'   (2) state = high last_moment_norm      -> coarse tols (mimics state after a wage solve that did not tightly converge)
#'   (3) state = low  last_moment_norm      -> fine tols
#'
#' If gamma_invert differs across (1)/(2)/(3), the cache-pollution / staged-
#' tolerance hypothesis explains why the uniform-weights bootstrap doesn't
#' reproduce 06_parameters.rds: the wage solve leaves the state in regime
#' (2) or (3) non-deterministically, and get_gammas under different
#' tolerances picks slightly different gamma values, propagating to p_adj
#' and to the L-BFGS-B price-step minimum.
#'
#' Output:
#'   results/data/gamma_cache_drill.rds  (data.table comparing gamma vectors)

suppressPackageStartupMessages(library(data.table))
source("config.R")
source("preamble.R")

estimation_sample <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix

point_parameters <- readRDS("results/data/06_parameters.rds")

beta <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)$beta
beta_2_subset <- extract_wage_start(point_parameters, CONFIG)
wage_coefs <- setNames(numeric(length(beta_2_subset)), names(beta_2_subset))
for (nm in names(beta_2_subset)) {
  match_idx <- which(point_parameters$parm_name == nm)
  stopifnot(length(match_idx) >= 1L)
  wage_coefs[nm] <- point_parameters$coefficients[match_idx[1]]
}

run_get_gammas <- function(state) {
  get_gammas(
    wage_coefs, estim_matrix,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = CONFIG,
    solver_state = state
  )
}

state_fresh <- make_solver_state()
gamma_fresh <- run_get_gammas(state_fresh)

state_coarse <- make_solver_state()
state_coarse$last_moment_norm <- 1.0
gamma_coarse <- run_get_gammas(state_coarse)

state_fine <- make_solver_state()
state_fine$last_moment_norm <- 1e-9
gamma_fine <- run_get_gammas(state_fine)

stopifnot(length(gamma_fresh) == length(gamma_coarse),
          length(gamma_fresh) == length(gamma_fine))

drill <- data.table(
  row_id = seq_along(gamma_fresh),
  gamma_fresh  = gamma_fresh,
  gamma_coarse = gamma_coarse,
  gamma_fine   = gamma_fine,
  diff_fresh_vs_fine   = gamma_fresh - gamma_fine,
  diff_coarse_vs_fine  = gamma_coarse - gamma_fine,
  diff_fresh_vs_coarse = gamma_fresh - gamma_coarse
)
saveRDS(drill, "results/data/gamma_cache_drill.rds")

cat("\n=== get_gammas determinism check ===\n")
cat("rows:", nrow(drill), "\n")
cat(sprintf("max |gamma_fresh  - gamma_fine|:   %.6g\n", max(abs(drill$diff_fresh_vs_fine))))
cat(sprintf("max |gamma_coarse - gamma_fine|:   %.6g\n", max(abs(drill$diff_coarse_vs_fine))))
cat(sprintf("max |gamma_fresh  - gamma_coarse|: %.6g\n", max(abs(drill$diff_fresh_vs_coarse))))
cat(sprintf("median |gamma_coarse - gamma_fine|: %.6g\n", median(abs(drill$diff_coarse_vs_fine))))
cat(sprintf("rows where coarse != fine (>1e-12): %d / %d\n",
            sum(abs(drill$diff_coarse_vs_fine) > 1e-12), nrow(drill)))

cat("\n--- top 10 largest |gamma_coarse - gamma_fine| ---\n")
print(head(drill[order(-abs(diff_coarse_vs_fine))], 10))
