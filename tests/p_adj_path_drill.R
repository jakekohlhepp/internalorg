#' Pinpoint where the bootstrap path diverges from 06.
#'
#' Two calls to estimate_structural_parameters with identical data and
#' identical (wage_coefs, beta_2_subset) starting state. The only difference
#' is the calling convention:
#'
#'   path_A ("06-style"): pass beta and beta_2_subset explicitly.
#'   path_B ("bootstrap-style"): pass starting_parameters and weights = uniform.
#'
#' If they diverge, inspect:
#'   - last_moment_norm on the global solver_state at each phase boundary
#'   - data$p_adj from a manual add_price_adjustment with the resulting wage_coefs

suppressPackageStartupMessages(library(data.table))
source("config.R")
source("preamble.R")

estimation_sample <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix
if ("min_wage_levels" %in% names(estimation_sample)) {
  min_wage_levels <- data.table(estimation_sample$min_wage_levels)
} else {
  min_wage_levels <- unique(working_data[, .(county, quarter_year, min_wage)])
}

point_parameters <- readRDS("results/data/06_parameters.rds")

# Build beta and beta_2_subset using the 06 path, plus extract beta_2_subset
# using the bootstrap path. These should be equal up to the wage block.
setup <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta <- setup$beta
beta_2 <- setup$beta_2

wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
beta_2_subset_06 <- readRDS(file.path(CONFIG$prep_output_dir, "seeit_bb.rds"))
names(beta_2_subset_06) <- rownames(beta_2)[wage_idx]

beta_2_subset_boot <- extract_wage_start(point_parameters, CONFIG)

cat("\n=== beta_2_subset alignment ===\n")
cat("06-style names:    ", paste(names(beta_2_subset_06)[1:3], collapse=" | "), "...\n")
cat("bootstrap names:   ", paste(names(beta_2_subset_boot)[1:3], collapse=" | "), "...\n")
cat("identical names:   ", identical(names(beta_2_subset_06), names(beta_2_subset_boot)), "\n")
cat("max abs diff:      ", max(abs(beta_2_subset_06 - beta_2_subset_boot[names(beta_2_subset_06)])), "\n")

# === Probe 1: does the wage solve write last_moment_norm? ===
cat("\n=== Probe 1: last_moment_norm before/after wage solve in nleqslv mode ===\n")
default_state <- get_default_solver_state(reset = TRUE)
cat("before wage solve, last_moment_norm =", default_state$last_moment_norm, "\n")

wage_result_A <- estimate_wage_parameters(
  beta_2_subset_06, estim_matrix, beta, beta_2_subset_06,
  config = CONFIG, clust = NULL, solver_state = NULL, moment_weights = NULL
)

cat("after wage solve (nleqslv path), last_moment_norm =",
    get_default_solver_state()$last_moment_norm, "\n")
cat("wage_result$convergence:", wage_result_A$convergence, "\n")
cat("(termcds:",
    paste(vapply(wage_result_A$county_results, function(r) r$termcd, integer(1)),
          collapse=" "), ")\n")

# === Probe 2: full 06-style call ===
cat("\n=== Probe 2: 06-style estimate_structural_parameters ===\n")
get_default_solver_state(reset = TRUE)
res_A <- estimate_structural_parameters(
  working_data, estim_matrix, min_wage_levels,
  config = CONFIG,
  clust = NULL,
  beta = beta,
  beta_2_subset = beta_2_subset_06,
  skip_structural_optimizer = FALSE
)
cat("price L-BFGS-B converged:", res_A$price_result$result$convergence == 0, "\n")
cat("price final f:", signif(res_A$price_result$result$value, 8), "\n")

# === Probe 3: bootstrap-style call ===
cat("\n=== Probe 3: bootstrap-style estimate_structural_parameters ===\n")
get_default_solver_state(reset = TRUE)
n_obs <- nrow(working_data)
res_B <- estimate_structural_parameters(
  working_data, estim_matrix, min_wage_levels,
  config = CONFIG,
  weights = rep(1.0 / n_obs, n_obs),
  starting_parameters = point_parameters,
  skip_structural_optimizer = FALSE
)
cat("price L-BFGS-B converged:", res_B$price_result$result$convergence == 0, "\n")
cat("price final f:", signif(res_B$price_result$result$value, 8), "\n")

# === Compare parameter tables row-by-row ===
cat("\n=== A vs B parameter_table row-by-row diff ===\n")
A <- as.data.table(res_A$parameter_table); A[, idx := seq_len(.N)]
B <- as.data.table(res_B$parameter_table); B[, idx := seq_len(.N)]
m <- merge(A, B, by = "idx", suffixes = c(".A", ".B"))
stopifnot(all(m$parm_name.A == m$parm_name.B))
m[, abs_diff := abs(coefficients.A - coefficients.B)]
m[, rel_diff := abs_diff / pmax(abs(coefficients.A), 1)]

cat("rows:", nrow(m), "\n")
cat("rows with rel_diff > 1e-9:", sum(m$rel_diff > 1e-9), "/", nrow(m), "\n")
cat("max abs diff:", signif(max(m$abs_diff), 6), "\n")
cat("max rel diff:", signif(max(m$rel_diff), 6), "\n")
saveRDS(m, "results/data/p_adj_path_drill_diff.rds")

cat("\nTop 5 abs diffs (06-style A vs bootstrap-style B):\n")
print(head(m[order(-abs_diff), .(parm_name=parm_name.A, A=coefficients.A, B=coefficients.B, abs_diff)], 5))
