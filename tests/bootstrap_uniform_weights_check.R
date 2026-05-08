#' Sanity check: run the bootstrap rep machinery with all-equal weights and
#' confirm the result matches 06_parameters.rds. With per-row weights set to
#' 1/N, `is_uniform_weights()` should null them out and the wage + price solve
#' should reproduce the cold 06 estimation up to numerical noise.
#'
#' Writes:
#'   results/data/uniform_weights_check.rds   (full estimation_result)
#'   results/data/uniform_weights_param_table.rds  (parameter_table only)

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

n_obs <- nrow(working_data)
weights <- rep(1.0 / n_obs, n_obs)

cat("\n=== uniform-weights bootstrap check ===\n")
cat("n_obs:", n_obs, "\n")
cat("per-row weight:", 1.0 / n_obs, "  sum:", sum(weights), "\n")
cat("is_uniform_weights():", is_uniform_weights(weights), "\n")

t0 <- Sys.time()
result <- estimate_structural_parameters(
  working_data, estim_matrix, min_wage_levels,
  config = CONFIG,
  weights = weights,
  starting_parameters = point_parameters,
  skip_structural_optimizer = FALSE
)
t1 <- Sys.time()
cat("\nElapsed:", round(as.numeric(difftime(t1, t0, units = "mins")), 2), "min\n")

dir.create("results/data", recursive = TRUE, showWarnings = FALSE)
saveRDS(result, "results/data/uniform_weights_check.rds")
saveRDS(result$parameter_table, "results/data/uniform_weights_param_table.rds")

cat("\nwage_result$convergence:", result$wage_result$convergence, "\n")
if (!is.null(result$wage_result$county_results)) {
  for (cnty in names(result$wage_result$county_results)) {
    tc <- result$wage_result$county_results[[cnty]]$termcd
    cat("  county", cnty, "nleqslv termcd:", tc, "\n")
  }
}
cat("price_result$result$convergence:", result$price_result$result$convergence, "\n")

cat("\n=== comparison: uniform-weights run vs 06_parameters.rds ===\n")
six  <- as.data.table(point_parameters)
test <- as.data.table(result$parameter_table)

common <- intersect(six$parm_name, test$parm_name)
cat("06 params:", nrow(six),
    "  test params:", nrow(test),
    "  common:", length(common), "\n")

six_v  <- setNames(six$coefficients,  six$parm_name)[common]
test_v <- setNames(test$coefficients, test$parm_name)[common]
diffs  <- test_v - six_v
abs_diffs <- abs(diffs)
ref_scale <- pmax(abs(six_v), 1.0)
rel_diffs <- abs_diffs / ref_scale

summary_dt <- data.table(
  parm_name   = common,
  six         = as.numeric(six_v),
  test        = as.numeric(test_v),
  abs_diff    = as.numeric(abs_diffs),
  rel_diff    = as.numeric(rel_diffs)
)
saveRDS(summary_dt, "results/data/uniform_weights_diff.rds")

cat(sprintf("max abs diff:    %.6g\n", max(abs_diffs)))
cat(sprintf("median abs diff: %.6g\n", median(abs_diffs)))
cat(sprintf("max rel diff:    %.6g\n", max(rel_diffs)))
cat(sprintf("median rel diff: %.6g\n", median(rel_diffs)))

cat("\nTop 10 largest abs diffs:\n")
print(head(summary_dt[order(-abs_diff)], 10))
