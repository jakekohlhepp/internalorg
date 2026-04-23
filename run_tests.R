# Run all unit tests for the estimation pipeline
#
# Usage: source("run_tests.R")
# Or from command line: Rscript run_tests.R

# Ensure testthat is available
if (!requireNamespace("testthat", quietly = TRUE)) {
  message("Installing testthat...")
  install.packages("testthat")
}

library(testthat)

# Set working directory to project root if needed
if (!file.exists("tests/testthat")) {
  stop("Please run this script from the project root directory (refactor_estimation/)")
}

message("Running unit tests for estimation pipeline...")
message("=" |> rep(60) |> paste(collapse = ""))

# Run tests
test_results <- test_dir(
  "tests/testthat",
  reporter = "summary",
  stop_on_failure = FALSE
)

# Print summary
message("\n")
message("=" |> rep(60) |> paste(collapse = ""))

summary_df <- as.data.frame(test_results)
n_passed <- sum(summary_df[["passed"]], na.rm = TRUE)
n_failed <- sum(summary_df[["failed"]], na.rm = TRUE)
n_errors <- sum(summary_df[["error"]], na.rm = TRUE)
n_warnings <- sum(summary_df[["warning"]], na.rm = TRUE)

if (n_failed > 0 || n_errors > 0) {
  message(
    sprintf(
      "Tests completed with %d failed expectations and %d file-level error(s). Review output above.",
      n_failed,
      n_errors
    )
  )
  quit(status = 1)
} else if (n_warnings > 0) {
  message(sprintf("All tests PASSED with %d warning(s) across %d passing expectations.", n_warnings, n_passed))
} else {
  message(sprintf("All tests PASSED! (%d passing expectations)", n_passed))
}
