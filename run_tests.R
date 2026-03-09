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

if (any(as.data.frame(test_results)$failed > 0)) {
  message("Some tests FAILED. Review output above.")
} else {
  message("All tests PASSED!")
}
