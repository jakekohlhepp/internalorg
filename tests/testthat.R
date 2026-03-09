# Test runner for the estimation pipeline
# Run all tests with: source("tests/testthat.R")

library(testthat)

# Source the functions we're testing
source("preamble.R", local = TRUE)

# Run all tests in the testthat directory
test_dir("tests/testthat")
