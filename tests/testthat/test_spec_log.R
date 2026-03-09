# Unit tests for spec_log function
# spec_log returns 0 for x=0 or NaN, otherwise returns log(x)

context("spec_log function")

# Define spec_log locally for testing (avoids sourcing full preamble)
spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

test_that("spec_log handles zero correctly", {
  expect_equal(spec_log(0), 0)
  expect_equal(spec_log(c(0, 0, 0)), c(0, 0, 0))
})

test_that("spec_log handles NaN correctly", {
  expect_equal(spec_log(NaN), 0)
  expect_equal(spec_log(c(NaN, NaN)), c(0, 0))
})

test_that("spec_log returns log for positive values", {
  expect_equal(spec_log(1), 0)
  expect_equal(spec_log(exp(1)), 1)
  expect_equal(spec_log(exp(2)), 2)
  expect_equal(spec_log(10), log(10))
})

test_that("spec_log handles mixed input vectors", {
  input <- c(0, 1, exp(1), NaN, 10)
  expected <- c(0, 0, 1, 0, log(10))
  expect_equal(spec_log(input), expected)
})

test_that("spec_log handles very small positive numbers", {
  expect_equal(spec_log(1e-16), log(1e-16))
  expect_equal(spec_log(1e-100), log(1e-100))
})

test_that("spec_log handles negative numbers", {
  # log of negative returns NaN, but spec_log doesn't special-case this
  # This documents current behavior
  result <- spec_log(-1)
  expect_true(is.nan(result))
})
