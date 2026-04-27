# Unit tests for the live bisection function

context("bisection function")

PROJECT_ROOT <- if (file.exists("config.R")) "." else file.path("..", "..")
source(file.path(PROJECT_ROOT, "config.R"))
source(file.path(PROJECT_ROOT, "preamble.R"), chdir = TRUE)

BISECTION_CONFIG <- modifyList(CONFIG, list(
  bisection_low_test_point = 0.1,
  bisection_high_start_if_positive = 40,
  bisection_nan_step = 10,
  bisection_lower_step = 0.005,
  bisection_upper_step = 10
))

test_that("bisection finds root of simple linear function", {
  f <- function(x) x - 5
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6,
                      config = BISECTION_CONFIG)
  expect_true(result$conv)
  expect_equal(result$root, 5, tolerance = 1e-5)
})

test_that("bisection finds root of quadratic function", {
  f <- function(x) x^2 - 4
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6,
                      config = BISECTION_CONFIG)
  expect_true(result$conv)
  expect_equal(result$root, 2, tolerance = 1e-5)
})

test_that("bisection respects tolerance parameters", {
  f <- function(x) x - 10

  result_loose <- bisection(f, a = 1, b = 100, n = 100, xtol = 0.1, ftol = 0.1,
                            config = BISECTION_CONFIG)
  expect_true(result_loose$conv)
  expect_equal(result_loose$root, 10, tolerance = 0.2)

  result_tight <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-8, ftol = 1e-8,
                            config = BISECTION_CONFIG)
  expect_true(result_tight$conv)
  expect_equal(result_tight$root, 10, tolerance = 1e-7)
})

test_that("bisection returns correct convergence status", {
  f <- function(x) x - 50
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6,
                      config = BISECTION_CONFIG)

  expect_true(result$conv)
  expect_lt(abs(result$val), 1e-5)
})

test_that("bisection handles logarithmic functions", {
  f <- function(x) log(x) - 1
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6,
                      config = BISECTION_CONFIG)
  expect_true(result$conv)
  expect_equal(result$root, exp(1), tolerance = 1e-5)
})

test_that("bisection uses config parameters", {
  custom_config <- modifyList(BISECTION_CONFIG, list(
    bisection_low_test_point = 0.5,
    bisection_high_start_if_positive = 20,
    bisection_nan_step = 5,
    bisection_lower_step = 0.01,
    bisection_upper_step = 5
  ))

  f <- function(x) x - 5
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6,
                      config = custom_config)
  expect_true(result$conv)
  expect_equal(result$root, 5, tolerance = 1e-5)
})
