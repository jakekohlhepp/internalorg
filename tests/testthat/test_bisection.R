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

## ---------------------------------------------------------------------------
## Soft-failure paths added in commit b186b1e: bisection must return
## bracket_failure = TRUE instead of stopifnot()-ing when no finite bracket
## exists. Without these, solve_worker_rows() crashes in parallel workers and
## the wage_fallbacks layered ladder cannot run on the failing seed.
## ---------------------------------------------------------------------------

test_that("bisection returns bracket_failure when fa stays NaN across the bracket", {
  config <- modifyList(BISECTION_CONFIG, list(
    bisection_low_test_point = 0.5,
    bisection_nan_step = 50
  ))
  ## Finite negative at the low-test point so a_prime stays at `a`, then NaN
  ## above x=1 so the nan-step loop has to advance until it crosses b.
  f <- function(x) if (x < 1) -1 else NaN
  result <- bisection(f, a = 1, b = 100, n = 100, xtol = 1e-6, ftol = 1e-6,
                      config = config)
  expect_false(result$conv)
  expect_true(isTRUE(result$bracket_failure))
  expect_true(is.na(result$root))
  expect_true(is.nan(result$val))
})

test_that("bisection returns bracket_failure when fa stays positive as a_prime decreases to zero", {
  config <- modifyList(BISECTION_CONFIG, list(
    bisection_low_test_point = 0.1,
    bisection_high_start_if_positive = 1,
    bisection_lower_step = 0.5
  ))
  ## Always positive: low-test point triggers a_prime = high_start_if_positive,
  ## then the fa>0 loop decrements a_prime by lower_step each iteration; it
  ## should cross zero and trigger the second bracket_failure return.
  f <- function(x) 1
  result <- bisection(f, a = 0.5, b = 100, n = 100, xtol = 1e-6, ftol = 1e-6,
                      config = config)
  expect_false(result$conv)
  expect_true(isTRUE(result$bracket_failure))
  expect_true(is.na(result$root))
})

test_that("bisection returns bracket_failure when warm bracket gives b_prime <= a_prime", {
  ## Force the final stopifnot path: pass start so the warm-bracket branch runs
  ## and the post-warm-setup check at line 473 catches a degenerate bracket.
  ## Function returns NaN everywhere so neither warm bracket nor cold setup
  ## succeed; the final guard short-circuits to bracket_failure.
  config <- modifyList(BISECTION_CONFIG, list(
    bisection_low_test_point = 0.5,
    bisection_nan_step = 1000
  ))
  f <- function(x) if (x < 1) -1 else NaN
  ## Tight (a, b) so even one nan_step jump crosses b_prime.
  result <- bisection(f, a = 1, b = 2, n = 100, xtol = 1e-6, ftol = 1e-6,
                      config = config)
  expect_false(result$conv)
  expect_true(isTRUE(result$bracket_failure))
})

test_that("bisection regression: existing successful roots still bracket_failure=NULL", {
  ## Successful roots must NOT carry the bracket_failure flag.
  f <- function(x) x - 5
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6,
                      config = BISECTION_CONFIG)
  expect_true(result$conv)
  expect_null(result$bracket_failure)
})
