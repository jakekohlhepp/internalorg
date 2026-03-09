# Unit tests for bisection function
# The bisection function finds roots of continuous functions

context("bisection function")

# Bisection config for testing
BISECTION_CONFIG <- list(
  bisection_low_test_point = 0.1,
  bisection_high_start_if_positive = 40,
  bisection_nan_step = 10,
  bisection_lower_step = 0.005,
  bisection_upper_step = 10
)

# Copy the bisection function for isolated testing
bisection <- function(f, a, b, n, xtol, ftol, config = BISECTION_CONFIG) {
  a_prime <- a
  b_prime <- b

  # when very low function value is positive, start a higher
  if (f(config$bisection_low_test_point) > 0) a_prime <- config$bisection_high_start_if_positive

  if (is.nan(f(a_prime))) {
    while (is.nan(f(a_prime))) {
      a_prime <- a_prime + config$bisection_nan_step
      stopifnot(b_prime > a_prime)
    }
  }

  while (f(a_prime) > 0) {
    a_prime <- a_prime - config$bisection_lower_step
    stopifnot(a_prime > 0)
  }

  while (f(b_prime) < 0) {
    b_prime <- b_prime + config$bisection_upper_step
  }

  stopifnot(b_prime > a_prime)

  for (i in 1:n) {
    c <- (a_prime + b_prime) / 2

    if (abs(f(c)) < ftol || ((b_prime - a_prime) / 2) < xtol) {
      return(list("root" = c, "val" = f(c), "conv" = abs(f(c)) < ftol || ((b_prime - a_prime) / 2) < xtol))
    }

    ifelse(sign(f(c)) == sign(f(a_prime)),
           a_prime <- c,
           b_prime <- c)
  }
  return(list("root" = c, "val" = f(c), "conv" = abs(f(c)) < ftol || ((b_prime - a_prime) / 2) < xtol))
}

test_that("bisection finds root of simple linear function", {
  # f(x) = x - 5, root at x = 5
  f <- function(x) x - 5
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6)
  expect_true(result$conv)
  expect_equal(result$root, 5, tolerance = 1e-5)
})

test_that("bisection finds root of quadratic function", {
  # f(x) = x^2 - 4, positive root at x = 2
  f <- function(x) x^2 - 4
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6)
  expect_true(result$conv)
  expect_equal(result$root, 2, tolerance = 1e-5)
})

test_that("bisection respects tolerance parameters", {
  f <- function(x) x - 10

  # Loose tolerance
  result_loose <- bisection(f, a = 1, b = 100, n = 100, xtol = 0.1, ftol = 0.1)
  expect_true(result_loose$conv)
  expect_equal(result_loose$root, 10, tolerance = 0.2)

  # Tight tolerance
  result_tight <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-8, ftol = 1e-8)
  expect_true(result_tight$conv)
  expect_equal(result_tight$root, 10, tolerance = 1e-7)
})

test_that("bisection returns correct convergence status", {
  f <- function(x) x - 50
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6)

  expect_true(result$conv)
  expect_lt(abs(result$val), 1e-5)
})

test_that("bisection handles logarithmic functions", {
  # f(x) = log(x) - 1, root at x = e
  # This function is negative for x < e and positive for x > e
  f <- function(x) log(x) - 1
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6)
  expect_true(result$conv)
  expect_equal(result$root, exp(1), tolerance = 1e-5)
})

test_that("bisection uses config parameters", {
  # Test with custom config
  custom_config <- list(
    bisection_low_test_point = 0.5,
    bisection_high_start_if_positive = 20,
    bisection_nan_step = 5,
    bisection_lower_step = 0.01,
    bisection_upper_step = 5
  )

  f <- function(x) x - 5
  result <- bisection(f, a = 1, b = 100, n = 1000, xtol = 1e-6, ftol = 1e-6, config = custom_config)
  expect_true(result$conv)
  expect_equal(result$root, 5, tolerance = 1e-5)
})
