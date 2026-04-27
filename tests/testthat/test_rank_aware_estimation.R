context("rank-aware estimation helpers")

PROJECT_ROOT <- if (file.exists("config.R")) "." else file.path("..", "..")
source(file.path(PROJECT_ROOT, "config.R"))
source(file.path(PROJECT_ROOT, "utils", "estimation_pipeline.R"))

test_that("rank_aware_solve matches solve on full-rank systems", {
  a <- matrix(c(4, 1, 1, 3), nrow = 2)
  b <- matrix(c(1, 2), ncol = 1)

  result <- rank_aware_solve(a, b, context = "test full-rank system")

  expect_equal(result, solve(a, b), tolerance = 1e-10)
})

test_that("rank_aware_solve returns finite minimum-norm solution for rank-deficient systems", {
  a <- matrix(c(1, 2, 2, 4), nrow = 2)
  b <- matrix(c(3, 6), ncol = 1)

  expect_warning(
    result <- rank_aware_solve(a, b, context = "test deficient system"),
    "rank deficient"
  )

  expect_true(all(is.finite(result)))
  expect_equal(a %*% result, b, tolerance = 1e-8)
})

test_that("rank_aware_ols keeps coefficients finite with duplicated columns", {
  x <- cbind(x1 = 1:5, x2 = 1:5)
  y <- matrix(2 * (1:5), ncol = 1)

  expect_warning(
    beta <- rank_aware_ols(x, y, context = "duplicated-column OLS"),
    "rank deficient"
  )

  expect_true(all(is.finite(beta)))
  expect_equal(as.numeric(x %*% beta), as.numeric(y), tolerance = 1e-8)
})

test_that("rank_aware_2sls keeps coefficients finite with duplicated instruments", {
  x <- cbind(x1 = 1:6, x2 = c(2, 1, 3, 5, 4, 6))
  z <- cbind(z1 = 1:6, z2 = 1:6, z3 = c(1, 0, 1, 0, 1, 0))
  y <- matrix(1 + x[, "x1"] - 0.5 * x[, "x2"], ncol = 1)

  expect_warning(
    beta <- rank_aware_2sls(x, z, y, context = "duplicated-instrument 2SLS"),
    "rank deficient"
  )

  expect_true(all(is.finite(beta)))
  expect_equal(nrow(beta), ncol(x))
})
