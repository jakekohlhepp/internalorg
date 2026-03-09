# Unit tests for matrix operations used in IV estimation
# Tests projection matrix computation and IV estimator logic

context("Matrix operations for IV estimation")

test_that("projection matrix is idempotent", {
  # P = Z(Z'Z)^{-1}Z' should satisfy P*P = P
  set.seed(123)
  n <- 50
  k <- 3
  Z <- matrix(rnorm(n * k), nrow = n, ncol = k)

  ZtZ_inv <- solve(t(Z) %*% Z)
  P <- Z %*% ZtZ_inv %*% t(Z)

  # P^2 should equal P (idempotent property)
  P_squared <- P %*% P
  expect_equal(P, P_squared, tolerance = 1e-10)
})

test_that("projection matrix is symmetric", {
  set.seed(456)
  n <- 30
  k <- 2
  Z <- matrix(rnorm(n * k), nrow = n, ncol = k)

  ZtZ_inv <- solve(t(Z) %*% Z)
  P <- Z %*% ZtZ_inv %*% t(Z)

  expect_equal(P, t(P), tolerance = 1e-10)
})

test_that("IV estimator equals OLS when X = Z", {
  # When instruments equal regressors, IV should equal OLS
  set.seed(789)
  n <- 100
  k <- 3

  X <- matrix(rnorm(n * k), nrow = n, ncol = k)
  beta_true <- c(1, 2, 3)
  y <- X %*% beta_true + rnorm(n, sd = 0.1)

  # OLS
  beta_ols <- solve(t(X) %*% X) %*% t(X) %*% y

  # IV with Z = X
  ZtZ_inv <- solve(t(X) %*% X)
  proj_z <- X %*% ZtZ_inv %*% t(X)
  beta_iv <- solve(t(X) %*% proj_z %*% X) %*% (t(X) %*% proj_z %*% y)

  expect_equal(as.vector(beta_ols), as.vector(beta_iv), tolerance = 1e-10)
})

test_that("precomputed projection gives same result as direct computation", {
  # Tests that the optimization in preamble.R (precomputing ZtZ_inv) is correct
  set.seed(111)
  n <- 40
  k <- 4
  p <- 3

  Z <- matrix(rnorm(n * k), nrow = n, ncol = k)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- rnorm(n)

  # Direct computation (inefficient - computes solve twice)
  beta_direct <- solve(t(X) %*% Z %*% solve(t(Z) %*% Z) %*% t(Z) %*% X) %*%
                 (t(X) %*% Z %*% solve(t(Z) %*% Z) %*% t(Z) %*% y)

  # Precomputed (efficient)
  ZtZ_inv <- solve(t(Z) %*% Z)
  proj_z <- Z %*% ZtZ_inv %*% t(Z)
  beta_precomputed <- solve(t(X) %*% proj_z %*% X) %*%
                      (t(X) %*% proj_z %*% y)

  expect_equal(as.vector(beta_direct), as.vector(beta_precomputed), tolerance = 1e-10)
})

test_that("singular matrix detection works", {
  # Tests that we properly detect collinearity
  n <- 50
  k <- 3

  # Create a singular matrix (column 3 = column 1 + column 2)
  Z <- matrix(rnorm(n * 2), nrow = n, ncol = 2)
  Z <- cbind(Z, Z[, 1] + Z[, 2])

  expect_error(solve(t(Z) %*% Z), "singular")
})
