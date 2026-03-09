# Unit tests for fixed-point iteration logic
# Tests the core SQUAREM-based fixed-point solver used in the estimation

context("Fixed-point iteration")

test_that("simple fixed-point converges", {
  skip_if_not_installed("SQUAREM")
  library(SQUAREM)

  # Test with a simple contraction mapping: f(x) = 0.5*x + 1
  # Fixed point is x* = 2
  fxpt <- function(x) 0.5 * x + 1

  result <- squarem(1, fixptfn = fxpt, control = list(maxiter = 1000, tol = 1e-10))
  expect_equal(result$par, 2, tolerance = 1e-8)
})

test_that("vector fixed-point converges", {
  skip_if_not_installed("SQUAREM")
  library(SQUAREM)

  # Normalize a vector to sum to 1 (simplex projection)
  fxpt <- function(x) {
    x <- abs(x)  # Ensure positive
    x / sum(x)
  }

  start <- c(1, 2, 3, 4, 5)
  result <- squarem(start, fixptfn = fxpt, control = list(maxiter = 1000, tol = 1e-10))

  expect_equal(sum(result$par), 1, tolerance = 1e-8)
  expect_true(all(result$par > 0))
})

test_that("Sinkhorn-like iteration converges", {
  skip_if_not_installed("SQUAREM")
  library(SQUAREM)

  # This mimics the structure of the assignment model in the estimation
  # A simple 3x3 case with uniform marginals
  A <- matrix(c(1, 2, 3, 2, 1, 2, 3, 2, 1), nrow = 3, ncol = 3)
  alpha <- c(1/3, 1/3, 1/3)  # Target column marginals

  fxpt <- function(E) {
    E <- pmax(E, 1e-10)  # Prevent division by zero
    C <- colSums(t(A) * alpha / colSums(A * E))
    return(E * C)
  }

  start <- rep(1/3, 3)
  result <- squarem(start, fixptfn = fxpt, control = list(maxiter = 10000, tol = 1e-8))

  # Result should be positive and sum to 1
  expect_true(all(result$par > 0))
  expect_equal(sum(result$par), 1, tolerance = 1e-6)
})

test_that("entropy-regularized assignment produces valid matrices", {
  # Test that the B matrix construction logic is correct
  set.seed(42)

  theta <- matrix(runif(25), nrow = 5, ncol = 5)
  gamma <- 1.0  # Regularization parameter
  alpha <- rep(0.2, 5)  # Uniform task shares

  A <- exp(-1/gamma * theta)
  A[A >= Inf] <- 1e16
  A[A <= 0] <- 1e-16

  E <- rep(0.2, 5)  # Initial worker shares

  # Run a few iterations of the fixed-point
  for (i in 1:100) {
    C <- colSums(t(A) * alpha / colSums(A * E))
    E_new <- E * C
    if (max(abs(E_new - E)) < 1e-10) break
    E <- E_new
  }

  # Check that E is a valid probability distribution
  expect_true(all(E > 0), info = "All elements should be positive")
  expect_equal(sum(E), 1, tolerance = 1e-6, info = "Should sum to 1")

  # Construct B matrix
  B <- t(t(A) * alpha / colSums(A * E)) * E

  # Check B is valid
  expect_true(all(B >= 0), info = "B should be non-negative")
  expect_equal(sum(B), 1, tolerance = 1e-6, info = "B should sum to 1")
})
