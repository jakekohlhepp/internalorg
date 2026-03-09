# Unit tests for core model functions
# Tests solve_equilibrium, compute_corner_solution, find_gamma_for_sindex

context("Core model functions")

# Setup: Define minimal config and helper functions for testing
TEST_CONFIG <- list(
  counties = c("17031", "36061", "6037"),
  counties_padded = c("17031", "36061", "06037"),
  n_worker_types = 3,
  n_task_types = 3,
  initial_E = NULL,
  numeric_ceiling = 1e16,
  numeric_floor = 1e-16,
  B_zero_threshold = 1e-16,
  bisection_lower = 1,
  bisection_upper = 10000,
  bisection_max_iter = 10000,
  bisection_low_test_point = 0.1,
  bisection_high_start_if_positive = 40,
  bisection_nan_step = 10,
  bisection_lower_step = 0.005,
  bisection_upper_step = 10,
  fixedpoint_max_iter = 100000,
  innertol = 1e-06,
  outertol = 1e-04,
  obj_tol = 1e-04,
  pl_on = TRUE,
  core_count = NULL,
  task_mix_pattern = "task_mix_",
  E_raw_pattern = "E_raw_",
  B_raw_pattern = "B_raw_",
  dye_task_index = 2
)

# Helper functions (copied from config.R for isolated testing)
get_initial_E <- function(config) {
  if (is.null(config$initial_E)) {
    return(rep(1 / config$n_worker_types, config$n_worker_types))
  }
  return(config$initial_E / sum(config$initial_E))
}

spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

# Bisection function (copied from preamble.R, with config parameter)
bisection <- function(f, a, b, n, xtol, ftol, config = TEST_CONFIG) {
  a_prime <- a
  b_prime <- b
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
    ifelse(sign(f(c)) == sign(f(a_prime)), a_prime <- c, b_prime <- c)
  }
  return(list("root" = c, "val" = f(c), "conv" = abs(f(c)) < ftol || ((b_prime - a_prime) / 2) < xtol))
}

# solve_equilibrium function (copied from preamble.R)
solve_equilibrium <- function(cost_matrix, alpha, gamma, innertol, config = TEST_CONFIG) {
  n_w <- nrow(cost_matrix)
  n_t <- ncol(cost_matrix)

  log_A <- -cost_matrix / gamma
  log_A_max <- max(log_A)
  log_A_shifted <- log_A - log_A_max
  A <- exp(log_A_shifted)
  A <- pmax(A, config$numeric_floor)
  A <- pmin(A, config$numeric_ceiling)
  A_t <- t(A)
  E <- get_initial_E(config)
  if (length(E) != n_w) {
    E <- rep(1/n_w, n_w)
  }
  fxpt <- function(p) {
    col_sums <- as.vector(A_t %*% p)
    col_sums <- pmax(col_sums, config$numeric_floor)
    task_scales <- alpha / col_sums
    update_factors <- as.vector(A %*% task_scales)
    return(p * update_factors)
  }
  result <- SQUAREM::squarem(E, fixptfn = fxpt,
                    control = list(maxiter = config$fixedpoint_max_iter, tol = innertol))
  E <- result$par
  converged <- (result$convergence == 0)
  E <- pmax(E, config$numeric_floor)  # Ensure positive
  E <- E / sum(E)  # Normalize
  col_sums <- as.vector(A_t %*% E)
  col_sums <- pmax(col_sums, config$numeric_floor)
  B <- A * E
  B <- t(t(B) * alpha / col_sums)
  B[abs(B) < config$B_zero_threshold] <- 0
  E_safe <- pmax(E, config$numeric_floor)
  alpha_safe <- pmax(alpha, config$numeric_floor)
  B_rel <- B / E_safe
  B_rel <- t(t(B_rel) / alpha_safe)
  entropy <- sum(B * spec_log(B_rel))
  return(list(E = E, B = B, A = A, converged = converged, entropy = entropy))
}

# compute_corner_solution function (copied from preamble.R)
compute_corner_solution <- function(cost_matrix, alpha, config = TEST_CONFIG) {
  n_w <- nrow(cost_matrix)
  n_t <- ncol(cost_matrix)
  B <- matrix(0, nrow = n_w, ncol = n_t)
  for (j in 1:n_t) {
    best_worker <- which.min(cost_matrix[, j])
    B[best_worker, j] <- alpha[j]
  }
  E <- rowSums(B)
  E_safe <- pmax(E, config$numeric_floor)
  alpha_safe <- pmax(alpha, config$numeric_floor)
  B_rel <- B / E_safe
  B_rel <- t(t(B_rel) / alpha_safe)
  entropy_bound <- sum(B * spec_log(B_rel))
  return(list(E = E, B = B, entropy_bound = entropy_bound))
}

# find_gamma_for_sindex function (copied from preamble.R)
find_gamma_for_sindex <- function(cost_matrix, alpha, s_index, innertol, outertol, config = TEST_CONFIG) {
  corner <- compute_corner_solution(cost_matrix, alpha, config)

  # Handle NaN in entropy_bound
  if (is.na(corner$entropy_bound) || is.nan(corner$entropy_bound)) {
    corner$entropy_bound <- Inf
  }

  if (s_index > corner$entropy_bound) {
    return(list(gamma = 0, E = corner$E, B = corner$B, is_corner = TRUE,
                converged = TRUE, entropy = corner$entropy_bound))
  }
  objective <- function(gamma) {
    eq <- solve_equilibrium(cost_matrix, alpha, gamma, innertol, config)
    return(s_index - eq$entropy)
  }
  result <- bisection(f = objective, a = config$bisection_lower, b = config$bisection_upper,
                      n = config$bisection_max_iter, xtol = outertol, ftol = outertol, config = config)
  if (!result$conv) {
    warning("Bisection did not converge for s_index = ", s_index)
  }
  gamma_final <- result$root
  eq_final <- solve_equilibrium(cost_matrix, alpha, gamma_final, innertol, config)
  return(list(gamma = gamma_final, E = eq_final$E, B = eq_final$B,
              is_corner = FALSE, converged = result$conv, entropy = eq_final$entropy))
}

# ===========================================================================
# Tests for solve_equilibrium
# ===========================================================================

test_that("solve_equilibrium returns valid probability distribution for E", {
  skip_if_not_installed("SQUAREM")

  # Simple 3x3 cost matrix
  cost_matrix <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.3, 0.4, 0.3)
  gamma <- 2.0  # Use moderate gamma for stability
  innertol <- 1e-6

  result <- solve_equilibrium(cost_matrix, alpha, gamma, innertol, TEST_CONFIG)

  # E should sum to 1
  expect_equal(sum(result$E), 1, tolerance = 1e-4)

  # E should be all positive
  expect_true(all(result$E > 0))

  # No NaN values
  expect_true(all(is.finite(result$E)))
})

test_that("solve_equilibrium returns valid assignment matrix B", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.3, 0.4, 0.3)
  gamma <- 2.0
  innertol <- 1e-6

  result <- solve_equilibrium(cost_matrix, alpha, gamma, innertol, TEST_CONFIG)

  # B should sum to 1
  expect_equal(sum(result$B), 1, tolerance = 1e-4)

  # B should be non-negative
  expect_true(all(result$B >= 0))

  # Row sums should approximately equal E
  expect_equal(rowSums(result$B), result$E, tolerance = 1e-4)

  # Column sums should approximately equal alpha
  expect_equal(colSums(result$B), alpha, tolerance = 1e-4)

  # No NaN values
  expect_true(all(is.finite(result$B)))
})

test_that("solve_equilibrium handles different gamma values", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.3, 0.4, 0.3)
  innertol <- 1e-5

  # High gamma (high temperature) - more mixing
  result_high <- solve_equilibrium(cost_matrix, alpha, 10, innertol, TEST_CONFIG)
  expect_equal(sum(result_high$E), 1, tolerance = 1e-4)
  expect_true(all(is.finite(result_high$E)))

  # Moderate gamma
  result_mod <- solve_equilibrium(cost_matrix, alpha, 2, innertol, TEST_CONFIG)
  expect_equal(sum(result_mod$E), 1, tolerance = 1e-4)
  expect_true(all(is.finite(result_mod$E)))
})

test_that("solve_equilibrium entropy relationship with gamma", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 2, 3,
    2, 0, 2,
    3, 2, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.33, 0.34, 0.33)
  innertol <- 1e-5

  # Higher gamma means more mixing, lower specialization entropy
  result_high <- solve_equilibrium(cost_matrix, alpha, 5.0, innertol, TEST_CONFIG)
  result_low <- solve_equilibrium(cost_matrix, alpha, 1.0, innertol, TEST_CONFIG)

  # Both should have valid entropy values
  expect_true(is.finite(result_high$entropy))
  expect_true(is.finite(result_low$entropy))

  # Lower gamma should give higher entropy (more specialization)
  expect_true(result_low$entropy >= result_high$entropy - 0.1)  # Allow small tolerance
})

# ===========================================================================
# Tests for compute_corner_solution
# ===========================================================================

test_that("compute_corner_solution assigns tasks to lowest-cost workers", {
  # Cost matrix where worker 1 is best at task 1, worker 2 at task 2, etc.
  cost_matrix <- matrix(c(
    0, 5, 5,
    5, 0, 5,
    5, 5, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.3, 0.4, 0.3)

  result <- compute_corner_solution(cost_matrix, alpha, TEST_CONFIG)

  # B should have all mass on diagonal
  expect_equal(result$B[1, 1], 0.3, tolerance = 1e-10)
  expect_equal(result$B[2, 2], 0.4, tolerance = 1e-10)
  expect_equal(result$B[3, 3], 0.3, tolerance = 1e-10)

  # Off-diagonal should be zero
  expect_equal(sum(result$B) - sum(diag(result$B)), 0, tolerance = 1e-10)

  # E should equal alpha in this symmetric case
  expect_equal(result$E, alpha, tolerance = 1e-10)

  # Entropy bound should be finite
  expect_true(is.finite(result$entropy_bound))
})

test_that("compute_corner_solution handles tied costs", {
  # All costs equal - which.min will pick first worker
  cost_matrix <- matrix(1, nrow = 3, ncol = 3)

  alpha <- c(0.3, 0.4, 0.3)

  result <- compute_corner_solution(cost_matrix, alpha, TEST_CONFIG)

  # All tasks assigned to worker 1 (first worker wins ties)
  expect_equal(result$E[1], 1, tolerance = 1e-10)
  expect_equal(sum(result$B[1, ]), 1, tolerance = 1e-10)
})

# ===========================================================================
# Tests for find_gamma_for_sindex
# ===========================================================================

test_that("find_gamma_for_sindex returns corner solution for high s_index", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.33, 0.34, 0.33)

  # Very high s_index that's beyond achievable bound
  result <- find_gamma_for_sindex(cost_matrix, alpha, s_index = 100,
                                   innertol = 1e-5, outertol = 1e-3, TEST_CONFIG)

  expect_true(result$is_corner)
  expect_equal(result$gamma, 0)
})

test_that("find_gamma_for_sindex finds valid gamma for achievable s_index", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.33, 0.34, 0.33)

  # First find what entropy values are achievable
  corner <- compute_corner_solution(cost_matrix, alpha, TEST_CONFIG)
  eq_high_gamma <- solve_equilibrium(cost_matrix, alpha, 10, 1e-5, TEST_CONFIG)

  # Pick a target between the two extremes
  target_s <- (corner$entropy_bound + eq_high_gamma$entropy) / 2

  # Skip if target is not well-defined
  if (is.na(target_s) || !is.finite(target_s)) {
    skip("Could not compute valid target entropy")
  }

  result <- find_gamma_for_sindex(cost_matrix, alpha, s_index = target_s,
                                   innertol = 1e-5, outertol = 1e-3, TEST_CONFIG)

  expect_false(result$is_corner)
  expect_true(result$gamma > 0)
  expect_true(is.finite(result$entropy))
})

# ===========================================================================
# Numerical stability tests
# ===========================================================================

test_that("solve_equilibrium handles near-zero costs", {
  skip_if_not_installed("SQUAREM")

  # Very small costs (near uniform preferences)
  cost_matrix <- matrix(c(
    0, 0.001, 0.001,
    0.001, 0, 0.001,
    0.001, 0.001, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.33, 0.34, 0.33)

  result <- solve_equilibrium(cost_matrix, alpha, 1.0, 1e-5, TEST_CONFIG)

  expect_equal(sum(result$E), 1, tolerance = 1e-4)
  expect_true(all(is.finite(result$E)))
  expect_true(all(is.finite(result$B)))
})

test_that("solve_equilibrium handles moderate cost differences", {
  skip_if_not_installed("SQUAREM")

  # Moderate cost differences
  cost_matrix <- matrix(c(
    0, 5, 5,
    5, 0, 5,
    5, 5, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.33, 0.34, 0.33)

  result <- solve_equilibrium(cost_matrix, alpha, 2.0, 1e-5, TEST_CONFIG)

  expect_equal(sum(result$E), 1, tolerance = 1e-4)
  expect_true(all(is.finite(result$E)))
  expect_true(all(is.finite(result$B)))
})

test_that("functions handle non-square matrices", {
  skip_if_not_installed("SQUAREM")

  # 4 workers, 3 tasks
  config_nonsquare <- TEST_CONFIG
  config_nonsquare$n_worker_types <- 4
  config_nonsquare$n_task_types <- 3

  cost_matrix <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0,
    1, 1, 1
  ), nrow = 4, ncol = 3, byrow = TRUE)

  alpha <- c(0.3, 0.4, 0.3)

  result <- solve_equilibrium(cost_matrix, alpha, 2.0, 1e-5, config_nonsquare)

  expect_equal(length(result$E), 4)
  expect_equal(sum(result$E), 1, tolerance = 1e-4)
  expect_true(all(is.finite(result$E)))
  expect_true(all(is.finite(result$B)))
})
