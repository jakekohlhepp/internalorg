# Unit tests for GMM helper functions
# Tests get_worker_allocation and build_cost_matrix integration

context("GMM helper functions")

# Setup: minimal config for testing
TEST_CONFIG <- list(
  counties = c("17031", "36061", "6037"),
  counties_padded = c("17031", "36061", "06037"),
  n_worker_types = 5,
  n_task_types = 5,
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

# Bisection function (with config parameter)
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

# solve_equilibrium function
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
  result <- SQUAREM::squarem(E, fixptfn = fxpt, control = list(maxiter = config$fixedpoint_max_iter, tol = innertol))
  E <- result$par
  converged <- (result$convergence == 0)
  E <- pmax(E, config$numeric_floor)
  E <- E / sum(E)
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

# compute_corner_solution function
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

# find_gamma_for_sindex function
find_gamma_for_sindex <- function(cost_matrix, alpha, s_index, innertol, outertol, config = TEST_CONFIG) {
  corner <- compute_corner_solution(cost_matrix, alpha, config)
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

# get_worker_allocation function
get_worker_allocation <- function(cost_matrix, alpha, s_index, innertol, outertol, config = TEST_CONFIG) {
  result <- find_gamma_for_sindex(cost_matrix, alpha, s_index, innertol, outertol, config)
  return(result$E[2:config$n_worker_types])
}

# ===========================================================================
# Tests for get_worker_allocation
# ===========================================================================

test_that("get_worker_allocation returns correct dimensions", {
  skip_if_not_installed("SQUAREM")

  # 5x5 cost matrix
  cost_matrix <- matrix(c(
    0, 1, 2, 3, 4,
    1, 0, 1, 2, 3,
    2, 1, 0, 1, 2,
    3, 2, 1, 0, 1,
    4, 3, 2, 1, 0
  ), nrow = 5, ncol = 5, byrow = TRUE)

  alpha <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  s_index <- 0.5

  result <- get_worker_allocation(cost_matrix, alpha, s_index, 1e-6, 1e-4, TEST_CONFIG)

  # Should return 4 elements (E[2:5])
  expect_equal(length(result), 4)

  # All should be non-negative
  expect_true(all(result >= 0))

  # All should be finite
  expect_true(all(is.finite(result)))
})

test_that("get_worker_allocation works with corner solution", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 1, 2, 3, 4,
    1, 0, 1, 2, 3,
    2, 1, 0, 1, 2,
    3, 2, 1, 0, 1,
    4, 3, 2, 1, 0
  ), nrow = 5, ncol = 5, byrow = TRUE)

  alpha <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  s_index <- 100  # Very high, should trigger corner solution

  result <- get_worker_allocation(cost_matrix, alpha, s_index, 1e-6, 1e-4, TEST_CONFIG)

  expect_equal(length(result), 4)
  expect_true(all(is.finite(result)))
})

test_that("get_worker_allocation produces consistent results", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 2, 2, 2, 2,
    2, 0, 2, 2, 2,
    2, 2, 0, 2, 2,
    2, 2, 2, 0, 2,
    2, 2, 2, 2, 0
  ), nrow = 5, ncol = 5, byrow = TRUE)

  alpha <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  s_index <- 0.3

  # Run twice with same inputs
  result1 <- get_worker_allocation(cost_matrix, alpha, s_index, 1e-6, 1e-4, TEST_CONFIG)
  result2 <- get_worker_allocation(cost_matrix, alpha, s_index, 1e-6, 1e-4, TEST_CONFIG)

  # Should get same results
  expect_equal(result1, result2, tolerance = 1e-6)
})

test_that("get_worker_allocation respects specialization level", {
  skip_if_not_installed("SQUAREM")

  # Symmetric cost matrix where specialization is possible
  cost_matrix <- matrix(c(
    0, 5, 5, 5, 5,
    5, 0, 5, 5, 5,
    5, 5, 0, 5, 5,
    5, 5, 5, 0, 5,
    5, 5, 5, 5, 0
  ), nrow = 5, ncol = 5, byrow = TRUE)

  alpha <- c(0.2, 0.2, 0.2, 0.2, 0.2)

  # Get results for different s_index values
  result_high <- get_worker_allocation(cost_matrix, alpha, 2.0, 1e-5, 1e-3, TEST_CONFIG)
  result_low <- get_worker_allocation(cost_matrix, alpha, 0.2, 1e-5, 1e-3, TEST_CONFIG)

  # Both should be valid
  expect_equal(length(result_high), 4)
  expect_equal(length(result_low), 4)
  expect_true(all(is.finite(result_high)))
  expect_true(all(is.finite(result_low)))
})
