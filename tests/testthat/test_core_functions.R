# Unit tests for live core model functions

context("Core model functions")

PROJECT_ROOT <- if (file.exists("config.R")) "." else file.path("..", "..")
source(file.path(PROJECT_ROOT, "config.R"))
source(file.path(PROJECT_ROOT, "preamble.R"), chdir = TRUE)

TEST_CONFIG <- modifyList(CONFIG, list(
  counties = c("17031", "36061", "6037"),
  counties_padded = c("17031", "36061", "06037"),
  n_worker_types = 3,
  n_task_types = 3,
  initial_E = NULL,
  fixedpoint_max_iter = 100000,
  innertol = 1e-6,
  outertol = 1e-4,
  obj_tol = 1e-4,
  pl_on = FALSE,
  use_solver_cache = FALSE,
  use_solver_warm_starts = FALSE,
  use_fixedpoint_warm_starts = FALSE,
  use_rcpp_equilibrium = FALSE,
  task_mix_pattern = "task_mix_",
  E_raw_pattern = "E_raw_",
  B_raw_pattern = "B_raw_",
  dye_task_index = 2
))

test_that("solve_equilibrium returns valid probability distribution for E", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.3, 0.4, 0.3)
  result <- solve_equilibrium(cost_matrix, alpha, 2.0, 1e-6, TEST_CONFIG)

  expect_equal(sum(result$E), 1, tolerance = 1e-4)
  expect_true(all(result$E > 0))
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
  result <- solve_equilibrium(cost_matrix, alpha, 2.0, 1e-6, TEST_CONFIG)

  expect_equal(sum(result$B), 1, tolerance = 1e-4)
  expect_true(all(result$B >= 0))
  expect_equal(rowSums(result$B), result$E, tolerance = 1e-4)
  expect_equal(colSums(result$B), alpha, tolerance = 1e-4)
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
  result_high <- solve_equilibrium(cost_matrix, alpha, 10, 1e-5, TEST_CONFIG)
  result_mod <- solve_equilibrium(cost_matrix, alpha, 2, 1e-5, TEST_CONFIG)

  expect_equal(sum(result_high$E), 1, tolerance = 1e-4)
  expect_true(all(is.finite(result_high$E)))
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
  result_high <- solve_equilibrium(cost_matrix, alpha, 5.0, 1e-5, TEST_CONFIG)
  result_low <- solve_equilibrium(cost_matrix, alpha, 1.0, 1e-5, TEST_CONFIG)

  expect_true(is.finite(result_high$entropy))
  expect_true(is.finite(result_low$entropy))
  expect_true(result_low$entropy >= result_high$entropy - 0.1)
})

test_that("compute_corner_solution assigns tasks to lowest-cost workers", {
  cost_matrix <- matrix(c(
    0, 5, 5,
    5, 0, 5,
    5, 5, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  alpha <- c(0.3, 0.4, 0.3)
  result <- compute_corner_solution(cost_matrix, alpha, TEST_CONFIG)

  expect_equal(result$B[1, 1], 0.3, tolerance = 1e-10)
  expect_equal(result$B[2, 2], 0.4, tolerance = 1e-10)
  expect_equal(result$B[3, 3], 0.3, tolerance = 1e-10)
  expect_equal(sum(result$B) - sum(diag(result$B)), 0, tolerance = 1e-10)
  expect_equal(result$E, alpha, tolerance = 1e-10)
  expect_true(is.finite(result$entropy_bound))
})

test_that("compute_corner_solution handles tied costs", {
  cost_matrix <- matrix(1, nrow = 3, ncol = 3)
  alpha <- c(0.3, 0.4, 0.3)
  result <- compute_corner_solution(cost_matrix, alpha, TEST_CONFIG)

  expect_equal(result$E[1], 1, tolerance = 1e-10)
  expect_equal(sum(result$B[1, ]), 1, tolerance = 1e-10)
})

test_that("find_gamma_for_sindex returns corner solution for high s_index", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  result <- find_gamma_for_sindex(cost_matrix, c(0.33, 0.34, 0.33), s_index = 100,
                                  innertol = 1e-5, outertol = 1e-3,
                                  config = TEST_CONFIG)

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
  corner <- compute_corner_solution(cost_matrix, alpha, TEST_CONFIG)
  eq_high_gamma <- solve_equilibrium(cost_matrix, alpha, 10, 1e-5, TEST_CONFIG)
  target_s <- (corner$entropy_bound + eq_high_gamma$entropy) / 2

  if (is.na(target_s) || !is.finite(target_s)) {
    skip("Could not compute valid target entropy")
  }

  result <- find_gamma_for_sindex(cost_matrix, alpha, s_index = target_s,
                                  innertol = 1e-5, outertol = 1e-3,
                                  config = TEST_CONFIG)

  expect_false(result$is_corner)
  expect_true(result$gamma > 0)
  expect_true(is.finite(result$entropy))
})

test_that("solve_equilibrium handles near-zero costs", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 0.001, 0.001,
    0.001, 0, 0.001,
    0.001, 0.001, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  result <- solve_equilibrium(cost_matrix, c(0.33, 0.34, 0.33), 1.0, 1e-5,
                              TEST_CONFIG)

  expect_equal(sum(result$E), 1, tolerance = 1e-4)
  expect_true(all(is.finite(result$E)))
  expect_true(all(is.finite(result$B)))
})

test_that("solve_equilibrium handles moderate cost differences", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 5, 5,
    5, 0, 5,
    5, 5, 0
  ), nrow = 3, ncol = 3, byrow = TRUE)

  result <- solve_equilibrium(cost_matrix, c(0.33, 0.34, 0.33), 2.0, 1e-5,
                              TEST_CONFIG)

  expect_equal(sum(result$E), 1, tolerance = 1e-4)
  expect_true(all(is.finite(result$E)))
  expect_true(all(is.finite(result$B)))
})

test_that("functions handle non-square matrices", {
  skip_if_not_installed("SQUAREM")

  config_nonsquare <- modifyList(TEST_CONFIG, list(n_worker_types = 4, n_task_types = 3))
  cost_matrix <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0,
    1, 1, 1
  ), nrow = 4, ncol = 3, byrow = TRUE)

  result <- solve_equilibrium(cost_matrix, c(0.3, 0.4, 0.3), 2.0, 1e-5,
                              config_nonsquare)

  expect_equal(length(result$E), 4)
  expect_equal(sum(result$E), 1, tolerance = 1e-4)
  expect_true(all(is.finite(result$E)))
  expect_true(all(is.finite(result$B)))
})
