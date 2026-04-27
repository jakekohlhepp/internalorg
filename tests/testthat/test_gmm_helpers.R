# Unit tests for live GMM helper functions

context("GMM helper functions")

PROJECT_ROOT <- if (file.exists("config.R")) "." else file.path("..", "..")
source(file.path(PROJECT_ROOT, "config.R"))
source(file.path(PROJECT_ROOT, "preamble.R"), chdir = TRUE)

TEST_CONFIG <- modifyList(CONFIG, list(
  counties = c("17031", "36061", "6037"),
  counties_padded = c("17031", "36061", "06037"),
  n_worker_types = 5,
  n_task_types = 5,
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

test_that("get_worker_allocation returns correct dimensions", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 1, 2, 3, 4,
    1, 0, 1, 2, 3,
    2, 1, 0, 1, 2,
    3, 2, 1, 0, 1,
    4, 3, 2, 1, 0
  ), nrow = 5, ncol = 5, byrow = TRUE)

  result <- get_worker_allocation(cost_matrix, rep(0.2, 5), 0.5, 1e-6, 1e-4,
                                  TEST_CONFIG)

  expect_equal(length(result), 4)
  expect_true(all(result >= 0))
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

  result <- get_worker_allocation(cost_matrix, rep(0.2, 5), 100, 1e-6, 1e-4,
                                  TEST_CONFIG)

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

  result1 <- get_worker_allocation(cost_matrix, rep(0.2, 5), 0.3, 1e-6, 1e-4,
                                   TEST_CONFIG)
  result2 <- get_worker_allocation(cost_matrix, rep(0.2, 5), 0.3, 1e-6, 1e-4,
                                   TEST_CONFIG)

  expect_equal(result1, result2, tolerance = 1e-6)
})

test_that("get_worker_allocation respects specialization level", {
  skip_if_not_installed("SQUAREM")

  cost_matrix <- matrix(c(
    0, 5, 5, 5, 5,
    5, 0, 5, 5, 5,
    5, 5, 0, 5, 5,
    5, 5, 5, 0, 5,
    5, 5, 5, 5, 0
  ), nrow = 5, ncol = 5, byrow = TRUE)

  result_high <- get_worker_allocation(cost_matrix, rep(0.2, 5), 2.0, 1e-5,
                                       1e-3, TEST_CONFIG)
  result_low <- get_worker_allocation(cost_matrix, rep(0.2, 5), 0.2, 1e-5,
                                      1e-3, TEST_CONFIG)

  expect_equal(length(result_high), 4)
  expect_equal(length(result_low), 4)
  expect_true(all(is.finite(result_high)))
  expect_true(all(is.finite(result_low)))
})
