# Unit tests for utils/counterfactuals_core.R helpers used by 13-19.
# Focused on numerical equivalence between the helpers and the hand-rolled
# expressions that the counterfactual scripts inline.

context("Counterfactual helpers")

PROJECT_ROOT <- if (file.exists("config.R")) "." else file.path("..", "..")
source(file.path(PROJECT_ROOT, "config.R"))
source(file.path(PROJECT_ROOT, "utils", "counterfactuals_core.R"), chdir = TRUE)
suppressPackageStartupMessages({
  library(data.table)
  library(SQUAREM)
  library(lamW)
})

TEST_CONFIG <- modifyList(CONFIG, list(
  counties = c("17031", "36061", "6037"),
  n_worker_types = 5,
  n_task_types = 5,
  numeric_floor = 1e-16,
  numeric_ceiling = 1e16,
  B_zero_threshold = 1e-16,
  counterfactual_zero_gamma_floor = 0
))

make_cost_matrix <- function(seed, n_w = 5, n_t = 5) {
  set.seed(seed)
  m <- matrix(runif(n_w * n_t, 0, 2), nrow = n_w, ncol = n_t)
  # Force a clear minimum in each column so corner solutions are unambiguous.
  for (col in seq_len(n_t)) {
    m[col %% n_w + 1L, col] <- 0
  }
  m
}

test_that("counterfactual_logit_shares matches the hand-rolled formula on finite inputs", {
  set.seed(11)
  for (.trial in seq_len(5)) {
    n <- sample(3:8, 1)
    Q <- rnorm(n)
    price <- runif(n, 5, 50)
    wgt <- runif(n, 0.5, 2)
    rho <- -runif(1, 0.05, 0.5)

    helper <- counterfactual_logit_shares(Q, price, wgt, rho, TEST_CONFIG)

    manual <- exp(Q + rho * price)
    manual <- manual / (sum(wgt * manual) + 1)

    expect_equal(helper, manual, tolerance = 1e-12,
                 info = paste("trial", .trial))
  }
})

test_that("counterfactual_logit_shares is shift-invariant and stable for large utilities", {
  Q <- c(800, 810, 820)
  price <- c(0, 0, 0)
  wgt <- c(1, 1, 1)
  rho <- -0.1

  helper <- counterfactual_logit_shares(Q, price, wgt, rho, TEST_CONFIG)

  # Hand-rolled exp(Q) overflows for these magnitudes, so we compare against
  # softmax-with-shift, which is the same algebra the helper uses internally.
  shifted <- exp(Q + rho * price - max(Q + rho * price))
  expected <- shifted / (sum(wgt * shifted) + exp(-max(Q + rho * price)))

  expect_true(all(is.finite(helper)))
  expect_equal(sum(helper) <= 1 + 1e-12, TRUE)
  expect_equal(helper, expected, tolerance = 1e-10)
})

test_that("counterfactual_logit_shares returns NA when inputs contain non-finite values", {
  helper <- counterfactual_logit_shares(c(1, NA, 3), c(2, 2, 2), c(1, 1, 1), -0.1, TEST_CONFIG)
  expect_true(all(is.na(helper)))
})

test_that("counterfactual_assignment matches hand-rolled solve_org for finite gamma", {
  cost_matrix <- make_cost_matrix(42)
  alpha <- c(0.2, 0.25, 0.15, 0.2, 0.2)
  gamma <- 1.5

  helper <- counterfactual_assignment(cost_matrix, alpha, gamma,
                                      innertol = 1e-10, config = TEST_CONFIG)

  # Hand-rolled SQUAREM-style fixed point that 13-18 use inline
  A <- exp(-cost_matrix / gamma)
  A[A >= Inf] <- TEST_CONFIG$numeric_ceiling
  A[A <= 0] <- TEST_CONFIG$numeric_floor
  E <- rep(1 / TEST_CONFIG$n_worker_types, TEST_CONFIG$n_worker_types)
  fxpt <- function(p) {
    C <- colSums(t(A) * alpha / colSums(A * p))
    p * C
  }
  E_manual <- SQUAREM::squarem(E, fixptfn = fxpt,
                               control = list(maxiter = 100000, tol = 1e-10))$par
  B_manual <- t(t(A) * alpha / colSums(A * E_manual)) * E_manual
  B_manual[abs(B_manual) < TEST_CONFIG$B_zero_threshold] <- 0
  E_manual <- rowSums(B_manual)

  expect_equal(helper$E, E_manual, tolerance = 1e-8)
  expect_equal(helper$B, B_manual, tolerance = 1e-8)
})

test_that("counterfactual_assignment matches hand-rolled corner solutions for gamma = 0 and gamma = Inf", {
  cost_matrix <- make_cost_matrix(7)
  alpha <- c(0.2, 0.25, 0.15, 0.2, 0.2)

  cfg <- modifyList(TEST_CONFIG, list(counterfactual_zero_gamma_floor = 0))

  # gamma = 0 corner: each task to lowest-cost worker.
  zero_helper <- counterfactual_assignment(cost_matrix, alpha, 0,
                                           innertol = 1e-10, config = cfg)
  B_expected <- matrix(0, nrow = 5, ncol = 5)
  for (col in seq_len(5)) {
    B_expected[which.min(cost_matrix[, col]), col] <- alpha[col]
  }
  E_expected <- rowSums(B_expected)
  expect_equal(zero_helper$B, B_expected)
  expect_equal(zero_helper$E, E_expected)

  # gamma = Inf (max frictions): one worker takes everything.
  inf_helper <- counterfactual_assignment(cost_matrix, alpha, Inf,
                                          innertol = 1e-10, config = cfg)
  B_inf <- matrix(0, nrow = 5, ncol = 5)
  best_row <- which.min(rowSums(t(t(cost_matrix) * alpha)))
  B_inf[best_row, ] <- alpha
  expect_equal(inf_helper$B, B_inf)
  expect_equal(inf_helper$E, rowSums(B_inf))
})

test_that("counterfactual_effective_gamma applies floor only when configured", {
  cfg_zero <- modifyList(TEST_CONFIG, list(counterfactual_zero_gamma_floor = 0))
  cfg_floor <- modifyList(TEST_CONFIG, list(counterfactual_zero_gamma_floor = 1e-3))

  expect_identical(counterfactual_effective_gamma(0, cfg_zero), 0)
  expect_equal(as.numeric(counterfactual_effective_gamma(0, cfg_floor)), 1e-3)
  expect_equal(as.numeric(counterfactual_effective_gamma(2.5, cfg_floor)), 2.5)
  expect_true(is.na(counterfactual_effective_gamma(NA_real_, cfg_floor)))
})

test_that("counterfactual_best_response_prices reaches a Lambert-W fixed point", {
  Q <- c(0.5, -0.2, 0.1, 0.3, -0.4)
  C <- c(15, 22, 18, 25, 12)
  wgt <- rep(1, 5)
  rho <- -0.05
  p0 <- C + 5

  out <- counterfactual_best_response_prices(p0, Q, C, wgt, rho,
                                             outertol = 1e-10,
                                             label = "test",
                                             config = TEST_CONFIG)
  status <- attr(out, "price_status")

  expect_true(status$converged)
  # Re-evaluate the FOC at the returned prices: a fixed point means a step
  # of the Lambert-W iteration moves by less than outertol.
  demand_index <- exp(Q + rho * out)
  denom <- 1 + sum(wgt * demand_index) - demand_index
  log_arg <- -1 + Q + rho * C - log(denom)
  step <- -1 / rho + C - lamW::lambertW0(exp(log_arg)) / rho
  expect_lt(max(abs(step - out)), 1e-8)
})

test_that("counterfactual_best_response_prices flags non-finite inputs without looping forever", {
  Q <- c(0.5, NaN, 0.1)
  C <- c(15, 22, 18)
  wgt <- c(1, 1, 1)
  rho <- -0.05
  p0 <- c(20, 25, 22)

  out <- suppressWarnings(counterfactual_best_response_prices(
    p0, Q, C, wgt, rho,
    outertol = 1e-6, label = NULL, config = TEST_CONFIG
  ))
  status <- attr(out, "price_status")
  expect_true(status$nonfinite)
  expect_false(status$converged)
})

test_that("counterfactual_labor_gap respects the requested scale", {
  total_labor <- data.table::data.table(
    county = "6037",
    quarter_year = 2021.2,
    tot_1 = 100, tot_2 = 200, tot_3 = 300, tot_4 = 400, tot_5 = 500
  )
  new_total_labor <- data.table::data.table(
    tot_1 = 110, tot_2 = 200, tot_3 = 270, tot_4 = 400, tot_5 = 500
  )

  log_gap <- counterfactual_labor_gap(new_total_labor, total_labor, "6037", 2021.2,
                                      scale = "log", config = TEST_CONFIG)
  rel_gap <- counterfactual_labor_gap(new_total_labor, total_labor, "6037", 2021.2,
                                      scale = "relative", config = TEST_CONFIG)
  raw_gap <- counterfactual_labor_gap(new_total_labor, total_labor, "6037", 2021.2,
                                      scale = "raw", config = TEST_CONFIG)

  expect_equal(unname(raw_gap), c(10, 0, -30, 0, 0))
  expect_equal(unname(rel_gap), c(10 / 100, 0, -30 / 300, 0, 0))
  expect_equal(unname(log_gap),
               c(log(110) - log(100), 0, log(270) - log(300), 0, 0))
  expect_equal(names(raw_gap), c("6037-2021.2-1", "6037-2021.2-2",
                                 "6037-2021.2-3", "6037-2021.2-4",
                                 "6037-2021.2-5"))
})

test_that("new_counterfactual_wages_grid produces the schema 14-17 expect", {
  grid_with <- new_counterfactual_wages_grid(c(2021.1, 2021.2),
                                             include_solution_type = TRUE,
                                             config = TEST_CONFIG)

  expect_true(all(c("county", "quarter_year", "sol_type", "fval", "converged",
                    "method", "termcd", "message", "target_tol",
                    paste0("w", 1:5), paste0("resid_", 1:5)) %in%
                    names(grid_with)))
  expect_equal(nrow(grid_with),
               length(TEST_CONFIG$counties) * 2L * 2L)  # counties x quarters x sol_types
  expect_setequal(unique(grid_with$sol_type), c("reorg", "realloc"))

  grid_without <- new_counterfactual_wages_grid(2021.2,
                                                include_solution_type = FALSE,
                                                config = TEST_CONFIG)
  expect_false("sol_type" %in% names(grid_without))
  expect_equal(nrow(grid_without), length(TEST_CONFIG$counties))
})

test_that("counterfactual_store_wage_solution writes into the correct row", {
  grid <- new_counterfactual_wages_grid(2021.2,
                                        include_solution_type = TRUE,
                                        config = TEST_CONFIG)
  result <- list(
    par = c(10, 20, 30, 40, 50),
    residual = 1e-6,
    residual_components = rep(1e-6, 5),
    method = "nleqslv",
    termcd = 1L,
    message = "ok",
    converged = TRUE,
    target_tol = 0.01
  )

  counterfactual_store_wage_solution(grid, result, "6037", 2021.2,
                                     solution_type = "realloc",
                                     config = TEST_CONFIG)

  row <- grid[county == "6037" & quarter_year == 2021.2 & sol_type == "realloc"]
  expect_equal(as.numeric(row[, paste0("w", 1:5), with = FALSE]), result$par)
  expect_equal(row$method, "nleqslv")
  expect_equal(row$converged, TRUE)
  expect_equal(row$target_tol, 0.01)

  # Untouched rows stay at the sentinel Inf wages.
  other <- grid[!(county == "6037" & sol_type == "realloc")]
  expect_true(all(is.infinite(other$w1)))
})

test_that("get_counterfactual_market_parms preserves names from the parameter table", {
  pt <- data.table::data.table(
    demand = c(TRUE, TRUE, FALSE),
    parm_name = c("a", "factor(county)6037:cust_price", "x"),
    coefficients = c(0.5, -0.1, 7)
  )
  parms <- get_counterfactual_market_parms(pt)
  expect_equal(unname(parms), pt$coefficients)
  expect_equal(names(parms), pt$parm_name)
})

test_that("counterfactual_residual_norm reports Inf on non-finite input and max-abs otherwise", {
  expect_equal(counterfactual_residual_norm(c(0.1, -0.3, 0.2)), 0.3)
  expect_equal(counterfactual_residual_norm(c(0.1, NaN, 0.2)), Inf)
  expect_equal(counterfactual_residual_norm(c(0.1, Inf, 0.2)), Inf)
})

test_that("counterfactual_org_outputs reproduces the inline solve_org return for finite gamma", {
  cost_matrix <- make_cost_matrix(101)
  new_theta <- matrix(rnorm(25), 5, 5)
  alpha <- c(0.2, 0.3, 0.15, 0.2, 0.15)
  gamma <- 1.7
  wage_guess <- c(10, 15, 20, 25, 30)

  helper <- counterfactual_org_outputs(
    cost_matrix, alpha, gamma, wage_guess, new_theta,
    innertol = 1e-10, with_s_index = TRUE, with_b = TRUE,
    config = TEST_CONFIG
  )

  # Inline reconstruction of solve_org from 14-17.
  A <- exp(-cost_matrix / gamma)
  A[A >= Inf] <- TEST_CONFIG$numeric_ceiling
  A[A <= 0] <- TEST_CONFIG$numeric_floor
  E <- rep(1 / 5, 5)
  fxpt <- function(p) p * colSums(t(A) * alpha / colSums(A * p))
  E <- SQUAREM::squarem(E, fixptfn = fxpt,
                        control = list(maxiter = 100000, tol = 1e-10))$par
  B <- t(t(A) * alpha / colSums(A * E)) * E
  E <- rowSums(B)
  B[abs(B) < TEST_CONFIG$B_zero_threshold] <- 0
  Brel <- t(t(B / E) / alpha)
  s_index <- sum(B * ifelse(B == 0 | is.nan(B), 0, log(Brel)))  # match counterfactual_spec_log on Brel
  s_index <- sum(B * (ifelse(Brel == 0 | is.nan(Brel), 0, log(Brel))))
  cendog <- sum(E * wage_guess) + gamma * s_index
  qendog <- sum(B * new_theta)

  expect_equal(helper$c_endog, cendog, tolerance = 1e-8)
  expect_equal(helper$q_endog, qendog, tolerance = 1e-8)
  expect_equal(helper$s_index, s_index, tolerance = 1e-8)
  expect_equal(unlist(helper[paste0("E_", 1:5)], use.names = FALSE),
               E, tolerance = 1e-8)
  expect_equal(unlist(helper[paste0("B_", 1, "_", 1:5)], use.names = FALSE),
               B[, 1], tolerance = 1e-8)
  expect_equal(unlist(helper[paste0("B_", 5, "_", 1:5)], use.names = FALSE),
               B[, 5], tolerance = 1e-8)
})

test_that("counterfactual_org_outputs handles gamma = 0 (no frictions) corner", {
  cost_matrix <- make_cost_matrix(202)
  new_theta <- matrix(rnorm(25), 5, 5)
  alpha <- c(0.2, 0.3, 0.15, 0.2, 0.15)
  wage_guess <- c(10, 15, 20, 25, 30)

  helper <- counterfactual_org_outputs(
    cost_matrix, alpha, 0, wage_guess, new_theta,
    innertol = 1e-10, with_s_index = TRUE, with_b = TRUE,
    config = TEST_CONFIG
  )

  B_expected <- matrix(0, 5, 5)
  for (col in 1:5) B_expected[which.min(cost_matrix[, col]), col] <- alpha[col]
  E_expected <- rowSums(B_expected)
  expect_equal(unlist(helper[paste0("E_", 1:5)], use.names = FALSE),
               E_expected)
  expect_equal(helper$c_endog, sum(E_expected * wage_guess))
})

test_that("counterfactual_org_outputs with_swept_b multiplies B by column-min-zeroed theta", {
  cost_matrix <- make_cost_matrix(303)
  new_theta <- matrix(seq(1, 25), 5, 5) + 0.1
  alpha <- c(0.2, 0.3, 0.15, 0.2, 0.15)
  wage_guess <- rep(10, 5)
  gamma <- 1.2

  out_swept <- counterfactual_org_outputs(
    cost_matrix, alpha, gamma, wage_guess, new_theta,
    innertol = 1e-10, with_swept_b = TRUE, config = TEST_CONFIG
  )
  out_raw <- counterfactual_org_outputs(
    cost_matrix, alpha, gamma, wage_guess, new_theta,
    innertol = 1e-10, with_b = TRUE, config = TEST_CONFIG
  )

  # Reconstruct the helper's internal B from the raw output.
  B_raw <- matrix(unlist(out_raw[paste0("B_", rep(1:5, each = 5),
                                        "_", rep(1:5, times = 5))]),
                  5, 5, byrow = FALSE)
  swept_theta <- sweep(new_theta, 2, apply(new_theta, 2, min), FUN = "-")
  B_swept_expected <- B_raw * swept_theta

  B_swept_helper <- matrix(unlist(out_swept[paste0("B_", rep(1:5, each = 5),
                                                  "_", rep(1:5, times = 5))]),
                          5, 5, byrow = FALSE)
  expect_equal(B_swept_helper, B_swept_expected, tolerance = 1e-10)
})

test_that("counterfactual_org_outputs_from_b reproduces the inline realloc branch", {
  set.seed(404)
  B <- matrix(0, 5, 5)
  # Build a synthetic but realistic B (rows sum to E_i, cols sum approximate alpha).
  for (col in seq_len(5)) {
    weights <- runif(5)
    weights <- weights / sum(weights)
    B[, col] <- weights * 0.2
  }
  alpha <- colSums(B)
  new_theta <- matrix(rnorm(25), 5, 5)
  wage_guess <- c(10, 12, 14, 16, 18)
  gamma <- 2.5

  helper <- counterfactual_org_outputs_from_b(
    B, alpha, gamma, wage_guess, new_theta,
    with_s_index = TRUE, with_b = TRUE, config = TEST_CONFIG
  )

  E <- rowSums(B)
  Brel <- t(t(B / E) / colSums(B))
  s_idx <- sum(B * ifelse(Brel == 0 | is.nan(Brel), 0, log(Brel)))
  cendog <- sum(E * wage_guess) + gamma * s_idx
  qendog <- sum(B * new_theta)

  expect_equal(helper$c_endog, cendog, tolerance = 1e-12)
  expect_equal(helper$q_endog, qendog, tolerance = 1e-12)
  expect_equal(helper$s_index, s_idx, tolerance = 1e-12)
  expect_equal(unlist(helper[paste0("E_", 1:5)], use.names = FALSE),
               E, tolerance = 1e-12)
})

test_that("counterfactual_b_field_names and counterfactual_e_field_names match the documented schema", {
  expect_equal(counterfactual_e_field_names(TEST_CONFIG),
               c("E_1", "E_2", "E_3", "E_4", "E_5"))
  expect_equal(counterfactual_b_field_names(TEST_CONFIG),
               c("B_1_1","B_1_2","B_1_3","B_1_4","B_1_5",
                 "B_2_1","B_2_2","B_2_3","B_2_4","B_2_5",
                 "B_3_1","B_3_2","B_3_3","B_3_4","B_3_5",
                 "B_4_1","B_4_2","B_4_3","B_4_4","B_4_5",
                 "B_5_1","B_5_2","B_5_3","B_5_4","B_5_5"))
})

test_that("counterfactual_multistarts dedupes and respects the worker-type length", {
  cfg <- modifyList(TEST_CONFIG, list(numeric_floor = 1e-12))
  starts <- counterfactual_multistarts(c(10, 20, 30, 40, 50),
                                       additional_starts = list(),
                                       config = cfg)
  expect_true(all(vapply(starts, length, integer(1)) == 5L))
  # No duplicate starts after rounding.
  keys <- vapply(starts, function(x) paste(round(x, 8), collapse = "|"),
                 character(1))
  expect_equal(length(unique(keys)), length(keys))

  # Wrong-length additional starts are dropped.
  bad <- counterfactual_multistarts(c(10, 20, 30, 40, 50),
                                    additional_starts = list(c(1, 2, 3)),
                                    config = cfg)
  expect_true(all(vapply(bad, length, integer(1)) == 5L))
})
