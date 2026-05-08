# Smoke tests for the data.table call patterns 13-18 use to populate per-firm
# counterfactual outputs. We don't have access to the upstream artifacts here,
# but we can construct synthetic working_data and confirm the helper-driven
# by-group call shape produces the expected number of rows / columns.

context("Counterfactual data.table integration")

PROJECT_ROOT <- if (file.exists("config.R")) "." else file.path("..", "..")
source(file.path(PROJECT_ROOT, "config.R"))
source(file.path(PROJECT_ROOT, "utils", "counterfactuals_core.R"), chdir = TRUE)
suppressPackageStartupMessages({
  library(data.table)
  library(SQUAREM)
  library(lamW)
})

TEST_CONFIG <- modifyList(CONFIG, list(
  counties = c("17031"),
  n_worker_types = 5,
  n_task_types = 5,
  numeric_floor = 1e-16,
  numeric_ceiling = 1e16,
  B_zero_threshold = 1e-16,
  counterfactual_zero_gamma_floor = 0,
  counterfactual_innertol = 1e-8,
  counterfactual_outertol = 1e-4,
  counterfactual_wage_tol = 0.01
))

build_synthetic_market <- function(n_firms = 6, seed = 1) {
  set.seed(seed)
  task_mix <- t(apply(matrix(runif(n_firms * 5), n_firms, 5), 1,
                      function(x) x / sum(x)))
  dt <- data.table::data.table(
    location_id = sprintf("loc%02d", seq_len(n_firms)),
    county = "17031",
    quarter_year = 2021.2,
    gamma_invert = c(rep(2.5, max(n_firms - 1, 1)), Inf)[seq_len(n_firms)],
    avg_labor = runif(n_firms, 0.5, 2),
    qual_exo = rnorm(n_firms),
    cost_exo = runif(n_firms, 0, 10),
    weight = rep(1.0, n_firms),
    cust_price = runif(n_firms, 25, 60),
    CSPOP = rep(1e6, n_firms)
  )
  for (k in seq_len(5)) {
    dt[, (paste0("task_mix_", k)) := task_mix[, k]]
  }
  dt
}

test_that("by-location_id solve_org_fresh writes one value per requested field", {
  working_data <- build_synthetic_market(6)

  task_mix_cols <- paste0("task_mix_", 1:5)
  e_field_names <- counterfactual_e_field_names(TEST_CONFIG)
  b_field_names <- counterfactual_b_field_names(TEST_CONFIG)
  output_fields <- c("c_endog", "q_endog", "s_index", e_field_names, b_field_names)

  cost_matrix <- matrix(runif(25), 5, 5)
  new_theta <- matrix(runif(25), 5, 5)
  wage_guess <- c(10, 15, 20, 25, 30)
  innertol <- TEST_CONFIG$counterfactual_innertol

  solve_org_fresh <- function(alpha, gamma) {
    counterfactual_org_outputs(
      cost_matrix, alpha, gamma, wage_guess, new_theta,
      innertol = innertol,
      with_s_index = TRUE, with_b = TRUE,
      config = TEST_CONFIG
    )
  }

  working_data[, (output_fields) := solve_org_fresh(as.numeric(.SD), gamma_invert),
               by = "location_id", .SDcols = task_mix_cols]

  expect_true(all(output_fields %in% names(working_data)))
  expect_true(all(is.finite(working_data$c_endog)))
  expect_true(all(is.finite(working_data$s_index)))
  # SQUAREM acceleration can produce O(1e-8) negatives near boundary solutions;
  # the inline solve_org has the same property. Just check the values are
  # essentially zero or positive within numerical noise.
  for (col in e_field_names) {
    expect_true(all(working_data[[col]] >= -1e-6))
  }
  # The E_i values per row should sum to ~1 (worker shares).
  E_mat <- as.matrix(working_data[, ..e_field_names])
  expect_equal(rowSums(E_mat), rep(1, nrow(working_data)), tolerance = 1e-6)
})

test_that("by-location_id solve_org_from_saved reads each firm's saved B", {
  working_data <- build_synthetic_market(5, seed = 42)
  task_mix_cols <- paste0("task_mix_", 1:5)
  e_field_names <- counterfactual_e_field_names(TEST_CONFIG)
  b_field_names <- counterfactual_b_field_names(TEST_CONFIG)

  # synthetic orig_struct row per firm with B columns
  orig <- copy(working_data[, .(location_id)])
  for (j in seq_along(b_field_names)) {
    orig[, (b_field_names[j]) := runif(.N, 0, 0.05)]
  }

  new_theta <- matrix(runif(25), 5, 5)
  wage_guess <- c(10, 15, 20, 25, 30)

  output_fields <- c("c_endog", "q_endog", e_field_names)
  saved_b_cols <- b_field_names

  solve_org_from_saved <- function(loc, alpha, gamma) {
    saved_B <- matrix(
      as.numeric(orig[location_id == loc, .SD, .SDcols = saved_b_cols]),
      byrow = FALSE, nrow = 5, ncol = 5
    )
    counterfactual_org_outputs_from_b(
      saved_B, alpha, gamma, wage_guess, new_theta,
      with_s_index = FALSE, with_b = FALSE, with_swept_b = FALSE,
      config = TEST_CONFIG
    )
  }

  working_data[, (output_fields) := solve_org_from_saved(
                   location_id, as.numeric(.SD), gamma_invert),
               by = "location_id", .SDcols = task_mix_cols]

  expect_true(all(output_fields %in% names(working_data)))
  expect_true(all(is.finite(working_data$c_endog)))

  # Spot-check: the first firm's c_endog equals the function result computed
  # by hand from its saved B.
  first_loc <- working_data$location_id[1]
  saved_B <- matrix(as.numeric(orig[location_id == first_loc, .SD, .SDcols = saved_b_cols]),
                    nrow = 5, ncol = 5, byrow = FALSE)
  alpha_first <- as.numeric(working_data[location_id == first_loc,
                                         .SD, .SDcols = task_mix_cols])
  expected <- counterfactual_org_outputs_from_b(
    saved_B, alpha_first, working_data[location_id == first_loc, gamma_invert],
    wage_guess, new_theta,
    with_s_index = FALSE, with_b = FALSE,
    config = TEST_CONFIG
  )
  expect_equal(working_data[location_id == first_loc, c_endog],
               expected$c_endog, tolerance = 1e-12)
  expect_equal(working_data[location_id == first_loc, q_endog],
               expected$q_endog, tolerance = 1e-12)
  for (col in e_field_names) {
    expect_equal(working_data[location_id == first_loc, get(col)],
                 expected[[col]], tolerance = 1e-12)
  }
})

test_that("the get_everything / solve_reloc / solve_wages flow used by 14-17 runs without errors", {
  # Build synthetic per-firm market state for a single county-quarter.
  set.seed(7)
  n_firms <- 8
  task_mix <- t(apply(matrix(runif(n_firms * 5), n_firms, 5), 1,
                      function(x) x / sum(x)))
  working_data <- data.table::data.table(
    location_id = sprintf("loc%02d", seq_len(n_firms)),
    county = "17031",
    quarter_year = 2021.2,
    gamma_invert = c(rep(2.5, n_firms - 2), 0.0, Inf),
    avg_labor = runif(n_firms, 0.5, 2),
    qual_exo = rnorm(n_firms),
    cost_exo = runif(n_firms, 0, 10),
    weight = rep(1.0, n_firms),
    cust_price = runif(n_firms, 25, 60),
    CSPOP = rep(1e6, n_firms)
  )
  for (k in seq_len(5)) {
    working_data[, (paste0("task_mix_", k)) := task_mix[, k]]
  }

  task_mix_cols <- paste0("task_mix_", 1:5)
  e_field_names <- counterfactual_e_field_names(TEST_CONFIG)
  b_field_names <- counterfactual_b_field_names(TEST_CONFIG)

  cnty <- "17031"
  qy <- 2021.2

  # cost / theta matrices exposed via lexical scope, as in 14-17.
  new_theta <- matrix(runif(25, -1, 1), 5, 5)
  new_tild_theta <- abs(new_theta) + matrix(runif(25), 5, 5)
  mats <- list(new_theta = new_theta, new_tild_theta = new_tild_theta)
  rho <- c("17031" = -0.05)
  wage_guess <- c(10, 15, 20, 25, 30)
  innertol <- TEST_CONFIG$counterfactual_innertol
  outertol <- TEST_CONFIG$counterfactual_outertol
  n_worker_types <- 5
  n_task_types <- 5

  # Step 1: get_everything-style solve produces the full (s_index + B + E) panel.
  output_full <- c("c_endog", "q_endog", "s_index", e_field_names, b_field_names)
  panel <- copy(working_data)
  panel[, (output_full) := counterfactual_org_outputs(
        cost_matrix  = mats$new_tild_theta,
        alpha        = as.numeric(.SD),
        gamma        = gamma_invert,
        wage_guess   = wage_guess,
        new_theta    = mats$new_theta,
        innertol     = innertol,
        with_s_index = TRUE,
        with_b       = TRUE,
        config       = TEST_CONFIG
      ),
      by = "location_id",
      .SDcols = task_mix_cols]

  expect_true(all(output_full %in% names(panel)))
  expect_true(all(is.finite(panel$c_endog)))
  expect_true(all(is.finite(panel$s_index)))

  # Step 2: solve_reloc-style solve uses the saved B from panel and recomputes
  # the cost / quality summary at a new wage.
  saved_struct <- panel
  saved_b_cols <- b_field_names
  realloc_panel <- copy(working_data)
  realloc_panel[, c("c_endog", "q_endog", e_field_names) := {
        loc <- location_id
        saved_B <- matrix(
          as.numeric(saved_struct[location_id == loc, .SD, .SDcols = saved_b_cols]),
          byrow = FALSE, nrow = n_worker_types, ncol = n_task_types
        )
        counterfactual_org_outputs_from_b(
          B          = saved_B,
          alpha      = as.numeric(.SD),
          gamma      = gamma_invert,
          wage_guess = wage_guess + 1,  # different wage
          new_theta  = mats$new_theta,
          config     = TEST_CONFIG
        )
      },
      by = "location_id",
      .SDcols = task_mix_cols]

  expect_true(all(is.finite(realloc_panel$c_endog)))
  expect_true(all(is.finite(realloc_panel$q_endog)))

  # Step 3: apply_pricing-style block produces newprice and new_share.
  realloc_panel[, Q := q_endog * avg_labor + qual_exo]
  realloc_panel[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
  realloc_panel[, newprice := counterfactual_best_response_prices(
                  cust_price, Q, C, weight, rho[cnty],
                  outertol, paste(cnty, qy), TEST_CONFIG)]
  realloc_panel[, new_share := counterfactual_logit_shares(
                  Q, newprice, weight, rho[cnty], TEST_CONFIG)]

  expect_true(all(is.finite(realloc_panel$newprice)))
  expect_true(all(is.finite(realloc_panel$new_share)))
  expect_true(sum(realloc_panel$weight * realloc_panel$new_share) <= 1 + 1e-9)

  # Step 4: total-labor aggregation matches the helper helper.
  total_labor <- realloc_panel[, setNames(
        lapply(seq_len(n_worker_types), function(idx) {
          sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)
        }),
        counterfactual_tot_labor_field_names(TEST_CONFIG)
      ),
      by = c("county", "quarter_year")]
  expect_equal(nrow(total_labor), 1L)
  for (col in counterfactual_tot_labor_field_names(TEST_CONFIG)) {
    expect_true(is.finite(total_labor[[col]]))
  }
})

test_that("apply_pricing pattern produces finite Q, C, newprice, new_share", {
  working_data <- build_synthetic_market(8, seed = 99)
  rho_value <- -0.05

  # Pretend a previous step populated c_endog and q_endog.
  working_data[, c_endog := runif(.N, 5, 30)]
  working_data[, q_endog := rnorm(.N, 0, 1)]

  working_data[, Q := q_endog * avg_labor + qual_exo]
  working_data[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
  working_data[, newprice := counterfactual_best_response_prices(
                  cust_price, Q, C, weight, rho_value,
                  TEST_CONFIG$counterfactual_outertol,
                  "test", TEST_CONFIG)]
  working_data[, new_share := counterfactual_logit_shares(
                  Q, newprice, weight, rho_value, TEST_CONFIG)]

  expect_true(all(is.finite(working_data$Q)))
  expect_true(all(is.finite(working_data$C)))
  expect_true(all(is.finite(working_data$newprice)))
  expect_true(all(is.finite(working_data$new_share)))
  expect_true(sum(working_data$new_share * working_data$weight) <= 1 + 1e-9)
})
