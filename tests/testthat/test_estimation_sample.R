# Smoke tests for mkdata/data/04_estimation_sample.rds
# The file is produced by 04_estimation_sample.R as part of run_all.R.

context("Estimation sample artifact")

get_estimation_sample_root <- function() {
  if (file.exists("mkdata/data/04_estimation_sample.rds")) {
    return(".")
  } else if (file.exists("../../mkdata/data/04_estimation_sample.rds")) {
    return("../..")
  }
  NULL
}

skip_if_no_estimation_sample <- function() {
  root <- get_estimation_sample_root()
  if (is.null(root)) {
    skip("mkdata/data/04_estimation_sample.rds not found")
  }
}

load_estimation_sample <- function() {
  root <- get_estimation_sample_root()
  readRDS(file.path(root, "mkdata/data/04_estimation_sample.rds"))
}

test_that("estimation sample has the expected shape", {
  skip_if_no_estimation_sample()

  es <- load_estimation_sample()
  expect_true(is.list(es))
  expect_setequal(
    names(es),
    c("working_data", "estim_matrix", "min_wage_levels",
      "quarter_count", "county_count", "skill_count")
  )
  expect_true(data.table::is.data.table(es$working_data))
  expect_true(is.data.frame(es$estim_matrix))
  expect_gt(nrow(es$working_data), 0)
  expect_gt(nrow(es$estim_matrix), 0)
  expect_equal(nrow(es$working_data), nrow(es$estim_matrix))
})

test_that("estimation sample has the columns that 05/06 and preamble.R require", {
  skip_if_no_estimation_sample()

  es <- load_estimation_sample()
  required_working <- c(
    "county", "quarter_year", "cust_price", "avg_labor",
    "log_rel_mkt", "mk_piece", "org_cost",
    "labor_instrument", "dye_instrument", "hausman_other_price", "qy_cnty",
    "ppi_inputs", "min_wage"
  )
  expect_true(all(required_working %in% names(es$working_data)))

  required_estim <- c(
    "avg_labor", "dye_instrument", "hausman_other_price",
    "county", "quarter_year",
    "log_rel_mkt", "cust_price", "org_cost", "mk_piece",
    "qy_cnty", "gamma_normalized", "s_index"
  )
  expect_true(all(required_estim %in% names(es$estim_matrix)))
})
