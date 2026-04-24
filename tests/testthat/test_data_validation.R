# Unit tests for data validation
# These tests verify that input data meets expected requirements

context("Data validation")

# Get the project root directory (two levels up from tests/testthat)
get_project_root <- function() {
  # When running via test_dir(), working dir is project root
  # When running interactively from tests/testthat, need to go up two levels
  if (file.exists("mkdata/data/01_working.rds")) {
    return(".")
  } else if (file.exists("../../mkdata/data/01_working.rds")) {
    return("../..")
  } else {
    return(NULL)
  }
}

# Skip tests if data files don't exist
skip_if_no_data <- function() {
  root <- get_project_root()
  if (is.null(root) || !file.exists(file.path(root, "mkdata/data/01_working.rds"))) {
    skip("Data file mkdata/data/01_working.rds not found")
  }
}

# Helper to get data path
data_path <- function(filename) {
  root <- get_project_root()
  if (is.null(root)) return(filename)
  file.path(root, "mkdata/data", filename)
}

test_that("working data file exists and is readable", {
  skip_if_no_data()

  working_data <- readRDS(data_path("01_working.rds"))
  expect_true(is.data.frame(working_data) || data.table::is.data.table(working_data))
})

test_that("working data has required columns", {
  skip_if_no_data()

  working_data <- data.table::data.table(readRDS(data_path("01_working.rds")))

  required_cols <- c(
    "county", "quarter_year", "cust_price", "avg_labor",
    "salon_share_subdiv", "outside_share", "s_index"
  )

  for (col in required_cols) {
    expect_true(col %in% names(working_data),
                info = paste("Missing required column:", col))
  }
})

test_that("working data has no negative prices", {
  skip_if_no_data()

  working_data <- data.table::data.table(readRDS(data_path("01_working.rds")))
  expect_true(all(working_data$cust_price > 0),
              info = "Found negative or zero prices")
})

test_that("working data has expected counties", {
  skip_if_no_data()

  working_data <- data.table::data.table(readRDS(data_path("01_working.rds")))
  expected_counties <- c(6037, 17031, 36061)  # LA, Cook, Manhattan

  actual_counties <- unique(as.numeric(working_data$county))
  expect_true(all(expected_counties %in% actual_counties),
              info = paste("Missing counties. Found:", paste(actual_counties, collapse = ", ")))
})

test_that("E_raw columns sum to approximately 1", {
  skip_if_no_data()

  working_data <- data.table::data.table(readRDS(data_path("01_working.rds")))
  e_raw_cols <- grep("^E_raw_[0-9]+$", names(working_data), value = TRUE)

  if (length(e_raw_cols) > 0) {
    row_sums <- rowSums(working_data[, ..e_raw_cols])
    expect_true(all(abs(row_sums - 1) < 0.01),
                info = "E_raw columns do not sum to 1")
  } else {
    skip("No E_raw columns found")
  }
})

test_that("task_mix columns sum to approximately 1", {
  skip_if_no_data()

  working_data <- data.table::data.table(readRDS(data_path("01_working.rds")))
  task_cols <- grep("^task_mix_[0-9]+$", names(working_data), value = TRUE)

  if (length(task_cols) > 0) {
    row_sums <- rowSums(working_data[, ..task_cols])
    expect_true(all(abs(row_sums - 1) < 0.01),
                info = "task_mix columns do not sum to 1")
  } else {
    skip("No task_mix columns found")
  }
})

test_that("minwage file exists and covers required quarters", {
  minwage_path <- data_path("minwage.xlsx")
  if (!file.exists(minwage_path)) {
    skip("Minwage file not found")
  }

  min_wage <- readxl::read_excel(minwage_path)
  expected_quarters <- c(2018.1, 2018.2, 2018.3, 2018.4,
                         2019.1, 2019.2, 2019.3, 2019.4,
                         2020.1, 2020.4, 2021.1, 2021.2)

  actual_quarters <- unique(min_wage$quarter_year)
  missing_quarters <- setdiff(expected_quarters, actual_quarters)
  expect_true(length(missing_quarters) == 0,
              info = paste("Missing quarters:", paste(missing_quarters, collapse = ", ")))
})

test_that("starting values file exists", {
  seeit_path <- data_path("seeit_bb.rds")
  if (!file.exists(seeit_path)) {
    skip("Starting values file not found")
  }

  starting_vals <- readRDS(seeit_path)
  expect_true(is.numeric(starting_vals))
  expect_true(length(starting_vals) > 0)
})

test_that("04 estimation sample artifact exists and is readable", {
  sample_path <- data_path("04_estimation_sample.rds")
  if (!file.exists(sample_path)) {
    skip("Estimation-sample artifact not found")
  }

  estimation_sample <- readRDS(sample_path)
  expect_true(is.list(estimation_sample))
  expect_true("working_data" %in% names(estimation_sample))
  expect_true("estim_matrix" %in% names(estimation_sample))
  expect_true("min_wage_levels" %in% names(estimation_sample))
})

test_that("04 estimation sample has required enriched columns", {
  sample_path <- data_path("04_estimation_sample.rds")
  if (!file.exists(sample_path)) {
    skip("Estimation-sample artifact not found")
  }

  estimation_sample <- readRDS(sample_path)
  working_data <- data.table::data.table(estimation_sample$working_data)
  estim_matrix <- as.data.frame(estimation_sample$estim_matrix)

  required_working_cols <- c(
    "ppi_inputs", "min_wage", "dye_instrument", "labor_instrument",
    "hausman_other_price", "org_cost", "log_rel_mkt", "mk_piece"
  )
  for (col in required_working_cols) {
    expect_true(col %in% names(working_data),
                info = paste("Missing enriched working_data column:", col))
  }

  required_estim_cols <- c("location_id", "county", "quarter_year", "cust_price",
                           "log_rel_mkt", "hausman_other_price")
  for (col in required_estim_cols) {
    expect_true(col %in% names(estim_matrix),
                info = paste("Missing estim_matrix column:", col))
  }

  expect_true(all(!is.na(working_data$ppi_inputs)), info = "ppi_inputs contains missing values")
  expect_true(all(!is.na(working_data$min_wage)), info = "min_wage contains missing values")
  expect_true(all(!is.na(working_data$org_cost)), info = "org_cost contains missing values")
  expect_true(all(is.finite(working_data$hausman_other_price)),
              info = "hausman_other_price contains non-finite values")
  expect_true(all(working_data$cust_price > 0), info = "04 working_data contains nonpositive prices")
  expect_equal(nrow(working_data), nrow(estim_matrix))
})
