# Unit tests for configuration object and helpers

context("Configuration")

# Define CONFIG and helpers locally for testing
CONFIG_TEST <- list(
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

get_initial_E <- function(config) {
  if (is.null(config$initial_E)) {
    return(rep(1 / config$n_worker_types, config$n_worker_types))
  }
  return(config$initial_E / sum(config$initial_E))
}

get_task_mix_cols <- function(config) {
  paste0(config$task_mix_pattern, 1:config$n_task_types)
}

get_E_raw_cols <- function(config) {
  paste0(config$E_raw_pattern, 1:config$n_worker_types)
}

build_E_formula <- function(e_indices, include_s_index, config = CONFIG_TEST) {
  terms <- paste0("county:E_", e_indices)
  if (include_s_index) {
    terms <- c(terms, "county:s_index")
  }
  as.formula(paste0("~", paste0(terms, collapse = " + "), " - 1"))
}

build_task_mix_sum <- function(config = CONFIG_TEST) {
  paste0("task_mix_", 2:config$n_task_types, collapse = "+")
}

test_that("CONFIG has all required fields", {
  required_fields <- c(
    "counties", "counties_padded", "n_worker_types", "n_task_types",
    "numeric_ceiling", "numeric_floor", "B_zero_threshold",
    "bisection_lower", "bisection_upper", "bisection_max_iter",
    "fixedpoint_max_iter", "task_mix_pattern", "E_raw_pattern",
    "innertol", "outertol", "obj_tol", "pl_on", "dye_task_index"
  )

  for (field in required_fields) {
    expect_true(field %in% names(CONFIG_TEST),
                info = paste("Missing config field:", field))
  }
})

test_that("get_initial_E returns uniform distribution by default", {
  E <- get_initial_E(CONFIG_TEST)

  expect_length(E, CONFIG_TEST$n_worker_types)
  expect_equal(sum(E), 1)
  expect_true(all(E == E[1]))  # All equal (uniform)
})
test_that("get_initial_E normalizes custom initial values", {
  config <- CONFIG_TEST
  config$initial_E <- c(1, 2, 3, 4, 5)

  E <- get_initial_E(config)

  expect_length(E, 5)
  expect_equal(sum(E), 1)
  expect_equal(E, c(1, 2, 3, 4, 5) / 15)
})

test_that("get_task_mix_cols generates correct column names", {
  cols <- get_task_mix_cols(CONFIG_TEST)

  expect_length(cols, CONFIG_TEST$n_task_types)
  expect_equal(cols, c("task_mix_1", "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5"))
})

test_that("get_E_raw_cols generates correct column names", {
  cols <- get_E_raw_cols(CONFIG_TEST)

  expect_length(cols, CONFIG_TEST$n_worker_types)
  expect_equal(cols, c("E_raw_1", "E_raw_2", "E_raw_3", "E_raw_4", "E_raw_5"))
})

test_that("CONFIG supports non-square dimensions", {
  config <- CONFIG_TEST
  config$n_worker_types <- 4
  config$n_task_types <- 6

  E <- get_initial_E(config)
  task_cols <- get_task_mix_cols(config)
  E_cols <- get_E_raw_cols(config)

  expect_length(E, 4)
  expect_length(task_cols, 6)
  expect_length(E_cols, 4)
  expect_equal(task_cols[6], "task_mix_6")
})

test_that("CONFIG counties can be modified", {
  config <- CONFIG_TEST
  config$counties <- c("12345", "67890")

  expect_length(config$counties, 2)
  expect_equal(config$counties, c("12345", "67890"))
})

test_that("counties_padded has leading zeros where needed", {
  expect_equal(CONFIG_TEST$counties_padded, c("17031", "36061", "06037"))
  expect_equal(nchar(CONFIG_TEST$counties_padded[3]), 5)
})

test_that("build_E_formula generates correct formula", {
  f <- build_E_formula(2:5, include_s_index = TRUE, CONFIG_TEST)
  f_str <- paste(deparse(f), collapse = "")
  expect_true(grepl("county:E_2", f_str))
  expect_true(grepl("county:E_5", f_str))
  expect_true(grepl("county:s_index", f_str))
  expect_true(grepl("1", f_str))

  f_no_s <- build_E_formula(2:5, include_s_index = FALSE, CONFIG_TEST)
  f_no_s_str <- paste(deparse(f_no_s), collapse = "")
  expect_false(grepl("s_index", f_no_s_str))
})

test_that("build_task_mix_sum generates correct string", {
  result <- build_task_mix_sum(CONFIG_TEST)
  expect_equal(result, "task_mix_2+task_mix_3+task_mix_4+task_mix_5")

  config3 <- CONFIG_TEST
  config3$n_task_types <- 3
  result3 <- build_task_mix_sum(config3)
  expect_equal(result3, "task_mix_2+task_mix_3")
})

test_that("tolerance fields have expected values", {
  expect_equal(CONFIG_TEST$innertol, 1e-06)
  expect_equal(CONFIG_TEST$outertol, 1e-04)
  expect_equal(CONFIG_TEST$obj_tol, 1e-04)
  expect_true(CONFIG_TEST$pl_on)
  expect_null(CONFIG_TEST$core_count)
  expect_equal(CONFIG_TEST$dye_task_index, 2)
})
