#' =============================================================================
#' MODEL CONFIGURATION
#' =============================================================================
#' This configuration object controls the estimation specification.
#' Modify these values to adapt the model to different settings.
#' =============================================================================

CONFIG <- list(

  # ---------------------------------------------------------------------------
  # Logging and execution control
  # ---------------------------------------------------------------------------
  # Directory for log files
  log_dir = "logs",

  # Set TRUE to force re-run all scripts regardless of timestamps
  force_rerun = FALSE,

  # Print log messages to console
  verbose_logging = TRUE,

  # ---------------------------------------------------------------------------
  # Data paths
  # ---------------------------------------------------------------------------
  # Path to raw transaction data.
  # Override with env var raw_data_base for machine-specific locations.
  raw_data_path = Sys.getenv("JMP_RAW_DATA_PATH", unset = "mkdata/raw"),
  raw_data_base = Sys.getenv("raw_data_base"),
  prep_output_dir = "mkdata/data",
  qcew_cache_path = "mkdata/raw/20220427_qcew_code/qcew_county_cache_2014_2021_812112.rds",
  qcew_force_refresh = FALSE,
  qcew_years = 2014:2021,
  qcew_quarters = 1:4,
  qcew_industry_code = 812112,
  # ---------------------------------------------------------------------------
  # Geographic scope
  # ---------------------------------------------------------------------------
  # County FIPS codes to include in estimation
  # Default: Cook County (Chicago), Manhattan (NYC), Los Angeles County
  counties = c("17031", "36061", "6037"),

  # Padded county FIPS codes (with leading zeros) used in data construction
  # and clustering where the raw data uses zero-padded codes
  counties_padded = c("17031", "36061", "06037"),

  # Estimation sample quarters (exclude 2020 Q2/Q3 COVID disruption quarters)
  estimation_quarters = c(
    2018.1, 2018.2, 2018.3, 2018.4,
    2019.1, 2019.2, 2019.3, 2019.4,
    2020.1, 2020.4,
    2021.1, 2021.2
  ),

  # ---------------------------------------------------------------------------
  # Model dimensions
  # ---------------------------------------------------------------------------
  # Number of worker types (rows of assignment matrix B)
  n_worker_types = 5,


  # Number of task types (columns of assignment matrix B)
  n_task_types = 5,

  # ---------------------------------------------------------------------------
  # Numerical parameters
  # ---------------------------------------------------------------------------
  # Initial guess for worker type distribution (uniform by default)
  # Length must equal n_worker_types; will be normalized to sum to 1
  initial_E = NULL,  # NULL means use uniform: rep(1/n_worker_types, n_worker_types)

  # Numerical ceiling to replace Inf (prevents overflow in exp calculations)
  numeric_ceiling = 1e16,

  # Numerical floor to replace 0 (prevents underflow/division by zero)
  numeric_floor = 1e-16,

  # Threshold below which B matrix entries are set to 0
  B_zero_threshold = 1e-16,

  # ---------------------------------------------------------------------------
  # Bisection parameters (for finding gamma given s_index)
  # ---------------------------------------------------------------------------
  # Initial lower bound for gamma search
  bisection_lower = 1,

  # Initial upper bound for gamma search
  bisection_upper = 10000,

  # Maximum iterations for bisection
  bisection_max_iter = 10000,

  # Bisection bracket adjustment parameters
  bisection_low_test_point = 0.1,      # Test point to check if f(low) > 0
  bisection_high_start_if_positive = 40, # Starting lower bound when f(0.1) > 0
  bisection_nan_step = 10,             # Step size when searching for non-NaN region
  bisection_lower_step = 0.005,        # Step size when lowering lower bound
  bisection_upper_step = 10,           # Step size when raising upper bound

  # ---------------------------------------------------------------------------
  # Fixed-point iteration parameters (SQUAREM)
  # ---------------------------------------------------------------------------
  # Maximum iterations for inner fixed-point

  fixedpoint_max_iter = 100000,

  # ---------------------------------------------------------------------------
  # Tolerance and control parameters
  # ---------------------------------------------------------------------------
  # Inner fixed-point tolerance
  innertol = 1e-06,

  # Bisection tolerance for gamma
  outertol = 1e-04,

  # Outer optimization objective tolerance
  obj_tol = 1e-04,

  # Whether to use parallel processing
  pl_on = TRUE,

  # Number of cores for parallel processing (NULL = auto-detect)
  core_count = NULL,

  # ---------------------------------------------------------------------------
  # Column naming patterns (for extracting data from data.table)
  # ---------------------------------------------------------------------------
  # Pattern for task mix columns (e.g., "task_mix_1", "task_mix_2", ...)
  task_mix_pattern = "task_mix_",

  # Pattern for E_raw columns (observed worker type shares)
  E_raw_pattern = "E_raw_",

  # Pattern for B_raw columns (skill parameters in beta)
  B_raw_pattern = "B_raw_",

  # ---------------------------------------------------------------------------
  # Instrument construction
  # ---------------------------------------------------------------------------
  # Index of the task type used for the dye instrument
  dye_task_index = 2
)

#' Helper function to get initial E vector based on config
#' @param config Configuration list
#' @return Numeric vector of initial worker type distribution
get_initial_E <- function(config = CONFIG) {
  if (is.null(config$initial_E)) {
    return(rep(1 / config$n_worker_types, config$n_worker_types))
  }
  # Normalize to sum to 1
  return(config$initial_E / sum(config$initial_E))
}

#' Helper function to generate column names for task mix
#' @param config Configuration list
#' @return Character vector of task mix column names
get_task_mix_cols <- function(config = CONFIG) {
  paste0(config$task_mix_pattern, 1:config$n_task_types)
}

#' Helper function to generate column names for E_raw
#' @param config Configuration list
#' @return Character vector of E_raw column names
get_E_raw_cols <- function(config = CONFIG) {
  paste0(config$E_raw_pattern, 1:config$n_worker_types)
}

#' Get core count for parallel processing
#'
#' Returns either the configured core count or auto-detects based on OS.
#' On Windows, uses detectCores()-1. On other systems, defaults to 75.
#'
#' @param config Configuration list
#' @return Integer number of cores to use
get_core_count <- function(config = CONFIG) {
  if (!is.null(config$core_count)) {
    return(config$core_count)
  }
  if (get_os() == "windows") {
    return(parallel::detectCores() - 1)
  } else {
    return(75)
  }
}

#' Build model.matrix formula for E_raw columns
#'
#' Generates a formula like ~county:E_2 + county:E_3 + ... + county:E_N + county:s_index - 1
#' for use with model.matrix in GMM estimation.
#'
#' @param e_indices Integer vector of E indices to include (e.g., 2:5 for E_2..E_5)
#' @param include_s_index Logical, whether to include county:s_index term
#' @param config Configuration list
#' @return A formula object
build_E_formula <- function(e_indices, include_s_index, config = CONFIG) {
  terms <- paste0("county:E_", e_indices)
  if (include_s_index) {
    terms <- c(terms, "county:s_index")
  }
  as.formula(paste0("~", paste0(terms, collapse = " + "), " - 1"))
}

#' Build task_mix sum string for formulas
#'
#' Generates a string like "task_mix_2+task_mix_3+task_mix_4+task_mix_5"
#' for use in regression formulas.
#'
#' @param config Configuration list
#' @return Character string of task_mix terms joined by "+"
build_task_mix_sum <- function(config = CONFIG) {
  paste0("task_mix_", 2:config$n_task_types, collapse = "+")
}
get_project_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)

  repeat {
    has_config <- file.exists(file.path(current_dir, "config.R"))
    has_project <- file.exists(file.path(current_dir, "refactor_estimation.Rproj")) |
      dir.exists(file.path(current_dir, ".git"))

    if (has_config & has_project) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Could not locate project root from: ", start_dir)
    }

    current_dir <- parent_dir
  }
}

project_path <- function(..., root_dir = get_project_root()) {
  file.path(root_dir, ...)
}

ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  return(invisible(path))
}

assert_required_files <- function(paths) {
  missing_paths <- paths[!file.exists(paths)]

  if (length(missing_paths) > 0) {
    stop("Missing required file(s):\n  ", paste(missing_paths, collapse = "\n  "))
  }

  return(invisible(paths))
}

assert_required_columns <- function(dt, required_cols, object_name) {
  missing_cols <- setdiff(required_cols, names(dt))

  if (length(missing_cols) > 0) {
    stop(object_name, " is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  return(invisible(TRUE))
}

activate_project_renv <- function(root_dir = get_project_root()) {
  activate_path <- file.path(root_dir, "renv", "activate.R")

  if (!file.exists(activate_path)) {
    return(invisible(FALSE))
  }

  source(activate_path, local = TRUE)
  return(invisible(TRUE))
}


