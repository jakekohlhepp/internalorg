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
  counterfactual_data_dir = file.path("results", "data", "counterfactuals"),
  counterfactual_table_dir = file.path("results", "out", "tables"),
  counterfactual_figure_dir = file.path("results", "out", "figures"),
  legacy_counterfactual_data_dir = file.path("analysis_final", "data"),
  legacy_counterfactual_table_dir = file.path("analysis_final", "out", "tables"),
  legacy_counterfactual_figure_dir = file.path("analysis_final", "out", "figures"),
  nyc_rent_path = "mkdata/data/nyc_rent_zip_quarterly.rds",
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

  # Partial quarters to exclude from the stylized-facts / full-sample analyses.
  # 2021.3 is only partially observed in the raw data pull.
  excluded_quarters_analysis = c(2021.3),
  counterfactual_focus_quarters = c(2021.2),

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
  innertol = 1e-10,

  # Bisection tolerance for gamma
  outertol = 1e-08,

  # Outer optimization objective tolerance
  obj_tol = 1e-06,

  # The structural wage-parameter solve is expensive. 06_estimation.R runs the
  # full BBsolve pass by default; set JMP_SKIP_STRUCTURAL_OPTIMIZER=true only for
  # refactor smoke tests that need to validate downstream assembly quickly.
  skip_structural_optimizer = tolower(Sys.getenv("JMP_SKIP_STRUCTURAL_OPTIMIZER", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  structural_optimizer_maxit = as.integer(Sys.getenv("JMP_STRUCTURAL_OPTIMIZER_MAXIT", unset = "1000000")),
  check_structural_start = tolower(Sys.getenv("JMP_CHECK_STRUCTURAL_START", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),

  # ---------------------------------------------------------------------------
  # Shared structural solver speed controls
  # ---------------------------------------------------------------------------
  # The wage estimator and counterfactual solvers both solve the same inner
  # entropy-regularized assignment problem. These switches keep speed-oriented
  # behavior explicit and reusable.
  use_solver_cache = tolower(Sys.getenv("JMP_USE_SOLVER_CACHE", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  solver_cache_round_digits = as.integer(Sys.getenv("JMP_SOLVER_CACHE_ROUND_DIGITS", unset = "12")),
  use_solver_warm_starts = tolower(Sys.getenv("JMP_USE_SOLVER_WARM_STARTS", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  use_fixedpoint_warm_starts = tolower(Sys.getenv("JMP_USE_FIXEDPOINT_WARM_STARTS", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  use_staged_solver_tolerances = tolower(Sys.getenv("JMP_USE_STAGED_SOLVER_TOLERANCES", unset = "true")) %in%
    c("true", "t", "1", "yes", "y"),
  coarse_innertol = as.numeric(Sys.getenv("JMP_COARSE_INNERTOL", unset = "1e-5")),
  coarse_outertol = as.numeric(Sys.getenv("JMP_COARSE_OUTERTOL", unset = "1e-3")),
  staged_tolerance_switch_norm = as.numeric(Sys.getenv("JMP_STAGED_TOLERANCE_SWITCH_NORM", unset = "1e-3")),
  use_rcpp_equilibrium = tolower(Sys.getenv("JMP_USE_RCPP_EQUILIBRIUM", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  wage_optimizer_mode = Sys.getenv("JMP_WAGE_OPTIMIZER_MODE", unset = "nleqslv"),
  nleqslv_method = Sys.getenv("JMP_NLEQSLV_METHOD", unset = "Broyden"),
  nleqslv_global = Sys.getenv("JMP_NLEQSLV_GLOBAL", unset = "dbldog"),
  nleqslv_maxit = as.integer(Sys.getenv("JMP_NLEQSLV_MAXIT", unset = "200")),
  nleqslv_trace = as.integer(Sys.getenv("JMP_NLEQSLV_TRACE", unset = "1")),
  county_optimizer_rounds = as.integer(Sys.getenv("JMP_COUNTY_OPTIMIZER_ROUNDS", unset = "1")),
  price_optimizer_maxit = as.integer(Sys.getenv("JMP_PRICE_OPTIMIZER_MAXIT", unset = "1000000")),
  price_optimizer_trace = as.integer(Sys.getenv("JMP_PRICE_OPTIMIZER_TRACE", unset = "3")),

  # ---------------------------------------------------------------------------
  # BBsolve warm-restart checkpoint (debugging only)
  # ---------------------------------------------------------------------------
  # When TRUE and wage_optimizer_mode is "joint" or "county", every Nth call to
  # objective_gmm writes the current theta to mkdata/data/06_bb_warmstart.rds via
  # an atomic temp-rename, so an interrupted BBsolve run can be resumed from the
  # last checkpoint. Default is FALSE because the per-evaluation disk I/O is
  # only useful when actively debugging long BB runs.
  bb_checkpoint_enabled = tolower(Sys.getenv("JMP_BB_CHECKPOINT_ENABLED", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  bb_checkpoint_every = as.integer(Sys.getenv("JMP_BB_CHECKPOINT_EVERY", unset = "10")),

  # ---------------------------------------------------------------------------
  # Bootstrap execution
  # ---------------------------------------------------------------------------
  bootstrap_reps = as.integer(Sys.getenv("JMP_BOOTSTRAP_REPS", unset = "200")),
  bootstrap_seed = as.integer(Sys.getenv("JMP_BOOTSTRAP_SEED", unset = "833927")),
  bootstrap_backend = Sys.getenv("JMP_BOOTSTRAP_BACKEND", unset = "auto"),
  bootstrap_workers = as.integer(Sys.getenv("JMP_BOOTSTRAP_WORKERS", unset = NA_character_)),
  bootstrap_results_dir = Sys.getenv(
    "JMP_BOOTSTRAP_RESULTS_DIR",
    unset = file.path("results", "data", "bootstrap_reps")
  ),
  bootstrap_iteration = as.integer(Sys.getenv("JMP_BOOTSTRAP_ITERATION", unset = NA_character_)),
  bootstrap_iteration_start = as.integer(Sys.getenv("JMP_BOOTSTRAP_ITERATION_START", unset = NA_character_)),
  bootstrap_iteration_end = as.integer(Sys.getenv("JMP_BOOTSTRAP_ITERATION_END", unset = NA_character_)),
  bootstrap_combine_only = tolower(Sys.getenv("JMP_BOOTSTRAP_COMBINE_ONLY", unset = "false")) %in%
    c("true", "t", "1", "yes", "y"),
  slurm_array_task_id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA_character_)),

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

#' Detect operating system
#'
#' @return Character string: "windows", "osx", or "linux"
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  unname(tolower(os))
}

#' Get core count for parallel processing
#'
#' Returns cores in the following priority:
#' 1. CONFIG$core_count (if explicitly set)
#' 2. SLURM_CPUS_PER_TASK environment variable (if on a cluster)
#' 3. MC_CORES environment variable
#' 4. OS-specific auto-detection (Windows: detectCores()-1, Linux/Mac: 1)
#'
#' @param config Configuration list
#' @return Integer number of cores to use
get_core_count <- function(config = CONFIG) {
  if (!is.null(config$core_count)) {
    return(config$core_count)
  }

  # Check environment variables
  slurm_cores <- Sys.getenv("SLURM_CPUS_PER_TASK")
  if (slurm_cores != "") {
    return(as.integer(slurm_cores))
  }

  mc_cores <- Sys.getenv("MC_CORES")
  if (mc_cores != "") {
    return(as.integer(mc_cores))
  }

  # Fallback to auto-detection
  if (get_os() == "windows") {
    return(parallel::detectCores() - 1)
  } else {
    # On Linux/Unix, default to 1 core unless explicitly told otherwise.
    # This prevents 'core-bombing' shared login nodes or cluster nodes.
    return(1)
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


