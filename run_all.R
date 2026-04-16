#' =============================================================================
#' MASTER SCRIPT: Estimation Pipeline
#' =============================================================================
#' This script runs the full estimation pipeline in the correct order.
#' It ensures reproducible package versions using renv with a fixed CRAN snapshot.
#'
#' Features:
#'   - Conditional execution: only re-runs scripts that have changed
#'   - Logging: creates logs in logs/ directory for each step
#'   - Dependency tracking: if an upstream step runs, downstream steps also run
#'
#' Usage: source("run_all.R")
#'
#' Pipeline steps:
#'   0. Make task data (00_mk_tasks_cosmo.R) - only if raw data file exists
#'   1. Setup environment (packages from fixed snapshot date)
#'   2. Build data (01_build_data.R)
#'   3. Run estimation (02_estimation.R)
#'   4. Demand IV spec comparison (02_iv_spec_comparison.R)
#'
#' Outputs: results/data/02_parameters.rds; results/out/tables/02_demand_iv_spec_comparison.tex; results/out/tables/02_nested_iv_spec_comparison.tex
#' =============================================================================

# Clear environment
rm(list = ls())

# Record start time
pipeline_start <- Sys.time()

#' -----------------------------------------------------------------------------
#' CONFIGURATION
#' -----------------------------------------------------------------------------

# Load configuration first (needed for logging settings)
source("config.R")

# Load logging utilities
source("utils/logging.R")

# CRAN snapshot date - all packages will be from this date
# Using Posit Public Package Manager (PPPM) for dated snapshots
# Change this date to update all package versions consistently
SNAPSHOT_DATE <- "2024-01-15"

# Minimum R version required
MIN_R_VERSION <- "4.3.0"

# Pipeline steps to run (set FALSE to skip entirely)
RUN_MAKE_TASKS <- TRUE
RUN_SETUP <- TRUE
RUN_BUILD_DATA <- TRUE
RUN_ESTIMATION <- TRUE
RUN_IV_SPEC_COMPARISON <- TRUE

# Track whether downstream steps should be forced due to upstream changes
force_downstream <- TRUE

# Store results for summary
pipeline_results <- list()

#' -----------------------------------------------------------------------------
#' ENVIRONMENT SETUP
#' -----------------------------------------------------------------------------

message("\n", strrep("=", 70))
message("ESTIMATION PIPELINE")
message(strrep("=", 70))
message("Start time: ", pipeline_start)
message("Working directory: ", getwd())

# Create logs directory if needed
if (!dir.exists(CONFIG$log_dir)) {
  dir.create(CONFIG$log_dir, recursive = TRUE)
  message("Created logs directory: ", CONFIG$log_dir)
}

# Check R version (minimum version required)
current_r_version <- paste0(R.version$major, ".", R.version$minor)
if (compareVersion(current_r_version, MIN_R_VERSION) < 0) {
  stop("R version ", MIN_R_VERSION, " or later required.\n",
       "Current version: ", current_r_version, "\n",
       "Install R from: https://cran.r-project.org/bin/")
}
message("R version: ", current_r_version, " (minimum: ", MIN_R_VERSION, ")")

#' -----------------------------------------------------------------------------
#' STEP 0: Make Task Data (00_mk_tasks_cosmo.R)
#' -----------------------------------------------------------------------------
#' Only runs if the raw data file exists on this machine.

if (RUN_MAKE_TASKS) {
  message("\n", strrep("-", 70))
  message("STEP 0: Checking 00_mk_tasks_cosmo.R")
  message(strrep("-", 70))

  raw_data_file <- file.path(CONFIG$raw_data_base, "compiled_trxns.rds")

  if (!file.exists(raw_data_file)) {
    message("Raw data file not found: ", raw_data_file)
    message("STEP 0 skipped (raw data not available on this machine)")
    pipeline_results[["00_mk_tasks_cosmo.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  } else {
    # Dependencies: config.R
    step0_deps <- c("config.R")

    if (force_downstream || needs_rerun("00_mk_tasks_cosmo.R", step0_deps)) {
      step0_start <- Sys.time()

      log_init("00_mk_tasks_cosmo.R")
      log_message("Starting task data creation")
      log_message(paste("Raw data file:", raw_data_file))

      tryCatch({
        source("00_mk_tasks_cosmo.R", local = new.env(parent = globalenv()))
        log_message("Task data created successfully")
        log_complete(success = TRUE)

        step0_time <- difftime(Sys.time(), step0_start, units = "mins")
        message("STEP 0 complete (", round(step0_time, 2), " minutes)")

        pipeline_results[["00_mk_tasks_cosmo.R"]] <- list(
          ran = TRUE, success = TRUE, duration = as.numeric(step0_time),
          error = NULL, skipped = FALSE
        )

        # Force downstream steps since this step ran
        force_downstream <- TRUE

      }, error = function(e) {
        log_message(paste("ERROR:", e$message), "ERROR")
        log_complete(success = FALSE)

        pipeline_results[["00_mk_tasks_cosmo.R"]] <<- list(
          ran = TRUE, success = FALSE, duration = 0,
          error = e$message, skipped = FALSE
        )

        stop("Task data creation failed: ", e$message)
      })
    } else {
      message("STEP 0 skipped (no changes detected)")
      pipeline_results[["00_mk_tasks_cosmo.R"]] <- list(
        ran = FALSE, success = TRUE, duration = 0,
        error = NULL, skipped = TRUE
      )
    }
  }
}

#' -----------------------------------------------------------------------------
#' STEP 1: Package Environment Setup
#' -----------------------------------------------------------------------------

if (RUN_SETUP) {
  message("\n", strrep("-", 70))
  message("STEP 1: Setting up package environment")
  message(strrep("-", 70))

  # Check if renv is already initialized with a lockfile
  if (file.exists("renv.lock")) {
    message("Found existing renv.lock - restoring packages...")

    if (!requireNamespace("renv", quietly = TRUE)) {
      install.packages("renv")
    }

    # Restore from lockfile
    renv::restore(prompt = FALSE)
    message("Packages restored from renv.lock")

  } else {
    message("No renv.lock found - initializing fresh environment...")
    message("Using CRAN snapshot date: ", SNAPSHOT_DATE)

    # Install renv if needed
    if (!requireNamespace("renv", quietly = TRUE)) {
      install.packages("renv")
    }

    # Set repository to Posit Package Manager snapshot
    snapshot_repo <- paste0(
      "https://packagemanager.posit.co/cran/", SNAPSHOT_DATE
    )

    # Initialize renv
    renv::init(bare = TRUE, restart = FALSE)

    # Configure to use snapshot repository
    options(repos = c(CRAN = snapshot_repo))
    renv::settings$snapshot.type("all")

    # Define required packages
    required_packages <- c(
      # Core data manipulation
      "data.table",

      # Date/time and string handling
      "lubridate",
      "stringr",
      "zoo",

      # Statistics and optimization
      "gmm",
      "nloptr",
      "SQUAREM",
      "BB",

      # Parallel computing
      "parallel",

      # Data I/O
      "readxl",

      # Utilities
      "lessR",
      "xtable",

      # IV diagnostics and inference
      "ivreg",
      "sandwich",
      "lmtest",

      # For build_data.R
      "binsreg",
      "ggplot2",
      "dendextend",
      "qgraph",
      "adespatial",

      # Testing
      "testthat",

      # Fast I/O (optional, for large files)
      "qs"
    )

    message("Installing packages from snapshot...")
    renv::install(required_packages, prompt = FALSE)

    # Create lockfile
    message("Creating renv.lock with pinned versions...")
    renv::snapshot(prompt = FALSE)

    message("Package environment initialized successfully")
  }

  message("STEP 1 complete")
}

#' -----------------------------------------------------------------------------
#' STEP 2: Build Data (01_build_data.R)
#' -----------------------------------------------------------------------------

if (RUN_BUILD_DATA) {
  message("\n", strrep("-", 70))
  message("STEP 2: Checking 01_build_data.R")
  message(strrep("-", 70))

  # Check if input data exists
  if (!file.exists("mkdata/data/00_tasks_cosmo.rds")) {
    warning("Input data file mkdata/data/00_tasks_cosmo.rds not found. ",
            "Run Step 0 first or check data paths.")
    RUN_BUILD_DATA <- FALSE
    pipeline_results[["01_build_data.R"]] <- list(
      ran = FALSE, success = FALSE, duration = 0,
      error = "Input file missing", skipped = FALSE
    )
  } else {
    # Dependencies: config.R, cluster.R (sourced internally)
    step2_deps <- c("config.R", "cluster.R")

    if (force_downstream || needs_rerun("01_build_data.R", step2_deps)) {
      step2_start <- Sys.time()

      log_init("01_build_data.R")
      log_message("Starting data build")

      tryCatch({
        source("01_build_data.R", local = new.env(parent = globalenv()))
        log_message("Data build completed successfully")
        log_message(paste("Output: mkdata/data/01_working.rds"))
        log_complete(success = TRUE)

        step2_time <- difftime(Sys.time(), step2_start, units = "mins")
        message("STEP 2 complete (", round(step2_time, 2), " minutes)")

        pipeline_results[["01_build_data.R"]] <- list(
          ran = TRUE, success = TRUE, duration = as.numeric(step2_time),
          error = NULL, skipped = FALSE
        )

        # Force downstream steps since this step ran
        force_downstream <- TRUE

      }, error = function(e) {
        log_message(paste("ERROR:", e$message), "ERROR")
        log_complete(success = FALSE)

        pipeline_results[["01_build_data.R"]] <<- list(
          ran = TRUE, success = FALSE, duration = 0,
          error = e$message, skipped = FALSE
        )

        stop("Data build failed: ", e$message)
      })
    } else {
      message("STEP 2 skipped (no changes detected)")
      pipeline_results[["01_build_data.R"]] <- list(
        ran = FALSE, success = TRUE, duration = 0,
        error = NULL, skipped = TRUE
      )
    }
  }
}

#' -----------------------------------------------------------------------------
#' STEP 3: Run Estimation (02_estimation.R)
#' -----------------------------------------------------------------------------

if (RUN_ESTIMATION) {
  message("\n", strrep("-", 70))
  message("STEP 3: Checking 02_estimation.R")
  message(strrep("-", 70))

  # Check if required data exists
  if (!file.exists("mkdata/data/01_working.rds")) {
    stop("Required data file mkdata/data/01_working.rds not found. ",
         "Run data build step first or check data paths.")
  }

  if (!file.exists("mkdata/data/seeit_bb.rds")) {
    stop("Starting values file mkdata/data/seeit_bb.rds not found.")
  }

  # Dependencies: config.R, preamble.R
  step3_deps <- c("config.R", "preamble.R")

  if (force_downstream || needs_rerun("02_estimation.R", step3_deps)) {
    step3_start <- Sys.time()

    log_init("02_estimation.R")
    log_message("Starting estimation")

    tryCatch({
      source("02_estimation.R")
      log_message("Estimation completed successfully")
      log_message("Output: results/data/02_parameters.rds")
      log_complete(success = TRUE)

      step3_time <- difftime(Sys.time(), step3_start, units = "mins")
      message("STEP 3 complete (", round(step3_time, 2), " minutes)")

      pipeline_results[["02_estimation.R"]] <- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step3_time),
        error = NULL, skipped = FALSE
      )

      # Force downstream steps since this step ran
      force_downstream <- TRUE

    }, error = function(e) {
      log_message(paste("ERROR:", e$message), "ERROR")
      log_complete(success = FALSE)

      pipeline_results[["02_estimation.R"]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )

      stop("Estimation failed: ", e$message)
    })
  } else {
    message("STEP 3 skipped (no changes detected)")
    pipeline_results[["02_estimation.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  }
}

#' -----------------------------------------------------------------------------
#' STEP 4: Demand IV Specification Comparison (02_iv_spec_comparison.R)
#' -----------------------------------------------------------------------------

if (RUN_IV_SPEC_COMPARISON) {
  message("\n", strrep("-", 70))
  message("STEP 4: Checking 02_iv_spec_comparison.R")
  message(strrep("-", 70))

  if (!file.exists("mkdata/data/01_working.rds")) {
    stop("Required data file mkdata/data/01_working.rds not found. ",
         "Run data build step first or check data paths.")
  }

  # Dependencies: config.R, preamble.R
  step4_deps <- c("config.R", "preamble.R")

  if (force_downstream || needs_rerun("02_iv_spec_comparison.R", step4_deps)) {
    step4_start <- Sys.time()

    log_init("02_iv_spec_comparison.R")
    log_message("Starting demand IV specification comparison")

    tryCatch({
      source("02_iv_spec_comparison.R")
      step4_outputs <- c("results/out/tables/02_demand_iv_spec_comparison.tex", "results/out/tables/02_nested_iv_spec_comparison.tex")
      log_message("Demand IV specification comparison completed successfully")
      log_message(paste("Outputs:", paste(step4_outputs, collapse = "; ")))
      log_complete(success = TRUE)

      step4_time <- difftime(Sys.time(), step4_start, units = "mins")
      message("STEP 4 complete (", round(step4_time, 2), " minutes)")

      pipeline_results[["02_iv_spec_comparison.R"]] <- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step4_time),
        error = NULL, skipped = FALSE
      )

    }, error = function(e) {
      log_message(paste("ERROR:", e$message), "ERROR")
      log_complete(success = FALSE)

      pipeline_results[["02_iv_spec_comparison.R"]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )

      stop("Demand IV specification comparison failed: ", e$message)
    })
  } else {
    message("STEP 4 skipped (no changes detected)")
    pipeline_results[["02_iv_spec_comparison.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  }
}

#' -----------------------------------------------------------------------------
#' SUMMARY
#' -----------------------------------------------------------------------------

pipeline_end <- Sys.time()
total_time <- difftime(pipeline_end, pipeline_start, units = "mins")

# Write pipeline summary log
write_pipeline_summary(pipeline_results, pipeline_start)

message("\n", strrep("=", 70))
message("PIPELINE COMPLETE")
message(strrep("=", 70))
message("End time: ", pipeline_end)
message("Total time: ", round(total_time, 2), " minutes")

# Print step summary
message("\nStep Summary:")
for (name in names(pipeline_results)) {
  r <- pipeline_results[[name]]
  if (r$skipped) {
    status_str <- "SKIPPED (up to date)"
  } else if (r$success) {
    status_str <- sprintf("SUCCESS (%.2f min)", r$duration)
  } else {
    status_str <- sprintf("FAILURE: %s", r$error)
  }
  message(sprintf("  %s: %s", name, status_str))
}

# Print session info for reproducibility
message("\n--- Session Info ---")
message("R version: ", R.version$version.string)
message("Platform: ", R.version$platform)
if (file.exists("renv.lock")) {
  lock <- renv::lockfile_read("renv.lock")
  message("Packages pinned: ", length(lock$Packages))
  message("Snapshot date: ", SNAPSHOT_DATE)
}
message("Log directory: ", CONFIG$log_dir)
message(strrep("=", 70))


