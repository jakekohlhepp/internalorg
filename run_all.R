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
#'   3. Assemble estimation sample (04_estimation_sample.R)
#'   4. Run estimation (05_estimation.R)
#'   5. Demand IV spec comparison (06_iv_spec_comparison.R)
#'   6. Counterfactual pipeline (run_counterfactuals.R)
#'
#' Outputs: mkdata/data/04_estimation_sample.rds; results/out/tables/04_summary_stats_structural.tex; results/data/05_parameters.rds; results/out/tables/06_standard_iv_comparison.tex; results/out/tables/06_standard_hausman_fe_comparison.tex; results/out/tables/06_nested_fe_comparison.tex; results/data/counterfactuals/*
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
RUN_ESTIMATION_SAMPLE <- TRUE
RUN_ESTIMATION <- TRUE
RUN_IV_SPEC_COMPARISON <- TRUE
RUN_COUNTERFACTUALS <- TRUE

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
#' STEP 3: Assemble Estimation Sample (04_estimation_sample.R)
#' -----------------------------------------------------------------------------

if (RUN_ESTIMATION_SAMPLE) {
  message("\n", strrep("-", 70))
  message("STEP 3: Checking 04_estimation_sample.R")
  message(strrep("-", 70))

  if (!file.exists("mkdata/data/01_working.rds")) {
    stop("Required data file mkdata/data/01_working.rds not found. ",
         "Run data build step first or check data paths.")
  }
  if (!file.exists("mkdata/data/01_staff_task_full.rds")) {
    stop("Required data file mkdata/data/01_staff_task_full.rds not found. ",
         "Run data build step first or check data paths.")
  }

  step3_deps <- c("config.R", "04_estimation_sample.R")

  if (force_downstream || needs_rerun("04_estimation_sample.R", step3_deps)) {
    step3_start <- Sys.time()

    log_init("04_estimation_sample.R")
    log_message("Starting estimation-sample assembly")

    tryCatch({
      source("04_estimation_sample.R", local = new.env(parent = globalenv()))
      log_message("Estimation-sample assembly completed successfully")
      log_message("Outputs: mkdata/data/04_estimation_sample.rds; results/out/tables/04_summary_stats_structural.tex")
      log_complete(success = TRUE)

      step3_time <- difftime(Sys.time(), step3_start, units = "mins")
      message("STEP 3 complete (", round(step3_time, 2), " minutes)")

      pipeline_results[["04_estimation_sample.R"]] <- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step3_time),
        error = NULL, skipped = FALSE
      )

      # Force downstream steps since this step ran
      force_downstream <- TRUE

    }, error = function(e) {
      log_message(paste("ERROR:", e$message), "ERROR")
      log_complete(success = FALSE)

      pipeline_results[["04_estimation_sample.R"]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )

      stop("Estimation-sample assembly failed: ", e$message)
    })
  } else {
    message("STEP 3 skipped (no changes detected)")
    pipeline_results[["04_estimation_sample.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  }
}

#' -----------------------------------------------------------------------------
#' STEP 4: Run Estimation (05_estimation.R)
#' -----------------------------------------------------------------------------

if (RUN_ESTIMATION) {
  message("\n", strrep("-", 70))
  message("STEP 4: Checking 05_estimation.R")
  message(strrep("-", 70))

  # Check if required data exists
  if (!file.exists("mkdata/data/04_estimation_sample.rds")) {
    stop("Required data file mkdata/data/04_estimation_sample.rds not found. ",
         "Run 04_estimation_sample.R first or check data paths.")
  }

  if (!file.exists("mkdata/data/seeit_bb.rds")) {
    stop("Starting values file mkdata/data/seeit_bb.rds not found.")
  }

  # Dependencies: config.R, preamble.R
  step4_deps <- c("config.R", "preamble.R")

  if (force_downstream || needs_rerun("05_estimation.R", step4_deps)) {
    step4_start <- Sys.time()

    log_init("05_estimation.R")
    log_message("Starting estimation")

    tryCatch({
      source("05_estimation.R")
      log_message("Estimation completed successfully")
      log_message("Output: results/data/05_parameters.rds")
      log_complete(success = TRUE)

      step4_time <- difftime(Sys.time(), step4_start, units = "mins")
      message("STEP 4 complete (", round(step4_time, 2), " minutes)")

      pipeline_results[["05_estimation.R"]] <- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step4_time),
        error = NULL, skipped = FALSE
      )

      # Force downstream steps since this step ran
      force_downstream <- TRUE

    }, error = function(e) {
      log_message(paste("ERROR:", e$message), "ERROR")
      log_complete(success = FALSE)

      pipeline_results[["05_estimation.R"]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )

      stop("Estimation failed: ", e$message)
    })
  } else {
    message("STEP 4 skipped (no changes detected)")
    pipeline_results[["05_estimation.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  }
}

#' -----------------------------------------------------------------------------
#' STEP 5: Demand IV Specification Comparison (06_iv_spec_comparison.R)
#' -----------------------------------------------------------------------------

if (RUN_IV_SPEC_COMPARISON) {
  message("\n", strrep("-", 70))
  message("STEP 5: Checking 06_iv_spec_comparison.R")
  message(strrep("-", 70))

  if (!file.exists("mkdata/data/04_estimation_sample.rds")) {
    stop("Required data file mkdata/data/04_estimation_sample.rds not found. ",
         "Run 04_estimation_sample.R first or check data paths.")
  }

  # Dependencies: config.R, preamble.R
  step5_deps <- c("config.R", "preamble.R")

  if (force_downstream || needs_rerun("06_iv_spec_comparison.R", step5_deps)) {
    step5_start <- Sys.time()

    log_init("06_iv_spec_comparison.R")
    log_message("Starting demand IV specification comparison")

    tryCatch({
      source("06_iv_spec_comparison.R")
      step5_outputs <- c("results/out/tables/06_standard_iv_comparison.tex", "results/out/tables/06_standard_hausman_fe_comparison.tex", "results/out/tables/06_nested_fe_comparison.tex")
      log_message("Demand IV specification comparison completed successfully")
      log_message(paste("Outputs:", paste(step5_outputs, collapse = "; ")))
      log_complete(success = TRUE)

      step5_time <- difftime(Sys.time(), step5_start, units = "mins")
      message("STEP 5 complete (", round(step5_time, 2), " minutes)")

      pipeline_results[["06_iv_spec_comparison.R"]] <- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step5_time),
        error = NULL, skipped = FALSE
      )

    }, error = function(e) {
      log_message(paste("ERROR:", e$message), "ERROR")
      log_complete(success = FALSE)

      pipeline_results[["06_iv_spec_comparison.R"]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )

      stop("Demand IV specification comparison failed: ", e$message)
    })
  } else {
    message("STEP 5 skipped (no changes detected)")
    pipeline_results[["06_iv_spec_comparison.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  }
}

#' -----------------------------------------------------------------------------
#' STEP 6: Counterfactual Pipeline (run_counterfactuals.R)
#' -----------------------------------------------------------------------------

if (RUN_COUNTERFACTUALS) {
  message("\n", strrep("-", 70))
  message("STEP 6: Checking run_counterfactuals.R")
  message(strrep("-", 70))

  if (!file.exists("results/data/05_parameters.rds")) {
    warning("Counterfactual pipeline skipped because results/data/05_parameters.rds is missing.")
    pipeline_results[["run_counterfactuals.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  } else {
    step6_deps <- c("config.R", "utils/counterfactuals_core.R", "run_counterfactuals.R")
    step6_outputs <- c("results/data/counterfactuals", "results/out/tables", "results/out/figures")

    step6_missing_outputs <- step6_outputs[!file.exists(step6_outputs)]
    step6_force <- length(step6_missing_outputs) > 0

    if (step6_force || force_downstream || needs_rerun("run_counterfactuals.R", step6_deps)) {
      step6_start <- Sys.time()

      log_init("run_counterfactuals.R")
      log_message("Starting counterfactual pipeline")

      tryCatch({
        source("run_counterfactuals.R", local = new.env(parent = globalenv()))
        log_message("Counterfactual pipeline completed successfully")
        log_message("Outputs: results/data/counterfactuals/*; results/out/tables/05_06_*.tex; results/out/figures/05_07_*.png")
        log_complete(success = TRUE)

        step6_time <- difftime(Sys.time(), step6_start, units = "mins")
        message("STEP 6 complete (", round(step6_time, 2), " minutes)")

        pipeline_results[["run_counterfactuals.R"]] <- list(
          ran = TRUE, success = TRUE, duration = as.numeric(step6_time),
          error = NULL, skipped = FALSE
        )

      }, error = function(e) {
        log_message(paste("ERROR:", e$message), "ERROR")
        log_complete(success = FALSE)

        pipeline_results[["run_counterfactuals.R"]] <<- list(
          ran = TRUE, success = FALSE, duration = 0,
          error = e$message, skipped = FALSE
        )

        stop("Counterfactual pipeline failed: ", e$message)
      })
    } else {
      message("STEP 6 skipped (no changes detected)")
      pipeline_results[["run_counterfactuals.R"]] <- list(
        ran = FALSE, success = TRUE, duration = 0,
        error = NULL, skipped = TRUE
      )
    }
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
