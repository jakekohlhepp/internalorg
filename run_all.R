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
#'   2b. Stylized facts (02_stylized_facts.R) - only if raw pulls available
#'   2c. Spatial correlation maps (03_spatial_corr.R) - only if geo files available
#'   3. Assemble estimation sample (04_estimation_sample.R)
#'   4. Demand IV spec comparison (05_iv_spec_comparison.R)
#'   5. Run estimation (06_estimation.R)
#'   5b. Monotone-restricted estimation (06b_estimation_monotone.R)
#'   5c. Wage identification diagnostic (06c_wage_identification.R)
#'   6. Standard errors: first-stage 2SLS + Murphy-Topel (07_vcov.R)
#'   7. Display estimates (08_display_estimates.R)
#'   8. Invert gammas (09_invert_gammas.R)
#'   9. Validate model (12_validate.R)
#'  10. Counterfactual pipeline (run_counterfactuals.R)
#'  11. Substitution patterns at equilibrium (20_substitution.R)
#'  12. Productivity substitution patterns at equilibrium (21_substitution_prod.R)
#'
#' Outputs: results/data/02_stylized_facts_data.rds; results/out/tables/02_*.tex; results/out/figures/02_*.png; results/out/figures/03_*.png; mkdata/data/04_estimation_sample.rds; results/out/tables/04_summary_stats_structural.tex; results/data/06_parameters.rds; results/data/06b_parameters_monotone.rds; results/out/tables/05_*.tex; results/data/07_first_stage_vcov.rds; results/data/07_murphy_topel_vcov.rds; results/out/tables/08_*.tex; results/data/09_withgammas.rds; results/data/12_data_for_counterfactuals.rds; results/data/counterfactuals/13_warm_start_wages.rds; results/data/counterfactuals/*; results/out/tables/20_substitute.tex; results/out/tables/21_substitute_prod.tex
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
RUN_STYLIZED_FACTS <- TRUE
RUN_SPATIAL_CORR <- TRUE
RUN_ESTIMATION_SAMPLE <- TRUE
RUN_ESTIMATION <- TRUE
RUN_ESTIMATION_MONOTONE <- TRUE
RUN_IV_SPEC_COMPARISON <- TRUE
RUN_VCOV <- TRUE
RUN_DISPLAY_ESTIMATES <- TRUE
RUN_INVERT_GAMMAS <- TRUE
RUN_SUBSTITUTION <- TRUE
RUN_SUBSTITUTION_PROD <- TRUE
RUN_SKILL_UNITS <- TRUE
RUN_VALIDATE <- TRUE
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
#' STEP 2b: Stylized Facts (02_stylized_facts.R)
#' -----------------------------------------------------------------------------
#' Descriptive industry-wide evidence on the FULL national sample. Requires the
#' confidential raw pulls (chair renters / tips / products) under
#' CONFIG$raw_data_base; skipped gracefully when those are unavailable on this
#' machine, like STEP 0.

if (RUN_STYLIZED_FACTS) {
  message("\n", strrep("-", 70))
  message("STEP 2b: Checking 02_stylized_facts.R")
  message(strrep("-", 70))

  step2b_inputs <- c(
    "mkdata/data/00_tasks_cosmo.rds",
    "mkdata/data/01_staff_task_full.rds"
  )
  chairrenter_path <- if (nzchar(CONFIG$raw_data_base)) {
    file.path(CONFIG$raw_data_base, "20201204_chair_renters/staff_renters.csv")
  } else {
    ""
  }

  if (!nzchar(CONFIG$raw_data_base) ||
      any(!file.exists(step2b_inputs)) ||
      !file.exists(chairrenter_path)) {
    message("STEP 2b skipped (raw pulls / build-data inputs not available on this machine)")
    pipeline_results[["02_stylized_facts.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  } else {
    step2b_deps <- c("config.R", step2b_inputs)
    step2b_outputs <- c("results/data/02_stylized_facts_data.rds")

    if (force_downstream || !file.exists(step2b_outputs) ||
        needs_rerun("02_stylized_facts.R", step2b_deps)) {
      step2b_start <- Sys.time()

      log_init("02_stylized_facts.R")
      log_message("Starting stylized-facts analysis")

      tryCatch({
        source("02_stylized_facts.R", local = new.env(parent = globalenv()))
        assert_required_files(step2b_outputs)
        log_message("Stylized-facts analysis completed successfully")
        log_complete(success = TRUE)

        step2b_time <- difftime(Sys.time(), step2b_start, units = "mins")
        message("STEP 2b complete (", round(step2b_time, 2), " minutes)")

        pipeline_results[["02_stylized_facts.R"]] <- list(
          ran = TRUE, success = TRUE, duration = as.numeric(step2b_time),
          error = NULL, skipped = FALSE
        )
      }, error = function(e) {
        log_message(paste("ERROR:", e$message), "ERROR")
        log_complete(success = FALSE)
        pipeline_results[["02_stylized_facts.R"]] <<- list(
          ran = TRUE, success = FALSE, duration = 0,
          error = e$message, skipped = FALSE
        )
        stop("Stylized-facts analysis failed: ", e$message)
      })
    } else {
      message("STEP 2b skipped (no changes detected)")
      pipeline_results[["02_stylized_facts.R"]] <- list(
        ran = FALSE, success = TRUE, duration = 0,
        error = NULL, skipped = TRUE
      )
    }
  }
}

#' -----------------------------------------------------------------------------
#' STEP 2c: Spatial Correlation Maps (03_spatial_corr.R)
#' -----------------------------------------------------------------------------
#' ZIP-level choropleths of productivity / specialization / return rate for the
#' three focal counties plus a sample-coverage map. Consumes 02's firm-quarter
#' panel and the Census/GEOCORR geo files under CONFIG$raw_data_path; skipped
#' gracefully when those are unavailable.

if (RUN_SPATIAL_CORR) {
  message("\n", strrep("-", 70))
  message("STEP 2c: Checking 03_spatial_corr.R")
  message(strrep("-", 70))

  stylized_path <- "results/data/02_stylized_facts_data.rds"
  zcta_county_path <- if (nzchar(CONFIG$raw_data_path)) {
    file.path(CONFIG$raw_data_path, "20220727_countypop/geocorr2022_2220801561.csv")
  } else {
    ""
  }
  zcta_shape_path <- if (nzchar(CONFIG$raw_data_path)) {
    file.path(CONFIG$raw_data_path,
              "20240415_census_zcta_shapefiles/cb_2018_us_zcta510_500k.shp")
  } else {
    ""
  }

  if (!file.exists(stylized_path) ||
      !nzchar(CONFIG$raw_data_path) ||
      !file.exists(zcta_county_path) ||
      !file.exists(zcta_shape_path)) {
    message("STEP 2c skipped (02 output or Census geo files not available on this machine)")
    pipeline_results[["03_spatial_corr.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  } else {
    step2c_deps <- c("config.R", stylized_path)
    step2c_outputs <- c("results/out/figures/03_coverage.png")

    if (force_downstream || !file.exists(step2c_outputs) ||
        needs_rerun("03_spatial_corr.R", step2c_deps)) {
      step2c_start <- Sys.time()

      log_init("03_spatial_corr.R")
      log_message("Starting spatial-correlation maps")

      tryCatch({
        source("03_spatial_corr.R", local = new.env(parent = globalenv()))
        assert_required_files(step2c_outputs)
        log_message("Spatial-correlation maps completed successfully")
        log_complete(success = TRUE)

        step2c_time <- difftime(Sys.time(), step2c_start, units = "mins")
        message("STEP 2c complete (", round(step2c_time, 2), " minutes)")

        pipeline_results[["03_spatial_corr.R"]] <- list(
          ran = TRUE, success = TRUE, duration = as.numeric(step2c_time),
          error = NULL, skipped = FALSE
        )
      }, error = function(e) {
        log_message(paste("ERROR:", e$message), "ERROR")
        log_complete(success = FALSE)
        pipeline_results[["03_spatial_corr.R"]] <<- list(
          ran = TRUE, success = FALSE, duration = 0,
          error = e$message, skipped = FALSE
        )
        stop("Spatial-correlation maps failed: ", e$message)
      })
    } else {
      message("STEP 2c skipped (no changes detected)")
      pipeline_results[["03_spatial_corr.R"]] <- list(
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

  step3_deps <- c(
    "config.R",
    "mkdata/data/01_working.rds",
    "mkdata/data/01_staff_task_full.rds",
    "mkdata/data/ppi.rds",
    "mkdata/data/minwage.xlsx"
  )

  if (force_downstream || needs_rerun("04_estimation_sample.R", step3_deps)) {
    step3_start <- Sys.time()
    step3_outputs <- c(
      "mkdata/data/04_estimation_sample.rds",
      "results/out/tables/04_summary_stats_structural.tex"
    )

    log_init("04_estimation_sample.R")
    log_message("Starting estimation-sample assembly")

    tryCatch({
      source("04_estimation_sample.R", local = new.env(parent = globalenv()))
      assert_required_files(step3_outputs)
      log_message("Estimation-sample assembly completed successfully")
      log_message(paste("Outputs:", paste(step3_outputs, collapse = "; ")))
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
#' STEP 4: Demand IV Specification Comparison (05_iv_spec_comparison.R)
#' -----------------------------------------------------------------------------

if (RUN_IV_SPEC_COMPARISON) {
  message("\n", strrep("-", 70))
  message("STEP 4: Checking 05_iv_spec_comparison.R")
  message(strrep("-", 70))

  if (!file.exists("mkdata/data/04_estimation_sample.rds")) {
    stop("Required data file mkdata/data/04_estimation_sample.rds not found. ",
         "Run 04_estimation_sample.R first or check data paths.")
  }

  # Dependencies: config.R
  step4_deps <- c(
    "config.R",
    "mkdata/data/04_estimation_sample.rds"
  )

  if (force_downstream || needs_rerun("05_iv_spec_comparison.R", step4_deps)) {
    step4_start <- Sys.time()
    step4_outputs <- c(
      "results/out/tables/05_standard_iv_comparison.tex",
      "results/out/tables/05_standard_hausman_fe_comparison.tex",
      "results/out/tables/05_nested_fe_comparison.tex"
    )

    log_init("05_iv_spec_comparison.R")
    log_message("Starting demand IV specification comparison")

    tryCatch({
      source("05_iv_spec_comparison.R")
      assert_required_files(step4_outputs)
      log_message("Demand IV specification comparison completed successfully")
      log_message(paste("Outputs:", paste(step4_outputs, collapse = "; ")))
      log_complete(success = TRUE)

      step4_time <- difftime(Sys.time(), step4_start, units = "mins")
      message("STEP 4 complete (", round(step4_time, 2), " minutes)")

      pipeline_results[["05_iv_spec_comparison.R"]] <- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step4_time),
        error = NULL, skipped = FALSE
      )

      force_downstream <- TRUE

    }, error = function(e) {
      log_message(paste("ERROR:", e$message), "ERROR")
      log_complete(success = FALSE)

      pipeline_results[["05_iv_spec_comparison.R"]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )

      stop("Demand IV specification comparison failed: ", e$message)
    })
  } else {
    message("STEP 4 skipped (no changes detected)")
    pipeline_results[["05_iv_spec_comparison.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  }
}

#' -----------------------------------------------------------------------------
#' STEP 5: Run Estimation (06_estimation.R)
#' -----------------------------------------------------------------------------

if (RUN_ESTIMATION) {
  message("\n", strrep("-", 70))
  message("STEP 5: Checking 06_estimation.R")
  message(strrep("-", 70))

  if (!file.exists("mkdata/data/04_estimation_sample.rds")) {
    stop("Required data file mkdata/data/04_estimation_sample.rds not found. ",
         "Run 04_estimation_sample.R first or check data paths.")
  }

  if (!file.exists("mkdata/data/seeit_bb.rds")) {
    stop("Starting values file mkdata/data/seeit_bb.rds not found.")
  }

  step5_deps <- c(
    "config.R",
    "preamble.R",
    "mkdata/data/04_estimation_sample.rds",
    "mkdata/data/seeit_bb.rds"
  )

  if (force_downstream || needs_rerun("06_estimation.R", step5_deps)) {
    step5_start <- Sys.time()
    step5_output <- "results/data/06_parameters.rds"

    log_init("06_estimation.R")
    log_message("Starting estimation")

    tryCatch({
      source("06_estimation.R")
      assert_required_files(step5_output)
      log_message("Estimation completed successfully")
      log_message(paste("Output:", step5_output))
      log_complete(success = TRUE)

      step5_time <- difftime(Sys.time(), step5_start, units = "mins")
      message("STEP 5 complete (", round(step5_time, 2), " minutes)")

      pipeline_results[["06_estimation.R"]] <- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step5_time),
        error = NULL, skipped = FALSE
      )

      force_downstream <- TRUE

    }, error = function(e) {
      log_message(paste("ERROR:", e$message), "ERROR")
      log_complete(success = FALSE)

      pipeline_results[["06_estimation.R"]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )

      stop("Estimation failed: ", e$message)
    })
  } else {
    message("STEP 5 skipped (no changes detected)")
    pipeline_results[["06_estimation.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  }
}

#' -----------------------------------------------------------------------------
#' Helper: run a standard pipeline step
#' -----------------------------------------------------------------------------

run_pipeline_step <- function(step_label, script_name, deps, outputs,
                              required_inputs = character(0),
                              description = script_name) {
  message("\n", strrep("-", 70))
  message(step_label, ": Checking ", script_name)
  message(strrep("-", 70))

  missing_inputs <- required_inputs[!file.exists(required_inputs)]
  if (length(missing_inputs) > 0) {
    stop(step_label, " missing required inputs: ",
         paste(missing_inputs, collapse = ", "))
  }

  missing_outputs <- outputs[!file.exists(outputs)]
  force_step <- length(missing_outputs) > 0

  if (!force_step && !force_downstream && !needs_rerun(script_name, deps)) {
    message(step_label, " skipped (no changes detected)")
    pipeline_results[[script_name]] <<- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
    return(invisible(NULL))
  }

  step_start <- Sys.time()
  log_init(script_name)
  log_message(paste("Starting", description))

  tryCatch({
    source(script_name, local = new.env(parent = globalenv()))
    assert_required_files(outputs)
    log_message(paste(description, "completed successfully"))
    log_message(paste("Outputs:", paste(outputs, collapse = "; ")))
    log_complete(success = TRUE)

    step_time <- difftime(Sys.time(), step_start, units = "mins")
    message(step_label, " complete (", round(step_time, 2), " minutes)")

    pipeline_results[[script_name]] <<- list(
      ran = TRUE, success = TRUE, duration = as.numeric(step_time),
      error = NULL, skipped = FALSE
    )

    force_downstream <<- TRUE
  }, error = function(e) {
    log_message(paste("ERROR:", e$message), "ERROR")
    log_complete(success = FALSE)
    pipeline_results[[script_name]] <<- list(
      ran = TRUE, success = FALSE, duration = 0,
      error = e$message, skipped = FALSE
    )
    stop(description, " failed: ", e$message)
  })
}

#' -----------------------------------------------------------------------------
#' STEP 5b: Monotone-restricted estimation (06b_estimation_monotone.R)
#' -----------------------------------------------------------------------------
#' Robustness variant of STEP 5: re-estimates the structural model imposing the
#' workers-as-rows monotonicity restriction on each county's skill matrix. Feeds
#' the 06b comparison diagnostics (compare/figures/display scripts).

if (RUN_ESTIMATION_MONOTONE) {
  run_pipeline_step(
    step_label = "STEP 5b",
    script_name = "06b_estimation_monotone.R",
    deps = c("config.R", "preamble.R",
             "utils/constrained_demand_iv.R",
             "mkdata/data/04_estimation_sample.rds",
             "mkdata/data/seeit_bb.rds"),
    outputs = c("results/data/06b_parameters_monotone.rds",
                "results/data/06b_perms.rds",
                "results/data/06b_qp_diagnostics.rds"),
    required_inputs = c("mkdata/data/04_estimation_sample.rds",
                        "mkdata/data/seeit_bb.rds"),
    description = "monotone-restricted (workers-as-rows) estimation"
  )
}

#' -----------------------------------------------------------------------------
#' STEP 5c: Wage-stage local identification diagnostic (06c_wage_identification.R)
#' -----------------------------------------------------------------------------

if (!exists("RUN_WAGE_IDENTIFICATION") || isTRUE(RUN_WAGE_IDENTIFICATION)) {
  run_pipeline_step(
    step_label = "STEP 5c",
    script_name = "06c_wage_identification.R",
    deps = c("config.R", "preamble.R",
             "mkdata/data/04_estimation_sample.rds",
             "mkdata/data/seeit_bb.rds"),
    outputs = c("results/data/06c_wage_identification.rds",
                "results/out/tables/06c_wage_eigenvalues.tex",
                "results/out/tables/06c_wage_perturbation.tex"),
    required_inputs = c("mkdata/data/04_estimation_sample.rds",
                        "mkdata/data/seeit_bb.rds"),
    description = "Wage-stage Hessian + perturbation diagnostic"
  )
}

#' -----------------------------------------------------------------------------
#' STEP 6: Standard errors -- analytical first-stage 2SLS + Murphy-Topel
#'         structural sandwich (07_vcov.R)
#' -----------------------------------------------------------------------------
#' Replaces the retired Petrin-Train bootstrap (07_bootstrap.R, now in legacy/).
#' 07_vcov.R produces both the analytical clustered 2SLS demand vcov and the
#' two-step Murphy-Topel structural vcov; see docs/murphy_topel_proposal.md.

if (RUN_VCOV) {
  run_pipeline_step(
    step_label = "STEP 6",
    script_name = "07_vcov.R",
    deps = c("config.R", "preamble.R", "utils/estimation_pipeline.R",
             "mkdata/data/04_estimation_sample.rds",
             "results/data/06_parameters.rds"),
    outputs = c("results/data/07_first_stage_vcov.rds",
                "results/data/07_murphy_topel_vcov.rds"),
    required_inputs = c("mkdata/data/04_estimation_sample.rds",
                        "results/data/06_parameters.rds"),
    description = "first-stage 2SLS + Murphy-Topel structural SEs"
  )
}

#' -----------------------------------------------------------------------------
#' STEP 7: Display Estimates (08_display_estimates.R)
#' -----------------------------------------------------------------------------

if (RUN_DISPLAY_ESTIMATES) {
  run_pipeline_step(
    step_label = "STEP 7",
    script_name = "08_display_estimates.R",
    deps = c("config.R", "preamble.R",
             "mkdata/data/01_keytask.rds",
             "mkdata/data/04_estimation_sample.rds",
             "results/data/06_parameters.rds"),
    outputs = c("results/out/tables/08_org_price.tex",
                "results/out/tables/08_time_effects.tex",
                "results/out/tables/08_model_fit.tex"),
    required_inputs = c("mkdata/data/01_keytask.rds",
                        "mkdata/data/04_estimation_sample.rds",
                        "results/data/06_parameters.rds"),
    description = "display estimates"
  )
}

#' -----------------------------------------------------------------------------
#' STEP 8: Invert Gammas (09_invert_gammas.R)
#' -----------------------------------------------------------------------------

if (RUN_INVERT_GAMMAS) {
  run_pipeline_step(
    step_label = "STEP 8",
    script_name = "09_invert_gammas.R",
    deps = c("config.R", "preamble.R",
             "mkdata/data/01_staff_task.rds",
             "mkdata/data/01_staff_task_full.rds",
             "mkdata/data/04_estimation_sample.rds",
             "results/data/06_parameters.rds"),
    outputs = c("results/data/09_withgammas.rds",
                "results/out/figures/09_gamma_dist.png"),
    required_inputs = c("mkdata/data/01_staff_task.rds",
                        "mkdata/data/01_staff_task_full.rds",
                        "mkdata/data/04_estimation_sample.rds",
                        "results/data/06_parameters.rds"),
    description = "gamma inversion"
  )
}

#' -----------------------------------------------------------------------------
#' STEP 9: Validate Model (12_validate.R)
#' -----------------------------------------------------------------------------

if (RUN_VALIDATE) {
  run_pipeline_step(
    step_label = "STEP 9",
    script_name = "12_validate.R",
    deps = c("config.R",
             "results/data/06_parameters.rds",
             "results/data/09_withgammas.rds",
             "mkdata/data/01_staff_task_full.rds"),
    outputs = c("results/data/12_data_for_counterfactuals.rds",
                "results/out/tables/12_validate_corr.tex"),
    required_inputs = c("results/data/06_parameters.rds",
                        "results/data/09_withgammas.rds",
                        "mkdata/data/01_staff_task_full.rds"),
    description = "model validation"
  )
}

#' -----------------------------------------------------------------------------
#' Counterfactual warm starts -- committed inputs, NOT a pipeline step
#' -----------------------------------------------------------------------------
#' The baseline warm start results/data/counterfactuals/13_warm_start_wages.rds
#' (and the per-counterfactual 14_/15_/16_/17_warm_start_wages_*.rds) are
#' committed to version control and read directly by 13_counterfactual_prep.R /
#' 14_-17_. compile_warm_start_wages.R and build_la_warm_start_*.R are standalone
#' tools you re-run BY HAND only to refresh those seeds when a better-clearing
#' wage vector is found -- they are intentionally not part of run_all.R. See
#' docs/data_dependencies.md and docs/counterfactual_tolerances.md.

#' -----------------------------------------------------------------------------
#' STEP 10: Counterfactual Pipeline (run_counterfactuals.R)
#' -----------------------------------------------------------------------------

if (RUN_COUNTERFACTUALS) {
  message("\n", strrep("-", 70))
  message("STEP 10: Checking run_counterfactuals.R")
  message(strrep("-", 70))

  if (!file.exists("results/data/06_parameters.rds")) {
    warning("Counterfactual pipeline skipped because results/data/06_parameters.rds is missing.")
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
        log_message("Outputs: results/data/counterfactuals/*; results/out/tables/18_*.tex; results/out/figures/19_*.png")
        log_complete(success = TRUE)

        step6_time <- difftime(Sys.time(), step6_start, units = "mins")
        message("STEP 10 complete (", round(step6_time, 2), " minutes)")

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
      message("STEP 10 skipped (no changes detected)")
      pipeline_results[["run_counterfactuals.R"]] <- list(
        ran = FALSE, success = TRUE, duration = 0,
        error = NULL, skipped = TRUE
      )
    }
  }
}

#' -----------------------------------------------------------------------------
#' STEP 11: Substitution Patterns at Equilibrium (20_substitution.R)
#' -----------------------------------------------------------------------------

if (RUN_SUBSTITUTION) {
  run_pipeline_step(
    step_label = "STEP 11",
    script_name = "20_substitution.R",
    deps = c("config.R",
             "utils/counterfactuals_core.R",
             "results/data/06_parameters.rds",
             "results/data/counterfactuals/13_initial_wages.rds",
             "results/data/counterfactuals/13_working_data.rds"),
    outputs = c("results/out/tables/20_substitute.tex"),
    required_inputs = c("results/data/06_parameters.rds",
                        "results/data/counterfactuals/13_initial_wages.rds",
                        "results/data/counterfactuals/13_working_data.rds"),
    description = "substitution patterns at equilibrium wages"
  )
}

#' -----------------------------------------------------------------------------
#' STEP 12: Productivity Substitution Patterns at Equilibrium (21_substitution_prod.R)
#' -----------------------------------------------------------------------------

if (RUN_SUBSTITUTION_PROD) {
  run_pipeline_step(
    step_label = "STEP 12",
    script_name = "21_substitution_prod.R",
    deps = c("config.R",
             "utils/counterfactuals_core.R",
             "results/data/06_parameters.rds",
             "results/data/counterfactuals/13_initial_wages.rds",
             "results/data/counterfactuals/13_working_data.rds"),
    outputs = c("results/out/tables/21_substitute_prod.tex"),
    required_inputs = c("results/data/06_parameters.rds",
                        "results/data/counterfactuals/13_initial_wages.rds",
                        "results/data/counterfactuals/13_working_data.rds"),
    description = "productivity substitution patterns at equilibrium wages"
  )
}

#' -----------------------------------------------------------------------------
#' STEP 13: Skill Parameters in Interpretable Units (22_skill_parameter_units.R)
#' -----------------------------------------------------------------------------

if (RUN_SKILL_UNITS) {
  run_pipeline_step(
    step_label = "STEP 13",
    script_name = "22_skill_parameter_units.R",
    deps = c("config.R",
             "mkdata/data/01_keytask.rds",
             "results/data/06_parameters.rds",
             "mkdata/data/04_estimation_sample.rds"),
    outputs = c("results/out/tables/22_skill_units.tex",
                "results/data/22_skill_units.csv"),
    required_inputs = c("mkdata/data/01_keytask.rds",
                        "results/data/06_parameters.rds",
                        "mkdata/data/04_estimation_sample.rds"),
    description = "skill parameters in interpretable units"
  )
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
