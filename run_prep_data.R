#' =============================================================================
#' RUN DATA PREPARATION SCRIPTS
#' =============================================================================
#' Runs all prep_*.R scripts to generate intermediate data files.
#' These scripts process raw data into the format needed by the main pipeline.
#'
#' Usage: source("run_prep_data.R")
#'
#' Prerequisites:
#'   - Raw data must be present in mkdata/raw/
#'   - For prep_06_qcew.R: Internet access required for BLS API
#'   - For prep_00_compile_transactions.R: External data access required
#'
#' Output:
#'   mkdata/data/classified_descriptions.rds (from prep_01)
#'   mkdata/data/county_census_pop.rds (from prep_02)
#'   mkdata/data/county_msa_xwalk.rds (from prep_03)
#'   mkdata/data/cex_outside.rds (from prep_04)
#'   mkdata/data/ppi.rds (from prep_05)
#'   mkdata/data/qcew_county.rds (from prep_06)
#' =============================================================================

# Clear environment
rm(list = ls())

# Record start time
prep_start <- Sys.time()

message("\n", strrep("=", 70))
message("DATA PREPARATION PIPELINE")
message(strrep("=", 70))
message("Start time: ", prep_start)
message("Working directory: ", getwd())

#' -----------------------------------------------------------------------------
#' CONFIGURATION
#' -----------------------------------------------------------------------------

# Load configuration and logging utilities
source("config.R")
source("utils/logging.R")

# Which prep scripts to run (set FALSE to skip)
RUN_PREP_01_COSMO <- TRUE
RUN_PREP_02_CENSUS <- TRUE
RUN_PREP_03_MSA <- TRUE
RUN_PREP_04_CEX <- TRUE
RUN_PREP_05_PPI <- TRUE
RUN_PREP_06_QCEW <- TRUE
RUN_PREP_00_TRANSACTIONS <- TRUE  # Requires external data - disabled by default

# Store results for summary
prep_results <- list()

# Create logs directory if needed
if (!dir.exists(CONFIG$log_dir)) {
  dir.create(CONFIG$log_dir, recursive = TRUE)
  message("Created logs directory: ", CONFIG$log_dir)
}

# Create output directory if needed
if (!dir.exists("mkdata/data")) {
  dir.create("mkdata/data", recursive = TRUE)
  message("Created mkdata/data/ directory")
}

#' -----------------------------------------------------------------------------
#' PREP 01: Service Classifications
#' -----------------------------------------------------------------------------

if (RUN_PREP_01_COSMO) {
  message("\n", strrep("-", 70))
  message("PREP 01: Service Classifications")
  message(strrep("-", 70))

  step_start <- Sys.time()
  log_init("prep_01_cosmo_classify.R")
  log_message("Starting service classifications")

  tryCatch({
    source("prep_01_cosmo_classify.R", local = new.env())
    log_message("Service classifications completed successfully")
    log_complete(success = TRUE)

    step_time <- difftime(Sys.time(), step_start, units = "mins")
    message("PREP 01 complete (", round(step_time, 2), " minutes)")

    prep_results[["prep_01_cosmo_classify.R"]] <- list(
      ran = TRUE, success = TRUE, duration = as.numeric(step_time),
      error = NULL, skipped = FALSE
    )
  }, error = function(e) {
    log_message(paste("ERROR:", e$message), "ERROR")
    log_complete(success = FALSE)

    prep_results[["prep_01_cosmo_classify.R"]] <<- list(
      ran = TRUE, success = FALSE, duration = 0,
      error = e$message, skipped = FALSE
    )

    stop("PREP 01 failed: ", e$message)
  })
}

#' -----------------------------------------------------------------------------
#' PREP 02: Census Population
#' -----------------------------------------------------------------------------

if (RUN_PREP_02_CENSUS) {
  message("\n", strrep("-", 70))
  message("PREP 02: Census Population")
  message(strrep("-", 70))

  step_start <- Sys.time()
  log_init("prep_02_censuspop.R")
  log_message("Starting census population processing")

  tryCatch({
    source("prep_02_censuspop.R", local = new.env())
    log_message("Census population completed successfully")
    log_complete(success = TRUE)

    step_time <- difftime(Sys.time(), step_start, units = "mins")
    message("PREP 02 complete (", round(step_time, 2), " minutes)")

    prep_results[["prep_02_censuspop.R"]] <- list(
      ran = TRUE, success = TRUE, duration = as.numeric(step_time),
      error = NULL, skipped = FALSE
    )
  }, error = function(e) {
    log_message(paste("ERROR:", e$message), "ERROR")
    log_complete(success = FALSE)

    prep_results[["prep_02_censuspop.R"]] <<- list(
      ran = TRUE, success = FALSE, duration = 0,
      error = e$message, skipped = FALSE
    )

    stop("PREP 02 failed: ", e$message)
  })
}

#' -----------------------------------------------------------------------------
#' PREP 03: County-MSA Crosswalk
#' -----------------------------------------------------------------------------

if (RUN_PREP_03_MSA) {
  message("\n", strrep("-", 70))
  message("PREP 03: County-MSA Crosswalk")
  message(strrep("-", 70))

  step_start <- Sys.time()
  log_init("prep_03_county_msa_xwalk.R")
  log_message("Starting county-MSA crosswalk")

  tryCatch({
    source("prep_03_county_msa_xwalk.R", local = new.env())
    log_message("County-MSA crosswalk completed successfully")
    log_complete(success = TRUE)

    step_time <- difftime(Sys.time(), step_start, units = "mins")
    message("PREP 03 complete (", round(step_time, 2), " minutes)")

    prep_results[["prep_03_county_msa_xwalk.R"]] <- list(
      ran = TRUE, success = TRUE, duration = as.numeric(step_time),
      error = NULL, skipped = FALSE
    )
  }, error = function(e) {
    log_message(paste("ERROR:", e$message), "ERROR")
    log_complete(success = FALSE)

    prep_results[["prep_03_county_msa_xwalk.R"]] <<- list(
      ran = TRUE, success = FALSE, duration = 0,
      error = e$message, skipped = FALSE
    )

    stop("PREP 03 failed: ", e$message)
  })
}

#' -----------------------------------------------------------------------------
#' PREP 04: Consumer Expenditure Survey
#' -----------------------------------------------------------------------------

if (RUN_PREP_04_CEX) {
  message("\n", strrep("-", 70))
  message("PREP 04: Consumer Expenditure Survey")
  message(strrep("-", 70))

  step_start <- Sys.time()
  log_init("prep_04_consumerexpenditure.R")
  log_message("Starting consumer expenditure processing")

  tryCatch({
    source("prep_04_consumerexpenditure.R", local = new.env())
    log_message("Consumer expenditure completed successfully")
    log_complete(success = TRUE)

    step_time <- difftime(Sys.time(), step_start, units = "mins")
    message("PREP 04 complete (", round(step_time, 2), " minutes)")

    prep_results[["prep_04_consumerexpenditure.R"]] <- list(
      ran = TRUE, success = TRUE, duration = as.numeric(step_time),
      error = NULL, skipped = FALSE
    )
  }, error = function(e) {
    log_message(paste("ERROR:", e$message), "ERROR")
    log_complete(success = FALSE)

    prep_results[["prep_04_consumerexpenditure.R"]] <<- list(
      ran = TRUE, success = FALSE, duration = 0,
      error = e$message, skipped = FALSE
    )

    stop("PREP 04 failed: ", e$message)
  })
}

#' -----------------------------------------------------------------------------
#' PREP 05: Producer Price Index
#' -----------------------------------------------------------------------------

if (RUN_PREP_05_PPI) {
  message("\n", strrep("-", 70))
  message("PREP 05: Producer Price Index")
  message(strrep("-", 70))

  step_start <- Sys.time()
  log_init("prep_05_ppi.R")
  log_message("Starting PPI processing")

  tryCatch({
    source("prep_05_ppi.R", local = new.env())
    log_message("PPI processing completed successfully")
    log_complete(success = TRUE)

    step_time <- difftime(Sys.time(), step_start, units = "mins")
    message("PREP 05 complete (", round(step_time, 2), " minutes)")

    prep_results[["prep_05_ppi.R"]] <- list(
      ran = TRUE, success = TRUE, duration = as.numeric(step_time),
      error = NULL, skipped = FALSE
    )
  }, error = function(e) {
    log_message(paste("ERROR:", e$message), "ERROR")
    log_complete(success = FALSE)

    prep_results[["prep_05_ppi.R"]] <<- list(
      ran = TRUE, success = FALSE, duration = 0,
      error = e$message, skipped = FALSE
    )

    stop("PREP 05 failed: ", e$message)
  })
}

#' -----------------------------------------------------------------------------
#' PREP 06: QCEW Wage Data
#' -----------------------------------------------------------------------------

if (RUN_PREP_06_QCEW) {
  message("\n", strrep("-", 70))
  message("PREP 06: QCEW Wage Data (requires internet)")
  message(strrep("-", 70))

  # Check if API script exists
  if (!file.exists("mkdata/raw/20220427_qcew_code/qcew_rscript_example.R")) {
    warning("PREP 06 skipped: BLS API script not found at mkdata/raw/20220427_qcew_code/qcew_rscript_example.R")
    prep_results[["prep_06_qcew.R"]] <- list(
      ran = FALSE, success = TRUE, duration = 0,
      error = NULL, skipped = TRUE
    )
  } else {
    step_start <- Sys.time()
    log_init("prep_06_qcew.R")
    log_message("Starting QCEW wage data retrieval")

    tryCatch({
      source("prep_06_qcew.R", local = new.env())
      log_message("QCEW wage data completed successfully")
      log_complete(success = TRUE)

      step_time <- difftime(Sys.time(), step_start, units = "mins")
      message("PREP 06 complete (", round(step_time, 2), " minutes)")

      prep_results[["prep_06_qcew.R"]] <- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step_time),
        error = NULL, skipped = FALSE
      )
    }, error = function(e) {
      log_message(paste("ERROR:", e$message), "ERROR")
      log_complete(success = FALSE)

      prep_results[["prep_06_qcew.R"]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )

      stop("PREP 06 failed: ", e$message)
    })
  }
}

#' -----------------------------------------------------------------------------
#' PREP 00: Compile Transactions (OPTIONAL - requires external data)
#' -----------------------------------------------------------------------------

if (RUN_PREP_00_TRANSACTIONS) {
  message("\n", strrep("-", 70))
  message("PREP 00: Compile Transactions (external data required)")
  message(strrep("-", 70))

  step_start <- Sys.time()
  log_init("prep_00_compile_transactions.R")
  log_message("Starting transaction compilation")

  tryCatch({
    source("prep_00_compile_transactions.R", local = new.env())
    log_message("Transaction compilation completed successfully")
    log_complete(success = TRUE)

    step_time <- difftime(Sys.time(), step_start, units = "mins")
    message("PREP 00 complete (", round(step_time, 2), " minutes)")

    prep_results[["prep_00_compile_transactions.R"]] <- list(
      ran = TRUE, success = TRUE, duration = as.numeric(step_time),
      error = NULL, skipped = FALSE
    )
  }, error = function(e) {
    log_message(paste("ERROR:", e$message), "ERROR")
    log_complete(success = FALSE)

    prep_results[["prep_00_compile_transactions.R"]] <<- list(
      ran = TRUE, success = FALSE, duration = 0,
      error = e$message, skipped = FALSE
    )

    stop("PREP 00 failed: ", e$message)
  })
}

#' -----------------------------------------------------------------------------
#' SUMMARY
#' -----------------------------------------------------------------------------

prep_end <- Sys.time()
total_time <- difftime(prep_end, prep_start, units = "mins")

# Write pipeline summary log
write_pipeline_summary(prep_results, prep_start)

message("\n", strrep("=", 70))
message("DATA PREPARATION COMPLETE")
message(strrep("=", 70))
message("End time: ", prep_end)
message("Total time: ", round(total_time, 2), " minutes")

# Print step summary
message("\nStep Summary:")
for (name in names(prep_results)) {
  r <- prep_results[[name]]
  if (r$skipped) {
    status_str <- "SKIPPED"
  } else if (r$success) {
    status_str <- sprintf("SUCCESS (%.2f min)", r$duration)
  } else {
    status_str <- sprintf("FAILURE: %s", r$error)
  }
  message(sprintf("  %s: %s", name, status_str))
}

# Check what files were created
message("\nFiles in mkdata/data/:")
if (dir.exists("mkdata/data")) {
  files <- list.files("mkdata/data", full.names = TRUE)
  for (f in files) {
    info <- file.info(f)
    message("  ", basename(f), " (", round(info$size / 1024, 1), " KB)")
  }
} else {
  message("  (directory does not exist)")
}

message("\nLog directory: ", CONFIG$log_dir)
message(strrep("=", 70))
