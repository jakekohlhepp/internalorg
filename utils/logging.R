#' =============================================================================
#' LOGGING UTILITIES
#' =============================================================================
#' Provides logging infrastructure for the estimation pipeline.
#' Tracks script execution with timestamps to enable conditional re-runs.
#'
#' Usage:
#'   source("utils/logging.R")
#'   log_init("my_script.R")
#'   log_message("Processing data...", "INFO")
#'   log_complete(success = TRUE, duration = 5.2)
#'
#' Log format:
#'   Plain text with metadata header for easy parsing
#' =============================================================================

# Global state for current log session
.log_env <- new.env(parent = emptyenv())
.log_env$current_script <- NULL
.log_env$current_log_path <- NULL
.log_env$start_time <- NULL

#' Get the log directory path
#' @return Character path to logs directory
get_log_dir <- function() {

  # Use CONFIG if available, otherwise default to "logs"
  if (exists("CONFIG") && !is.null(CONFIG$log_dir)) {
    return(CONFIG$log_dir)
  }
  return("logs")
}

#' Get the path to a script's log file
#' @param script_name Name of the script (e.g., "01_00_build_data.R")
#' @return Character path to the log file
get_log_path <- function(script_name) {
  log_dir <- get_log_dir()
  # Convert script name to log name (replace .R with .log)
  log_name <- sub("\\.R$", ".log", basename(script_name))
  return(file.path(log_dir, log_name))
}

#' Initialize logging for a script
#' @param script_name Name of the script being run
#' @return Invisible NULL
log_init <- function(script_name) {
  log_dir <- get_log_dir()


  # Create logs directory if it doesn't exist
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Store state
  .log_env$current_script <- script_name
  .log_env$current_log_path <- get_log_path(script_name)
  .log_env$start_time <- Sys.time()

  # Write header
  header <- paste0(
    strrep("=", 60), "\n",
    "Script: ", script_name, "\n",
    "Started: ", format(.log_env$start_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "Completed: RUNNING\n",
    "Status: RUNNING\n",
    "Duration: RUNNING\n",
    strrep("=", 60), "\n"
  )

  writeLines(header, .log_env$current_log_path)

  invisible(NULL)
}

#' Add a message to the current log
#' @param msg Message text
#' @param level Log level: "INFO", "WARN", "ERROR"
#' @return Invisible NULL
log_message <- function(msg, level = "INFO") {
  if (is.null(.log_env$current_log_path)) {
    warning("log_message called before log_init")
    return(invisible(NULL))
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- sprintf("[%s] %s: %s\n", timestamp, level, msg)

  # Append to log file
  cat(log_line, file = .log_env$current_log_path, append = TRUE)

  # Also print to console if verbose
  if (exists("CONFIG") && isTRUE(CONFIG$verbose_logging)) {
    message(sprintf("[%s] %s", level, msg))
  }

  invisible(NULL)
}

#' Complete logging for a script
#' @param success Logical indicating if script completed successfully
#' @param duration Optional duration in minutes (calculated if not provided)
#' @return Invisible NULL
log_complete <- function(success, duration = NULL) {
  if (is.null(.log_env$current_log_path)) {
    warning("log_complete called before log_init")
    return(invisible(NULL))
  }

  end_time <- Sys.time()

  if (is.null(duration)) {
    duration <- as.numeric(difftime(end_time, .log_env$start_time, units = "mins"))
  }

  status <- if (success) "SUCCESS" else "FAILURE"

  # Read existing log content (skip header)
  log_content <- readLines(.log_env$current_log_path)

  # Find where messages start (after the header separator)
  header_end <- which(log_content == strrep("=", 60))[2]
  if (is.na(header_end)) header_end <- length(log_content)

  messages <- if (header_end < length(log_content)) {
    log_content[(header_end + 1):length(log_content)]
  } else {
    character(0)
  }

  # Rewrite with updated header
  header <- paste0(
    strrep("=", 60), "\n",
    "Script: ", .log_env$current_script, "\n",
    "Started: ", format(.log_env$start_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "Completed: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "Status: ", status, "\n",
    "Duration: ", sprintf("%.2f minutes", duration), "\n",
    strrep("=", 60)
  )

  full_log <- c(header, messages)
  writeLines(full_log, .log_env$current_log_path)

  # Clear state
  .log_env$current_script <- NULL
  .log_env$current_log_path <- NULL
  .log_env$start_time <- NULL

  invisible(NULL)
}

#' Parse log file metadata
#' @param script_name Name of the script
#' @return List with: exists, status, completed_time, or NULL if log doesn't exist
parse_log_metadata <- function(script_name) {
  log_path <- get_log_path(script_name)

  if (!file.exists(log_path)) {
    return(list(exists = FALSE))
  }

  log_lines <- readLines(log_path, n = 10)

  # Parse metadata from header
  completed_line <- grep("^Completed:", log_lines, value = TRUE)
  status_line <- grep("^Status:", log_lines, value = TRUE)

  if (length(completed_line) == 0 || length(status_line) == 0) {
    return(list(exists = TRUE, status = "UNKNOWN", completed_time = NULL))
  }

  status <- trimws(sub("^Status:", "", status_line[1]))
  completed_str <- trimws(sub("^Completed:", "", completed_line[1]))

  completed_time <- if (completed_str == "RUNNING") {
    NULL
  } else {
    tryCatch(
      as.POSIXct(completed_str, format = "%Y-%m-%d %H:%M:%S"),
      error = function(e) NULL
    )
  }

  return(list(
    exists = TRUE,
    status = status,
    completed_time = completed_time
  ))
}

#' Get the completion timestamp from a log file
#' @param script_name Name of the script
#' @return POSIXct timestamp or NULL if not available
get_log_timestamp <- function(script_name) {
  meta <- parse_log_metadata(script_name)
  return(meta$completed_time)
}

#' Get the status from a log file
#' @param script_name Name of the script
#' @return Character "SUCCESS", "FAILURE", "RUNNING", "UNKNOWN", or NA if no log
get_log_status <- function(script_name) {
  meta <- parse_log_metadata(script_name)
  if (!meta$exists) return(NA_character_)
  return(meta$status)
}

#' Check if a script needs to be re-run
#'
#' A script needs re-running if:
#' 1. No log exists
#' 2. Log shows FAILURE or RUNNING status
#' 3. Script file is newer than the log completion time
#' 4. Any dependency file is newer than the log completion time
#' 5. force_rerun is TRUE in CONFIG
#'
#' @param script_name Name of the script to check
#' @param dependencies Character vector of additional files to check (e.g., "config.R")
#' @return Logical TRUE if script needs to be re-run
needs_rerun <- function(script_name, dependencies = NULL) {
  # Check force_rerun flag
  if (exists("CONFIG") && isTRUE(CONFIG$force_rerun)) {
    message("  -> Force rerun enabled, will run ", script_name)
    return(TRUE)
  }

  # Parse log metadata
  meta <- parse_log_metadata(script_name)

  # No log exists
  if (!meta$exists) {
    message("  -> No log found for ", script_name, ", will run")
    return(TRUE)
  }

  # Log shows failure or still running
  if (meta$status != "SUCCESS") {
    message("  -> Previous run status: ", meta$status, ", will re-run ", script_name)
    return(TRUE)
  }

  # No completion time (shouldn't happen for SUCCESS, but check anyway)
  if (is.null(meta$completed_time)) {
    message("  -> No completion time in log for ", script_name, ", will run")
    return(TRUE)
  }

  log_time <- meta$completed_time

  # Check if script file is newer than log
  if (file.exists(script_name)) {
    script_mtime <- file.mtime(script_name)
    if (script_mtime > log_time) {
      message("  -> ", script_name, " modified since last run, will re-run")
      return(TRUE)
    }
  }

  # Check dependencies
  if (!is.null(dependencies)) {
    for (dep in dependencies) {
      if (file.exists(dep)) {
        dep_mtime <- file.mtime(dep)
        if (dep_mtime > log_time) {
          message("  -> Dependency ", dep, " modified since last run, will re-run ", script_name)
          return(TRUE)
        }
      }
    }
  }

  # All checks passed - no need to re-run
  message("  -> ", script_name, " is up to date, skipping")
  return(FALSE)
}

#' Run a script with logging and error handling
#'
#' Wraps script execution with logging, timing, and error handling.
#'
#' @param script_name Name of the script to run
#' @param dependencies Character vector of dependency files to check
#' @param force Logical to force re-run regardless of timestamps
#' @return List with: ran (logical), success (logical), duration (numeric), error (character or NULL)
run_with_logging <- function(script_name, dependencies = NULL, force = FALSE) {
  # Check if we need to run
  if (!force && !needs_rerun(script_name, dependencies)) {
    return(list(ran = FALSE, success = TRUE, duration = 0, error = NULL, skipped = TRUE))
  }

  # Initialize logging
  log_init(script_name)
  log_message(paste("Starting", script_name))

  start_time <- Sys.time()
  success <- FALSE
  error_msg <- NULL

  tryCatch({
    withCallingHandlers({
      # Source the script in a new environment to avoid polluting global
      source(script_name, local = new.env(parent = globalenv()))
      success <- TRUE
      log_message(paste("Completed", script_name, "successfully"))
    }, warning = function(w) {
      log_message(paste("WARNING:", conditionMessage(w)), "WARN")
      invokeRestart("muffleWarning")
    })
  }, error = function(e) {
    error_msg <<- conditionMessage(e)
    log_message(paste("ERROR:", error_msg), "ERROR")
  })

  duration <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  log_complete(success, duration)

  return(list(
    ran = TRUE,
    success = success,
    duration = duration,
    error = error_msg,
    skipped = FALSE
  ))
}

#' Write a summary log for the entire pipeline run
#'
#' @param results Named list of results from run_with_logging calls
#' @param pipeline_start POSIXct start time of pipeline
#' @return Invisible NULL
write_pipeline_summary <- function(results, pipeline_start,
                                   summary_name = "run_all.log",
                                   title = "PIPELINE SUMMARY") {
  log_dir <- get_log_dir()
  summary_path <- file.path(log_dir, summary_name)

  end_time <- Sys.time()
  total_duration <- as.numeric(difftime(end_time, pipeline_start, units = "mins"))

  # Determine overall status
  all_success <- all(sapply(results, function(r) r$success))
  overall_status <- if (all_success) "SUCCESS" else "FAILURE"

  # Build summary
  lines <- c(
    strrep("=", 60),
    title,
    paste("Started:", format(pipeline_start, "%Y-%m-%d %H:%M:%S")),
    paste("Completed:", format(end_time, "%Y-%m-%d %H:%M:%S")),
    paste("Status:", overall_status),
    paste("Total Duration:", sprintf("%.2f minutes", total_duration)),
    strrep("=", 60),
    "",
    "Step Results:",
    strrep("-", 40)
  )

  for (name in names(results)) {
    r <- results[[name]]
    if (r$skipped) {
      status_str <- "SKIPPED (up to date)"
    } else if (r$success) {
      status_str <- sprintf("SUCCESS (%.2f min)", r$duration)
    } else {
      status_str <- sprintf("FAILURE: %s", r$error)
    }
    lines <- c(lines, sprintf("  %s: %s", name, status_str))
  }

  lines <- c(lines, "", strrep("=", 60))

  writeLines(lines, summary_path)

  invisible(NULL)
}


