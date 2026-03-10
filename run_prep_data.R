# =============================================================================
# RUN DATA PREPARATION SCRIPTS
# =============================================================================
# Runs prep_*.R scripts to generate intermediate data files in mkdata/data/.
# Usage: source("run_prep_data.R")
# =============================================================================

rm(list = ls())

find_prep_project_root <- function(start_dir = getwd()) {
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

make_step_result <- function(ran, success, duration = 0, error = NULL, skipped = FALSE) {
  return(list(
    ran = ran,
    success = success,
    duration = duration,
    error = error,
    skipped = skipped
  ))
}

run_prep_step <- function(step) {
  message("\n", strrep("-", 70))
  message(step$label)
  message(strrep("-", 70))
  message("Script: ", step$script_name)
  message("Outputs: ", paste(step$outputs, collapse = ", "))

  if (!isTRUE(step$enabled)) {
    message("Step disabled. Skipping ", step$script_name)
    return(make_step_result(FALSE, TRUE, skipped = TRUE))
  }

  missing_outputs <- step$outputs[!file.exists(step$outputs)]
  force_step <- length(missing_outputs) > 0

  if (force_step) {
    message("Missing output(s) detected. Forcing rerun:")
    for (missing_output in missing_outputs) {
      message("  ", missing_output)
    }
  }

  should_run <- force_step | needs_rerun(step$script_name, step$dependencies)
  if (!should_run) {
    return(make_step_result(FALSE, TRUE, skipped = TRUE))
  }

  if (length(step$required_inputs) > 0) {
    missing_inputs <- step$required_inputs[!file.exists(step$required_inputs)]

    if (length(missing_inputs) > 0) {
      missing_input_msg <- paste(missing_inputs, collapse = "\n  ")

      if (isTRUE(step$skip_if_inputs_missing)) {
        warning(step$label, " skipped because required inputs are missing:\n  ", missing_input_msg)
        return(make_step_result(FALSE, TRUE, skipped = TRUE))
      }

      stop(step$label, " is missing required inputs:\n  ", missing_input_msg)
    }
  }

  step_start <- Sys.time()
  step_result <- run_with_logging(step$script_name, dependencies = step$dependencies, force = TRUE)

  if (!isTRUE(step_result$success)) {
    stop(step$label, " failed: ", step_result$error)
  }

  assert_required_files(step$outputs)

  step_time <- as.numeric(difftime(Sys.time(), step_start, units = "mins"))
  message(step$label, " complete (", round(step_time, 2), " minutes)")

  step_result$duration <- step_time
  return(step_result)
}

project_root <- find_prep_project_root()
current_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
if (!identical(current_dir, project_root)) {
  message("Switching working directory to project root: ", project_root)
  setwd(project_root)
}

source("config.R")

if (activate_project_renv(project_root)) {
  message("Activated renv environment from ", file.path("renv", "activate.R"))
} else {
  warning("renv/activate.R not found. Using current library paths.")
}

source("utils/logging.R")

prep_start <- Sys.time()

message("\n", strrep("=", 70))
message("DATA PREPARATION PIPELINE")
message(strrep("=", 70))
message("Start time: ", prep_start)
message("Working directory: ", getwd())

RUN_PREP_01_COSMO <- TRUE
RUN_PREP_02_CENSUS <- TRUE
RUN_PREP_03_MSA <- TRUE
RUN_PREP_04_CEX <- TRUE
RUN_PREP_05_PPI <- TRUE
RUN_PREP_06_QCEW <- TRUE
RUN_PREP_00_TRANSACTIONS <- TRUE

ensure_directory(CONFIG$log_dir)
ensure_directory(CONFIG$prep_output_dir)

transaction_required_inputs <- if (nzchar(CONFIG$raw_data_base)) {
  c(
    file.path(CONFIG$raw_data_base, "20200909_raw", "Marketing Intern Data", "x00.csv"),
    file.path(CONFIG$raw_data_base, "20210809_alldata_refresh_withzip", "appointment_export.csv")
  )
} else {
  c('<raw_data_base environment variable>')
}

transaction_output_path <- if (nzchar(CONFIG$raw_data_base)) {
  file.path(CONFIG$raw_data_base, 'compiled_trxns.rds')
} else {
  '<raw_data_base>/compiled_trxns.rds'
}

cex_required_inputs <- c()
for (y in 13:21) {
  cex_required_inputs <- c(
    cex_required_inputs,
    file.path('mkdata', 'raw', '20220711_cex', paste0('intrvw', y), paste0('expn', y), paste0('xpb', y, '.csv')),
    file.path('mkdata', 'raw', '20220711_cex', paste0('intrvw', y), paste0('intrvw', y), paste0('fmli', y, '2.csv')),
    file.path('mkdata', 'raw', '20220711_cex', paste0('intrvw', y), paste0('intrvw', y), paste0('fmli', y, '3.csv')),
    file.path('mkdata', 'raw', '20220711_cex', paste0('intrvw', y), paste0('intrvw', y), paste0('fmli', y, '4.csv')),
    file.path('mkdata', 'raw', '20220711_cex', paste0('intrvw', y + 1), paste0('intrvw', y + 1), paste0('fmli', y + 1, '1.csv'))
  )
}

prep_steps <- list(
  list(
    label = "PREP 01: Service Classifications",
    script_name = "prep_01_cosmo_classify.R",
    enabled = RUN_PREP_01_COSMO,
    dependencies = c(
      "config.R",
      "prep_01_cosmo_classify.R",
      file.path("mkdata", "raw", "20220526_cosmo_classify", "Upwork service desciptions - COMPLETE.xlsx"),
      file.path("mkdata", "raw", "20220526_cosmo_classify", "check_cosmo - COMPLETE (3).xlsx")
    ),
    required_inputs = c(
      file.path("mkdata", "raw", "20220526_cosmo_classify", "Upwork service desciptions - COMPLETE.xlsx"),
      file.path("mkdata", "raw", "20220526_cosmo_classify", "check_cosmo - COMPLETE (3).xlsx")
    ),
    outputs = c(file.path(CONFIG$prep_output_dir, "classified_descriptions.rds")),
    skip_if_inputs_missing = FALSE
  ),
  list(
    label = "PREP 02: Census Population",
    script_name = "prep_02_censuspop.R",
    enabled = RUN_PREP_02_CENSUS,
    dependencies = c(
      "config.R",
      "prep_02_censuspop.R",
      file.path("mkdata", "raw", "202403101_morecspops", "co-est2022-alldata.csv"),
      file.path("mkdata", "raw", "202403101_morecspops", "co-est2019-alldata.csv")
    ),
    required_inputs = c(
      file.path("mkdata", "raw", "202403101_morecspops", "co-est2022-alldata.csv"),
      file.path("mkdata", "raw", "202403101_morecspops", "co-est2019-alldata.csv")
    ),
    outputs = c(file.path(CONFIG$prep_output_dir, "county_census_pop.rds")),
    skip_if_inputs_missing = FALSE
  ),
  list(
    label = "PREP 03: County-MSA Crosswalk",
    script_name = "prep_03_county_msa_xwalk.R",
    enabled = RUN_PREP_03_MSA,
    dependencies = c(
      "config.R",
      "prep_03_county_msa_xwalk.R",
      file.path("mkdata", "raw", "20231023_county_msa_crosswalk", "cex_cleaned_codebook.csv"),
      file.path("mkdata", "raw", "20231023_county_msa_crosswalk", "qcew-county-msa-csa-crosswalk.csv")
    ),
    required_inputs = c(
      file.path("mkdata", "raw", "20231023_county_msa_crosswalk", "cex_cleaned_codebook.csv"),
      file.path("mkdata", "raw", "20231023_county_msa_crosswalk", "qcew-county-msa-csa-crosswalk.csv")
    ),
    outputs = c(file.path(CONFIG$prep_output_dir, "county_msa_xwalk.rds")),
    skip_if_inputs_missing = FALSE
  ),
  list(
    label = "PREP 04: Consumer Expenditure Survey",
    script_name = "prep_04_consumerexpenditure.R",
    enabled = RUN_PREP_04_CEX,
    dependencies = c(
      "config.R",
      "prep_04_consumerexpenditure.R",
      cex_required_inputs
    ),
    required_inputs = cex_required_inputs,
    outputs = c(file.path(CONFIG$prep_output_dir, "cex_outside.rds")),
    skip_if_inputs_missing = FALSE
  ),
  list(
    label = "PREP 05: Producer Price Index",
    script_name = "prep_05_ppi.R",
    enabled = RUN_PREP_05_PPI,
    dependencies = c(
      "config.R",
      "prep_05_ppi.R",
      file.path("mkdata", "raw", "20231227_ppi_cost", "file.csv")
    ),
    required_inputs = c(file.path("mkdata", "raw", "20231227_ppi_cost", "file.csv")),
    outputs = c(file.path(CONFIG$prep_output_dir, "ppi.rds")),
    skip_if_inputs_missing = FALSE
  ),
  list(
    label = "PREP 06: QCEW Wage Data",
    script_name = "prep_06_qcew.R",
    enabled = RUN_PREP_06_QCEW,
    dependencies = c(
      "config.R",
      "prep_06_qcew.R",
      CONFIG$qcew_cache_path,
      file.path("mkdata", "raw", "20220427_qcew_code", "qcew_rscript_example.R")
    ),
    required_inputs = character(0),
    outputs = c(file.path(CONFIG$prep_output_dir, "qcew_county.rds")),
    skip_if_inputs_missing = FALSE
  ),
  list(
    label = "PREP 00: Compile Transactions",
    script_name = "prep_00_compile_transactions.R",
    enabled = RUN_PREP_00_TRANSACTIONS,
    dependencies = c("config.R", "prep_00_compile_transactions.R", if (nzchar(CONFIG$raw_data_base)) transaction_required_inputs else character(0)),
    required_inputs = transaction_required_inputs,
    outputs = c(transaction_output_path),
    skip_if_inputs_missing = TRUE
  )
)

message("\nPrep step map:")
for (step in prep_steps) {
  message("  ", step$script_name, " -> ", paste(step$outputs, collapse = ", "))
}

prep_results <- list()
for (step in prep_steps) {
  prep_results[[step$script_name]] <- run_prep_step(step)
}

prep_end <- Sys.time()
total_time <- difftime(prep_end, prep_start, units = "mins")

write_pipeline_summary(
  prep_results,
  prep_start,
  summary_name = "run_prep_data.log",
  title = "DATA PREPARATION SUMMARY"
)

message("\n", strrep("=", 70))
message("DATA PREPARATION COMPLETE")
message(strrep("=", 70))
message("End time: ", prep_end)
message("Total time: ", round(total_time, 2), " minutes")

message("\nStep Summary:")
for (name in names(prep_results)) {
  step_result <- prep_results[[name]]
  if (step_result$skipped) {
    status_str <- "SKIPPED"
  } else if (step_result$success) {
    status_str <- sprintf("SUCCESS (%.2f min)", step_result$duration)
  } else {
    status_str <- sprintf("FAILURE: %s", step_result$error)
  }
  message(sprintf("  %s: %s", name, status_str))
}

message("\nFiles in ", CONFIG$prep_output_dir, ":")
if (dir.exists(CONFIG$prep_output_dir)) {
  output_files <- list.files(CONFIG$prep_output_dir, full.names = TRUE)
  for (output_file in output_files) {
    output_info <- file.info(output_file)
    message("  ", basename(output_file), " (", round(output_info$size / 1024, 1), " KB)")
  }
} else {
  message("  (directory does not exist)")
}

message("\nLog directory: ", CONFIG$log_dir)
message("Prep summary log: ", file.path(CONFIG$log_dir, "run_prep_data.log"))
message(strrep("=", 70))

