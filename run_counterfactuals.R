# =============================================================================
# RUN COUNTERFACTUAL SCRIPTS
# =============================================================================
# Runs the 13_* through 22_* counterfactual scripts after estimation artifacts are available.
# Usage: source("run_counterfactuals.R")
# =============================================================================

make_step_result <- function(ran, success, duration = 0, error = NULL, skipped = FALSE) {
  list(
    ran = ran,
    success = success,
    duration = duration,
    error = error,
    skipped = skipped
  )
}

find_counterfactual_project_root <- function(start_dir = getwd()) {
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

format_input_group <- function(paths) {
  paste(paths, collapse = " OR ")
}

check_required_input_groups <- function(step) {
  if (length(step$required_inputs_any) == 0) {
    return(character(0))
  }

  missing_groups <- vapply(
    step$required_inputs_any,
    function(paths) !any(file.exists(paths)),
    logical(1)
  )

  vapply(
    step$required_inputs_any[missing_groups],
    format_input_group,
    character(1)
  )
}

run_counterfactual_step <- function(step) {
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

  missing_groups <- check_required_input_groups(step)
  if (length(missing_groups) > 0) {
    msg <- paste(missing_groups, collapse = "\n  ")

    if (isTRUE(step$skip_if_inputs_missing)) {
      warning(step$label, " skipped because required inputs are missing:\n  ", msg)
      return(make_step_result(FALSE, TRUE, skipped = TRUE))
    }

    stop(step$label, " is missing required inputs:\n  ", msg)
  }

  should_run <- force_step | needs_rerun(step$script_name, step$dependencies)
  if (!should_run) {
    return(make_step_result(FALSE, TRUE, skipped = TRUE))
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
  step_result
}

project_root <- find_counterfactual_project_root()
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
source("utils/counterfactuals_core.R")

ensure_directory(CONFIG$log_dir)
ensure_counterfactual_dirs()

counterfactual_start <- Sys.time()

message("\n", strrep("=", 70))
message("COUNTERFACTUAL PIPELINE")
message(strrep("=", 70))
message("Start time: ", counterfactual_start)
message("Working directory: ", getwd())

RUN_CF_00_BASELINE <- TRUE
RUN_CF_01_AUTOMATION <- TRUE
RUN_CF_01_NO_VARIANCE <- TRUE
RUN_CF_02_DIFFUSION <- TRUE
RUN_CF_03_SALES_TAX <- TRUE
RUN_CF_04_IMMIGRATION <- TRUE
RUN_CF_05_NO_FRICTIONS <- TRUE
RUN_CF_06_MERGER <- TRUE
RUN_CF_06_SUMMARY <- TRUE
RUN_CF_07_FIGURES <- TRUE

parameter_inputs <- c(
  project_path("results", "data", "06_parameters.rds"),
  legacy_counterfactual_data_path("02_00_parameters.rds")
)

baseline_inputs <- list(
  parameter_inputs,
  c(project_path(CONFIG$prep_output_dir, "01_staff_task.rds"),
    legacy_counterfactual_data_path("01_00_staff_task.rds")),
  c(project_path(CONFIG$prep_output_dir, "01_staff_task_full.rds"),
    legacy_counterfactual_data_path("01_00_staff_task_full.rds")),
  c(project_path("results", "data", "12_data_for_counterfactuals.rds"),
    legacy_counterfactual_data_path("02_06_data_for_counterfactuals.rds")),
  project_path("mkdata", "data", "cex_outside.rds"),
  project_path("mkdata", "data", "county_msa_xwalk.rds")
)

counterfactual_steps <- list(
  list(
    label = "CF 00: Baseline Counterfactual Prep",
    script_name = "13_counterfactual_prep.R",
    enabled = RUN_CF_00_BASELINE,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "13_counterfactual_prep.R"),
    required_inputs_any = baseline_inputs,
    outputs = c(
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_00_working_data.rds")
    ),
    skip_if_inputs_missing = TRUE
  ),
  list(
    label = "CF 01A: Automation",
    script_name = "14_counterfactual_automation.R",
    enabled = RUN_CF_01_AUTOMATION,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "14_counterfactual_automation.R"),
    required_inputs_any = list(
      parameter_inputs,
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_00_working_data.rds")
    ),
    outputs = c(counterfactual_data_path("05_01_wages_automation.rds")),
    skip_if_inputs_missing = TRUE
  ),
  list(
    label = "CF 02: Management Diffusion",
    script_name = "16_counterfactual_diffusion.R",
    enabled = RUN_CF_02_DIFFUSION,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "16_counterfactual_diffusion.R"),
    required_inputs_any = list(
      parameter_inputs,
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_00_working_data.rds")
    ),
    outputs = c(
      counterfactual_data_path("05_02_wages_diffusion.rds"),
      counterfactual_data_path("05_02_prod_diffusion.rds")
    ),
    skip_if_inputs_missing = TRUE
  ),
  list(
    label = "CF 03: Sales Tax",
    script_name = "17_counterfactual_sales_tax.R",
    enabled = RUN_CF_03_SALES_TAX,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "17_counterfactual_sales_tax.R"),
    required_inputs_any = list(
      parameter_inputs,
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_00_working_data.rds")
    ),
    outputs = c(
      counterfactual_data_path("05_03_wages_salestax.rds"),
      counterfactual_data_path("05_03_prod_salestax.rds"),
      counterfactual_data_path("05_03_prod_initial.rds")
    ),
    skip_if_inputs_missing = TRUE
  ),
  list(
    label = "CF 01B: No Within-Market Variance",
    script_name = "15_counterfactual_no_variance.R",
    enabled = RUN_CF_01_NO_VARIANCE,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "15_counterfactual_no_variance.R"),
    required_inputs_any = list(
      parameter_inputs,
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_00_working_data.rds"),
      counterfactual_data_path("05_03_prod_initial.rds")
    ),
    outputs = c(
      counterfactual_data_path("05_01_remove_variance.rds"),
      counterfactual_data_path("05_01_prod_remove_variance.rds")
    ),
    skip_if_inputs_missing = TRUE
  ),
  list(
    label = "CF 04: Immigration",
    script_name = "18_counterfactual_immigration.R",
    enabled = RUN_CF_04_IMMIGRATION,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "18_counterfactual_immigration.R"),
    required_inputs_any = list(
      parameter_inputs,
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_00_working_data.rds")
    ),
    outputs = c(
      counterfactual_data_path("05_04_wages_immigration.rds"),
      counterfactual_data_path("05_04_prod_immigration.rds")
    ),
    skip_if_inputs_missing = TRUE
  ),
  list(
    label = "CF 05: No Frictions",
    script_name = "19_counterfactual_no_frictions.R",
    enabled = RUN_CF_05_NO_FRICTIONS,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "19_counterfactual_no_frictions.R"),
    required_inputs_any = list(
      parameter_inputs,
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_00_working_data.rds")
    ),
    outputs = c(counterfactual_data_path("05_05_nofriction.rds")),
    skip_if_inputs_missing = TRUE
  ),
  list(
    label = "CF 06A: Merger / Increased Concentration",
    script_name = "20_counterfactual_merger.R",
    enabled = RUN_CF_06_MERGER,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "20_counterfactual_merger.R"),
    required_inputs_any = list(
      parameter_inputs,
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_00_working_data.rds")
    ),
    outputs = c(
      counterfactual_data_path("05_06_wages_merger.rds"),
      counterfactual_data_path("05_06_prod_merger.rds")
    ),
    skip_if_inputs_missing = TRUE
  ),
  list(
    label = "CF 06B: Counterfactual Summary Tables",
    script_name = "21_counterfactual_summary.R",
    enabled = RUN_CF_06_SUMMARY,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "21_counterfactual_summary.R"),
    required_inputs_any = list(
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_03_prod_initial.rds"),
      counterfactual_data_path("05_02_wages_diffusion.rds"),
      counterfactual_data_path("05_02_prod_diffusion.rds"),
      counterfactual_data_path("05_03_wages_salestax.rds"),
      counterfactual_data_path("05_03_prod_salestax.rds"),
      counterfactual_data_path("05_04_wages_immigration.rds"),
      counterfactual_data_path("05_04_prod_immigration.rds"),
      counterfactual_data_path("05_06_wages_merger.rds"),
      counterfactual_data_path("05_06_prod_merger.rds")
    ),
    outputs = c(
      counterfactual_output_path("05_06_tot_counterfactuals.tex", "tables"),
      counterfactual_output_path("05_06_bytype_counterfactuals.tex", "tables")
    ),
    skip_if_inputs_missing = TRUE
  ),
  list(
    label = "CF 07: Counterfactual Figures",
    script_name = "22_counterfactual_figures.R",
    enabled = RUN_CF_07_FIGURES,
    dependencies = c("config.R", "utils/counterfactuals_core.R", "22_counterfactual_figures.R"),
    required_inputs_any = list(
      counterfactual_data_path("05_00_initial_wages.rds"),
      counterfactual_data_path("05_00_working_data.rds"),
      counterfactual_data_path("05_04_wages_immigration.rds")
    ),
    outputs = c(
      counterfactual_output_path("05_07_realloc_scatter.png", "figures"),
      counterfactual_output_path("05_07_realloc_price.png", "figures"),
      counterfactual_output_path("05_07_realloc_marketshare.png", "figures"),
      counterfactual_output_path("05_07_realloc_sindex.png", "figures"),
      counterfactual_output_path("05_07_reorg_scatter.png", "figures"),
      counterfactual_output_path("05_07_reorg_price.png", "figures"),
      counterfactual_output_path("05_07_reorg_marketshare.png", "figures"),
      counterfactual_output_path("05_07_reorg_sindex.png", "figures")
    ),
    skip_if_inputs_missing = TRUE
  )
)

counterfactual_results <- list()
for (step in counterfactual_steps) {
  counterfactual_results[[step$script_name]] <- run_counterfactual_step(step)
}

counterfactual_end <- Sys.time()
total_time <- difftime(counterfactual_end, counterfactual_start, units = "mins")

message("\n", strrep("=", 70))
message("COUNTERFACTUAL PIPELINE COMPLETE")
message(strrep("=", 70))
message("End time: ", counterfactual_end)
message("Total time: ", round(total_time, 2), " minutes")

message("\nStep Summary:")
for (name in names(counterfactual_results)) {
  r <- counterfactual_results[[name]]
  if (r$skipped) {
    status_str <- "SKIPPED (up to date or inputs missing)"
  } else if (r$success) {
    status_str <- sprintf("SUCCESS (%.2f min)", r$duration)
  } else {
    status_str <- sprintf("FAILURE: %s", r$error)
  }
  message(sprintf("  %s: %s", name, status_str))
}
