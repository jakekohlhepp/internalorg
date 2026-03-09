#' Setup renv environment only (without running pipeline)
#'
#' This script initializes the reproducible package environment.
#' For full pipeline execution, use run_all.R instead.
#'
#' Usage: source("setup_renv.R")

message("Setting up renv environment...")
message("(For full pipeline, use: source('run_all.R'))")

# Use the same configuration as run_all.R
SNAPSHOT_DATE <- "2024-01-15"
MIN_R_VERSION <- "4.3.0"

# Check R version (minimum version required)
current_r_version <- paste0(R.version$major, ".", R.version$minor)
if (compareVersion(current_r_version, MIN_R_VERSION) < 0) {
  stop("R version ", MIN_R_VERSION, " or later required.\n",
       "Current version: ", current_r_version)
}

# Install renv if needed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (file.exists("renv.lock")) {
  message("Restoring packages from existing renv.lock...")
  renv::restore(prompt = FALSE)
} else {
  message("Initializing new renv environment...")
  message("CRAN snapshot date: ", SNAPSHOT_DATE)

  # Set repository to Posit Package Manager snapshot
  snapshot_repo <- paste0("https://packagemanager.posit.co/cran/", SNAPSHOT_DATE)

  renv::init(bare = TRUE, restart = FALSE)
  options(repos = c(CRAN = snapshot_repo))

  # Required packages
  required_packages <- c(
    "data.table", "lubridate", "stringr", "zoo",
    "gmm", "nloptr", "SQUAREM", "BB", "parallel",
    "readxl", "lessR", "xtable",
    "binsreg", "ggplot2", "dendextend", "qgraph", "adespatial",
    "testthat"
  )

  renv::install(required_packages, prompt = FALSE)
  renv::snapshot(prompt = FALSE)
}

message("\nSetup complete. Packages are pinned in renv.lock")
message("To run the full pipeline: source('run_all.R')")
