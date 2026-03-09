#' =============================================================================
#' PREP 00: Compile Raw Transaction Data
#' =============================================================================
#' Compiles raw salon transaction data from multiple data pulls into a single
#' unified transaction file.
#'
#' *** IMPORTANT: THIS SCRIPT REQUIRES EXTERNAL DATA ACCESS ***
#' This script accesses raw data files that are NOT included in the repository
#' due to their size (10GB+) and data confidentiality. It should only be run
#' on machines with access to the original data files.
#'
#' The compiled output (compiled_trxns.rds) is the input for 00_mk_tasks_cosmo.R.
#'
#' Input:  CONFIG$raw_data_path/20200909_raw/Marketing Intern Data/*.csv
#'         CONFIG$raw_data_path/20210809_alldata_refresh_withzip/appointment_export.csv
#' Output: CONFIG$raw_data_path/compiled_trxns.rds
#'
#' Output Schema:
#'   app_id: Appointment ID (UUID)
#'   location_id: Salon location ID (UUID)
#'   staff_id: Employee ID (UUID)
#'   customer_id: Customer ID (UUID)
#'   service_id: Service ID (UUID)
#'   service_performed: Service description
#'   price: Service price
#'   duration: Service duration in minutes
#'   total_app_time: Total appointment time
#'   app_datetime: Appointment datetime
#'   date: Date extracted from datetime
#'   location_state, location_city, location_zip: Location info
#'   business_id, industry: Business info
#'
#' Dependencies: config.R (for CONFIG$raw_data_path)
#' =============================================================================

library('data.table')
library('stringr')
library('lubridate')

# Load configuration for data paths
source('config.R')

#' -----------------------------------------------------------------------------
#' CONFIGURATION
#' -----------------------------------------------------------------------------

# Define paths using CONFIG (set in config.R) raw_data_base should be set in .renviron
stopifnot(!is.null(CONFIG$raw_data_base))

# Path to original 2020 data pull
original_pull_path <- file.path(CONFIG$raw_data_base, "20200909_raw/Marketing Intern Data")

# Path to 2021 refresh data
refresh_pull_path <- file.path(CONFIG$raw_data_base, "20210809_alldata_refresh_withzip")

# Output path
output_path <- file.path(CONFIG$raw_data_base, "compiled_trxns.rds")

#' -----------------------------------------------------------------------------
#' PROCESS ORIGINAL 2020 DATA PULL
#' -----------------------------------------------------------------------------

message("Loading original data pull from: ", original_pull_path)

# Read header row to get column names
top <- fread(file.path(original_pull_path, "x00.csv"))

# List all CSV files in directory
files <- list.files(original_pull_path, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files (except the first one which is 'top')
compiled_txn <- rbindlist(lapply(files[!grepl("x00.csv", files)], fread))

# Apply column names from header file
names(compiled_txn) <- names(top)

# Combine with header file data
full_transactions <- rbind(compiled_txn, top)
rm(compiled_txn, top)

# Check for missing values in each column
for (v in names(full_transactions)) {
  if (all(!is.na(full_transactions[, get(v)]))) {
    message(v, " is ok and is a ", class(full_transactions[, get(v)]))
  } else {
    message(v, " has MISSING values and is a ", class(full_transactions[, get(v)]))
  }
}

# Remove problematic discount_amount column (has formatting issues)
full_transactions[, discount_amount := NULL]

#' -----------------------------------------------------------------------------
#' PROCESS 2021 REFRESH DATA
#' -----------------------------------------------------------------------------

message("Loading 2021 refresh from: ", refresh_pull_path)

all2020 <- fread(file.path(refresh_pull_path, "appointment_export.csv"))

# Standardize column names to lowercase
names(all2020) <- str_to_lower(names(all2020))

# Check for missing values
for (v in names(all2020)) {
  if (all(!is.na(all2020[, get(v)]))) {
    message(v, " is ok and is a ", class(all2020[, get(v)]))
  } else {
    message(v, " has MISSING values and is a ", class(all2020[, get(v)]))
  }
}

#' -----------------------------------------------------------------------------
#' COMBINE DATA PULLS
#' -----------------------------------------------------------------------------

# Verify no exact duplicates in original pull
stopifnot(nrow(full_transactions) - uniqueN(full_transactions) == 0)

# Remove location columns from original (will use from refresh)
full_transactions[, location_city := NULL]
full_transactions[, location_state := NULL]

# Define columns for deduplication (excluding location info)
cols <- colnames(all2020)
cols <- cols[cols != "discount_amount"]
cols <- cols[!(cols %in% c("location_city", "location_state", "location_zip",
                           "business_id", "industry",
                           "first_location_app_datetime", "first_business_app_datetime"))]

# Get unique rows from each source
uniq_2020 <- unique(all2020[, cols, with = FALSE])
uniq_main <- unique(full_transactions[, c(cols, "total_app_time"), with = FALSE])

stopifnot(nrow(uniq_main) == uniqueN(uniq_main))

# Mark source of original data
uniq_main[, main_src := 1]

# Combine datasets
uniq_combined <- rbind(uniq_2020, uniq_main, fill = TRUE)
setorderv(uniq_combined, c(cols, "main_src"))

# app_datetime is POSIXct (parsed by fread from strings with fractional seconds).
# Keep app_datetime as POSIXct and extract a Date column for grouping/dedup.
stopifnot(inherits(uniq_combined$app_datetime, "POSIXct"))
uniq_combined[, date := as.Date(app_datetime)]

# Identify duplicates
uniq_combined[, dup := .N > 1, by = cols]

# For duplicates, prioritize original data pull
uniq_combined <- uniq_combined[dup == 0 | (dup == 1 & main_src == 1), ]
stopifnot(nrow(uniq_combined) == uniqueN(uniq_combined[, -c("main_src")]))

rm(full_transactions, uniq_2020, uniq_main)

#' -----------------------------------------------------------------------------
#' ADD LOCATION INFORMATION
#' -----------------------------------------------------------------------------

# Get location info from 2021 refresh data
biz_locs_states <- unique(all2020[, c("location_id", "location_city", "location_state",
                                       "location_zip", "business_id", "industry",
                                       "first_location_app_datetime",
                                       "first_business_app_datetime")])

# Use earliest first_business_app_datetime for each location
biz_locs_states[, first_business_app_datetime := min(first_business_app_datetime), by = location_id]
biz_locs_states <- unique(biz_locs_states)
stopifnot(uniqueN(biz_locs_states$location_id) == nrow(biz_locs_states))

# Fix city name typos
biz_locs_states[, location_city := str_trim(location_city)]
biz_locs_states[location_city %in% c("Los Angelas"), location_city := "Los Angeles"]
biz_locs_states[location_city %in% c("nyc"), location_city := "New York"]
biz_locs_states[location_city %in% c("West Los Angeles", "East Los Angeles",
                                      "Hollywood", "West Hollywood"),
                location_city := "Hollywood"]

# Merge location info
uniq_combined <- merge(uniq_combined, biz_locs_states, by = "location_id", all.x = TRUE)

#' -----------------------------------------------------------------------------
#' FINAL CLEANING
#' -----------------------------------------------------------------------------

# Keep only final transactions
uniq_combined <- uniq_combined[app_stage == "final", ]

# Fill in missing states for known cities
uniq_combined[location_city == "Denver", location_state := "CO"]
uniq_combined[location_city == "Stockton", location_state := "CA"]
uniq_combined[location_city == "Austin", location_state := "TX"]

# Fix appointment with multiple dates (data error)
uniq_combined[, first_date := min(date), by = "app_id"]
uniq_combined[app_id == "7f388e8a-4c7c-4b4c-8f3b-b3dbda35e5f7" & date != first_date,
              app_id := "7f388e8a-4c7c-4b4c-8f3b-b3dbda35e5f7-change"]
uniq_combined[app_id == "7f388e8a-4c7c-4b4c-8f3b-b3dbda35e5f7",
              first_date := min(date)]
stopifnot(all(uniq_combined$first_date == uniq_combined$date))

# Verify no fake business IDs remain
stopifnot(nrow(uniq_combined[business_id == "0b8bc512-705d-4be9-90e4-2fad62a4e4f4", ]) == 0)

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

saveRDS(uniq_combined, file = output_path)
message("Saved compiled transactions to: ", output_path)
message("  Total transactions: ", nrow(uniq_combined))
message("  Date range: ", min(uniq_combined$app_datetime), " to ", max(uniq_combined$app_datetime))
