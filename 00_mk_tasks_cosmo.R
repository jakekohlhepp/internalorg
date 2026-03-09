#' =============================================================================
#' STEP 0: Create Task Dataset from Raw Salon Transactions
#' =============================================================================
#' Creates the task-level dataset from raw salon transaction data.
#' Only runs on machines with access to the raw data file.
#'
#' Input:  CONFIG$raw_data_path/compiled_trxns.rds (10GB+)
#' Output: mkdata/data/00_tasks_cosmo.rds
#'
#' LOADING OPTIMIZATION NOTE:
#' The raw file is 10GB+ and takes several minutes to load with readRDS().
#' For faster loading, consider converting to one of these formats:
#'
#'   qs::qsave(data, "compiled_trxns.qs", preset = "fast")
#'   # Then load with: tasks <- data.table(qs::qread("compiled_trxns.qs"))
#'   # Speed improvement: 3-5x faster
#'
#'   fst::write_fst(data, "compiled_trxns.fst")
#'   # Then load with: tasks <- data.table(fst::read_fst("compiled_trxns.fst"))
#'   # Speed improvement: 5-10x faster
#'
#'   arrow::write_parquet(data, "compiled_trxns.parquet")
#'   # Then load with: tasks <- data.table(arrow::read_parquet("compiled_trxns.parquet"))
#'   # Speed improvement: 5-10x faster, with selective column loading
#' =============================================================================

# Load required packages:
library('data.table')   # High-performance data manipulation (fast grouping, joins, by-reference updates)
library('lubridate')    # Date/time parsing and manipulation (year(), month(), week(), quarter())
library('stringr')      # String manipulation functions (str_detect())

# Load configuration
source('config.R')

#' -----------------------------------------------------------------------------
#' LOAD RAW DATA
#' -----------------------------------------------------------------------------

raw_data_file <- file.path(CONFIG$raw_data_base, "compiled_trxns.rds")

message("Loading raw transaction data from: ", raw_data_file)
message("(This may take several minutes for 10GB+ file...)")

load_start <- Sys.time()
# Read the compiled transactions data from an RDS file into a data.table
tasks <- data.table(readRDS(raw_data_file))
load_time <- difftime(Sys.time(), load_start, units = "mins")
message("Loaded ", nrow(tasks), " rows in ", round(load_time, 2), " minutes")

# Create a column 'raword' storing the original row order (1 to N) for potential later reference
tasks[, raword := 1:.N]

#' -----------------------------------------------------------------------------
#' FILTER TO HAIR SALONS
#' -----------------------------------------------------------------------------

# Filter to keep only transactions from hair-related businesses (Hair Salon, hairSalon, Barber Shop, Blowouts & Styling)
# This excludes other business types in the data (nail salons, spas, etc.)
tasks <- tasks[industry %in% c("Hair Salon", "hairSalon", "Barber Shop", "Blowouts & Styling"), ]
message("Filtered to ", nrow(tasks), " hair salon transactions")

# Create time period variables:
# month_num: year + month/13 (creates a continuous monthly time index, e.g., 2019.0769 for Jan 2019)
# week_num: year + week/53 (creates a continuous weekly time index)
tasks[, month_num := year(date) + month(date)/13]
tasks[, week_num := year(date) + week(date)/53]

## Price data exclusions
##  exclude one $1m transaction that appears to be a data entry error.
stopifnot(tasks[price>999999]$raword==3538640) 
tasks<-tasks[raword!=3538640,]

## one business appears to have incorrect unit input (potentially legacy system import issue)
## prices jump from under 1000USD for extension to 2000USD for trim.
## change units from cents to dollars.
tasks[business_id=='eee49809-aae1-4a5f-be32-d2015dd5bd25' & price>=2000, price:=price/100]

## one business appears to have incorrect unit input (potentially legacy system import issue)
## prices jump from 160USD to 1000USD
## change units from cents to dollars.
tasks[business_id=='9bea0ea3-8fba-4b09-9762-8793cbdfa66f' & price>=1000, price:=price/100]
stopifnot(tasks$price<10000)

# Standardize service names by adding spaces where the original data had concatenated names
tasks[service_performed == "GlazeOld", service_performed := "Glaze Old"]
tasks[service_performed == "HaircutOld", service_performed := "Haircut Old"]

#' -----------------------------------------------------------------------------
#' ATTACH SERVICE CLASSIFICATIONS
#' -----------------------------------------------------------------------------

# Load the classified service descriptions file which maps raw service names to task categories
# This file contains the task classification (taskcat1-6) for each service type
classified <- readRDS('mkdata/data/classified_descriptions.rds')

# Manual corrections for specific raw_id values with incorrect/missing descriptions
classified[raw_id == 2147, `Service Description` := "BOTOX CONSULT ONLY"]
classified[raw_id == 9900, `Service Description` := "Hair Extensions Install - Bonded 18\"\"  (per bundle)"]

# Rename 'Service Description' column to 'service_performed' to match the tasks table
classified[, service_performed := `Service Description`]
classified[, `Service Description` := NULL]  # Remove the original column

# Merge tasks with classified descriptions using service_performed as the key
# all.x=TRUE keeps all rows from tasks even if no match in classified (left join)
tasks <- merge(tasks, classified, by = "service_performed", all.x = TRUE)

# Create a unique row identifier for later grouping operations
tasks[, helper_id := 1:.N]

# Fix character encoding issues for specific service_ids
# Define columns to overwrite for problematic service_ids (columns 1 and 6-11 from classified)
# This fixes encoding/character issues where the merge didn't work properly
col <- colnames(classified)[c(1, 6:11)]

# For service_id "ec581975-...", copy values from row 6 of classified
n_fix <- nrow(tasks[service_id == "ec581975-a08a-42a5-9eae-4cefe23627bf"])
if (n_fix > 0) {
  tasks[service_id == "ec581975-a08a-42a5-9eae-4cefe23627bf",
        (col) := classified[rep(6, n_fix), .SD, .SDcols = col]]
}

# For service_id "3a15cd78-...", copy values from row 20363 of classified
n_fix <- nrow(tasks[service_id == "3a15cd78-dbea-4379-ae2d-13ed26c6d228"])
if (n_fix > 0) {
  tasks[service_id == "3a15cd78-dbea-4379-ae2d-13ed26c6d228",
        (col) := classified[rep(20363, n_fix), .SD, .SDcols = col]]
}

# For service_id "001cb7db-...", copy values from row 9900 of classified
n_fix <- nrow(tasks[service_id == "001cb7db-72b8-4744-8fe2-157441ed8e30"])
if (n_fix > 0) {
  tasks[service_id == "001cb7db-72b8-4744-8fe2-157441ed8e30",
        (col) := classified[rep(9900, n_fix), .SD, .SDcols = col]]
}

# Verify that only 1 row has NA for service_performed after the merge (expected data quality check)
stopifnot(nrow(tasks[is.na(service_performed)]) == 1)

# Remove the one row with NA service_performed
tasks <- tasks[!is.na(service_performed), ]

#' -----------------------------------------------------------------------------
#' ALLOCATE DURATION FOR MULTI-TASK SERVICES
#' -----------------------------------------------------------------------------

# Count number of services in each appointment
tasks[, app_service_count := .N, by = app_id]

# When total_app_time is available, use it instead of duration (more accurate)
tasks[!is.na(total_app_time), duration := total_app_time]

# Compute average duration for each task category using only:
# - Single-category services (count_cat==1)
# - The specific task category flag is 1 (e.g., taskcat1==1)
# - Single-service appointments (app_service_count==1)
# These averages will be used to allocate time for multi-category services
tasks[, avg_task1 := sum(duration * (count_cat == 1 & taskcat1 == 1 & app_service_count == 1)) /
                     sum((count_cat == 1 & taskcat1 == 1 & app_service_count == 1))]
tasks[, avg_task2 := sum(duration * (count_cat == 1 & taskcat2 == 1 & app_service_count == 1)) /
                     sum((count_cat == 1 & taskcat2 == 1 & app_service_count == 1))]
tasks[, avg_task3 := sum(duration * (count_cat == 1 & taskcat3 == 1 & app_service_count == 1)) /
                     sum((count_cat == 1 & taskcat3 == 1 & app_service_count == 1))]
tasks[, avg_task4 := sum(duration * (count_cat == 1 & taskcat4 == 1 & app_service_count == 1)) /
                     sum((count_cat == 1 & taskcat4 == 1 & app_service_count == 1))]
tasks[, avg_task5 := sum(duration * (count_cat == 1 & taskcat5 == 1 & app_service_count == 1)) /
                     sum((count_cat == 1 & taskcat5 == 1 & app_service_count == 1))]
# Note: taskcat6 doesn't require count_cat==1 filter (different logic for miscellaneous category)
tasks[, avg_task6 := sum(duration * (taskcat6 == 1 & app_service_count == 1)) /
                     sum((taskcat6 == 1 & app_service_count == 1))]

# Reshape data from wide to long format:
# - measure.vars=list(...) creates pairs of columns: taskcat1-6 (value1) and avg_task1-6 (value2)
# - Each row becomes multiple rows (one per task category the service belongs to)
tasks <- melt(tasks,
              measure.vars = list(which(str_detect(colnames(tasks), "taskcat")),
                                  which(str_detect(colnames(tasks), "avg_task"))),
              variable.factor = FALSE)

# Keep only rows where the service belongs to that task category (value1==1)
tasks <- tasks[value1 == 1, ]

# For appointments missing total_app_time, assign them all the same helper_id (max value)
# so they share duration proportionally
tasks[is.na(total_app_time), helper_id := max(helper_id), by = c("app_id")]

# Allocate duration to each task proportionally based on average task times
# new_duration = total_duration * (avg_time_for_this_task / sum_of_avg_times_for_all_tasks_in_appointment)
tasks[, new_duration := duration * value2 / sum(value2), by = c("helper_id")]

# Replace duration with the newly computed allocated duration
tasks[, duration := NULL]
tasks[, duration := new_duration]

# Recreate taskcat flags from the 'variable' column (which contains "1", "2", etc. from the melt)
tasks[, taskcat1 := variable == "1"]  # TRUE/FALSE: is this a Haircut/Shave task?
tasks[, taskcat2 := variable == "2"]  # TRUE/FALSE: is this a Color/Highlight/Wash task?
tasks[, taskcat3 := variable == "3"]  # TRUE/FALSE: is this an Extensions task?
tasks[, taskcat4 := variable == "4"]  # TRUE/FALSE: is this a Blowdry/Style/Treatment task?
tasks[, taskcat5 := variable == "5"]  # TRUE/FALSE: is this an Administrative task?
tasks[, taskcat6 := variable == "6"]  # TRUE/FALSE: is this a Nail/Spa/Eye/Misc task?

# Remove temporary columns no longer needed
tasks <- tasks[, -c("new_duration", "value1", "value2", "variable", "helper_id")]

#' -----------------------------------------------------------------------------
#' CREATE FINAL TASK CATEGORIES
#' -----------------------------------------------------------------------------

# Verify each row belongs to exactly one task category (sum of all taskcat flags should equal 1)
stopifnot(nrow(tasks[taskcat1 + taskcat2 + taskcat3 + taskcat4 + taskcat5 + taskcat6 != 1, ]) == 0)

# Create integer cluster ID (1-6) from the boolean taskcat flags
tasks[, clust := taskcat1 + taskcat2*2 + taskcat3*3 + taskcat4*4 + taskcat5*5 + taskcat6*6]

# Create human-readable task category labels
tasks[taskcat1 == 1, rep_text_cluster := "Haircut/Shave"]
tasks[taskcat2 == 1, rep_text_cluster := "Color/Highlight/Wash"]
tasks[taskcat3 == 1, rep_text_cluster := "Extensions"]
tasks[taskcat4 == 1, rep_text_cluster := "Blowdry/Style/Treatment"]
tasks[taskcat5 == 1, rep_text_cluster := "Administrative"]
tasks[taskcat6 == 1, rep_text_cluster := "Nail/Spa/Eye/Misc."]

# Remove the individual taskcat columns (no longer needed now that we have clust)
tasks <- tasks[, -c("taskcat1", "taskcat2", "taskcat3", "taskcat4", "taskcat5", "taskcat6")]

# Create binary flags for customer gender/age from the Male, Female, Child columns
# male_flag: 1 if Male==1, 0 otherwise (including NA)
tasks[, male_flag := Male == 1]
tasks[is.na(Male), male_flag := 0]

# female_flag: 1 if Female==1, 0 otherwise (including NA)
tasks[, female_flag := Female == 1]
tasks[is.na(Female), female_flag := 0]

# child_flag: 1 if Child==1, 0 otherwise (including NA)
tasks[, child_flag := Child == 1]
tasks[is.na(Child), child_flag := 0]

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

if (!dir.exists("mkdata/data")) {
  dir.create("mkdata/data", recursive = TRUE)
}

# Save the processed tasks dataset to an RDS file for use by downstream scripts
# This file is read by 01_build_data.R in the main estimation pipeline
saveRDS(tasks, "mkdata/data/00_tasks_cosmo.rds")
message("Saved mkdata/data/00_tasks_cosmo.rds with ", nrow(tasks), " rows")

# Export a few example rows for presentation (optional)
# These two app_ids are selected as representative examples
# Uncomment if needed for presentations:
# write.csv(tasks[app_id %in% c('000c4d3e-6445-489d-84d3-6b2e91c39e60',
#                               '00342f4b-49c0-4100-8786-7333f4308163')],
#           "analysis_final/out/mk_tasks_cosmo_examples.csv")

