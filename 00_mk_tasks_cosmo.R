#' =============================================================================
#' STEP 0: Create Task Dataset from Raw Salon Transactions
#' =============================================================================
#' Creates the task-level dataset from raw salon transaction data.
#' Only runs on machines with access to the raw data file.
#'
#' Performs all shared data cleaning so downstream scripts receive a ready-to-use
#' dataset: task category consolidation (6 → 5), zero-duration imputation,
#' zip-to-county mapping, census population merge, and zero-revenue firm-quarter
#' removal.
#'
#' Input:  CONFIG$raw_data_base/compiled_trxns.rds (10GB+)
#'         CONFIG$raw_data_path/20220727_countypop/geocorr2022_*.csv
#'         CONFIG$prep_output_dir/county_census_pop.rds
#' Output: mkdata/data/00_tasks_cosmo.rds
#'         mkdata/data/01_keytask.rds
#'
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
## change units from cents to dollars using auditable manifest rules.
price_rule_path <- project_path("mkdata", "manual_rules", "price_unit_corrections.csv")
assert_required_files(price_rule_path)
price_rules <- fread(price_rule_path)
assert_required_columns(
  price_rules,
  c("business_id", "min_price", "divisor", "reason"),
  "price unit correction rules"
)
stopifnot(!anyDuplicated(price_rules$business_id))
for (rule_idx in seq_len(nrow(price_rules))) {
  rule <- price_rules[rule_idx]
  tasks[
    business_id == rule$business_id & price >= rule$min_price,
    price := price / rule$divisor
  ]
}
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

## Incorporate approved review matches for newly observed service descriptions
reviewed_match_path <- file.path(CONFIG$raw_data_path, '20260310_manual_review/check2_truly_new_sorted_for_classification.csv')
stopifnot(file.exists(reviewed_match_path))

reviewed_matches <- fread(reviewed_match_path)
reviewed_matches <- reviewed_matches[
  approved %chin% c('closest_1', 'closest_2', 'alternative_1', 'alternative_2'),
  .(
    service_performed,
    approved_raw_id = fcase(
      approved == 'closest_1', as.integer(closest_1_raw_id),
      approved == 'closest_2', as.integer(closest_2_raw_id),
      approved == 'alternative_1', as.integer(alternative_1_raw_id),
      approved == 'alternative_2', as.integer(alternative_2_raw_id),
      default = NA_integer_
    )
  )
]


stopifnot(nrow(reviewed_matches) > 0)
stopifnot(!anyNA(reviewed_matches$service_performed))
stopifnot(!anyNA(reviewed_matches$approved_raw_id))
stopifnot(!anyDuplicated(reviewed_matches$service_performed))

reviewed_classified <- merge(
  reviewed_matches[, .(reviewed_service_performed = service_performed, approved_raw_id)],
  classified,
  by.x = 'approved_raw_id',
  by.y = 'raw_id',
  all.x = TRUE,
  sort = FALSE
)

stopifnot(nrow(reviewed_classified) == nrow(reviewed_matches))
stopifnot(!anyNA(reviewed_classified$count_cat))

setnames(reviewed_classified, 'approved_raw_id', 'raw_id')
reviewed_classified[, service_performed := reviewed_service_performed]
reviewed_classified[, reviewed_service_performed := NULL]
reviewed_classified <- reviewed_classified[, .SD, .SDcols = colnames(classified)]

classified <- rbind(
  classified[!service_performed %chin% reviewed_classified$service_performed],
  reviewed_classified,
  use.names = TRUE
)

# Merge tasks with classified descriptions using service_performed as the key
# all.x=TRUE keeps all rows from tasks even if no match in classified (left join)
tasks <- merge(tasks, classified, by = "service_performed", all.x = TRUE)

# Create a unique row identifier for later grouping operations
tasks[, helper_id := 1:.N]

# Fix character encoding issues for specific service_ids.
# Define columns to overwrite for problematic service_ids (columns 1 and 6-11 from classified).
# The source classifications live in a manifest keyed by stable raw_id, not by
# mutable row position in the classified table.
col <- colnames(classified)[c(1, 6:11)]

classification_rule_path <- project_path("mkdata", "manual_rules", "service_classification_overrides.csv")
assert_required_files(classification_rule_path)
classification_rules <- fread(classification_rule_path)
assert_required_columns(
  classification_rules,
  c("service_id", "classified_raw_id", "reason"),
  "service classification override rules"
)
stopifnot(!anyDuplicated(classification_rules$service_id))

for (rule_idx in seq_len(nrow(classification_rules))) {
  rule <- classification_rules[rule_idx]
  source_classification <- classified[raw_id == rule$classified_raw_id, .SD, .SDcols = col]
  stopifnot(nrow(source_classification) == 1)

  n_fix <- nrow(tasks[service_id == rule$service_id])
  if (n_fix > 0) {
    tasks[
      service_id == rule$service_id,
      (col) := source_classification[rep(1, n_fix), .SD, .SDcols = col]
    ]
  }
}

# Verify that only 1 row has NA for service_performed after the merge (expected data quality check)
stopifnot(nrow(tasks[is.na(service_performed)]) == 1)

# Remove the one row with NA service_performed
tasks <- tasks[!is.na(service_performed), ]

# all tasks should be categorized
stopifnot(nrow(tasks[is.na(taskcat1),])==0)

#' -----------------------------------------------------------------------------
#' ALLOCATE DURATION FOR MULTI-TASK SERVICES
#' -----------------------------------------------------------------------------

# Count number of services in each appointment
tasks[, app_service_count := .N, by = app_id]

# two variables capture the total time in the appointment - total app time and total duration
# duration is always available, but total_app_time is not.
stopifnot(uniqueN(tasks[is.na(duration) ,"app_id"])==0)
stopifnot(uniqueN(tasks[is.na(total_app_time) ,"app_id"])>0)
# total_app_time has 5 negative times, duration has 0
stopifnot(uniqueN(tasks[duration<0 ,"app_id"])==0)
stopifnot(uniqueN(tasks[total_app_time<0 ,"app_id"])==5)
stopifnot(uniqueN(tasks[is.na(duration) ,"app_id"])==0)
# total_app_time is not unique within appointment, duration is the same
tasks[,uniq_times:=uniqueN(duration), by="app_id"]
stopifnot(uniqueN(tasks[uniq_times>1 ,"app_id"])==0)
tasks[,uniq_times:=uniqueN(total_app_time), by="app_id"]
stopifnot(uniqueN(tasks[total_app_time>1 ,"app_id"])>0)
tasks[,uniq_times:=NULL,]

# for these reasons use duration
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

# Allocate duration to each task proportionally based on average task times
# new_duration = total_duration * (avg_time_for_this_task / sum_of_avg_times_for_all_tasks_in_appointment)
tasks[, new_duration := duration * value2 / sum(value2), by = c("app_id")]

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
#' DROP LEGACY IMPORT DATA
#' -----------------------------------------------------------------------------
#' Some salons imported historical transactions from a prior POS system when they
#' joined Boulevard. These imported records have placeholder durations (typically
#' 0, 5, or 15 minutes) that do not reflect real appointment times.
#'
#' Transition-based rule: drop rows before the identified transition month for the
#' 10 salons that show a clear switch from legacy-import placeholder durations to
#' Boulevard-native booking data. These location-month cutoffs come from the
#' monthly short-duration transition analysis documented in
#' docs/duration_reliability_2026-04-08.md.

n_before_legacy <- nrow(tasks)
legacy_cutoffs <- data.table(
  location_id = c(
    "8310d870-fb13-414b-ba6c-2902eaf0276f",
    "191172d5-2e41-4cf7-b94f-5b15145f18ec",
    "b9f5c7cf-38d0-42ec-9669-2304d7abfedd",
    "f44c05e5-5c88-4ab5-99ab-32ac1ee1d470",
    "23468507-cec2-458d-a053-ac09060d4721",
    "01fbf61d-01ef-4c3a-9a88-3e64cc8f99fe",
    "fb282d29-cc1f-41ab-87b1-b33cdba2ea3f",
    "ec64ba65-7a8e-49be-a8ca-e04dd0be4ff2",
    "a413d2fc-b120-4b5f-b13c-c5fac327866d",
    "f4938ca8-a8cf-49a7-9358-1df7ca32e336"
  ),
  transition_start = as.Date(c(
    "2018-09-01",
    "2019-08-01",
    "2019-08-01",
    "2020-12-01",
    "2020-02-01",
    "2020-04-01",
    "2019-01-01",
    "2018-11-01",
    "2018-07-01",
    "2019-04-01"
  ))
)

tasks <- merge(tasks, legacy_cutoffs, by = "location_id", all.x = TRUE)
n_legacy <- nrow(tasks[!is.na(transition_start) & date < transition_start])
tasks <- tasks[is.na(transition_start) | date >= transition_start]
tasks[, transition_start := NULL]

message("Dropped ", n_legacy, " legacy-import rows (",
        round(100 * n_legacy / n_before_legacy, 2), "%) from ",
        nrow(legacy_cutoffs), " locations")
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

#' -----------------------------------------------------------------------------
#' MERGE TASK CATEGORIES: EXTENSIONS INTO BLOWDRY/STYLE/TREATMENT
#' -----------------------------------------------------------------------------
#' Reduce from 6 to 5 task categories by folding Extensions (clust 3) into
#' Blowdry/Style/Treatment (clust 4), then re-rank to fill the gap.

tasks[, clust := ifelse(clust == 3, 4, clust)]
tasks[, rep_text_cluster := ifelse(clust == 4, "Blowdry/Style/Treatment/Extension", rep_text_cluster)]
tasks[, clust := frank(clust, ties.method = "dense")]

keytask <- unique(tasks[, c("clust", "rep_text_cluster")])
keytask[, task := clust]
keytask[, type := clust]
saveRDS(keytask, "mkdata/data/01_keytask.rds")
message("Task categories reduced to ", uniqueN(tasks$clust), " (merged Extensions into Blowdry/Style/Treatment)")

#' -----------------------------------------------------------------------------
#' TIME VARIABLES AND ZERO-DURATION IMPUTATION
#' -----------------------------------------------------------------------------

tasks[, quarter_year := year(date) + quarter(date) / 10]
tasks[, year := year(date)]

## all durations should be non-negative (verified upstream)
stopifnot(nrow(tasks[duration < 0]) == 0)

## impute zero-duration rows with quarter-cluster average
n_imputed <- nrow(tasks[duration == 0, ])
tasks[, temp_dur := ifelse(duration == 0, NA, duration)]
tasks[, temp_mean_dur := mean(temp_dur, na.rm = TRUE), by = c("quarter_year", "clust")]
tasks[, duration := ifelse(duration == 0, temp_mean_dur, duration)]
tasks[, c("temp_dur", "temp_mean_dur") := list(NULL, NULL)]
message("Imputed ", n_imputed, " zero-duration rows (",
        round(100 * n_imputed / nrow(tasks), 1), "%)")

#' -----------------------------------------------------------------------------
#' MAP ZIP CODES TO COUNTY AND MERGE CENSUS POPULATION
#' -----------------------------------------------------------------------------

countypop <- fread(file.path(CONFIG$raw_data_path, '20220727_countypop/geocorr2022_2220806816.csv'))[-1]
countypop[, CSPOP := pop20]
stopifnot(uniqueN(countypop$county) == nrow(countypop))
data <- fread(file.path(CONFIG$raw_data_path, '20220727_countypop/geocorr2022_2220801561.csv'))[-1]
data[, count := uniqueN(county), by = zcta]
data <- data[afact > 0.50 | count == 1]  # mapping only if more than 50 percent of zip is within county
stopifnot(uniqueN(data$zcta) == nrow(data))
data <- merge(data, countypop[, c("county")], by = "county")
data[, location_zip := as.numeric(zcta)]
tasks <- merge(tasks, data[, c("location_zip", "county")], by = "location_zip", all.x = TRUE)
stopifnot(uniqueN(tasks[is.na(county), location_id]) == 2)  # one NA zip and one unmatched

## merge county-year census population estimates
tasks <- merge(tasks, readRDS(file.path(CONFIG$prep_output_dir, "county_census_pop.rds")),
               by = c("county", "year"), all.x = TRUE)

#' -----------------------------------------------------------------------------
#' DROP ZERO-REVENUE FIRM-QUARTERS
#' -----------------------------------------------------------------------------

temp <- tasks[, .(rev = sum(price)), by = c("quarter_year", "location_id")]
temp[, is_zero := rev <= 0]
n_zero_fq <- nrow(temp[is_zero == 1])
temp[, rev := NULL]
tasks <- merge(tasks, temp, by = c("quarter_year", "location_id"), all.x = TRUE)
tasks <- tasks[is_zero == 0, ]
tasks[, is_zero := NULL]
rm(temp)
message("Dropped ", n_zero_fq, " zero-revenue firm-quarters")

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


