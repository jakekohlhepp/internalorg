#' =============================================================================
#' PREP 01: Create Service Classification Dataset
#' =============================================================================
#' Creates the service-to-task classification mapping from cosmetologist annotations.
#' This file maps each service description to one of 6 task categories.
#'
#' Input:  mkdata/raw/20220526_cosmo_classify/Upwork service desciptions - COMPLETE.xlsx
#'         mkdata/raw/20220526_cosmo_classify/check_cosmo - COMPLETE (3).xlsx
#' Output: mkdata/data/classified_descriptions.rds
#'
#' Task Categories:
#'   taskcat1 = Haircut/Shave (Cut or Shave services)
#'   taskcat2 = Color/Highlight/Wash (Color/Bleach, Highlight/Balayage, Wash/Shampoo)
#'   taskcat3 = Extensions (Hair extensions)
#'   taskcat4 = Blowdry/Style/Treatment (Blowdry, Style, Other Treatment)
#'   taskcat5 = Administrative (Admin/Consult when it's the only category)
#'   taskcat6 = Nail/Spa/Eye/Misc (Everything else - no main category marked)
#'
#' Dependencies: None (runs first in prep sequence)
#' =============================================================================

library('data.table')
library('stringr')
library("readxl")

#' -----------------------------------------------------------------------------
#' LOAD RAW CLASSIFICATION DATA
#' -----------------------------------------------------------------------------

# Load the main service descriptions file (classified by Upwork contractors)
# This file contains service descriptions and their task category markers
cosmo_class <- data.table(read_excel('mkdata/raw/20220526_cosmo_classify/Upwork service desciptions - COMPLETE.xlsx'))

# Load updates/corrections to the classifications
# This file contains revised classifications for specific services
cosmo_update <- data.table(read_excel('mkdata/raw/20220526_cosmo_classify/check_cosmo - COMPLETE (3).xlsx'))

# Merge: replace rows in cosmo_class with updated versions from cosmo_update
# Keep rows from cosmo_class that are NOT in cosmo_update, then add all of cosmo_update
cosmo_class <- rbind(cosmo_class[!(raw_id %in% cosmo_update$raw_id)], cosmo_update)

# Verify uniqueness: each raw_id should appear exactly once
stopifnot(nrow(cosmo_class) == uniqueN(cosmo_class$raw_id))

#' -----------------------------------------------------------------------------
#' CLEAN SERVICE DESCRIPTIONS
#' -----------------------------------------------------------------------------

# Store original column names for reference
orig_vars <- copy(colnames(cosmo_class))

# Remove leading apostrophe from service descriptions (artifact from Excel formatting)
# Some descriptions start with ' which needs to be stripped
cosmo_class[, `Service Description` := ifelse(
  substring(`Service Description`, 1, 1) == "'",
  substring(`Service Description`, 2),
  `Service Description`
)]

#' -----------------------------------------------------------------------------
#' COUNT CATEGORY MARKERS
#' -----------------------------------------------------------------------------

# Count how many category columns are marked for each service
# This helps identify services that span multiple categories
cosmo_class[, marked := 0]
for (var in colnames(cosmo_class)[!(colnames(cosmo_class) %in% c("Service Description", "raw_id", "marked"))]) {
  cosmo_class[, marked := marked + !is.na(get(var))]
}

# Count markers in the main categories only (columns 4-16)
# Main categories exclude certain auxiliary flags
cosmo_class[, main_cat := 0]
for (var in colnames(cosmo_class)[4:16]) {
  cosmo_class[, main_cat := marked + !is.na(get(var))]
}

# Special handling: Admin/Consult can be a secondary marker
# If a service has Admin/Consult AND another main category, don't double-count
cosmo_class[, main_cat := ifelse(main_cat > 1, main_cat - !is.na(`Admin/Consult`), main_cat)]

#' -----------------------------------------------------------------------------
#' GENERATE TASK CATEGORIES
#' -----------------------------------------------------------------------------
#' Task category definitions based on cosmetologist expertise:
#'   taskcat1: Core hair cutting services (Cut, Shave)
#'   taskcat2: Chemical/color services (Color/Bleach, Highlight/Balayage, Wash/Shampoo)
#'   taskcat3: Extension services (Extensions only)
#'   taskcat4: Styling services (Blowdry, Style, Other Treatment)
#'   taskcat5: Administrative services (Admin/Consult when sole category)
#'   taskcat6: Non-hair services (Nail, Spa, Eye, Misc - default for unmarked)

# taskcat1: Haircut/Shave
cosmo_class[Cut == 1 | Shave == 1, taskcat1 := 1]

# taskcat2: Color/Highlight/Wash
cosmo_class[`Color/Bleach` == 1 | `Highlight/Balayage` == 1 | `Wash/Shampoo` == 1, taskcat2 := 1]

# taskcat3: Extensions
cosmo_class[Extensions == 1, taskcat3 := 1]

# taskcat4: Blowdry/Style/Treatment
cosmo_class[Blowdry == 1 | Style == 1 | `Other Treatment` == 1, taskcat4 := 1]

# taskcat5: Administrative (only when it's the SOLE category for the service)
cosmo_class[(`Admin/Consult` == 1 & main_cat == 1), taskcat5 := 1]

# Convert NA to 0 for all taskcat columns (for summation)
for (var in colnames(cosmo_class)[colnames(cosmo_class) %like% "taskcat"]) {
  cosmo_class[, (var) := ifelse(is.na(get(var)), 0, get(var))]
}

# Count total task categories assigned to each service
cosmo_class[, count_cat := rowSums(.SD), .SDcols = colnames(cosmo_class)[colnames(cosmo_class) %like% "taskcat"]]

# taskcat6: Catch-all for services not in any main category (Nail/Spa/Eye/Misc)
# These are services that didn't match any of the hair-related categories
cosmo_class[, taskcat6 := as.numeric(count_cat == 0)]

#' -----------------------------------------------------------------------------
#' FINALIZE AND SAVE
#' -----------------------------------------------------------------------------

# Select only the columns needed for downstream processing
# count_cat: number of categories (for multi-task duration allocation)
# Male, Female, Child: demographic flags for the service
# raw_id: unique identifier
# Service Description: the text description
# taskcat1-6: the task category flags
cosmo_class <- cosmo_class[, .SD, .SDcols = c(
  "count_cat", "Male", "Female", "Child", "raw_id", "Service Description",
  colnames(cosmo_class)[colnames(cosmo_class) %like% "taskcat"]
)]

# Save to intermediate data directory
# This file is read by 00_mk_tasks_cosmo.R to classify transaction-level data
saveRDS(cosmo_class, "mkdata/data/classified_descriptions.rds")

message("Saved mkdata/data/classified_descriptions.rds with ", nrow(cosmo_class), " service classifications")
message("Task category distribution:")
message("  taskcat1 (Haircut/Shave): ", sum(cosmo_class$taskcat1))
message("  taskcat2 (Color/Wash): ", sum(cosmo_class$taskcat2))
message("  taskcat3 (Extensions): ", sum(cosmo_class$taskcat3))
message("  taskcat4 (Blowdry/Style): ", sum(cosmo_class$taskcat4))
message("  taskcat5 (Admin): ", sum(cosmo_class$taskcat5))
message("  taskcat6 (Misc): ", sum(cosmo_class$taskcat6))
