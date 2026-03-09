#' =============================================================================
#' PREP 05: Create Producer Price Index Dataset
#' =============================================================================
#' Processes BLS Producer Price Index data for use in cost estimation.
#' Extracts quarterly PPI values (using first month of each quarter).
#'
#' Input:  mkdata/raw/20231227_ppi_cost/file.csv
#' Output: mkdata/data/ppi.rds
#'
#' Output Schema:
#'   ppi_inputs: PPI value for inputs to personal services
#'   quarter_year: Numeric quarter-year (e.g., 2019.1)
#'
#' Dependencies: None
#' =============================================================================

library('data.table')
library('lubridate')

#' -----------------------------------------------------------------------------
#' LOAD AND PROCESS PPI DATA
#' -----------------------------------------------------------------------------

# Load PPI data from BLS
# The file contains monthly PPI values
ppi <- fread('mkdata/raw/20231227_ppi_cost/file.csv')

# Rename the 'Value' column to 'ppi_inputs' for clarity
setnames(ppi, "Value", "ppi_inputs")

# Extract month number from Period column (format: "M01" for January, etc.)
ppi[, month := as.numeric(gsub("M", "", Period))]

# Compute quarter from month (1-3 = Q1, 4-6 = Q2, etc.)
ppi[, quarter := quarter(month)]

# Create quarter_year in decimal format (e.g., 2019.1 for 2019 Q1)
ppi[, quarter_year := as.numeric(Year) + quarter / 10]

# Identify first month of each quarter (for consistent quarterly value)
ppi[, helper := frank(month), by = c("quarter_year")]

# Keep only the first month of each quarter
# This gives us one PPI observation per quarter
ppi <- ppi[helper == 1, c("ppi_inputs", "quarter_year")]

# Verify uniqueness: one observation per quarter
stopifnot(nrow(ppi) == uniqueN(ppi))

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

saveRDS(ppi, "mkdata/data/ppi.rds")

message("Saved mkdata/data/ppi.rds")
message("  Quarters: ", nrow(ppi))
message("  Date range: ", min(ppi$quarter_year), " to ", max(ppi$quarter_year))
