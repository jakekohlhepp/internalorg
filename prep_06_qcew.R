#' =============================================================================
#' PREP 06: Create QCEW County Wage Data
#' =============================================================================
#' Retrieves wage data from the BLS Quarterly Census of Employment and Wages
#' (QCEW) for the beauty salon industry (NAICS 812112).
#'
#' This script uses the BLS QCEW API to download quarterly wage data
#' for all counties, which is used to calibrate wage parameters in the model.
#'
#' Input:  mkdata/raw/20220427_qcew_code/qcew_rscript_example.R (BLS API functions)
#' Output: mkdata/data/qcew_county.rds
#'
#' Output Schema:
#'   area_fips: County FIPS code
#'   county: County FIPS as numeric
#'   quarter_year: Numeric quarter-year (e.g., 2019.1)
#'   year, qtr: Year and quarter components
#'   avg_wkly_wage: Average weekly wage
#'   total_qtrly_wages: Total quarterly wages
#'   (plus other QCEW variables)
#'
#' NOTE: This script requires internet access to query the BLS API.
#'       The qcew_rscript_example.R file provides the qcewGetIndustryData() function.
#'
#' Dependencies: mkdata/raw/20220427_qcew_code/qcew_rscript_example.R
#' =============================================================================

library('data.table')

#' -----------------------------------------------------------------------------
#' LOAD BLS QCEW API FUNCTIONS
#' -----------------------------------------------------------------------------

# Source the BLS-provided R script with QCEW API functions
# This provides qcewGetIndustryData(year, quarter, industry_code)
source('mkdata/raw/20220427_qcew_code/qcew_rscript_example.R')

#' -----------------------------------------------------------------------------
#' RETRIEVE QCEW DATA FOR BEAUTY SALONS
#' -----------------------------------------------------------------------------

# Initialize container for results
puzzle <- c()

# Loop through years 2014-2021 and quarters 1-4
# NAICS 812112 = Beauty salons
for (y in 2014:2021) {
  for (q in 1:4) {
    # Query BLS API for beauty salon industry data
    # Returns data for all counties reporting this industry
    piece <- qcewGetIndustryData(y, q, 812112)
    puzzle <- rbind(puzzle, piece)
  }
}

# Convert to data.table
puzzle <- data.table(puzzle)

#' -----------------------------------------------------------------------------
#' CREATE DERIVED VARIABLES
#' -----------------------------------------------------------------------------
# Create quarter_year in decimal format (e.g., 2019.1 for 2019 Q1)
puzzle[, quarter_year := year + qtr / 10]

# Convert area_fips to numeric county code
puzzle[, county := as.numeric(area_fips)]

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

saveRDS(puzzle, "mkdata/data/qcew_county.rds")

message("Saved mkdata/data/qcew_county.rds")
message("  Observations: ", nrow(puzzle))
message("  Counties: ", uniqueN(puzzle$county))
message("  Date range: ", min(puzzle$quarter_year), " to ", max(puzzle$quarter_year))
