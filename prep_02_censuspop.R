#' =============================================================================
#' PREP 02: Create County Census Population Dataset
#' =============================================================================
#' Creates county-level population estimates by year from Census Bureau data.
#' Combines 2010-2019 estimates with 2020-2022 estimates.
#'
#' Input:  mkdata/raw/202403101_morecspops/co-est2022-alldata.csv (2020-2022)
#'         mkdata/raw/202403101_morecspops/co-est2019-alldata.csv (2010-2019)
#' Output: mkdata/data/county_census_pop.rds
#'
#' Output Schema:
#'   county: 5-digit FIPS code (character, zero-padded)
#'   year: calendar year (2010-2022)
#'   CSPOP: population estimate
#'
#' Dependencies: None
#' =============================================================================

library('data.table')
library('lubridate')
library('stringr')

#' -----------------------------------------------------------------------------
#' LOAD 2020-2022 POPULATION ESTIMATES
#' -----------------------------------------------------------------------------

# Load Census Bureau population estimates for 2020-2022
# First row is header info, skip it with [-1]
countypop_more <- fread('mkdata/raw/202403101_morecspops/co-est2022-alldata.csv')[-1]

# Create properly formatted 5-digit county FIPS codes
# STATE is 2 digits, COUNTY is 3 digits
# Need to zero-pad to ensure consistent formatting

# Zero-pad state code to 2 digits
countypop_more[, state := ifelse(
  str_length(as.character(STATE)) == max(str_length(as.character(STATE))),
  STATE,
  paste0("0", STATE)
)]

# Zero-pad county code to 3 digits (may need multiple passes)
countypop_more[, county := ifelse(
  str_length(as.character(COUNTY)) == max(str_length(as.character(COUNTY))),
  COUNTY,
  paste0("0", COUNTY)
)]
countypop_more[, county := ifelse(
  str_length(as.character(county)) == max(str_length(as.character(county))),
  county,
  paste0("0", county)
)]
countypop_more[, county := ifelse(
  str_length(as.character(county)) == max(str_length(as.character(county))),
  county,
  paste0("0", county)
)]

# Concatenate state + county to create 5-digit FIPS code
countypop_more[, county := paste0(state, county)]

# Verify all county codes are 5 digits
stopifnot(nrow(countypop_more[str_length(county) != 5, ]) == 0)

# Keep only needed columns: county code and population estimates for 2020-2022
countypop_more <- countypop_more[, c("county", "POPESTIMATE2020", "POPESTIMATE2021", "POPESTIMATE2022")]

#' -----------------------------------------------------------------------------
#' LOAD 2010-2019 POPULATION ESTIMATES
#' -----------------------------------------------------------------------------

# Load Census Bureau population estimates for 2010-2019
countypop_more2 <- fread('mkdata/raw/202403101_morecspops/co-est2019-alldata.csv')[-1]

# Same zero-padding process for state codes
countypop_more2[, state := ifelse(
  str_length(as.character(STATE)) == max(str_length(as.character(STATE))),
  STATE,
  paste0("0", STATE)
)]

# Same zero-padding process for county codes
countypop_more2[, county := ifelse(
  str_length(as.character(COUNTY)) == max(str_length(as.character(COUNTY))),
  COUNTY,
  paste0("0", COUNTY)
)]
countypop_more2[, county := ifelse(
  str_length(as.character(county)) == max(str_length(as.character(county))),
  county,
  paste0("0", county)
)]
countypop_more2[, county := ifelse(
  str_length(as.character(county)) == max(str_length(as.character(county))),
  county,
  paste0("0", county)
)]

# Concatenate state + county for 5-digit FIPS
countypop_more2[, county := paste0(state, county)]

# Verify all county codes are 5 digits
stopifnot(nrow(countypop_more2[str_length(county) != 5, ]) == 0)

# Keep county code and population estimates for 2010-2019
countypop_more2 <- countypop_more2[, c(
  "county", "POPESTIMATE2010", "POPESTIMATE2011", "POPESTIMATE2012", "POPESTIMATE2013",
  "POPESTIMATE2014", "POPESTIMATE2015", "POPESTIMATE2016", "POPESTIMATE2017",
  "POPESTIMATE2018", "POPESTIMATE2019"
)]

#' -----------------------------------------------------------------------------
#' MERGE AND RESHAPE
#' -----------------------------------------------------------------------------

# Merge 2010-2019 and 2020-2022 data by county
countypop_more <- merge(countypop_more, countypop_more2, by = "county", all = TRUE)

# Reshape from wide to long format
# Each POPESTIMATE#### column becomes a row with year extracted from column name
countypop_more <- melt(
  countypop_more,
  id.vars = "county",
  measure = patterns("^POPESTIMATE"),
  value.name = "CSPOP"
)

# Extract year from the variable name (e.g., "POPESTIMATE2020" -> 2020)
countypop_more[, year := as.numeric(str_remove(variable, "POPESTIMATE"))]

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

# Remove the 'variable' column (no longer needed after extracting year)
# Final columns: county, CSPOP, year
saveRDS(countypop_more[, -c("variable")], "mkdata/data/county_census_pop.rds")

message("Saved mkdata/data/county_census_pop.rds")
message("  Counties: ", uniqueN(countypop_more$county))
message("  Years: ", paste(range(countypop_more$year), collapse = "-"))
