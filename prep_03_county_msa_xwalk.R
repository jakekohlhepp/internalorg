#' =============================================================================
#' PREP 03: Create County-MSA Crosswalk for CEX Data
#' =============================================================================
#' Creates a mapping from Consumer Expenditure Survey (CEX) PSU codes to
#' county FIPS codes. This is needed to merge CEX data with county-level data.
#'
#' The CEX uses Primary Sampling Units (PSUs) which correspond to MSAs.
#' This crosswalk maps PSU codes to the counties within each MSA.
#'
#' Input:  mkdata/raw/20231023_county_msa_crosswalk/cex_cleaned_codebook.csv
#'         mkdata/raw/20231023_county_msa_crosswalk/qcew-county-msa-csa-crosswalk.csv
#' Output: mkdata/data/county_msa_xwalk.rds
#'
#' Output Schema:
#'   county: 5-digit FIPS code
#'   PSU: CEX Primary Sampling Unit code (e.g., "S12A" for Atlanta)
#'
#' Dependencies: None
#' =============================================================================

library('data.table')
library('stringr')

#' -----------------------------------------------------------------------------
#' LOAD CEX PSU CODEBOOK
#' -----------------------------------------------------------------------------

# Load the CEX variable codebook which contains PSU definitions
# This maps PSU codes (like "S12A") to MSA names (like "Atlanta")
cex_codebook <- fread('mkdata/raw/20231023_county_msa_crosswalk/cex_cleaned_codebook.csv')

# Standardize column names: lowercase and remove spaces
colnames(cex_codebook) <- str_replace_all(str_to_lower(colnames(cex_codebook)), fixed(" "), "")

# Filter to keep only:
#   - variable == "PSU" (the PSU variable definition)
#   - codevalue contains "S" (indicates an MSA-based PSU)
#   - survey == "INTERVIEW" (we use interview survey data)
cex_codebook <- cex_codebook[
  variable == "PSU" & str_detect(codevalue, "S") > 0 & survey == "INTERVIEW",
  c("codevalue", "codedescription")
]

# Verify uniqueness of PSU codes
stopifnot(uniqueN(cex_codebook) == nrow(cex_codebook))

#' -----------------------------------------------------------------------------
#' LOAD COUNTY-MSA CROSSWALK
#' -----------------------------------------------------------------------------

# Load QCEW county-to-MSA crosswalk
# This maps each county FIPS code to its MSA
msa_xwalk <- fread('mkdata/raw/20231023_county_msa_crosswalk/qcew-county-msa-csa-crosswalk.csv')

# Standardize column names
colnames(msa_xwalk) <- str_replace_all(str_to_lower(colnames(msa_xwalk)), fixed(" "), "")

# Clean MSA title to match CEX codebook format
# Remove " MSA" suffix and trim whitespace
msa_xwalk[, codedescription := str_trim(str_replace_all(msatitle, " MSA", ""))]

# Keep only county code and cleaned MSA description
msa_xwalk <- msa_xwalk[, c("countycode", "codedescription")]

# Verify uniqueness (each county belongs to one MSA)
stopifnot(uniqueN(msa_xwalk) == nrow(msa_xwalk))

#' -----------------------------------------------------------------------------
#' MERGE AND CREATE FINAL CROSSWALK
#' -----------------------------------------------------------------------------

# Merge CEX PSU codes with county-MSA mapping
# Join on codedescription (MSA name)
# all.x=TRUE keeps all PSUs even if no county match (some PSUs are non-MSA)
cex_codebook <- merge(cex_codebook, msa_xwalk, by = "codedescription", all.x = TRUE)

# Rename columns to final schema
cex_codebook[, county := countycode]
cex_codebook[, PSU := codevalue]

# Keep only the final columns needed
cex_codebook <- cex_codebook[, c("county", "PSU")]

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

saveRDS(cex_codebook, file = "mkdata/data/county_msa_xwalk.rds")

message("Saved mkdata/data/county_msa_xwalk.rds")
message("  PSU codes: ", uniqueN(cex_codebook$PSU))
message("  Counties matched: ", sum(!is.na(cex_codebook$county)))
message("  Counties unmatched: ", sum(is.na(cex_codebook$county)))
