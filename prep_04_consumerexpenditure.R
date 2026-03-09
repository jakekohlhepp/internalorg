#' =============================================================================
#' PREP 04: Create Consumer Expenditure Survey Outside Option Data
#' =============================================================================
#' Computes the fraction of consumers who do not get haircuts (outside option)
#' from the Consumer Expenditure Survey (CEX) interview data.
#'
#' The CEX tracks household expenditures including salon services (SALONX).
#' We compute the share of respondents with zero salon spending by PSU/quarter,
#' which represents the "outside option" share in the demand model.
#'
#' Input:  mkdata/raw/20220711_cex/intrvw{YY}/ (for years 2013-2021)
#'         - expn{YY}/xpb{YY}.csv (expenditure data)
#'         - intrvw{YY}/fmli{YY}2-4.csv and fmli{YY+1}1.csv (interview data)
#' Output: mkdata/data/cex_outside.rds
#'
#' Output Schema:
#'   PSU: Primary Sampling Unit code
#'   QYEAR: Quarter-year identifier (e.g., 20191 for 2019 Q1)
#'   quarter_year: Numeric quarter-year (e.g., 2019.1)
#'   nohc_count: Count of households with no haircut spending
#'   count_sample: Total households in sample
#'   max_expend: Maximum salon expenditure in sample
#'   min_spend: Minimum (non-zero) salon expenditure
#'
#' Dependencies: None
#' =============================================================================

library('data.table')

#' -----------------------------------------------------------------------------
#' PROCESS CEX DATA FOR EACH YEAR (2013-2021)
#' -----------------------------------------------------------------------------

outside_option <- c()

# Loop through years 2013-2021 (coded as 13-21 in CEX file naming)
for (y in 13:21) {

  # Load expenditure data (xpb file contains personal care expenditures)
  # SALONX is the salon/haircut expenditure variable
  # Path pattern: mkdata/raw/20220711_cex/intrvw{YY}/expn{YY}/xpb{YY}.csv
  expend <- fread(gsub('19', as.character(y), 'mkdata/raw/20220711_cex/intrvw19/expn19/xpb19.csv'))

  # Load interview data (fmli files contain household characteristics)
  # Need Q2-Q4 of current year and Q1 of next year for full annual coverage
  income2 <- fread(gsub('19', as.character(y), 'mkdata/raw/20220711_cex/intrvw19/intrvw19/fmli192.csv'))
  income3 <- fread(gsub('19', as.character(y), 'mkdata/raw/20220711_cex/intrvw19/intrvw19/fmli193.csv'))
  income4 <- fread(gsub('19', as.character(y), 'mkdata/raw/20220711_cex/intrvw19/intrvw19/fmli194.csv'))

  # Q1 of next year (file naming: fmli{YY+1}1.csv)
  income5 <- fread(paste0(
    'mkdata/raw/20220711_cex/',
    gsub('19', as.character(y), 'intrvw19/'),
    gsub('19', as.character(y), 'intrvw19/'),
    gsub('20', as.character(y + 1), 'fmli201.csv')
  ))

  # Combine all interview quarters
  income <- rbind(income2, income3, income4, income5, fill = TRUE)

  # Verify uniqueness by NEWID (household identifier)
  stopifnot(nrow(income) == uniqueN(income[, NEWID]))

  # Merge expenditure and income data
  # Keep: NEWID (household ID), QYEAR (quarter-year), SALONX (salon spending),
  #       SALONX_ (imputation flag), FSALARYM (salary income), PSU (sampling unit)
  together <- merge(
    expend[, c("NEWID", "QYEAR", "SALONX", "SALONX_")],
    income[, c("NEWID", "FSALARYM", "PSU")],
    by = "NEWID"
  )

  # Convert SALONX to numeric
  together[, SALONX := as.numeric(SALONX)]

  # Verify: all NA values in SALONX have imputation flag "A" (valid skip)
  stopifnot(all(together[is.na(SALONX), ]$SALONX_ == "A"))

  # Create "no haircut" indicator: TRUE if SALONX is NA (zero spending)
  together[, nohc := is.na(SALONX)]

  # Aggregate by PSU and quarter
  # Compute: count of no-haircut households, spending range, sample size
  outside_option <- rbind(
    outside_option,
    together[, .(
      nohc_count = sum(nohc),
      max_expend = max(SALONX, na.rm = TRUE),
      min_spend = min(SALONX, na.rm = TRUE),
      count_sample = .N
    ), by = c("PSU", "QYEAR")],
    fill = TRUE
  )
}

# Add final year's aggregation (from last iteration)
outside_option <- rbind(
  outside_option,
  together[, .(
    nohc_count = sum(nohc),
    max_expend = max(SALONX, na.rm = TRUE),
    min_spend = min(SALONX, na.rm = TRUE),
    count_sample = .N
  ), by = c("PSU", "QYEAR")],
  fill = TRUE
)

#' -----------------------------------------------------------------------------
#' CREATE QUARTER-YEAR VARIABLE
#' -----------------------------------------------------------------------------

# Convert QYEAR (e.g., 20191) to quarter_year format (e.g., 2019.1)
# QYEAR format: YYYYQ where Q is 1-4
outside_option[, quarter_year := as.numeric(as.character(QYEAR / 10))]

# Adjust for Q1 which spans two calendar years in CEX
# If quarter decimal is 0.1, it's actually Q4 of previous year (adjust to X.4)
outside_option[, quarter_year := ifelse(
  round(quarter_year - floor(quarter_year), 1) == 0.1,
  floor(quarter_year) - 1 + 0.4,
  quarter_year - 0.1
)]

outside_option[, quarter_year := as.numeric(as.character(quarter_year))]

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

saveRDS(outside_option, file = "mkdata/data/cex_outside.rds")

message("Saved mkdata/data/cex_outside.rds")
message("  PSU-quarter observations: ", nrow(outside_option))
message("  Years covered: ", paste(range(floor(outside_option$quarter_year)), collapse = "-"))
