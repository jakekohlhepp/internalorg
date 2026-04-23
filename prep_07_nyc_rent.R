#' =============================================================================
#' PREP 07: NYC Commercial Rent Data (Manhattan)
#' =============================================================================
#' Downloads and processes storefront rent data from NYC Open Data.
#' Provides quarterly variation in commercial rent by ZIP code for Manhattan.
#' 
#' Source: Storefronts Reported Vacant or Not (NYC Open Data, ID: at9v-p9hk)
#' =============================================================================

library(data.table)
library(lubridate)
library(jsonlite)
library(httr)

source("config.R")
source("utils/logging.R")

log_init("prep_07_nyc_rent.R")
log_message("Starting NYC commercial rent data preparation")

# -----------------------------------------------------------------------------
# DOWNLOAD DATA
# -----------------------------------------------------------------------------
# NYC Open Data API URL (SODA)
# Filter for Manhattan only to reduce download size
api_url <- "https://data.cityofnewyork.us/resource/92iy-9c3n.json"
query_url <- paste0(api_url, "?borough=MANHATTAN&$limit=50000")

log_message(paste("Downloading data from:", api_url))

res <- GET(query_url)
if (status_code(res) != 200) {
  log_message(paste("Failed to download data from NYC Open Data. Status code:", status_code(res)), "ERROR")
  log_message(paste("Response:", content(res, as = "text")), "ERROR")
  stop("Download failed")
}

raw_data <- fromJSON(content(res, as = "text"))
rent_dt <- as.data.table(raw_data)

log_message(paste("Downloaded", nrow(rent_dt), "records"))

# -----------------------------------------------------------------------------
# CLEAN AND PROCESS
# -----------------------------------------------------------------------------

# Columns of interest: postcode, monthly_rent_per_sq_ft, primary_registration_date
# monthly_rent_per_sq_ft is character in JSON, convert to numeric
rent_dt[, rent_psf := as.numeric(monthly_rent_per_sq_ft)]

# Parse date and create quarter_year
rent_dt[, date := ymd_hms(primary_registration_date)]
rent_dt[, quarter_year := year(date) + quarter(date) / 10]

# Filter to relevant years (2018-2021) and non-zero rent
# Also filter for postcode (ZIP)
rent_dt <- rent_dt[!is.na(rent_psf) & rent_psf > 0 & rent_psf < 2000] # Cap extreme outliers
rent_dt <- rent_dt[year(date) >= 2018 & year(date) <= 2021]

log_message(paste("Filtered to", nrow(rent_dt), "valid rent records for 2018-2021"))

# Aggregate by ZIP and Quarter
zip_rent_quarterly <- rent_dt[, .(
  mean_rent_psf = mean(rent_psf),
  median_rent_psf = median(rent_psf),
  n_obs = .N
), by = .(postcode, quarter_year)]

# Standardize names
setnames(zip_rent_quarterly, "postcode", "location_zip")
zip_rent_quarterly[, location_zip := as.numeric(location_zip)]

# -----------------------------------------------------------------------------
# INTERPOLATION / COMPLETION
# -----------------------------------------------------------------------------
# Ensure all relevant quarters are present for each ZIP
all_zips <- unique(zip_rent_quarterly$location_zip)
all_qtrs <- CONFIG$estimation_quarters

full_grid <- as.data.table(expand.grid(
  location_zip = all_zips,
  quarter_year = all_qtrs
))

zip_rent_final <- merge(full_grid, zip_rent_quarterly, by = c("location_zip", "quarter_year"), all.x = TRUE)

# Fill missing values within ZIP using linear interpolation or LOCF
setkey(zip_rent_final, location_zip, quarter_year)
zip_rent_final[, median_rent_psf := nafill(median_rent_psf, type = "locf"), by = location_zip]
zip_rent_final[, median_rent_psf := nafill(median_rent_psf, type = "nocb"), by = location_zip]

# If still NA (ZIP has no data at all), use Manhattan-wide average for that quarter
manhattan_avg <- zip_rent_quarterly[, .(avg_manhattan_rent = mean(median_rent_psf, na.rm=TRUE)), by = quarter_year]
zip_rent_final <- merge(zip_rent_final, manhattan_avg, by = "quarter_year", all.x = TRUE)
zip_rent_final[is.na(median_rent_psf), median_rent_psf := avg_manhattan_rent]

log_message("Aggregated and interpolated rent data by ZIP and quarter")

# -----------------------------------------------------------------------------
# SAVE OUTPUT
# -----------------------------------------------------------------------------
output_path <- file.path(CONFIG$prep_output_dir, "nyc_rent_zip_quarterly.rds")
saveRDS(zip_rent_final, output_path)

log_message(paste("Saved output to:", output_path))
log_complete(success = TRUE)
