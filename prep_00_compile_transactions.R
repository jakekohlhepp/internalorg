# =============================================================================
# PREP 00: Compile Raw Transaction Data
# =============================================================================
# Compiles raw salon transaction data from multiple data pulls into a single
# unified transaction file at the machine-specific raw data location.
# =============================================================================

library('data.table')
library('stringr')
library('lubridate')

source('config.R')

stopifnot(nzchar(CONFIG$raw_data_base))

original_pull_path <- file.path(CONFIG$raw_data_base, '20200909_raw', 'Marketing Intern Data')
refresh_pull_path <- file.path(CONFIG$raw_data_base, '20210809_alldata_refresh_withzip')
output_path <- file.path(CONFIG$raw_data_base, 'compiled_trxns.rds')

header_path <- file.path(original_pull_path, 'x00.csv')
refresh_file_path <- file.path(refresh_pull_path, 'appointment_export.csv')
assert_required_files(c(header_path, refresh_file_path))

files <- list.files(original_pull_path, pattern = '\\.csv$', full.names = TRUE)
stopifnot(length(files) > 0)

message('Loading original data pull from: ', original_pull_path)

top <- fread(header_path)
assert_required_columns(top, c('app_id', 'location_id', 'customer_id', 'service_id', 'app_datetime', 'location_zip', 'app_stage'), 'top')

compiled_txn <- rbindlist(lapply(files[!grepl('x00\\.csv$', files)], fread))
names(compiled_txn) <- names(top)

full_transactions <- rbind(compiled_txn, top)
rm(compiled_txn, top)
assert_required_columns(full_transactions, c('app_id', 'location_id', 'customer_id', 'service_id', 'app_datetime', 'location_zip', 'app_stage'), 'full_transactions')

for (v in names(full_transactions)) {
  if (all(!is.na(full_transactions[, get(v)]))) {
    message(v, ' is ok and is a ', class(full_transactions[, get(v)]))
  } else {
    message(v, ' has MISSING values and is a ', class(full_transactions[, get(v)]))
  }
}

full_transactions[, discount_amount := NULL]

message('Loading 2021 refresh from: ', refresh_pull_path)

all2020 <- fread(refresh_file_path)
names(all2020) <- str_to_lower(names(all2020))
assert_required_columns(all2020, c('app_id', 'location_id', 'customer_id', 'service_id', 'app_datetime', 'location_zip', 'app_stage', 'business_id', 'industry'), 'all2020')

for (v in names(all2020)) {
  if (all(!is.na(all2020[, get(v)]))) {
    message(v, ' is ok and is a ', class(all2020[, get(v)]))
  } else {
    message(v, ' has MISSING values and is a ', class(all2020[, get(v)]))
  }
}

stopifnot(nrow(full_transactions) - uniqueN(full_transactions) == 0)

full_transactions[, location_city := NULL]
full_transactions[, location_state := NULL]

cols <- colnames(all2020)
cols <- cols[cols != 'discount_amount']
cols <- cols[!(cols %in% c('location_city', 'location_state', 'location_zip',
                           'business_id', 'industry',
                           'first_location_app_datetime', 'first_business_app_datetime'))]

uniq_2020 <- unique(all2020[, cols, with = FALSE])
uniq_main <- unique(full_transactions[, c(cols, 'total_app_time'), with = FALSE])

stopifnot(nrow(uniq_main) == uniqueN(uniq_main))

uniq_main[, main_src := 1]

uniq_combined <- rbind(uniq_2020, uniq_main, fill = TRUE)
setorderv(uniq_combined, c(cols, 'main_src'))

stopifnot(inherits(uniq_combined$app_datetime, 'POSIXct'))
uniq_combined[, date := as.Date(app_datetime)]
uniq_combined[, dup := .N > 1, by = cols]
uniq_combined <- uniq_combined[dup == 0 | (dup == 1 & main_src == 1), ]
stopifnot(nrow(uniq_combined) == uniqueN(uniq_combined[, -c('main_src')]))

rm(full_transactions, uniq_2020, uniq_main)

biz_locs_states <- unique(all2020[, c('location_id', 'location_city', 'location_state',
                                       'location_zip', 'business_id', 'industry',
                                       'first_location_app_datetime',
                                       'first_business_app_datetime')])
assert_required_columns(biz_locs_states, c('location_id', 'location_city', 'location_state', 'location_zip', 'business_id', 'industry'), 'biz_locs_states')

biz_locs_states[, first_business_app_datetime := min(first_business_app_datetime), by = location_id]
biz_locs_states <- unique(biz_locs_states)
stopifnot(uniqueN(biz_locs_states$location_id) == nrow(biz_locs_states))

biz_locs_states[, location_city := str_trim(location_city)]
biz_locs_states[location_city %in% c('Los Angelas'), location_city := 'Los Angeles']
biz_locs_states[location_city %in% c('nyc'), location_city := 'New York']
biz_locs_states[location_city %in% c('West Los Angeles', 'East Los Angeles',
                                      'Hollywood', 'West Hollywood'),
                location_city := 'Hollywood']

uniq_combined <- merge(uniq_combined, biz_locs_states, by = 'location_id', all.x = TRUE)

uniq_combined <- uniq_combined[app_stage == 'final', ]
uniq_combined[location_city == 'Denver', location_state := 'CO']
uniq_combined[location_city == 'Stockton', location_state := 'CA']
uniq_combined[location_city == 'Austin', location_state := 'TX']

uniq_combined[, first_date := min(date), by = 'app_id']
uniq_combined[app_id == '7f388e8a-4c7c-4b4c-8f3b-b3dbda35e5f7' & date != first_date,
              app_id := '7f388e8a-4c7c-4b4c-8f3b-b3dbda35e5f7-change']
uniq_combined[app_id == '7f388e8a-4c7c-4b4c-8f3b-b3dbda35e5f7',
              first_date := min(date)]
stopifnot(all(uniq_combined$first_date == uniq_combined$date))

stopifnot(nrow(uniq_combined[business_id == '0b8bc512-705d-4be9-90e4-2fad62a4e4f4', ]) == 0)

saveRDS(uniq_combined, file = output_path)
message('Saved compiled transactions to: ', output_path)
message('  Total transactions: ', nrow(uniq_combined))
message('  Date range: ', min(uniq_combined$app_datetime), ' to ', max(uniq_combined$app_datetime))
