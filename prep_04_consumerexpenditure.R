# =============================================================================
# PREP 04: Create Consumer Expenditure Survey Outside Option Data
# =============================================================================
# Computes the fraction of consumers who do not get haircuts from the CEX
# interview data and saves PSU-quarter aggregates for the outside option.
# =============================================================================

library('data.table')

source('config.R')

cex_expend_path <- function(y) {
  project_path('mkdata', 'raw', '20220711_cex', paste0('intrvw', y), paste0('expn', y), paste0('xpb', y, '.csv'))
}

cex_income_path <- function(y, q) {
  project_path('mkdata', 'raw', '20220711_cex', paste0('intrvw', y), paste0('intrvw', y), paste0('fmli', y, q, '.csv'))
}

required_inputs <- c()
for (y in 13:21) {
  required_inputs <- c(
    required_inputs,
    cex_expend_path(y),
    cex_income_path(y, 2),
    cex_income_path(y, 3),
    cex_income_path(y, 4),
    cex_income_path(y + 1, 1)
  )
}
assert_required_files(required_inputs)

output_path <- project_path(CONFIG$prep_output_dir, 'cex_outside.rds')
ensure_directory(dirname(output_path))

outside_option <- data.table()

for (y in 13:21) {
  expend <- fread(cex_expend_path(y))
  assert_required_columns(expend, c('NEWID', 'QYEAR', 'SALONX', 'SALONX_'), paste0('expend_', y))

  income2 <- fread(cex_income_path(y, 2))
  income3 <- fread(cex_income_path(y, 3))
  income4 <- fread(cex_income_path(y, 4))
  income5 <- fread(cex_income_path(y + 1, 1))

  assert_required_columns(income2, c('NEWID', 'FSALARYM', 'PSU'), paste0('income_', y, '_2'))
  assert_required_columns(income3, c('NEWID', 'FSALARYM', 'PSU'), paste0('income_', y, '_3'))
  assert_required_columns(income4, c('NEWID', 'FSALARYM', 'PSU'), paste0('income_', y, '_4'))
  assert_required_columns(income5, c('NEWID', 'FSALARYM', 'PSU'), paste0('income_', y + 1, '_1'))

  income <- rbind(income2, income3, income4, income5, fill = TRUE)
  stopifnot(nrow(income) == uniqueN(income[, NEWID]))

  together <- merge(
    expend[, c('NEWID', 'QYEAR', 'SALONX', 'SALONX_')],
    income[, c('NEWID', 'FSALARYM', 'PSU')],
    by = 'NEWID'
  )
  assert_required_columns(together, c('NEWID', 'QYEAR', 'SALONX', 'SALONX_', 'FSALARYM', 'PSU'), paste0('together_', y))

  together[, SALONX := as.numeric(SALONX)]
  stopifnot(all(together[is.na(SALONX), ]$SALONX_ == 'A'))

  together[, nohc := is.na(SALONX)]

  outside_option <- rbind(
    outside_option,
    together[, .(
      nohc_count = sum(nohc),
      max_expend = if (all(is.na(SALONX))) NA_real_ else max(SALONX, na.rm = TRUE),
      min_spend = if (all(is.na(SALONX))) NA_real_ else min(SALONX, na.rm = TRUE),
      count_sample = .N
    ), by = c('PSU', 'QYEAR')],
    fill = TRUE
  )
}

stopifnot(nrow(outside_option) == uniqueN(outside_option[, c('PSU', 'QYEAR')]))

outside_option[, quarter_year := as.numeric(as.character(QYEAR / 10))]
outside_option[, quarter_year := ifelse(
  round(quarter_year - floor(quarter_year), 1) == 0.1,
  floor(quarter_year) - 1 + 0.4,
  quarter_year - 0.1
)]
outside_option[, quarter_year := as.numeric(as.character(quarter_year))]
setorder(outside_option, PSU, QYEAR)

saveRDS(outside_option, file = output_path)

message('Saved ', output_path)
message('  PSU-quarter observations: ', nrow(outside_option))
message('  Years covered: ', paste(range(floor(outside_option$quarter_year)), collapse = '-'))
