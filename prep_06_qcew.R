# =============================================================================
# PREP 06: Create QCEW County Wage Data
# =============================================================================
# Retrieves county-level QCEW wage data for beauty salons and caches the raw
# API pull so reruns can rebuild from a fixed local snapshot.
# =============================================================================

library('data.table')

source('config.R')

qcew_helper_path <- project_path('mkdata', 'raw', '20220427_qcew_code', 'qcew_rscript_example.R')
qcew_cache_path <- project_path(CONFIG$qcew_cache_path)
output_path <- project_path(CONFIG$prep_output_dir, 'qcew_county.rds')

ensure_directory(dirname(qcew_cache_path))
ensure_directory(dirname(output_path))

if (file.exists(qcew_cache_path) & !isTRUE(CONFIG$qcew_force_refresh)) {
  message('Loading cached QCEW data from: ', qcew_cache_path)
  puzzle <- data.table(readRDS(qcew_cache_path))
} else {
  assert_required_files(qcew_helper_path)
  source(qcew_helper_path)

  puzzle <- data.table()

  for (y in CONFIG$qcew_years) {
    for (q in CONFIG$qcew_quarters) {
      piece <- data.table(qcewGetIndustryData(y, q, CONFIG$qcew_industry_code))
      assert_required_columns(piece, c('year', 'qtr', 'area_fips'), paste0('qcew_', y, '_', q))
      puzzle <- rbind(puzzle, piece, fill = TRUE)
    }
  }

  stopifnot(nrow(puzzle) > 0)
  saveRDS(puzzle, qcew_cache_path)
  message('Saved QCEW cache to: ', qcew_cache_path)
}

assert_required_columns(puzzle, c('year', 'qtr', 'area_fips'), 'puzzle')
stopifnot(nrow(puzzle) > 0)

puzzle[, quarter_year := year + qtr / 10]
puzzle[, county := as.numeric(area_fips)]

saveRDS(puzzle, output_path)

message('Saved ', output_path)
message('  Observations: ', nrow(puzzle))
message('  Counties: ', uniqueN(puzzle$county))
message('  Date range: ', min(puzzle$quarter_year), ' to ', max(puzzle$quarter_year))
