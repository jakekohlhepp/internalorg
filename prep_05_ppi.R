# =============================================================================
# PREP 05: Create Producer Price Index Dataset
# =============================================================================
# Processes the BLS Producer Price Index input file into one quarterly series.
# =============================================================================

library('data.table')
library('lubridate')

source('config.R')

ppi_input_path <- project_path('mkdata', 'raw', '20231227_ppi_cost', 'file.csv')
output_path <- project_path(CONFIG$prep_output_dir, 'ppi.rds')

assert_required_files(ppi_input_path)
ensure_directory(dirname(output_path))

ppi <- fread(ppi_input_path)
assert_required_columns(ppi, c('Value', 'Period', 'Year'), 'ppi')

setnames(ppi, 'Value', 'ppi_inputs')
ppi[, month := as.numeric(gsub('M', '', Period))]
ppi[, quarter := quarter(month)]
ppi[, quarter_year := as.numeric(Year) + quarter / 10]
ppi[, helper := frank(month), by = c('quarter_year')]
ppi <- ppi[helper == 1, c('ppi_inputs', 'quarter_year')]
stopifnot(nrow(ppi) == uniqueN(ppi))

saveRDS(ppi, output_path)

message('Saved ', output_path)
message('  Quarters: ', nrow(ppi))
message('  Date range: ', min(ppi$quarter_year), ' to ', max(ppi$quarter_year))
