# =============================================================================
# PREP 02: Create County Census Population Dataset
# =============================================================================
# Creates county-level Census population estimates by year from the 2010-2022
# county population files and saves a long-format county-year table.
# =============================================================================

library('data.table')
library('lubridate')
library('stringr')

source('config.R')

countypop_2022_path <- project_path('mkdata', 'raw', '202403101_morecspops', 'co-est2022-alldata.csv')
countypop_2019_path <- project_path('mkdata', 'raw', '202403101_morecspops', 'co-est2019-alldata.csv')
output_path <- project_path(CONFIG$prep_output_dir, 'county_census_pop.rds')

assert_required_files(c(countypop_2022_path, countypop_2019_path))
ensure_directory(dirname(output_path))

countypop_more <- fread(countypop_2022_path)[-1]
assert_required_columns(countypop_more, c('STATE', 'COUNTY', 'POPESTIMATE2020', 'POPESTIMATE2021', 'POPESTIMATE2022'), 'countypop_more')
countypop_more[, county := sprintf('%02d%03d', as.integer(STATE), as.integer(COUNTY))]
stopifnot(nrow(countypop_more[str_length(county) != 5, ]) == 0)
countypop_more <- countypop_more[, c('county', 'POPESTIMATE2020', 'POPESTIMATE2021', 'POPESTIMATE2022')]

countypop_more2 <- fread(countypop_2019_path)[-1]
assert_required_columns(countypop_more2, c('STATE', 'COUNTY', 'POPESTIMATE2010', 'POPESTIMATE2011', 'POPESTIMATE2012', 'POPESTIMATE2013',
                                           'POPESTIMATE2014', 'POPESTIMATE2015', 'POPESTIMATE2016', 'POPESTIMATE2017',
                                           'POPESTIMATE2018', 'POPESTIMATE2019'), 'countypop_more2')
countypop_more2[, county := sprintf('%02d%03d', as.integer(STATE), as.integer(COUNTY))]
stopifnot(nrow(countypop_more2[str_length(county) != 5, ]) == 0)
countypop_more2 <- countypop_more2[, c(
  'county', 'POPESTIMATE2010', 'POPESTIMATE2011', 'POPESTIMATE2012', 'POPESTIMATE2013',
  'POPESTIMATE2014', 'POPESTIMATE2015', 'POPESTIMATE2016', 'POPESTIMATE2017',
  'POPESTIMATE2018', 'POPESTIMATE2019'
)]

countypop_more <- merge(countypop_more, countypop_more2, by = 'county', all = TRUE)
countypop_more <- melt(
  countypop_more,
  id.vars = 'county',
  measure.vars = patterns('^POPESTIMATE'),
  value.name = 'CSPOP'
)
countypop_more[, year := as.numeric(str_remove(variable, 'POPESTIMATE'))]
stopifnot(nrow(countypop_more[is.na(CSPOP)]) == 0)

saveRDS(countypop_more[, -c('variable')], output_path)

message('Saved ', output_path)
message('  Counties: ', uniqueN(countypop_more$county))
message('  Years: ', paste(range(countypop_more$year), collapse = '-'))
