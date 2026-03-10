# =============================================================================
# PREP 03: Create County-MSA Crosswalk for CEX Data
# =============================================================================
# Creates a county-to-PSU crosswalk by merging the CEX PSU codebook with the
# QCEW county-MSA crosswalk.
# =============================================================================

library('data.table')
library('stringr')

source('config.R')

cex_codebook_path <- project_path('mkdata', 'raw', '20231023_county_msa_crosswalk', 'cex_cleaned_codebook.csv')
msa_xwalk_path <- project_path('mkdata', 'raw', '20231023_county_msa_crosswalk', 'qcew-county-msa-csa-crosswalk.csv')
output_path <- project_path(CONFIG$prep_output_dir, 'county_msa_xwalk.rds')

assert_required_files(c(cex_codebook_path, msa_xwalk_path))
ensure_directory(dirname(output_path))

cex_codebook <- fread(cex_codebook_path)
assert_required_columns(cex_codebook, c('variable', 'codevalue', 'codedescription', 'survey'), 'cex_codebook')
colnames(cex_codebook) <- str_replace_all(str_to_lower(colnames(cex_codebook)), fixed(' '), '')
assert_required_columns(cex_codebook, c('variable', 'codevalue', 'codedescription', 'survey'), 'cex_codebook_clean')

cex_codebook <- cex_codebook[
  variable == 'PSU' & str_detect(codevalue, 'S') > 0 & survey == 'INTERVIEW',
  c('codevalue', 'codedescription')
]
stopifnot(uniqueN(cex_codebook) == nrow(cex_codebook))

msa_xwalk <- fread(msa_xwalk_path)
assert_required_columns(msa_xwalk, c('countycode', 'msatitle'), 'msa_xwalk')
colnames(msa_xwalk) <- str_replace_all(str_to_lower(colnames(msa_xwalk)), fixed(' '), '')
assert_required_columns(msa_xwalk, c('countycode', 'msatitle'), 'msa_xwalk_clean')

msa_xwalk[, codedescription := str_trim(str_replace_all(msatitle, ' MSA', ''))]
msa_xwalk <- msa_xwalk[, c('countycode', 'codedescription')]
stopifnot(uniqueN(msa_xwalk) == nrow(msa_xwalk))

cex_codebook <- merge(cex_codebook, msa_xwalk, by = 'codedescription', all.x = TRUE)
cex_codebook[, county := countycode]
cex_codebook[, PSU := codevalue]
cex_codebook <- cex_codebook[, c('county', 'PSU')]

saveRDS(cex_codebook, file = output_path)

message('Saved ', output_path)
message('  PSU codes: ', uniqueN(cex_codebook$PSU))
message('  Counties matched: ', sum(!is.na(cex_codebook$county)))
message('  Counties unmatched: ', sum(is.na(cex_codebook$county)))
