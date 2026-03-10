# =============================================================================
# PREP 01: Create Service Classification Dataset
# =============================================================================
# Creates the service-to-task classification mapping from cosmetologist
# annotations and saves the lookup used by the task-construction pipeline.
# =============================================================================

library('data.table')
library('stringr')
library('readxl')

source('config.R')

cosmo_class_path <- project_path('mkdata', 'raw', '20220526_cosmo_classify', 'Upwork service desciptions - COMPLETE.xlsx')
cosmo_update_path <- project_path('mkdata', 'raw', '20220526_cosmo_classify', 'check_cosmo - COMPLETE (3).xlsx')
output_path <- project_path(CONFIG$prep_output_dir, 'classified_descriptions.rds')

assert_required_files(c(cosmo_class_path, cosmo_update_path))
ensure_directory(dirname(output_path))

cosmo_class <- data.table(read_excel(cosmo_class_path))
cosmo_update <- data.table(read_excel(cosmo_update_path))

required_cols <- c('raw_id', 'Service Description', 'Cut', 'Shave', 'Color/Bleach',
                   'Highlight/Balayage', 'Wash/Shampoo', 'Extensions', 'Blowdry',
                   'Style', 'Other Treatment', 'Admin/Consult', 'Male', 'Female', 'Child')
assert_required_columns(cosmo_class, required_cols, 'cosmo_class')
assert_required_columns(cosmo_update, required_cols, 'cosmo_update')

cosmo_class <- rbind(cosmo_class[!(raw_id %in% cosmo_update$raw_id)], cosmo_update)
stopifnot(nrow(cosmo_class) == uniqueN(cosmo_class$raw_id))

orig_vars <- copy(colnames(cosmo_class))

cosmo_class[, `Service Description` := ifelse(
  substring(`Service Description`, 1, 1) == "'",
  substring(`Service Description`, 2),
  `Service Description`
)]

cosmo_class[, marked := 0]
for (var in colnames(cosmo_class)[!(colnames(cosmo_class) %in% c('Service Description', 'raw_id', 'marked'))]) {
  cosmo_class[, marked := marked + !is.na(get(var))]
}

cosmo_class[, main_cat := 0]
for (var in colnames(cosmo_class)[4:16]) {
  cosmo_class[, main_cat := marked + !is.na(get(var))]
}

cosmo_class[, main_cat := ifelse(main_cat > 1, main_cat - !is.na(`Admin/Consult`), main_cat)]

cosmo_class[Cut == 1 | Shave == 1, taskcat1 := 1]
cosmo_class[`Color/Bleach` == 1 | `Highlight/Balayage` == 1 | `Wash/Shampoo` == 1, taskcat2 := 1]
cosmo_class[Extensions == 1, taskcat3 := 1]
cosmo_class[Blowdry == 1 | Style == 1 | `Other Treatment` == 1, taskcat4 := 1]
cosmo_class[(`Admin/Consult` == 1 & main_cat == 1), taskcat5 := 1]

for (var in colnames(cosmo_class)[colnames(cosmo_class) %like% 'taskcat']) {
  cosmo_class[, (var) := ifelse(is.na(get(var)), 0, get(var))]
}

cosmo_class[, count_cat := rowSums(.SD), .SDcols = colnames(cosmo_class)[colnames(cosmo_class) %like% 'taskcat']]
cosmo_class[, taskcat6 := as.numeric(count_cat == 0)]

cosmo_class <- cosmo_class[, .SD, .SDcols = c(
  'count_cat', 'Male', 'Female', 'Child', 'raw_id', 'Service Description',
  colnames(cosmo_class)[colnames(cosmo_class) %like% 'taskcat']
)]
stopifnot(all(orig_vars[orig_vars %in% c('Male', 'Female', 'Child')] %in% names(cosmo_class)))

saveRDS(cosmo_class, output_path)

message('Saved ', output_path, ' with ', nrow(cosmo_class), ' service classifications')
message('Task category distribution:')
message('  taskcat1 (Haircut/Shave): ', sum(cosmo_class$taskcat1))
message('  taskcat2 (Color/Wash): ', sum(cosmo_class$taskcat2))
message('  taskcat3 (Extensions): ', sum(cosmo_class$taskcat3))
message('  taskcat4 (Blowdry/Style): ', sum(cosmo_class$taskcat4))
message('  taskcat5 (Admin): ', sum(cosmo_class$taskcat5))
message('  taskcat6 (Misc): ', sum(cosmo_class$taskcat6))
