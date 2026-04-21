#' =============================================================================
#' STEP 04: Assemble Estimation Sample and Write Summary Statistics
#' =============================================================================
#' Produces two artifacts used downstream:
#'
#'   A. mkdata/data/04_estimation_sample.rds
#'        Estimation-ready list consumed by 05_estimation.R and
#'        06_iv_spec_comparison.R. Augments 01_working.rds with PPI, minimum
#'        wage, instruments (dye_instrument, labor_instrument), organizational
#'        cost, and the estim_matrix projection used by the GMM routines.
#'
#'   B. results/out/tables/04_summary_stats_structural.tex
#'        LaTeX summary-stats table for the structural estimation sample
#'        (three focal counties x CONFIG$estimation_quarters), read from the
#'        pre-filter panel 01_staff_task_full.rds so the sample window lives
#'        in a single CONFIG entry.
#' =============================================================================

library("data.table")
library("readxl")
library("stargazer")

source("config.R")

ensure_directory(CONFIG$prep_output_dir)
ensure_directory("results/out/tables")

# -----------------------------------------------------------------------------
# Section A: Build the estimation-ready dataset
# -----------------------------------------------------------------------------
working_path <- file.path(CONFIG$prep_output_dir, "01_working.rds")
ppi_path     <- file.path(CONFIG$prep_output_dir, "ppi.rds")
minwage_path <- file.path(CONFIG$prep_output_dir, "minwage.xlsx")
assert_required_files(c(working_path, ppi_path, minwage_path))

working_data <- data.table(readRDS(working_path))
stopifnot(nrow(working_data[cust_price <= 0]) == 0)

ppi <- data.table(readRDS(ppi_path))
setkey(ppi, "quarter_year")
ppi[, lag_ppi := data.table::shift(ppi_inputs)]
working_data <- merge(working_data, ppi, all.x = TRUE, by = "quarter_year")
stopifnot(nrow(working_data[is.na(ppi_inputs)]) == 0)
stopifnot(nrow(working_data[cust_price <= 0]) == 0)

working_data[, labor_instrument := avg_wkly_wage / 40 * avg_labor]
working_data[, year := floor(quarter_year)]
working_data[, quarter := round((quarter_year - floor(quarter_year)) * 10)]
working_data[, qy_cnty := paste0(county, " - ", as.character(quarter_year))]
stopifnot(any(rowSums(working_data[, .SD, .SDcols = names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]]) == 1))
stopifnot(any(rowSums(working_data[, .SD, .SDcols = names(working_data)[grep("^B_[0-9]_", names(working_data))]]) == 1))

min_wage <- data.table(read_excel(minwage_path))
working_data <- merge(working_data,
                     min_wage[, c("county", "quarter_year", "min_wage")],
                     by = c("county", "quarter_year"), all.x = TRUE)
stopifnot(nrow(working_data[is.na(min_wage)]) == 0)

working_data[, log_rel_mkt := log(salon_share_subdiv / outside_share)]
working_data[, mk_piece    := 1 / (1 - salon_share_subdiv)]
working_data[, org_cost    := gamma_normalized * s_index * avg_labor]
setorder(working_data, "location_id", "quarter_year")
working_data[, dye_instrument := get(paste0("task_mix_", CONFIG$dye_task_index)) * ppi_inputs]
stopifnot(nrow(working_data[is.na(org_cost)]) == 0)

## no situations where worker type is not observed
sumcount <- function(x) return(sum(x > 0))
check <- working_data[, lapply(.SD, sumcount),
                      by = c("qy_cnty"),
                      .SDcols = names(working_data)[grep("^E_raw_[0-9]", names(working_data))]]
# stopifnot(all(check > 0))

quarter_count <- uniqueN(working_data$quarter_year)
county_count  <- uniqueN(working_data$county)
skill_count   <- length(names(working_data)[grep("^B_raw_[0-9]_", names(working_data))])

working_data[, county := as.character(county)]
working_data[, mult_duration_hrs := tot_duration / 60]

estim_matrix <- as.data.frame(working_data[, .SD, .SDcols = c(
  "avg_labor", "dye_instrument", "county", "quarter_year", "log_rel_mkt", "cust_price",
  names(working_data)[grep("^B_raw_[0-9]_", names(working_data))],
  "org_cost", "mk_piece",
  names(working_data)[grep("^E_raw_[0-9]", names(working_data))],
  names(working_data)[grep("^task_mix_[0-9]", names(working_data))], "qy_cnty",
  "gamma_normalized", "s_index"
)])

estimation_sample_out <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
saveRDS(list(
  working_data  = working_data,
  estim_matrix  = estim_matrix,
  quarter_count = quarter_count,
  county_count  = county_count,
  skill_count   = skill_count
), estimation_sample_out)

if (isTRUE(CONFIG$verbose_logging)) {
  message("04: wrote ", estimation_sample_out,
          " (working_data rows: ", nrow(working_data),
          ", estim_matrix rows: ", nrow(estim_matrix), ")")
}

# -----------------------------------------------------------------------------
# Section B: Summary-statistics table for the structural estimation sample
# -----------------------------------------------------------------------------
## 01_staff_task_full.rds is the pre-filter firm-quarter panel. Read it here
## (rather than the already-filtered 01_working.rds) so the sample window
## lives in a single CONFIG entry and cannot drift between scripts.
staff_task_full_path <- file.path(CONFIG$prep_output_dir, "01_staff_task_full.rds")
assert_required_files(staff_task_full_path)

full_unsmoothed <- data.table(readRDS(staff_task_full_path))

keep_cols <- c(
  grep("^task_mix", colnames(full_unsmoothed), value = TRUE),
  "s_index", "location_id", "county", "quarter_year",
  "emps", "cust_count", "revenue"
)
full_unsmoothed <- unique(full_unsmoothed[, .SD, .SDcols = keep_cols])

full_unsmoothed <- full_unsmoothed[
  county %in% CONFIG$counties_padded &
    quarter_year %in% CONFIG$estimation_quarters,
]
stopifnot(nrow(full_unsmoothed) > 0)

if (isTRUE(CONFIG$verbose_logging)) {
  message("04: estimation-sample firm-quarters: ", nrow(full_unsmoothed))
  message("04: unique firms: ", uniqueN(full_unsmoothed$location_id))
}

firm_stats <- full_unsmoothed[, c(
  "revenue", "emps", "cust_count", "s_index",
  "task_mix_1", "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5"
)]

names(firm_stats) <- c(
  "Revenue", "Employees", "Customers", "S-Index",
  "Share Haircut/Shave",
  "Share Color/Highlight/Wash",
  "Share Blowdry/Style/Treatment/Extensions",
  "Share Administrative",
  "Share Nail/Spa/Eye/Misc."
)

output_tex <- "results/out/tables/04_summary_stats_structural.tex"
stargazer(firm_stats, header = FALSE, type = "text")
stargazer(firm_stats, header = FALSE, digits = 2, out = output_tex, single.row = TRUE)

message("04: wrote ", output_tex)
