#' =============================================================================
#' STEP 01_03: Summary Statistics for the Structural Estimation Sample
#' =============================================================================
#' Produces a single LaTeX summary-statistics table describing the firm-quarter
#' panel used in the structural estimation. The sample is restricted to the
#' three focal counties (Cook, Manhattan, Los Angeles) and the quarters listed
#' in CONFIG$estimation_quarters.
#'
#' Input:
#'   - mkdata/data/01_staff_task_full.rds  (firm-quarter panel with task_mix_*
#'     and s_index; produced by 01_build_data.R BEFORE the county / quarter
#'     filter, so that 01_03 can apply the same filter consistently via CONFIG)
#'
#' Output:
#'   - results/out/tables/01_03_summary_stats_structural.tex
#' =============================================================================

# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------
library('data.table')
library('stargazer')

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
source('config.R')

ensure_directory("results/out/tables")

# -----------------------------------------------------------------------------
# Load and filter to the estimation sample
# -----------------------------------------------------------------------------
## 01_staff_task_full.rds is the pre-filter firm-quarter panel. It contains
## rows for many counties and quarters; we restrict here rather than reading
## the already-filtered file so that the sample window lives in a single
## CONFIG entry and cannot drift between scripts.
staff_task_full_path <- file.path(CONFIG$prep_output_dir, "01_staff_task_full.rds")
assert_required_files(staff_task_full_path)

full_unsmoothed <- data.table(readRDS(staff_task_full_path))

## keep only the columns needed for the summary table (one row per
## firm-quarter after the unique() call)
keep_cols <- c(
  grep("^task_mix", colnames(full_unsmoothed), value = TRUE),
  "s_index", "location_id", "county", "quarter_year",
  "emps", "cust_count", "revenue"
)
full_unsmoothed <- unique(full_unsmoothed[, .SD, .SDcols = keep_cols])

## restrict to the structural estimation sample
full_unsmoothed <- full_unsmoothed[
  county %in% CONFIG$counties_padded &
    quarter_year %in% CONFIG$estimation_quarters,
]
stopifnot(nrow(full_unsmoothed) > 0)

if (isTRUE(CONFIG$verbose_logging)) {
  message("01_03: estimation-sample firm-quarters: ", nrow(full_unsmoothed))
  message("01_03: unique firms: ", uniqueN(full_unsmoothed$location_id))
}

# -----------------------------------------------------------------------------
# Build and write the summary-statistics table
# -----------------------------------------------------------------------------
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

output_tex <- "results/out/tables/01_03_summary_stats_structural.tex"
stargazer(firm_stats, header = FALSE, type = 'text')
stargazer(firm_stats, header = FALSE, digits = 2, out = output_tex, single.row = TRUE)

message("01_03: wrote ", output_tex)
