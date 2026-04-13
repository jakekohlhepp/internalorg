#' =============================================================================
#' STEP 01_03: Summary Statistics for Estimation Sample
#' =============================================================================
#' Generates summary statistics tables for the three-county estimation sample.
#'
#' Input:  mkdata/data/01_staff_task_full.rds
#' Output: results/out/tables/01_03_summary_stats_structural.tex
#' =============================================================================

# Load required packages
library('data.table')
library('stargazer')

# Load configuration
source('config.R')

ensure_directory("results/out/tables")

#' -----------------------------------------------------------------------------
#' LOAD AND FILTER TO ESTIMATION SAMPLE
#' -----------------------------------------------------------------------------

full_unsmoothed <- readRDS(file.path(CONFIG$prep_output_dir, "01_staff_task_full.rds"))

full_unsmoothed <- unique(full_unsmoothed[, .SD, .SDcols = c(colnames(full_unsmoothed)[grep("^task_mix", colnames(full_unsmoothed))], "s_index", "location_id", "county", "cust_count", "quarter_year", "emps", "cust_count", "revenue")])

## restrict to the three counties and quarters
full_unsmoothed <- full_unsmoothed[
  county %in% CONFIG$counties_padded &
    quarter_year %in% CONFIG$estimation_quarters,
]

#' -----------------------------------------------------------------------------
#' GENERATE SUMMARY STATISTICS TABLE
#' -----------------------------------------------------------------------------

firm_stats <- full_unsmoothed[, c("revenue", "emps", "cust_count", "s_index", "task_mix_1", "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5")]

names(firm_stats) <- c("Revenue", "Employees", "Customers", "S-Index",
                       "Share Haircut/Shave", "Share Color/Highlight/Wash", "Share Blowdry/Style/Treatment/Extensions",
                       "Share Admininstrative", "Share Nail/Spa/Eye/Misc.")
stargazer(firm_stats, header = FALSE, type = 'text')
stargazer(firm_stats, header = FALSE, digits = 2, out = 'results/out/tables/01_03_summary_stats_structural.tex', single.row = TRUE)
