#' =============================================================================
#' STEP 02b: Measurement Error in the S-index (Full National Sample)
#' =============================================================================
#' Appendix measurement-error check for the task-specialization index (s-index).
#' Task assignments are treated as observed without error when the s-index is
#' computed at the salon-quarter level. That is defensible only if the quarter
#' has enough assignments that sampling error is small. This script tests it the
#' way the paper does: recompute the s-index separately for each MONTH within a
#' quarter, giving up to three measurements per salon-quarter, and report the
#' pairwise correlations across those months. High within-quarter correlations
#' imply the quarter-level s-index carries little measurement error.
#'
#' The month-level s-index is built with exactly the same rate-distortion
#' formula the pipeline uses at the quarter level (build_staff_task_features in
#' cluster.R), only regrouped by month. A fidelity assertion recomputes the
#' quarter-level s-index here and checks it against the pipeline's stored
#' s_index before trusting the month-level numbers.
#'
#' Runs on the FULL national sample (all counties, all quarters), matching
#' 02_stylized_facts.R -- the measurement-error question is about the s-index as
#' a descriptive object, not about the three-county estimation sample.
#'
#' Inputs:
#'   - mkdata/data/00_tasks_cosmo.rds       (cleaned transactions, with date)
#'   - mkdata/data/01_staff_task_full.rds   (pre-filter firm-quarter s_index;
#'                                            used only for the fidelity check)
#'
#' Outputs:
#'   - results/out/tables/02b_sindex_month_corr.tex   (pairwise-correlation table)
#'   - results/data/02b_sindex_measurement_error.rds  (wide month panel + the
#'                                                      four correlation matrices)
#' =============================================================================

# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------
library('data.table')
library('lubridate')
library('xtable')

# -----------------------------------------------------------------------------
# Configuration and shared helpers
# -----------------------------------------------------------------------------
source('config.R')
source('cluster.R')  ## spec_log, transform_cols, prefixed_cols (s-index helpers)

ensure_directory("results/data")
ensure_directory("results/out/tables")

tasks_path <- "mkdata/data/00_tasks_cosmo.rds"
stafffull_path <- "mkdata/data/01_staff_task_full.rds"
assert_required_files(c(tasks_path, stafffull_path))

# -----------------------------------------------------------------------------
# S-index for an arbitrary firm-time grouping
# -----------------------------------------------------------------------------
#' Firm-level task-specialization index computed over (location_id, time_col).
#'
#' Mirrors the s_index construction in build_staff_task_features() but leaves
#' the time unit as a parameter so the same formula can run at the quarter level
#' (for the fidelity check) or the month level (for the measurement-error test).
#' The index is the labor-weighted KL divergence of each worker's task mix from
#' the firm task mix:  s_index = sum_workers sum_tasks B_raw * log(B_raw / task_mix / e_frac).
firm_time_s_index <- function(dt, time_col) {
  keys <- c("location_id", time_col)

  ## worker x task duration within the firm-time cell
  staff_task <- dt[, .(duration = sum(duration)),
                   by = c("staff_id", "location_id", time_col, "clust")]
  staff_task[, emp_duration := sum(duration), by = c("staff_id", "location_id", time_col)]
  staff_task[, tot_duration := sum(duration), by = keys]

  ## one row per worker, one column of duration per task
  cast_lhs <- paste(c("staff_id", "location_id", time_col, "emp_duration", "tot_duration"),
                    collapse = " + ")
  staff_task <- dcast(staff_task, as.formula(paste(cast_lhs, "~ clust")),
                      value.var = "duration", fill = 0)
  clust_levels <- setdiff(names(staff_task),
                          c("staff_id", "location_id", time_col, "emp_duration", "tot_duration"))
  setnames(staff_task, clust_levels, paste0("duration_", clust_levels))

  ## firm task mix, worker labor share, and the per-worker-task mi contribution
  transform_cols(staff_task, "duration_", "firm_duration_",
                 function(d, s) sum(d[[paste0("duration_", s)]]), by = keys)
  transform_cols(staff_task, "firm_duration_", "task_mix_",
                 function(d, s) d[[paste0("firm_duration_", s)]] / d$tot_duration)
  staff_task[, e_frac := emp_duration / tot_duration]
  transform_cols(staff_task, "duration_", "B_raw_",
                 function(d, s) d[[paste0("duration_", s)]] / d$tot_duration)
  transform_cols(staff_task, "B_raw_", "mipart_",
                 function(d, s) d[[paste0("B_raw_", s)]] *
                   spec_log(d[[paste0("B_raw_", s)]] / d[[paste0("task_mix_", s)]] / d$e_frac))

  mi <- as.matrix(staff_task[, .SD, .SDcols = grep("^mipart_", names(staff_task))])
  stopifnot(all(is.finite(mi)))
  staff_task[, s_index_worker := rowSums(mi)]
  staff_task[, s_index := sum(s_index_worker), by = keys]

  unique(staff_task[, .SD, .SDcols = c("location_id", time_col, "s_index")])
}

# -----------------------------------------------------------------------------
# Load transactions
# -----------------------------------------------------------------------------
working <- data.table(readRDS(tasks_path))
stopifnot(all(c("staff_id", "location_id", "clust", "duration", "price",
                "date", "quarter_year") %in% names(working)))
stopifnot(nrow(working[is.na(price)]) == 0)
stopifnot(nrow(working[duration == 0]) == 0)
message("02b: loaded ", nrow(working), " transactions")

# -----------------------------------------------------------------------------
# Fidelity check: reproduce the pipeline's quarter-level s_index exactly
# -----------------------------------------------------------------------------
## If this fails, either the s-index formula here has drifted from
## build_staff_task_features() or 01_staff_task_full.rds is stale -- rerun
## 01_build_data.R.
s_quarter <- firm_time_s_index(working, "quarter_year")
s_quarter_pipeline <- unique(
  data.table(readRDS(stafffull_path))[, .(location_id, quarter_year, s_index)]
)
fidelity <- merge(s_quarter, s_quarter_pipeline,
                  by = c("location_id", "quarter_year"),
                  suffixes = c("_here", "_pipeline"))
stopifnot(nrow(fidelity) == nrow(s_quarter))
stopifnot(nrow(fidelity) == nrow(s_quarter_pipeline))
stopifnot(max(abs(fidelity$s_index_here - fidelity$s_index_pipeline)) < 1e-8)
message("02b: fidelity check passed on ", nrow(fidelity),
        " firm-quarters (max abs diff ",
        signif(max(abs(fidelity$s_index_here - fidelity$s_index_pipeline)), 3), ")")

# -----------------------------------------------------------------------------
# Month-level s-index and within-quarter month ranking
# -----------------------------------------------------------------------------
## month_num in the source file is not the calendar month, so derive it from date.
working[, month_year := year(date) * 100 + month(date)]

s_month <- firm_time_s_index(working, "month_year")

## attach each month to its quarter (each month falls in exactly one quarter)
month_quarter <- unique(working[, .(month_year, quarter_year)])
stopifnot(nrow(month_quarter) == uniqueN(month_quarter$month_year))
s_month <- merge(s_month, month_quarter, by = "month_year")

## rank the (up to three) months within a salon-quarter in calendar order
s_month[, month_rank := frank(month_year, ties.method = "dense"),
        by = c("location_id", "quarter_year")]
stopifnot(max(s_month$month_rank) == 3)
s_month[, month_label := paste0("sindex_", month_rank)]

firm_casted <- dcast(s_month, location_id + quarter_year ~ month_label,
                     value.var = "s_index")
firm_casted[, has_all := !is.na(sindex_1) & !is.na(sindex_2) & !is.na(sindex_3)]
message("02b: ", nrow(firm_casted), " salon-quarters, ",
        sum(firm_casted$has_all), " with all three months observed")

# -----------------------------------------------------------------------------
# Pairwise correlations across the three within-quarter months
# -----------------------------------------------------------------------------
month_cols <- c("sindex_1", "sindex_2", "sindex_3")
sindex_month_cor <- function(rows) cor(as.matrix(rows[, ..month_cols]))

cor_full        <- sindex_month_cor(firm_casted[has_all == TRUE])
cor_excl_2020   <- sindex_month_cor(firm_casted[has_all == TRUE & floor(quarter_year) != 2020])
cor_only_2020   <- sindex_month_cor(firm_casted[has_all == TRUE & floor(quarter_year) == 2020])
cor_only_2019   <- sindex_month_cor(firm_casted[has_all == TRUE & floor(quarter_year) == 2019])

n_full      <- nrow(firm_casted[has_all == TRUE])
n_excl_2020 <- nrow(firm_casted[has_all == TRUE & floor(quarter_year) != 2020])
n_only_2020 <- nrow(firm_casted[has_all == TRUE & floor(quarter_year) == 2020])
n_only_2019 <- nrow(firm_casted[has_all == TRUE & floor(quarter_year) == 2019])

report_cor <- function(label, m, n) {
  message(sprintf("02b: %-16s N=%5d  1&2=%.3f  1&3=%.3f  2&3=%.3f",
                  label, n, m[1, 2], m[1, 3], m[2, 3]))
}
report_cor("full sample", cor_full, n_full)
report_cor("excluding 2020", cor_excl_2020, n_excl_2020)
report_cor("only 2020", cor_only_2020, n_only_2020)
report_cor("only 2019", cor_only_2019, n_only_2019)

# -----------------------------------------------------------------------------
# LaTeX table: full sample vs. excluding 2020 (the two panels the paper reports)
# -----------------------------------------------------------------------------
pairwise_column <- function(m, n) {
  c(sprintf("%.3f", c(m[1, 2], m[1, 3], m[2, 3])),
    formatC(n, format = "d", big.mark = ","))
}
corr_table <- data.frame(
  Months = c("First and second", "First and third", "Second and third",
             "Salon-quarters ($N$)"),
  `Full Sample` = pairwise_column(cor_full, n_full),
  `Excluding 2020` = pairwise_column(cor_excl_2020, n_excl_2020),
  check.names = FALSE, stringsAsFactors = FALSE
)

corr_xtable <- xtable(
  corr_table,
  align = c("l", "l", "c", "c"),
  caption = paste("Pairwise correlation of the s-index measured separately in",
                  "each month within a salon-quarter. High correlations imply",
                  "the quarter-level s-index has little measurement error."),
  label = "tab:sindex_month_corr"
)
print(corr_xtable,
      file = "results/out/tables/02b_sindex_month_corr.tex",
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = function(x) x)
message("02b: wrote results/out/tables/02b_sindex_month_corr.tex")

# -----------------------------------------------------------------------------
# Persist the full set of results for the record
# -----------------------------------------------------------------------------
saveRDS(
  list(
    firm_casted = firm_casted,
    cor_full = cor_full, n_full = n_full,
    cor_excl_2020 = cor_excl_2020, n_excl_2020 = n_excl_2020,
    cor_only_2020 = cor_only_2020, n_only_2020 = n_only_2020,
    cor_only_2019 = cor_only_2019, n_only_2019 = n_only_2019
  ),
  "results/data/02b_sindex_measurement_error.rds"
)
message("02b: wrote results/data/02b_sindex_measurement_error.rds")
