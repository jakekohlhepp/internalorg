suppressPackageStartupMessages({ library(data.table) })
dir.create("diagnostics/out", recursive = TRUE, showWarnings = FALSE)
old_path <- "diagnostics/out/06_parameters_HEAD_old.rds"
if (!file.exists(old_path)) {
  status <- system(sprintf("git show HEAD:results/data/06_parameters.rds > %s", shQuote(old_path)))
  if (!identical(status, 0L)) stop("git show failed")
}
old <- as.data.table(readRDS(old_path))[, version := "old_HEAD"]
new <- as.data.table(readRDS("results/data/06_parameters.rds"))[, version := "new_worktree"]
DT <- rbindlist(list(old, new), use.names = TRUE)
W <- dcast(DT, demand + parm_name ~ version, value.var = "coefficients")
W[, diff := new_worktree - old_HEAD]
W[, pct_diff := fifelse(abs(old_HEAD) > 1e-12, diff / abs(old_HEAD), NA_real_)]

la <- W[grepl("6037|06037", parm_name)]
la[, block := fifelse(demand & grepl(":cust_price$", parm_name), "demand_price_rho",
              fifelse(demand & grepl(":factor\\(quarter_year\\)", parm_name), "demand_quarter_FE",
              fifelse(demand & grepl("B_raw_", parm_name), "demand_skill_B",
              fifelse(!demand & grepl(":avg_labor:E_raw_", parm_name), "supply_wage_E",
              fifelse(!demand & grepl("^avg_labor:factor\\(county\\)6037:factor\\(quarter_year\\)", parm_name), "supply_wage_base_by_qtr",
              fifelse(!demand & grepl("factor\\(county\\)6037:factor\\(quarter_year\\)", parm_name), "supply_county_qtr_FE",
              fifelse(!demand & grepl("factor\\(quarter_year\\).*task_mix", parm_name), "supply_taskmix_time", "other")))))))]
setorder(la, demand, block, parm_name)
fwrite(la, "diagnostics/out/la_06_old_new_all_params.csv")

summ <- la[, .(
  n = .N,
  mean_abs_old = mean(abs(old_HEAD)),
  mean_abs_new = mean(abs(new_worktree)),
  mean_abs_diff = mean(abs(diff)),
  max_abs_diff = max(abs(diff)),
  rmse = sqrt(mean(diff^2)),
  corr = if (.N > 1 && sd(old_HEAD) > 0 && sd(new_worktree) > 0) cor(old_HEAD, new_worktree) else NA_real_
), by = block][order(block)]
fwrite(summ, "diagnostics/out/la_06_old_new_summary_by_block.csv")

largest <- la[order(-abs(diff))][1:min(.N, 30), .(block, demand, parm_name, old_HEAD, new_worktree, diff, pct_diff)]
fwrite(largest, "diagnostics/out/la_06_old_new_largest_moves.csv")

## Extract interpretable matrices/vectors for LA.
getv <- function(D, pattern) D[grepl(pattern, parm_name), coefficients]
Dold <- as.data.table(readRDS(old_path)); Dnew <- as.data.table(readRDS("results/data/06_parameters.rds"))

## Demand price coefficient (rho)
rho <- data.table(item = "cust_price", old = getv(Dold, "factor\\(county\\)6037:cust_price$"), new = getv(Dnew, "factor\\(county\\)6037:cust_price$"))
rho[, `:=`(diff = new - old, pct_diff = diff / abs(old))]
fwrite(rho, "diagnostics/out/la_06_old_new_price_coeff.csv")

## Demand skill matrix B_raw_task_worker: rows worker, cols task.
skill_rows <- function(D) {
  x <- D[demand == TRUE & grepl("factor\\(county\\)6037:avg_labor:B_raw_", parm_name)]
  x[, tmp := sub(".*B_raw_", "", parm_name)]
  x[, task := as.integer(sub("_.*", "", tmp))]
  x[, worker := as.integer(sub(".*_", "", tmp))]
  x[, .(worker, task, value = coefficients)]
}
so <- skill_rows(Dold); sn <- skill_rows(Dnew)
sk <- merge(so, sn, by = c("worker", "task"), suffixes = c("_old", "_new"))
sk[, `:=`(diff = value_new - value_old, pct_diff = fifelse(abs(value_old)>1e-12, diff/abs(value_old), NA_real_))]
setorder(sk, worker, task)
fwrite(sk, "diagnostics/out/la_06_old_new_skill_matrix_long.csv")

## Wage/base labor coefficients by quarter and worker-type E coefficients.
wbase_rows <- function(D) {
  x <- D[demand == FALSE & grepl("^avg_labor:factor\\(county\\)6037:factor\\(quarter_year\\)", parm_name)]
  x[, quarter_year := sub(".*factor\\(quarter_year\\)", "", parm_name)]
  x[, .(quarter_year, value = coefficients)]
}
wo <- wbase_rows(Dold); wn <- wbase_rows(Dnew)
wb <- merge(wo, wn, by = "quarter_year", suffixes = c("_old", "_new"))
wb[, `:=`(diff = value_new - value_old, pct_diff = fifelse(abs(value_old)>1e-12, diff/abs(value_old), NA_real_))]
setorder(wb, quarter_year)
fwrite(wb, "diagnostics/out/la_06_old_new_base_wage_by_quarter.csv")

we_rows <- function(D) {
  x <- D[demand == FALSE & grepl("factor\\(county\\)6037:avg_labor:E_raw_", parm_name)]
  x[, worker := as.integer(sub(".*E_raw_", "", parm_name))]
  x[, .(worker, value = coefficients)]
}
weo <- we_rows(Dold); wen <- we_rows(Dnew)
we <- merge(weo, wen, by = "worker", suffixes = c("_old", "_new"))
we[, `:=`(diff = value_new - value_old, pct_diff = fifelse(abs(value_old)>1e-12, diff/abs(value_old), NA_real_))]
setorder(we, worker)
fwrite(we, "diagnostics/out/la_06_old_new_wage_E_coeffs.csv")

## Pretty text report
out <- "diagnostics/out/la_06_old_new_report.txt"
sink(out)
cat("LA County (6037): old committed HEAD vs new working-tree results/data/06_parameters.rds\n")
cat("Generated: ", format(Sys.time()), "\n\n", sep = "")
cat("Price coefficient:\n"); print(rho)
cat("\nSummary by block:\n"); print(summ)
cat("\nWage E coefficients (worker 2-5 premiums/interactions):\n"); print(we)
cat("\nBase wage-by-quarter coefficients:\n"); print(wb)
cat("\nSkill matrix long (first 25 rows):\n"); print(sk)
cat("\nLargest 30 absolute parameter moves:\n"); print(largest)
sink()
cat("wrote ", out, "\n", sep = "")
