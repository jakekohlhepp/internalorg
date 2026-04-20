## Deep dive: how duration differences affect s_index
library(data.table)

base <- "G:/My Drive/Working Documents/econ_phd/jmp"
orig <- data.table(readRDS(file.path(base, "analysis_final/data/01_00_staff_task_full.rds")))
refac <- data.table(readRDS(file.path(base, "refactor_estimation/mkdata/data/01_staff_task_full.rds")))

# Key by staff_num + location_id + quarter_year
orig[, rk := paste(staff_num, location_id, quarter_year, sep="|")]
refac[, rk := paste(staff_num, location_id, quarter_year, sep="|")]

shared <- intersect(orig$rk, refac$rk)
o <- orig[rk %in% shared]; setkey(o, rk)
r <- refac[rk %in% shared]; setkey(r, rk)

cat("=== s_index overview ===\n")
cat("s_index is firm-quarter level (same for all workers in a firm-quarter)\n")
cat("s_index = sum over workers of: sum_k [ B_raw_k * spec_log(B_raw_k / (task_mix_k * e_frac)) ]\n\n")

# s_index comparison at firm-quarter level
o_firms <- unique(o[, .(location_id, quarter_year, s_index)])
r_firms <- unique(r[, .(location_id, quarter_year, s_index)])
setnames(o_firms, "s_index", "s_orig")
setnames(r_firms, "s_index", "s_refac")
sf <- merge(o_firms, r_firms, by=c("location_id", "quarter_year"))
sf[, s_diff := s_refac - s_orig]
sf[, s_pct := 100 * s_diff / abs(s_orig)]

cat("=== s_index firm-quarter level ===\n")
cat("Total firm-quarters:", nrow(sf), "\n")
cat("Exact match (diff < 1e-10):", sum(abs(sf$s_diff) < 1e-10), "\n")
cat("Differ:", sum(abs(sf$s_diff) >= 1e-10), "\n\n")

cat("Distribution of s_index differences:\n")
cat("  Mean abs diff:", signif(mean(abs(sf$s_diff)), 4), "\n")
cat("  Median abs diff:", signif(median(abs(sf$s_diff)), 4), "\n")
cat("  Max abs diff:", signif(max(abs(sf$s_diff)), 4), "\n")
cat("  Mean s_index (orig):", signif(mean(sf$s_orig), 4), "\n")
cat("  Mean s_index (refac):", signif(mean(sf$s_refac), 4), "\n\n")

cat("Quantiles of % difference:\n")
print(round(quantile(sf$s_pct, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)), 3))

cat("\nCorrelation:", round(cor(sf$s_orig, sf$s_refac), 6), "\n")
cat("Rank correlation:", round(cor(sf$s_orig, sf$s_refac, method="spearman"), 6), "\n")

cat("\n=== Decomposing s_index change ===\n")
cat("s_index = sum_i sum_k mipart_ik\n")
cat("mipart_ik = B_raw_k * spec_log(B_raw_k / (task_mix_k * e_frac))\n")
cat("where B_raw_k = duration_k / tot_duration\n")
cat("      task_mix_k = firm_duration_k / tot_duration\n")
cat("      e_frac = emp_duration / tot_duration\n\n")

# Recompute mipart from raw ingredients to verify and decompose
spec_log <- function(x) ifelse(x == 0 | x == -Inf | is.nan(x), 0, log(x))

# For each shared row, compute the mipart components
for (k in 1:5) {
  dur_col <- paste0("duration_", k)
  braw <- paste0("B_raw_", k)
  tmix <- paste0("task_mix_", k)

  # mipart_k = B_raw_k * spec_log(B_raw_k / (task_mix_k * e_frac))
  o[, paste0("mi_", k) := get(braw) * spec_log(get(braw) / (get(tmix) * e_frac))]
  r[, paste0("mi_", k) := get(braw) * spec_log(get(braw) / (get(tmix) * e_frac))]
}

o[, mi_total := mi_1 + mi_2 + mi_3 + mi_4 + mi_5]
r[, mi_total := mi_1 + mi_2 + mi_3 + mi_4 + mi_5]

# Aggregate to firm-quarter
o_s <- o[, .(s_recomp = sum(mi_total), s_stored = s_index[1],
             mi1 = sum(mi_1), mi2 = sum(mi_2), mi3 = sum(mi_3), mi4 = sum(mi_4), mi5 = sum(mi_5)),
         by = .(location_id, quarter_year)]
r_s <- r[, .(s_recomp = sum(mi_total), s_stored = s_index[1],
             mi1 = sum(mi_1), mi2 = sum(mi_2), mi3 = sum(mi_3), mi4 = sum(mi_4), mi5 = sum(mi_5)),
         by = .(location_id, quarter_year)]

# Verify recomputation
cat("Orig recomp matches stored:", isTRUE(all.equal(o_s$s_recomp, o_s$s_stored, tol=1e-8)), "\n")
cat("Refac recomp matches stored:", isTRUE(all.equal(r_s$s_recomp, r_s$s_stored, tol=1e-8)), "\n\n")

# Merge and decompose by task
sm <- merge(o_s, r_s, by=c("location_id", "quarter_year"), suffixes=c(".o", ".r"))
sm[, s_diff := s_recomp.r - s_recomp.o]
for (k in 1:5) {
  sm[, paste0("mi", k, "_diff") := get(paste0("mi", k, ".r")) - get(paste0("mi", k, ".o"))]
}

cat("=== s_index decomposition by task ===\n")
cat("Which tasks contribute most to the s_index change?\n\n")
cat(sprintf("  %-10s %12s %12s %12s\n", "Task", "Mean diff", "Mean abs", "Max abs"))
for (k in 1:5) {
  col <- paste0("mi", k, "_diff")
  cat(sprintf("  Task %-5d %12.6f %12.6f %12.6f\n", k,
              mean(sm[[col]]), mean(abs(sm[[col]])), max(abs(sm[[col]]))))
}
cat(sprintf("  %-10s %12.6f %12.6f %12.6f\n", "TOTAL",
            mean(sm$s_diff), mean(abs(sm$s_diff)), max(abs(sm$s_diff))))

cat("\n=== What drives mipart changes? B_raw vs task_mix vs e_frac ===\n")
# For shared rows, compare each ingredient
cat(sprintf("  %-15s %12s %12s %12s\n", "Variable", "Mean abs diff", "Median", "Max"))
for (v in c("B_raw_1","B_raw_2","B_raw_3","B_raw_4","B_raw_5","task_mix_1","task_mix_2","task_mix_3","task_mix_4","task_mix_5","e_frac","tot_duration","emp_duration")) {
  d <- abs(o[[v]] - r[[v]])
  cat(sprintf("  %-15s %12.6f %12.6f %12.6f\n", v, mean(d, na.rm=TRUE), median(d, na.rm=TRUE), max(d, na.rm=TRUE)))
}

cat("\n=== Focus on 3 estimation counties ===\n")
# These are what matter for the final output
est_counties <- c("17031", "36061", "06037")
o_est <- o[county %in% est_counties]
r_est <- r[county %in% est_counties]

o_s_est <- o_est[, .(s_recomp = sum(mi_total)), by = .(location_id, quarter_year)]
r_s_est <- r_est[, .(s_recomp = sum(mi_total)), by = .(location_id, quarter_year)]
sm_est <- merge(o_s_est, r_s_est, by=c("location_id", "quarter_year"), suffixes=c(".o", ".r"))
sm_est[, s_diff := s_recomp.r - s_recomp.o]
sm_est[, s_pct := 100 * s_diff / abs(s_recomp.o)]

cat("Estimation sample firm-quarters:", nrow(sm_est), "\n")
cat("s_index exact match:", sum(abs(sm_est$s_diff) < 1e-10), "\n")
cat("s_index differ:", sum(abs(sm_est$s_diff) >= 1e-10), "\n")
cat("Mean abs diff:", signif(mean(abs(sm_est$s_diff)), 4), "\n")
cat("Max abs diff:", signif(max(abs(sm_est$s_diff)), 4), "\n")
cat("Correlation:", round(cor(sm_est$s_recomp.o, sm_est$s_recomp.r), 8), "\n")
cat("Rank corr:", round(cor(sm_est$s_recomp.o, sm_est$s_recomp.r, method="spearman"), 8), "\n")

cat("\nQuantiles of % diff in estimation counties:\n")
print(round(quantile(sm_est$s_pct, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 3))

rm(orig, refac, o, r); gc()
