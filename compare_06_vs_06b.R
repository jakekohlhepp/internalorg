## Compare unconstrained (06) vs workers-as-rows-monotone (06b) point estimates.
## Renders side-by-side tables for the three parameter groups that 08 displays:
##   (1) price sensitivity + reference org cost (one row per county)
##   (2) wage premiums per worker type (one row per (county, worker) pair)
##   (3) skill matrices in QP-recovered rank order (one block per county)
## Also reports per-county Frobenius and L_inf moves on each block.

suppressPackageStartupMessages({
  library("data.table")
  library("stringr")
})

source("config.R")

unc <- as.data.table(readRDS(file.path("results", "data", "06_parameters.rds")))
con <- as.data.table(readRDS(file.path("results", "data", "06b_parameters_monotone.rds")))
perms <- readRDS(file.path("results", "data", "06b_perms.rds"))$perms_by_county

stopifnot(identical(sort(unc$parm_name), sort(con$parm_name)))

merged <- merge(
  unc[, .(parm_name, demand, unc = coefficients)],
  con[, .(parm_name, con = coefficients)],
  by = "parm_name", sort = FALSE
)
merged[, county := str_extract(parm_name, "(?<=factor\\(county\\))[0-9]+")]
merged[, quarter_year := str_extract(parm_name, "(?<=factor\\(quarter_year\\))[0-9]+\\.?[0-9]*")]
merged[, other := {
  o <- parm_name
  o <- str_replace(o, "factor\\(county\\)[0-9]+", "")
  o <- str_replace(o, "factor\\(quarter_year\\)[0-9]+\\.?[0-9]*", "")
  o <- str_replace_all(o, "^:|:$", "")
  o
}]
merged[, diff := con - unc]
merged[, abs_diff := abs(diff)]
merged[, rel_diff := ifelse(abs(unc) > 1e-10, diff / unc, NA_real_)]

county_label <- c(`17031` = "Cook (Chicago)",
                  `36061` = "New York (Manhattan)",
                  `6037`  = "Los Angeles")

cat("\n========================================================================\n")
cat("  TABLE 1: Price sensitivity and reference org cost (per county)\n")
cat("           rho display follows 08's convention: sign-flipped on price.\n")
cat("========================================================================\n")
tab1 <- merged[other %in% c("cust_price", "org_cost") & is.na(quarter_year)]
tab1[, c("unc_disp", "con_disp") := list(
  ifelse(other == "cust_price", -unc, unc),
  ifelse(other == "cust_price", -con, con)
)]
tab1[, name := ifelse(other == "cust_price", "Price Sensitivity",
                      "Reference Org. Cost")]
print(tab1[, .(County = county_label[county], Parameter = name,
               unconstrained = round(unc_disp, 4),
               constrained   = round(con_disp, 4),
               diff = round(con_disp - unc_disp, 4))][order(County, Parameter)])

cat("\n========================================================================\n")
cat("  TABLE 2: Wage premiums per worker type (E_raw_<i>:avg_labor)\n")
cat("           Worker type 1 is normalized to 0 (preamble.R:36).\n")
cat("========================================================================\n")
tab2 <- merged[grepl("avg_labor:E_raw_", other) & is.na(quarter_year)]
tab2[, worker := as.integer(str_extract(other, "(?<=E_raw_)[0-9]+"))]
print(tab2[, .(County = county_label[county], `Worker type` = worker,
               unconstrained = round(unc, 3),
               constrained   = round(con, 3),
               diff = round(diff, 3))][order(County, `Worker type`)])

cat("\n========================================================================\n")
cat("  TABLE 3: Skill matrices (B_raw[task, worker]) in QP rank order\n")
cat("           Columns reordered: rank 1 = least productive, rank 5 = most.\n")
cat("========================================================================\n")
n_t <- CONFIG$n_task_types
n_w <- CONFIG$n_worker_types
extract_B <- function(dt, cnty, value_col) {
  out <- matrix(NA_real_, n_t, n_w)
  rows <- dt[county == cnty & grepl("avg_labor:B_raw_", other)]
  for (i in seq_len(nrow(rows))) {
    m <- str_match(rows$other[i], "B_raw_([0-9]+)_([0-9]+)$")
    out[as.integer(m[1, 2]), as.integer(m[1, 3])] <- rows[[value_col]][i]
  }
  out
}
for (cnty in CONFIG$counties) {
  cnty_str <- as.character(cnty)
  pi_c <- as.integer(perms[[cnty_str]])
  M_unc <- extract_B(merged, cnty_str, "unc")
  M_con <- extract_B(merged, cnty_str, "con")
  M_unc_re <- M_unc[, pi_c, drop = FALSE]
  M_con_re <- M_con[, pi_c, drop = FALSE]
  cat("\n--- ", cnty_str, " — ", county_label[cnty_str],
      "  (rank order: ", paste(pi_c, collapse = " -> "), ") ---\n", sep = "")
  combined <- matrix(sprintf("%8.2f / %8.2f", M_unc_re, M_con_re),
                     nrow = n_t, ncol = n_w)
  rownames(combined) <- paste0("task ", seq_len(n_t))
  colnames(combined) <- paste0("rank ", seq_len(n_w),
                                " (w", pi_c, ")")
  cat("Each cell: unconstrained / constrained\n")
  print(noquote(combined))

  fro_unc <- sqrt(sum(M_unc^2))
  fro_con <- sqrt(sum(M_con^2))
  fro_diff <- sqrt(sum((M_unc - M_con)^2))
  linf_diff <- max(abs(M_unc - M_con))
  cat(sprintf(
    "  ||B_unc||_F = %.3f, ||B_con||_F = %.3f, ||diff||_F = %.3f (relative %.3f), L_inf diff = %.3f\n",
    fro_unc, fro_con, fro_diff, fro_diff / fro_unc, linf_diff
  ))
}

cat("\n========================================================================\n")
cat("  TABLE 4: Time-varying parameters (quarter-year level shifts)\n")
cat("           Demand level (FE), Cost level (FE), Material cost (task mix)\n")
cat("========================================================================\n")
tab4 <- merged[!is.na(quarter_year)]
tab4[, group := fcase(
  other == "" & demand == TRUE, "Demand Level",
  other == "" & demand == FALSE, "Cost Level",
  other == "avg_labor", "Wage Level",
  grepl("task_mix", other), "Material Cost"
)]
tab4 <- tab4[!is.na(group)]
movement <- tab4[, .(
  n = .N,
  mean_unc = mean(unc), mean_con = mean(con),
  mean_diff = mean(diff),
  rmse_diff = sqrt(mean(diff^2)),
  max_abs_diff = max(abs(diff))
), keyby = .(County = county_label[county], group)]
print(movement[, .(County, group, n,
                   mean_unc = round(mean_unc, 3),
                   mean_con = round(mean_con, 3),
                   mean_diff = round(mean_diff, 3),
                   rmse_diff = round(rmse_diff, 3),
                   max_abs_diff = round(max_abs_diff, 3))])

cat("\n========================================================================\n")
cat("  TABLE 5: Headline summary (Frobenius moves per parameter block)\n")
cat("========================================================================\n")
classify_block <- function(other, demand, qy) {
  fcase(
    !is.na(qy),                                    "time FE / material",
    other %in% c("cust_price", "org_cost"),        "price / org cost",
    grepl("avg_labor:E_raw_", other),              "wage premiums",
    grepl("avg_labor:B_raw_", other),              "skill matrix",
    default = "other"
  )
}
merged[, block := classify_block(other, demand, quarter_year)]
overall <- merged[, .(
  n         = .N,
  Frob_unc  = round(sqrt(sum(unc^2)),  3),
  Frob_diff = round(sqrt(sum(diff^2)), 3)
), keyby = .(County = county_label[county], Block = block)]
overall[, Relative := round(ifelse(Frob_unc > 0, Frob_diff / Frob_unc, NA_real_), 3)]
print(overall[Block != "other"])

cat("\nDone.\n")
