## Diagnostic: report v4 immigration productivity impacts and make explicit how
## immigrants are counted. Replicates summarize_prod_panel() and
## build_immigration_native_fraction() from 18_counterfactual_summary.R, then
## computes immigration-vs-initial impacts BOTH with the native-labor-only
## adjustment (as the manuscript table does) and without it (total-labor basis),
## so the effect of the immigrant-counting choice is visible. Read-only:
## prints to stdout, writes nothing to results/.
if (file.exists("renv/activate.R")) source("renv/activate.R")
library(data.table)
source("config.R")
source("utils/counterfactuals_core.R")

n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
e_field_names  <- counterfactual_e_field_names(CONFIG)

## ---- verbatim from 18_counterfactual_summary.R (gsub for str_replace) ----
summarize_prod_panel <- function(prod_dt, version_label, native_fraction_by_county = NULL) {
  prod_local <- copy(prod_dt)
  prod_local <- prod_local[, !duplicated(names(prod_local)), with = FALSE]
  if (!is.null(native_fraction_by_county)) {
    nf_wide <- dcast(native_fraction_by_county, county ~ worker_type, value.var = "native_fraction")
    setnames(nf_wide, old = as.character(seq_len(n_worker_types)), new = paste0("nf_", seq_len(n_worker_types)))
    prod_local <- merge(prod_local, nf_wide, by = "county", all.x = TRUE)
    for (k in seq_len(n_worker_types)) {
      nf_col <- paste0("nf_", k); prod_local[is.na(get(nf_col)), (nf_col) := 1]
    }
    prod_local[, native_share := Reduce(`+`, lapply(seq_len(n_worker_types),
      function(k) get(paste0("E_", k)) * get(paste0("nf_", k))))]
  } else {
    prod_local[, native_share := 1]
  }
  rev_per_labor_summary <- prod_local[, .(
    rev_per_labor = sum(newprice  * CSPOP * new_share * weight * native_share) /
                    sum(avg_labor * CSPOP * new_share * weight * native_share)
  ), by = c("county", "sol_type")]
  panel <- melt(prod_local,
    id.vars = c("location_id", "avg_labor", "new_share", "CSPOP", "county",
                "s_index", "sol_type", "weight", "native_share", e_field_names),
    measure.vars = patterns("^B_[0-9]"))
  panel[, multiplier := avg_labor * CSPOP * new_share * weight]
  panel[, native_multiplier := multiplier * native_share]
  panel[, worker_type := gsub("^B_[0-9]_", "", variable)]
  panel[, type_E := NA_real_]
  for (idx in seq_len(n_worker_types)) panel[worker_type == as.character(idx), type_E := get(paste0("E_", idx))]
  firm_summary <- panel[, .(tot_prod = sum(value)),
    by = c("s_index", "multiplier", "native_multiplier", "location_id", "county", "sol_type")]
  firm_summary <- firm_summary[, .(
    s_avg      = weighted.mean(s_index, multiplier),
    labor_prod = sum(tot_prod * native_multiplier) / sum(native_multiplier)
  ), by = c("county", "sol_type")]
  firm_summary <- merge(firm_summary, rev_per_labor_summary, by = c("county", "sol_type"))
  firm_summary[, version := version_label]
  list(firm = firm_summary)
}

build_immigration_native_fraction <- function() {
  tl <- as.data.table(readRDS(counterfactual_data_path("13_total_labor.rds")))
  tot_cols <- counterfactual_tot_labor_field_names(CONFIG)
  fq <- get_counterfactual_focus_quarter(); stopifnot(length(fq) == 1L)
  tl <- tl[quarter_year == fq, ]
  county_target <- list("6037" = 1L, "36061" = 2L, "17031" = 4L)
  rows <- list()
  for (cnty in names(county_target)) {
    row <- tl[county == cnty, ]; stopifnot(nrow(row) == 1L)
    base <- as.numeric(row[1, .SD, .SDcols = tot_cols])
    k_star <- county_target[[cnty]]
    delta_c <- 0.05 * sum(base)
    nf <- rep(1, length(base)); nf[k_star] <- base[k_star] / (base[k_star] + delta_c)
    rows[[cnty]] <- data.table(county = cnty, worker_type = as.character(seq_along(nf)),
                                native_fraction = nf, target = (seq_along(nf) == k_star),
                                base_k = base, delta_c = delta_c)
  }
  rbindlist(rows)
}

cdisp <- function(c) ifelse(c=="17031","Cook",ifelse(c=="36061","New York","Los Angeles"))

## ---- load panels ----
initial <- as.data.table(readRDS("results/data/counterfactuals/13_prod_initial.rds"))
immig   <- as.data.table(readRDS("results/data/counterfactuals/16_prod_immigration.rds"))
nf      <- build_immigration_native_fraction()

cat("################ HOW IMMIGRANTS ARE COUNTED ################\n")
cat("Shock: delta_c = 0.05 * sum_k(total_labor_c[k]), added entirely to the\n")
cat("lowest-wage worker type (LA->1, NYC->2, Cook->4). Units = effective labor.\n\n")
tgt <- nf[target == TRUE]
tgt[, immigrant_share_of_type := delta_c / (base_k + delta_c)]
tgt[, prop_shock_to_type := delta_c / base_k]
tgt[, county_name := cdisp(county)]
print(tgt[, .(county_name, target_type = worker_type, base_k = round(base_k,2),
              immigrants_delta = round(delta_c,2),
              native_fraction = round(native_fraction,4),
              immigrant_share_of_type = round(immigrant_share_of_type,4),
              prop_shock_to_type = round(prop_shock_to_type,4))], row.names = FALSE)

## ---- baseline firm summary (native_share = 1) ----
base_firm <- summarize_prod_panel(initial, "Initial")$firm
setnames(base_firm, c("s_avg","labor_prod","rev_per_labor"), c("init_s","init_prod","init_rev"))
base_firm <- base_firm[, .(county, init_s, init_prod, init_rev)]

impact_table <- function(cf_firm, tag) {
  m <- merge(cf_firm, base_firm, by = "county")
  m[, pct_sindex        := (s_avg - init_s) / init_s]
  m[, pct_prod          := (labor_prod - init_prod) / init_prod]
  m[, pct_rev_per_labor := (rev_per_labor - init_rev) / init_rev]
  m[, county_name := cdisp(county)]
  setorder(m, county_name, sol_type)
  cat("\n################", tag, "################\n")
  print(m[, .(county_name, sol_type,
              pct_sindex        = round(pct_sindex,4),
              pct_prod          = round(pct_prod,4),
              pct_rev_per_labor = round(pct_rev_per_labor,4))], row.names = FALSE)
  invisible(m)
}

## (A) Manuscript basis: native-labor-only productivity denominators
cf_native <- summarize_prod_panel(immig, "Immigration", native_fraction_by_county = nf)$firm
impact_table(cf_native, "IMMIGRATION PROD IMPACTS -- NATIVE-ONLY (manuscript table 18_tot)")

## (B) Total-labor basis: immigrants INCLUDED in denominators (native_share=1)
cf_total <- summarize_prod_panel(immig, "Immigration", native_fraction_by_county = NULL)$firm
impact_table(cf_total, "IMMIGRATION PROD IMPACTS -- TOTAL-LABOR (immigrants included)")

cat("\nNote: pct_prod = firm-productivity (sum of B) weighted by [native|total] labor,\n")
cat("relative to the no-immigrant baseline. (A) is what the manuscript reports.\n")
