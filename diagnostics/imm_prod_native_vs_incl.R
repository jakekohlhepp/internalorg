## Immigration labor productivity, BOTH ways: native-only (excludes the added
## immigrants, via native_share) vs immigrant-inclusive (native_share=1), with
## realloc & reorg side by side. Reuses 18's summarize_prod_panel +
## build_immigration_native_fraction verbatim (parse-eval). % change vs the
## Initial baseline (same baseline for both measures; baseline native_share=1).
suppressPackageStartupMessages({ library(data.table); library(stringr) })
source("config.R"); source("utils/counterfactuals_core.R")
n_worker_types <- CONFIG$n_worker_types; n_task_types <- CONFIG$n_task_types
e_field_names  <- counterfactual_e_field_names(CONFIG)

ex <- parse("18_counterfactual_summary.R")
want <- c("summarize_prod_panel", "build_immigration_native_fraction")
for (e in ex) if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]),"<-") &&
    is.symbol(e[[2]]) && as.character(e[[2]]) %in% want) eval(e, globalenv())
stopifnot(exists("summarize_prod_panel"), exists("build_immigration_native_fraction"))

nf  <- build_immigration_native_fraction()
imm <- as.data.table(readRDS(counterfactual_data_path("16_prod_immigration.rds")))
sn  <- summarize_prod_panel(imm, "imm", native_fraction_by_county = nf)$firm    # native-only
si  <- summarize_prod_panel(imm, "imm", native_fraction_by_county = NULL)$firm   # inclusive
bp  <- summarize_prod_panel(as.data.table(readRDS(counterfactual_data_path("13_prod_initial.rds"))), "init")$firm

disp <- c(`6037` = "Los Angeles", `36061` = "New York", `17031` = "Cook")
counties <- c("6037", "36061", "17031")
getp <- function(tab, c, st) { v <- tab[as.character(county)==c & sol_type==st]$labor_prod; if (length(v)) v[1] else NA_real_ }
res <- rbindlist(lapply(counties, function(c) {
  b <- bp[as.character(county)==c]$labor_prod[1]
  data.table(County = disp[[c]],
             nat_realloc = (getp(sn,c,"realloc") - b) / b,
             nat_reorg   = (getp(sn,c,"reorg")   - b) / b,
             inc_realloc = (getp(si,c,"realloc") - b) / b,
             inc_reorg   = (getp(si,c,"reorg")   - b) / b)
}))

cat("=== Immigration labor-productivity change vs baseline (fractions) ===\n")
cat(sprintf("%-12s | %-18s | %-18s\n", "", "Native-only (excl)", "Immigrant-inclusive"))
cat(sprintf("%-12s | %9s %8s | %9s %8s\n", "County", "realloc", "reorg", "realloc", "reorg"))
for (i in seq_len(nrow(res))) with(res[i], cat(sprintf(
  "%-12s | %9.4f %8.4f | %9.4f %8.4f\n", County, nat_realloc, nat_reorg, inc_realloc, inc_reorg)))
saveRDS(res, "imm_prod_native_vs_incl.rds")
cat("\nSaved: imm_prod_native_vs_incl.rds\n")
