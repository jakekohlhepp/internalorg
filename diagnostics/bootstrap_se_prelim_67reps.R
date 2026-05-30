## PRELIMINARY bootstrap SEs from the COMPLETED reps of the in-progress
## post-l1l4 run (results/data/bootstrap_reps/). Read-only: writes a non-canonical
## CSV and does NOT touch 07_bootstrap.rds or 07_param_bootstrap_se.csv.
##
## Replicates 08_display_estimates.R's SE logic: SE = sd across status=="ok"
## reps, per parameter. Reports status breakdown + structural-parameter SEs.
##
## Output: results/data/07_param_bootstrap_se_PRELIM_<N>reps.csv
suppressPackageStartupMessages({ library(data.table) })
source("config.R")

reps_dir <- file.path("results", "data", "bootstrap_reps")
files <- list.files(reps_dir, pattern = "^boot_res_.*\\.rds$", full.names = TRUE)
cat("Rep files found in ", reps_dir, ": ", length(files), "\n", sep = "")

combined <- rbindlist(lapply(files, readRDS), fill = TRUE)
cat("Combined rows: ", nrow(combined), "\n", sep = "")

if ("status" %in% names(combined)) {
  cat("\n=== status breakdown ===\n")
  print(combined[, .N, by = status][order(-N)])
}
## UNFILTERED: use ALL reps (ok + soft-revert wage_nonconverged), which carry
## full parameter tables. Only status=="error" reps lack params; drop those.
ok <- if ("status" %in% names(combined)) combined[is.na(status) | status != "error"] else combined
n_ok <- nrow(ok)
cat("\nreps used for SE (all non-error): ", n_ok, "\n", sep = "")
if (n_ok < 2L) stop("Need >=2 reps.")

## point estimates
pt <- as.data.table(readRDS(file.path("results", "data", "06_parameters.rds")))
point <- setNames(pt$coefficients, pt$parm_name)

## parameter columns present in the reps (exclude bookkeeping cols)
nonparm <- c("iteration", "wage_convergence", "price_convergence", "status", "error_message")
parameter_cols <- setdiff(names(ok), nonparm)
parameter_cols <- intersect(parameter_cols, names(point))

## coerce to numeric (t() in bootstrap_result_row can make character)
for (c in parameter_cols) if (!is.numeric(ok[[c]])) ok[, (c) := as.numeric(get(c))]

se_tab <- rbindlist(lapply(parameter_cols, function(c) {
  v <- ok[[c]]; vf <- v[is.finite(v)]
  data.table(parm_name = c, point = as.numeric(point[[c]]),
             boot_mean = mean(vf), boot_sd = stats::sd(vf), n_finite = length(vf))
}))

out_path <- file.path("results", "data", sprintf("07_param_bootstrap_se_PRELIM_%dreps_unfiltered.csv", n_ok))
fwrite(se_tab, out_path)
cat("Wrote ", out_path, " (", nrow(se_tab), " params)\n", sep = "")

## ---- focused display of structural parameters ----
show <- function(title, pat) {
  sub <- se_tab[grepl(pat, parm_name)]
  if (nrow(sub) == 0L) return(invisible())
  cat("\n=== ", title, " (", nrow(sub), " params) ===\n", sep = "")
  print(sub[, .(parm_name = sub$parm_name, point = round(point,4),
                boot_sd = round(boot_sd,4), n_finite)], nrows = 200)
}
cat("\n############ PRELIMINARY bootstrap SEs (n_ok=", n_ok, " reps) ############\n", sep = "")
show("Price coefficient rho (:cust_price)", ":cust_price$")
## skill matrix B and wage diffs E: summarize per county (too many to list all)
for (cnty in CONFIG$counties) {
  for (lab in c("B","E")) {
    sub <- se_tab[grepl(paste0(cnty, ":avg_labor:", lab), parm_name)]
    if (nrow(sub)==0L) next
    cat(sprintf("County %s  avg_labor:%s  (%d params): boot_sd median=%.4g  range=[%.4g, %.4g]  min n_finite=%d\n",
                cnty, lab, nrow(sub), median(sub$boot_sd, na.rm=TRUE),
                min(sub$boot_sd,na.rm=TRUE), max(sub$boot_sd,na.rm=TRUE), min(sub$n_finite)))
  }
}
cat("\nAll structural skill/wage SEs and the full ", nrow(se_tab),
    "-parameter table are in:\n  ", out_path, "\n", sep="")
cat("\nNOTE: PRELIMINARY - ", n_ok, " of 1100 target reps. SEs will tighten/shift as more reps land.\n", sep="")
