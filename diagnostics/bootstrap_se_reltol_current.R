## SEs from the currently-finished reps of the reltol-gate bootstrap run
## (results/data/bootstrap_reps/). Read-only; writes a non-canonical CSV.
## Reports the reltol-gated status breakdown, then SEs both filtered
## (status=="ok", as 08_ uses) and unfiltered (all non-error reps).
suppressPackageStartupMessages({ library(data.table) })
source("config.R")

reps_dir <- file.path("results","data","bootstrap_reps")
files <- list.files(reps_dir, pattern="^boot_res_.*\\.rds$", full.names=TRUE)
combined <- rbindlist(lapply(files, readRDS), fill=TRUE)
cat("Reps on disk: ", nrow(combined), "  (array still producing more)\n", sep="")
cat("\n=== status breakdown (reltol gate) ===\n"); print(combined[, .N, by=status][order(-N)])

pt <- as.data.table(readRDS(file.path("results","data","06_parameters.rds")))
point <- setNames(pt$coefficients, pt$parm_name)
nonparm <- c("iteration","wage_convergence","price_convergence","status","error_message")
pcols <- intersect(setdiff(names(combined), nonparm), names(point))

se_from <- function(dt) {
  for (c in pcols) if (!is.numeric(dt[[c]])) dt[, (c) := as.numeric(get(c))]
  rbindlist(lapply(pcols, function(c){ v <- dt[[c]]; vf <- v[is.finite(v)]
    data.table(parm_name=c, point=as.numeric(point[[c]]), boot_sd=stats::sd(vf), n=length(vf)) }))
}

ok <- combined[status=="ok"]
allr <- combined[is.na(status) | status!="error"]
cat(sprintf("\nstatus==ok: %d reps   |   all non-error: %d reps\n", nrow(ok), nrow(allr)))

se_ok  <- se_from(copy(ok))
se_all <- se_from(copy(allr))
fwrite(se_ok,  sprintf("results/data/07_param_bootstrap_se_RELTOL_ok_%dreps.csv", nrow(ok)))
fwrite(se_all, sprintf("results/data/07_param_bootstrap_se_RELTOL_all_%dreps.csv", nrow(allr)))

show <- function(tag, se) {
  cat("\n#################### ", tag, " ####################\n", sep="")
  rho <- se[grepl(":cust_price$", parm_name)]
  cat("price coef rho:\n"); print(rho[, .(parm_name, point=round(point,4), boot_sd=round(boot_sd,4), n)])
  for (cn in CONFIG$counties) for (lab in c("B","E")) {
    s <- se[grepl(paste0(cn,":avg_labor:",lab), parm_name)]
    if (nrow(s)) cat(sprintf("  %s avg_labor:%s (%d): median SE=%.4g  range[%.4g,%.4g]\n",
        cn, lab, nrow(s), median(s$boot_sd,na.rm=TRUE), min(s$boot_sd,na.rm=TRUE), max(s$boot_sd,na.rm=TRUE)))
  }
}
show(sprintf("FILTERED status==ok (%d reps)", nrow(ok)), se_ok)
show(sprintf("UNFILTERED all non-error (%d reps)", nrow(allr)), se_all)
cat("\nSaved: results/data/07_param_bootstrap_se_RELTOL_ok_", nrow(ok), "reps.csv and _all_", nrow(allr), "reps.csv\n", sep="")
