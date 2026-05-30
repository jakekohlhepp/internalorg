## One-shot diagnostic: list non-converged county-counterfactual cells
## with per-worker-type labor-market clearing residuals.
suppressPackageStartupMessages({
  library(data.table)
})

source("config.R")

files <- list(
  diffusion   = "results/data/counterfactuals/14_wages_diffusion.rds",
  salestax    = "results/data/counterfactuals/15_wages_salestax.rds",
  immigration = "results/data/counterfactuals/16_wages_immigration.rds",
  merger      = "results/data/counterfactuals/17_wages_merger.rds"
)

K <- CONFIG$n_worker_types
resid_cols <- paste0("resid_", seq_len(K))
wage_cols  <- paste0("w", seq_len(K))

key_cols <- c("county", "quarter_year", "sol_type")
meta_cols <- c("converged", "fval", "target_tol", "method", "termcd", "message")

focus_qys <- CONFIG$counterfactual_focus_quarters

print_block <- function(label, dt) {
  cat("================================================================\n")
  cat("Scenario:", label, "\n")
  cat("File:    ", files[[label]], "\n")
  dt_focus <- dt[quarter_year %in% focus_qys]
  cat("Focus quarter(s):", paste(focus_qys, collapse = ", "), "\n")
  cat("Focus-quarter rows:", nrow(dt_focus),
      " attempted (target_tol set):", sum(!is.na(dt_focus$target_tol)),
      " converged:", sum(dt_focus$converged, na.rm = TRUE),
      " non-converged among attempted:",
      sum(dt_focus$converged == FALSE & !is.na(dt_focus$target_tol)),
      "\n")
  cat("Target tol (unique on attempted):",
      paste(sort(unique(dt_focus$target_tol)), collapse = ", "), "\n")

  bad <- dt_focus[converged == FALSE & !is.na(target_tol)]
  if (nrow(bad) == 0L) {
    cat("All attempted focus-quarter cells converged.\n\n")
    return(invisible())
  }

  bad[, max_abs_resid := pmax(
    abs(resid_1), abs(resid_2), abs(resid_3), abs(resid_4), abs(resid_5)
  )]
  bad <- bad[order(county, quarter_year, sol_type)]

  cat("Non-converged cells (", nrow(bad), "):\n", sep = "")
  for (i in seq_len(nrow(bad))) {
    r <- bad[i]
    cat(sprintf(
      "  county=%s qy=%s sol=%-7s | converged=%s | fval=%-10.4g target_tol=%-8.4g | max|resid|=%-10.4g\n",
      r$county, as.character(r$quarter_year), r$sol_type,
      as.character(r$converged), r$fval, r$target_tol, r$max_abs_resid
    ))
    cat("    per-worker-type clearing residuals:\n")
    for (k in seq_len(K)) {
      cat(sprintf(
        "      worker %d: resid = %-12.6g    wage = %-12.6g\n",
        k, r[[resid_cols[k]]], r[[wage_cols[k]]]
      ))
    }
    cat(sprintf("    method=%s termcd=%s message=%s\n",
                as.character(r$method), as.character(r$termcd),
                as.character(r$message)))
  }
  cat("\n")
}

for (lab in names(files)) {
  path <- files[[lab]]
  if (!file.exists(path)) {
    cat("Skipping (missing):", path, "\n\n")
    next
  }
  dt <- as.data.table(readRDS(path))
  print_block(lab, dt)
}
