## ===========================================================================
## build_counterfactual_warm_starts.R
##
## Read each counterfactual's saved wage table (`{N}_wages_{label}.rds`)
## and write a scenario-specific warm-start file
## (`{N}_warm_start_wages_{label}.rds`) keyed by (county, quarter_year,
## sol_type) with wage columns w1..w{n_worker_types}.
##
## The new 14/15/16/17 scripts auto-load these via
## read_counterfactual_warm_start_table() and use them as primary starts in
## each (county × sol_type) cell, with the baseline initial_wages kept as a
## backup additional_starts entry.
## ===========================================================================

suppressPackageStartupMessages(library(data.table))
source("config.R")
source("utils/counterfactuals_core.R")

n_worker_types <- CONFIG$n_worker_types
wage_cols <- paste0("w", seq_len(n_worker_types))

scenarios <- list(
  list(num = 14, label = "diffusion",
       in_file = "14_wages_diffusion.rds",
       out_file = "14_warm_start_wages_diffusion.rds"),
  list(num = 15, label = "salestax",
       in_file = "15_wages_salestax.rds",
       out_file = "15_warm_start_wages_salestax.rds"),
  list(num = 16, label = "immigration",
       in_file = "16_wages_immigration.rds",
       out_file = "16_warm_start_wages_immigration.rds"),
  list(num = 17, label = "merger",
       in_file = "17_wages_merger.rds",
       out_file = "17_warm_start_wages_merger.rds")
)

for (sc in scenarios) {
  in_path  <- counterfactual_data_path(sc$in_file)
  out_path <- counterfactual_data_path(sc$out_file)

  if (!file.exists(in_path)) {
    cat(sprintf("[skip] %s: source file not found (%s)\n", sc$label, in_path))
    next
  }

  w <- as.data.table(readRDS(in_path))
  ## Drop placeholder rows where the solver was never run (fval=Inf or NA).
  w <- w[is.finite(fval) & is.finite(w1)]

  if (nrow(w) == 0L) {
    cat(sprintf("[skip] %s: no finite-fval rows in %s\n", sc$label, sc$in_file))
    next
  }

  keep_cols <- c("county", "quarter_year", "sol_type", wage_cols)
  warm <- w[, ..keep_cols]
  setkeyv(warm, c("county", "quarter_year", "sol_type"))

  saveRDS(warm, out_path)
  cat(sprintf("\n=== %s ===\n", sc$label))
  cat(sprintf("Wrote %s (%d rows)\n", out_path, nrow(warm)))
  print(warm)

  ## Tag note: which rows were not at target_tol (caller should still use them
  ## as warm starts; they're closer to the true solution than baseline).
  not_clean <- w[converged == FALSE | fval > target_tol]
  if (nrow(not_clean) > 0L) {
    cat(sprintf("[note] %s: %d markets did NOT meet target_tol (still saved as warm starts):\n",
                sc$label, nrow(not_clean)))
    print(not_clean[, .(county, quarter_year, sol_type,
                        fval = signif(fval, 4),
                        target_tol, method)])
  }
}
