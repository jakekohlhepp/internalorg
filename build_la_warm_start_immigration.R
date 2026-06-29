## Seed the immigration warm-start table's LA (6037) rows from the known-good v5
## wages (the pre-recap backup), so the fast re-eval (coord_descent_first=true)
## seeds Step-1 from those wages and short-circuits once it confirms they clear
## at cap 5e6. Leaves Cook/NY untouched. Backs up the existing warm table.
source("config.R")
source("utils/counterfactuals_core.R")
suppressMessages(library(data.table))

wcols <- paste0("w", seq_len(CONFIG$n_worker_types))
focus <- get_counterfactual_focus_quarter()
la <- "6037"

warm_path <- counterfactual_data_path("16_warm_start_wages_immigration.rds")
solved_path <- counterfactual_data_path("16_wages_immigration.rds.bak_pre_recap_20260605")
stopifnot(file.exists(warm_path), file.exists(solved_path))
warm <- as.data.table(readRDS(warm_path))
solved <- as.data.table(readRDS(solved_path))
cat("warm cols:", paste(names(warm), collapse=", "), "| rows:", nrow(warm), "\n")

bak <- paste0(warm_path, ".bak_pre_fasteval_20260606")
saveRDS(warm, bak); cat("backed up ->", bak, "\n\n")

for (st in c("realloc", "reorg")) for (qy in focus) {
  src <- solved[as.character(county)==la & sol_type==st & as.character(quarter_year)==as.character(qy)]
  sel <- warm[, as.character(county)==la & sol_type==st & as.character(quarter_year)==as.character(qy)]
  if (nrow(src)!=1 || sum(sel)!=1) { cat(sprintf("  [skip] %s %s (src=%d warm=%d)\n", st, qy, nrow(src), sum(sel))); next }
  old <- as.numeric(warm[sel, ..wcols]); new <- as.numeric(src[1, ..wcols])
  for (j in seq_along(wcols)) set(warm, which(sel), wcols[j], new[j])
  cat(sprintf("  LA %-7s %s: old=[%s]\n               new=[%s]\n", st, qy,
              paste(signif(old,5),collapse=", "), paste(signif(new,5),collapse=", ")))
}
saveRDS(warm, warm_path)
cat("\nwrote ->", warm_path, "\n[done]\n")
