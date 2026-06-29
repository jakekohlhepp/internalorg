## Seed the diffusion warm-start table's LA (6037) rows from the best-known
## solved wages (results/data/counterfactuals/14_wages_diffusion.rds), leaving
## Cook (17031) and NY (36061) rows untouched. Backs up the existing table
## first. Lets the tight-tol relaunch start LA-reorg at its ~0.029 floor seed
## instead of the warm point that nleqslv drove to ~0.31.
source("config.R")
source("utils/counterfactuals_core.R")
suppressMessages(library(data.table))

wcols <- paste0("w", seq_len(CONFIG$n_worker_types))
focus <- get_counterfactual_focus_quarter()
la <- "6037"

warm_path <- counterfactual_data_path("14_warm_start_wages_diffusion.rds")
stopifnot(file.exists(warm_path))
warm <- as.data.table(readRDS(warm_path))
solved <- as.data.table(readRDS(counterfactual_data_path("14_wages_diffusion.rds")))

cat("warm table cols :", paste(names(warm), collapse=", "), "\n")
cat("warm table rows :", nrow(warm), "\n")
cat("focus quarter   :", paste(focus, collapse=", "), "\n\n")

## backup
bak <- paste0(warm_path, ".bak_pre_lawarm_20260605")
saveRDS(warm, bak)
cat("backed up existing warm table ->", bak, "\n\n")

for (st in c("realloc", "reorg")) {
  for (qy in focus) {
    src <- solved[as.character(county)==la & sol_type==st & as.character(quarter_year)==as.character(qy)]
    if (nrow(src) != 1) { cat(sprintf("  [skip] no single solved row for %s %s %s (found %d)\n", la, st, qy, nrow(src))); next }
    sel <- warm[, as.character(county)==la & sol_type==st & as.character(quarter_year)==as.character(qy)]
    if (sum(sel) != 1) { cat(sprintf("  [skip] no single warm row for %s %s %s (found %d)\n", la, st, qy, sum(sel))); next }
    old <- as.numeric(warm[sel, ..wcols]); new <- as.numeric(src[1, ..wcols])
    for (j in seq_along(wcols)) set(warm, which(sel), wcols[j], new[j])
    cat(sprintf("  LA %-7s %s: w_old=[%s]\n                 w_new=[%s]\n", st, qy,
                paste(signif(old,5),collapse=", "), paste(signif(new,5),collapse=", ")))
  }
}

saveRDS(warm, warm_path)
cat("\nwrote updated warm table ->", warm_path, "\n[done]\n")
