## Audit: verify the reallocation equilibrium holds B (and hence E, s_index)
## fixed at the baseline equilibrium, while reorganization re-solves them.
## Pure post-hoc check on saved panels -- re-solves nothing.
suppressWarnings(suppressMessages({
  if (file.exists("renv/activate.R")) source("renv/activate.R")
  library(data.table)
}))

dpath <- "results/data/counterfactuals"
base <- as.data.table(readRDS(file.path(dpath, "13_prod_initial.rds")))
imm  <- as.data.table(readRDS(file.path(dpath, "16_prod_immigration.rds")))

## de-dup any repeated column names from the data.table(county=..,get_prod()) wrap
dedup <- function(dt) dt[, !duplicated(names(dt)), with = FALSE]
base <- dedup(base); imm <- dedup(imm)

b_cols <- grep("^B_[0-9]", names(base), value = TRUE)
e_cols <- grep("^E_[0-9]$", names(base), value = TRUE)

cmp <- function(cnty) {
  bb <- base[county == cnty][order(location_id)]
  rr <- imm[county == cnty & sol_type == "realloc"][order(location_id)]
  zz <- imm[county == cnty & sol_type == "reorg"][order(location_id)]
  stopifnot(nrow(bb) == nrow(rr), nrow(bb) == nrow(zz))
  stopifnot(identical(bb$location_id, rr$location_id),
            identical(bb$location_id, zz$location_id))

  maxdiff <- function(a, b, cols) max(abs(as.matrix(a[, ..cols]) - as.matrix(b[, ..cols])))
  cat(sprintf("\n=== county %s  (%d firms) ===\n", cnty, nrow(bb)))
  cat(sprintf("  REALLOC vs BASELINE   max|dB| = %.3e   max|dE| = %.3e   max|ds_index| = %.3e\n",
              maxdiff(bb, rr, b_cols), maxdiff(bb, rr, e_cols),
              max(abs(bb$s_index - rr$s_index))))
  cat(sprintf("  REORG   vs BASELINE   max|dB| = %.3e   max|dE| = %.3e   max|ds_index| = %.3e\n",
              maxdiff(bb, zz, b_cols), maxdiff(bb, zz, e_cols),
              max(abs(bb$s_index - zz$s_index))))
  ## prices/shares SHOULD move in realloc (firms re-price); structure should not
  cat(sprintf("  REALLOC vs BASELINE   max|dnewprice| = %.3e   max|dnew_share| = %.3e\n",
              max(abs(bb$newprice - rr$newprice)), max(abs(bb$new_share - rr$new_share))))
}

for (cnty in c("6037", "36061", "17031")) cmp(cnty)

cat("\nInterpretation:\n")
cat("  realloc dB/dE/ds_index ~ 0  => organization structure held fixed at baseline (correct)\n")
cat("  reorg   dB/dE/ds_index  > 0  => structure re-optimized (correct)\n")
cat("  realloc dnewprice/dnew_share > 0 => firms still re-price/re-scale (correct)\n")
