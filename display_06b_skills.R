## Display per-county constrained skill matrices reordered to show the
## workers-as-rows dominance pattern. Re-runs the demand-IV QP step
## (cheap; no wage / price solve) so this script is independent of the
## long-running 06b wage solve.

suppressPackageStartupMessages({
  library("data.table")
  library("stringr")
})

source("config.R")
source(file.path("utils", "estimation_pipeline.R"))
source(file.path("utils", "constrained_demand_iv.R"))

n_t <- CONFIG$n_task_types
n_w <- CONFIG$n_worker_types

estimation_sample <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data <- as.data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix

cfg <- CONFIG
cfg$skill_monotone_orientation <- "workers_rows"
setup <- suppressWarnings(
  build_estimation_setup_rank_aware(working_data, estim_matrix, config = cfg)
)
beta_con <- setup$beta
mfit <- setup$skill_monotone_fit

cfg$skill_monotone_orientation <- "none"
setup_unc <- suppressWarnings(
  build_estimation_setup_rank_aware(working_data, estim_matrix, config = cfg)
)
beta_unc <- setup_unc$beta

county_label <- c(`17031` = "Cook (Chicago)",
                  `36061` = "New York (Manhattan)",
                  `6037`  = "Los Angeles")

extract_B <- function(beta_vec, cnty) {
  pat <- paste0("^factor\\(county\\)", cnty, ":avg_labor:B_raw_")
  idx <- grep(pat, rownames(beta_vec))
  M <- matrix(NA_real_, n_t, n_w)
  for (i in idx) {
    nm <- rownames(beta_vec)[i]
    m <- str_match(nm, "B_raw_([0-9]+)_([0-9]+)$")
    M[as.integer(m[1, 2]), as.integer(m[1, 3])] <- beta_vec[i, 1]
  }
  M
}

print_block <- function(M, perm, header) {
  cat("\n", strrep("=", nchar(header) + 4), "\n", sep = "")
  cat("  ", header, "\n", sep = "")
  cat(strrep("=", nchar(header) + 4), "\n", sep = "")
  M_re <- M[, perm, drop = FALSE]
  rownames(M_re) <- paste0("task ", seq_len(n_t))
  colnames(M_re) <- paste0("rank ", seq_len(n_w),
                            " (worker ", perm, ")")
  print(round(M_re, 3))
  cat("\nWorker-skill ordering (least productive -> most productive):\n  ",
      paste(perm, collapse = " -> "), "\n", sep = "")
  cat("\nMonotone diff per task (each entry should be >= 0):\n")
  diff_mat <- t(apply(M_re, 1, diff))
  rownames(diff_mat) <- rownames(M_re)
  colnames(diff_mat) <- paste0("rank ", 2:n_w, "-", 1:(n_w - 1))
  print(round(diff_mat, 3))
}

for (cnty in CONFIG$counties) {
  cnty_str <- as.character(cnty)
  pi_c <- as.integer(mfit$perms_by_county[[cnty_str]])
  M_con <- extract_B(beta_con, cnty)
  M_unc <- extract_B(beta_unc, cnty)

  hdr_unc <- paste0("County ", cnty, " — ", county_label[cnty_str],
                     "  |  UNCONSTRAINED, columns reordered by chosen perm")
  hdr_con <- paste0("County ", cnty, " — ", county_label[cnty_str],
                     "  |  CONSTRAINED (workers-as-rows monotone)")

  print_block(M_unc, pi_c, hdr_unc)
  print_block(M_con, pi_c, hdr_con)

  cat("\n--- summary stats for ", cnty, " ---\n", sep = "")
  cat(sprintf("  ||B_unc||_F            = %.4f\n", sqrt(sum(M_unc^2))))
  cat(sprintf("  ||B_con||_F            = %.4f\n", sqrt(sum(M_con^2))))
  cat(sprintf("  ||B_unc - B_con||_F    = %.4f\n", sqrt(sum((M_unc - M_con)^2))))
  rel <- sqrt(sum((M_unc - M_con)^2)) / sqrt(sum(M_unc^2))
  cat(sprintf("  relative Frobenius     = %.4f\n", rel))

  obj_vec <- mfit$obj_by_perm_by_county[[cnty_str]]
  best_idx <- mfit$best_idx_by_county[[cnty_str]]
  cat(sprintf("  QP obj at chosen perm  = %.4f\n", obj_vec[best_idx]))
  cat(sprintf("  next-best perm obj     = %.4f\n",
              min(obj_vec[-best_idx], na.rm = TRUE)))
}
