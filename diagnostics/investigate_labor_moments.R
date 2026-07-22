## Investigate the fit of the labor-demand / wage moments (the block whose
## R2 collapsed 1.000 -> 0.190 under the monotone restriction).
## These moments = mean model-implied vs observed skill-set employment SHARE,
## per (county x worker type). eval_moments returns cbind(E_model, E_data).
setwd("/nas/longleaf/home/kohlhepp/internalorg")
suppressMessages({library(data.table); library(stringr)})
source("config.R")
spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

estimation_sample <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data0 <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix
source("preamble.R")
estimation_objects <- build_estimation_setup(working_data0, estim_matrix, config = CONFIG)
beta_unc <- estimation_objects$beta
n_wage_coefs <- (CONFIG$n_worker_types - 1L) * length(CONFIG$counties)
wage_pos <- nrow(beta_unc) + seq_len(n_wage_coefs)
clust <- if (get_os() == "windows") make_windows_solver_cluster(CONFIG) else NULL

get_labor_moments <- function(param_path, use_constrained_beta) {
  ar <- as.data.table(readRDS(param_path))
  pe <- ar$coefficients; names(pe) <- ar$parm_name
  beta <- beta_unc
  if (use_constrained_beta) { beta[, 1] <- pe[rownames(beta)]; stopifnot(!any(is.na(beta))) }
  b2 <- pe[wage_pos]
  em <- eval_moments(theta = b2, x = estim_matrix, beta = beta, beta_2_subset = b2,
                     config = CONFIG, clust = clust)
  dt <- data.table(moment_name = rownames(em), E_model = em[, 1], E_data = em[, 2])
  dt[, county := str_extract(moment_name, "(?<=county)[0-9]+")]
  dt[, worker := as.integer(str_extract(moment_name, "(?<=E_)[0-9]+"))]
  ## wage GMM objective (workers 2:5): sum over counties of squared mean residual
  gmm <- objective_gmm(theta = b2, x = estim_matrix, beta = beta, beta_2_subset = b2,
                       config = CONFIG, clust = clust)
  list(cells = dt, gmm_resid = gmm)
}

main <- get_labor_moments("results/data/06_parameters.rds", FALSE)
mono <- get_labor_moments("results/data/06b_parameters_monotone.rds", TRUE)

cn <- c(`17031`="Cook", `36061`="NYC", `6037`="LA")
tab <- merge(main$cells[, .(county, worker, E_data, E_model_main = E_model)],
             mono$cells[, .(county, worker, E_model_mono = E_model)],
             by = c("county","worker"))
tab[, resid_main := E_data - E_model_main]
tab[, resid_mono := E_data - E_model_mono]
tab[, County := cn[county]]
setorder(tab, county, worker)

cat("\n===== LABOR-DEMAND MOMENTS: skill-set employment share, model vs data =====\n")
cat("(E_data identical across runs; only E_model differs)\n\n")
print(tab[, .(County, type=worker,
              E_data = round(E_data,4),
              E_mod_main = round(E_model_main,4), resid_main = round(resid_main,4),
              E_mod_mono = round(E_model_mono,4), resid_mono = round(resid_mono,4))])

r2 <- function(d, m) 1 - sum((d-m)^2)/sum((d-mean(d))^2)
cat("\n----- R2 pooled over all 15 cells -----\n")
cat(sprintf("  main:  %.4f\n  mono:  %.4f\n", r2(tab$E_data, tab$E_model_main), r2(tab$E_data, tab$E_model_mono)))
cat("\n----- per-county R2 (5 cells each) & residual RMSE -----\n")
for (c in names(cn)) {
  s <- tab[county==c]
  cat(sprintf("  %-4s  R2 main=%.4f mono=%.4f | RMSE main=%.4f mono=%.4f | max|resid| main=%.4f mono=%.4f\n",
              cn[c], r2(s$E_data,s$E_model_main), r2(s$E_data,s$E_model_mono),
              sqrt(mean(s$resid_main^2)), sqrt(mean(s$resid_mono^2)),
              max(abs(s$resid_main)), max(abs(s$resid_mono))))
}
cat("\n----- wage GMM objective = sum(colMeans(model-data)^2) over workers 2:5 -----\n")
cat(sprintf("  main:  %.6g\n  mono:  %.6g\n", sum(colMeans(main$gmm_resid)^2), sum(colMeans(mono$gmm_resid)^2)))
## per-county wage objective floor (mono)
gd <- as.data.table(mono$gmm_resid); gd[, moment := rownames(mono$gmm_resid)]
saveRDS(list(tab=tab, main_gmm=main$gmm_resid, mono_gmm=mono$gmm_resid),
        "logs/investigate_labor_moments.rds")
cat("\nDONE\n")
