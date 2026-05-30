## Does perturbing LA's worker-5 wage change the estimation GMM objective?
## Sweeps the LA :avg_labor:E_raw_5 coefficient (-> w5 = w1 + coef) and at each
## value evaluates the production (uniform-weight) wage-stage GMM objective
## = sum(colMeans(objective_gmm)^2), plus the LA worker-5 moment column
## (county6037:E_5 = mean(model E_5 - observed E_5) over LA rows).
##
## Flat objective across w5 => w5 weakly identified => the estimated 831 is not
## a "bad residual", just an unidentified ridge. The LA worker-5 moment shows
## how small the level mismatch is (bounded by the ~0.5% observed share), which
## is why model E_5 -> 0 costs the objective almost nothing.
suppressPackageStartupMessages({ library(data.table) })
source("config.R"); source("preamble.R")

es <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data <- data.table(es$working_data)
estim_matrix <- es$estim_matrix

beta <- build_weighted_estimation_setup(working_data, estim_matrix, config = CONFIG, weights = NULL)$beta
setup <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta_2 <- setup$beta_2
wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
wage_names <- rownames(beta_2)[wage_idx]
p06 <- as.data.table(readRDS(file.path("results","data","06_parameters.rds")))
beta_2_subset <- setNames(p06$coefficients[match(wage_names, p06$parm_name)], wage_names)
stopifnot(all(is.finite(beta_2_subset)))

la_e5 <- wage_names[grepl("6037", wage_names) & grepl("E_raw_5$", wage_names)]
stopifnot(length(la_e5) == 1L)
w1 <- p06$coefficients[p06$parm_name == "avg_labor:factor(county)6037:factor(quarter_year)2021.2"]
est_coef <- beta_2_subset[[la_e5]]
cat("LA worker-5 wage coef name: ", la_e5, "\n", sep="")
cat("estimated coef = ", round(est_coef,3), "  (w1 = ", round(w1,3),
    " => estimated w5 = ", round(w1 + est_coef,3), ")\n", sep="")

obj_of <- function(theta) {
  E_mat <- objective_gmm(theta, x = estim_matrix, beta = beta,
                         beta_2_subset = beta_2_subset, config = CONFIG)
  m <- colMeans(E_mat)
  list(obj = sum(m^2), la_w5_moment = unname(m["county6037:E_5"]))
}

## sweep w5 from below w1 up past the estimate; coef = w5 - w1
w5_grid <- sort(unique(c(50, 77, 100, 150, 200, 300, 500, 700, w1 + est_coef, 900)))
rows <- rbindlist(lapply(w5_grid, function(w5) {
  th <- beta_2_subset; th[[la_e5]] <- w5 - w1
  r <- obj_of(as.numeric(th))
  data.table(w5 = w5, coef = w5 - w1, gmm_objective = r$obj, la_w5_moment = r$la_w5_moment,
             is_estimate = isTRUE(abs(w5 - (w1 + est_coef)) < 1e-6))
}))
setorder(rows, w5)

est_obj <- rows[is_estimate == TRUE, gmm_objective][1]
rows[, obj_vs_est := gmm_objective - est_obj]

cat("\n=== GMM objective vs LA w5 (production uniform weights) ===\n")
print(rows[, .(w5 = round(w5,1), coef = round(coef,1),
               gmm_objective = signif(gmm_objective,5),
               obj_minus_est = signif(obj_vs_est,3),
               la_w5_moment = signif(la_w5_moment,4),
               is_estimate)])

cat(sprintf("\nobj range over w5 in [50,900]: [%.6g, %.6g]; spread = %.3g\n",
            min(rows$gmm_objective), max(rows$gmm_objective),
            max(rows$gmm_objective) - min(rows$gmm_objective)))
cat(sprintf("obj_tol gate = %.4g.  Is the whole sweep within obj_tol of the estimate? %s\n",
            CONFIG$obj_tol, all(abs(rows$obj_vs_est) < CONFIG$obj_tol)))
cat("LA worker-5 moment magnitude stays ~", signif(max(abs(rows$la_w5_moment)),3),
    " (bounded by the tiny observed type-5 share) across the whole w5 range.\n", sep="")

saveRDS(rows, "smoke_w5_objective_flatness.rds")
fwrite(rows, "results/data/w5_objective_flatness.csv")
cat("Saved: results/data/w5_objective_flatness.csv\n")
