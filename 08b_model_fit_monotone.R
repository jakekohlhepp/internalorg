#' =============================================================================
#' STEP 7b: Moment Fit for the Monotone-Restricted Estimates (08b)
#' =============================================================================
#' Recomputes the 08_display_estimates.R moment-fit R2 summary under BOTH the
#' main estimates (06_parameters.rds) and the workers-as-rows monotone
#' estimates (06b_parameters_monotone.rds), using the same design/instrument
#' matrices, and writes the two side by side. The moment construction is a
#' faithful mirror of the moment-fit block at the end of
#' 08_display_estimates.R; the only difference between the two runs is which
#' coefficient vector is loaded and, for 06b, that the demand beta from the
#' unconstrained 2SLS solve is overwritten with the constrained skill-matrix
#' coefficients saved by 06b_estimation_monotone.R.
#'
#' Also renders the per-county constrained-2SLS QP objective at the chosen
#' permutation (from 06b_qp_diagnostics.rds): the direct measure of
#' demand-moment misfit induced by the monotonicity restriction (0 = the
#' constraint does not bind).
#'
#' Inputs:
#'   - mkdata/data/04_estimation_sample.rds
#'   - results/data/06_parameters.rds
#'   - results/data/06b_parameters_monotone.rds
#'   - results/data/06b_qp_diagnostics.rds
#'   - preamble.R
#'
#' Outputs:
#'   - results/out/tables/08b_model_fit_monotone.tex
#'   - results/out/tables/08b_qp_objective_monotone.tex
#'   - results/data/08b_model_fit_monotone.rds
#' =============================================================================

library('data.table')
library('stringr')
library('knitr')
library('kableExtra')

source('config.R')

ensure_directory('results/out/tables')
ensure_directory(file.path('results', 'data'))

spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
parameters_main_path   <- file.path("results", "data", "06_parameters.rds")
parameters_mono_path   <- file.path("results", "data", "06b_parameters_monotone.rds")
qp_diagnostics_path    <- file.path("results", "data", "06b_qp_diagnostics.rds")
assert_required_files(c(estimation_sample_path, parameters_main_path,
                        parameters_mono_path, qp_diagnostics_path))

estimation_sample <- readRDS(estimation_sample_path)
working_data0 <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix

source('preamble.R')
estimation_objects <- build_estimation_setup(working_data0, estim_matrix, config = CONFIG)
beta_unc <- estimation_objects$beta   # unconstrained demand solve (== 06 demand block)
mm_1     <- estimation_objects$mm_1
z_mm_1   <- estimation_objects$z_mm_1

## Wage block sits immediately after the demand block; its width is
## (n_worker_types - 1) x n_counties rather than a hardcoded 12 (as in 08).
n_wage_coefs <- (CONFIG$n_worker_types - 1L) * length(CONFIG$counties)
wage_pos <- nrow(beta_unc) + seq_len(n_wage_coefs)

if (get_os() == "windows") {
  clust <- make_windows_solver_cluster(CONFIG)
  on.exit(stopCluster(clust), add = TRUE)
} else {
  clust <- NULL
}

## One pass of the 08 moment-fit block for a saved coefficient vector.
## use_constrained_beta = TRUE replaces the unconstrained demand solve with the
## constrained skill-matrix coefficients stored in the 06b parameter file.
compute_fit <- function(param_path, use_constrained_beta) {
  ar <- as.data.table(readRDS(param_path))
  point_estimates <- ar$coefficients
  names(point_estimates) <- ar$parm_name

  beta <- beta_unc
  if (use_constrained_beta) {
    stopifnot(all(rownames(beta) %in% names(point_estimates)))
    beta[, 1] <- point_estimates[rownames(beta)]
    stopifnot(!any(is.na(beta)))
  }

  beta_2_subset <- point_estimates[wage_pos]
  stopifnot(all(grepl(":avg_labor:E_raw_[0-9]+$", names(beta_2_subset))))
  final_coefs <- point_estimates[(nrow(beta_unc) + n_wage_coefs + 1L):length(point_estimates)]

  wd <- copy(working_data0)

  moments_part1 <- data.table(
    moment_name = colnames(z_mm_1), model_interact = "Log Market Share",
    raw_moments = colMeans(matrix(estim_matrix$log_rel_mkt, nrow = nrow(z_mm_1), ncol = ncol(z_mm_1), byrow = FALSE) * z_mm_1),
    model_moments = colMeans(matrix(mm_1 %*% beta, nrow = nrow(z_mm_1), ncol = ncol(z_mm_1), byrow = FALSE) * z_mm_1))

  moments_part2 <- eval_moments(theta = beta_2_subset, x = estim_matrix,
                                beta = beta, beta_2_subset = beta_2_subset,
                                config = CONFIG, clust = clust)
  moments_part2 <- cbind(data.table(moments_part2), rownames(moments_part2))
  colnames(moments_part2) <- c("model_moments", "raw_moments", "moment_name")
  moments_part2[, model_interact := "Labor Demand"]

  wd[, wb_2 := beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_2")] * E_raw_2 * avg_labor]
  wd[, wb_3 := beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_3")] * E_raw_3 * avg_labor]
  wd[, wb_4 := beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_4")] * E_raw_4 * avg_labor]
  wd[, wb_5 := beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_5")] * E_raw_5 * avg_labor]
  wd[, gamma_invert := get_gammas(beta_2_subset, estim_matrix,
                                  beta = beta, beta_2_subset = beta_2_subset,
                                  config = CONFIG, clust = clust)]
  wd[, p_adj := cust_price - wb_2 - wb_3 - wb_4 - wb_5 - gamma_invert * s_index * avg_labor +
       mk_piece / beta[paste0("factor(county)", county, ":cust_price"), ]]
  xnam <- as.formula("~avg_labor:factor(county):factor(quarter_year)+factor(quarter_year):factor(county)+factor(quarter_year):(task_mix_2+task_mix_3+task_mix_4+task_mix_5)-1")
  mod_mm_2 <- model.matrix(xnam, data = wd)

  moments_part3 <- data.table(model_interact = "Price", moment_name = colnames(mod_mm_2),
    raw_moments = colMeans(matrix(wd$p_adj, nrow = nrow(mod_mm_2), ncol = ncol(mod_mm_2), byrow = FALSE) * mod_mm_2),
    model_moments = colMeans(matrix(mod_mm_2 %*% final_coefs, nrow = nrow(mod_mm_2), ncol = ncol(mod_mm_2), byrow = FALSE) * mod_mm_2))

  moment_mat <- rbind(moments_part1, moments_part2, moments_part3)
  moment_mat[, county := str_extract(moment_name, "(?i)(?<=factor\\(county\\))[0-9]*")]
  moment_mat[, quarter_year := str_extract(moment_name, "(?i)(?<=factor\\(quarter_year\\))\\d+\\.*\\d*")]
  moment_mat[, other_part := ""]
  moment_mat[!is.na(county), other_part := paste0("factor\\(county\\)", county, "\\:")]
  moment_mat[!is.na(quarter_year), other_part := paste0(other_part, "factor\\(quarter_year\\)", quarter_year)]
  moment_mat[, other_part := str_replace(moment_name, other_part, "")]
  moment_mat[is.na(other_part), other_part := ""]

  ## drop worker-type-1 labor-demand moments (collinear reference type), same
  ## filter as 08_display_estimates.R: those moments are named "county<fips>:E_1".
  moment_mat <- moment_mat[!(model_interact == "Labor Demand" & str_detect(moment_name, ":E_1$"))]

  moment_mat[, other_part := str_replace(other_part, "E_raw_[0-9]", "E_raw")]
  moment_mat[, other_part := str_replace(other_part, "B_raw_[0-9]_[0-9]", "B_raw")]
  moment_mat[, other_part := str_replace(other_part, "task_mix_[0-9]", "task_mix")]

  sm <- moment_mat[, .(count = .N, mean_model = mean(model_moments), mean_data = mean(raw_moments),
                       r2 = 1 - sum((raw_moments - model_moments)^2) / sum((raw_moments - mean(raw_moments))^2)),
                   by = c("model_interact", "other_part")]
  sm[model_interact == "Labor Demand", other_part := "County-Skill Set"]
  sm[model_interact == "Price" & other_part == "", other_part := "County-Quarter"]
  sm[model_interact == "Log Market Share" & other_part == "", other_part := "County-Quarter"]
  sm[model_interact == "Price" & other_part == ":task_mix", other_part := "Quarter-Task Mix"]
  sm[model_interact == "Price" & other_part == "avg_labor:E_raw", other_part := "Labor Demand"]
  sm[model_interact == "Price" & other_part == "avg_labor:", other_part := "County-Quarter-Labor"]
  sm[model_interact == "Log Market Share" & other_part == "dye_instrument", other_part := "County-Dye Instrument"]
  sm[model_interact == "Log Market Share" & other_part == "avg_labor:B_raw", other_part := "County-Task Assignments"]
  stopifnot(nrow(sm[is.na(other_part) | other_part == "", ]) == 0)
  sm[]
}

fit_main <- compute_fit(parameters_main_path, use_constrained_beta = FALSE)
fit_mono <- compute_fit(parameters_mono_path, use_constrained_beta = TRUE)

key <- c("model_interact", "other_part")
cmp <- merge(fit_main[, c(key, "count", "r2"), with = FALSE],
             fit_mono[, c(key, "r2"), with = FALSE],
             by = key, suffixes = c("_main", "_mono"))
setnames(cmp, c("Equation", "Instrument", "Count", "R2_main", "R2_mono"))
cmp[, dR2 := R2_mono - R2_main]
setorder(cmp, Equation, Instrument)

cat("\nMoment-fit R2: main (06) vs monotone (06b)\n")
print(cmp[, .(Equation, Instrument, Count,
              R2_main = round(R2_main, 4), R2_mono = round(R2_mono, 4),
              dR2 = round(dR2, 4))])

output <- copy(cmp)
output[, R2_main := as.character(format(round(R2_main, 3), nsmall = 3))]
output[, R2_mono := as.character(format(round(R2_mono, 3), nsmall = 3))]
output[, dR2 := as.character(format(round(dR2, 3), nsmall = 3))]
setnames(output, old = c("R2_main", "R2_mono", "dR2"),
         new = c("R2 (Main)", "R2 (Monotone)", "$\\Delta$ R2"))
kable(output, "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  cat(., file = "results/out/tables/08b_model_fit_monotone.tex")

## per-county constrained-2SLS QP objective at the chosen permutation
qp <- readRDS(qp_diagnostics_path)
county_labels <- c("17031" = "Cook", "36061" = "New York", "6037" = "Los Angeles")
qp_table <- rbindlist(lapply(as.character(CONFIG$counties), function(cnty) {
  ov <- qp$obj_by_perm_by_county[[cnty]]
  bi <- qp$best_idx_by_county[[cnty]]
  data.table(county = cnty,
             county_name = ifelse(cnty %in% names(county_labels), county_labels[[cnty]], cnty),
             qp_objective = ov[bi])
}))

cat("\nDemand-IV constrained-2SLS QP objective at chosen perm (0 = exact fit):\n")
print(qp_table)

output_qp <- qp_table[, .(County = county_name,
                          `QP Objective` = as.character(format(round(qp_objective, 2), nsmall = 2)))]
kable(output_qp, "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  cat(., file = "results/out/tables/08b_qp_objective_monotone.tex")

saveRDS(list(comparison = cmp, fit_main = fit_main, fit_mono = fit_mono,
             qp_objective = qp_table),
        file.path("results", "data", "08b_model_fit_monotone.rds"))
