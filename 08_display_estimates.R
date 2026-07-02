#' =============================================================================
#' STEP 08: Display Point Estimates and Moment Fit
#' =============================================================================
#' Renders the three estimation-result tables (price/org-cost, wages/skills,
#' time effects) and the moment-fit summary from the saved point estimates and
#' any bootstrap replications that are already on disk.
#'
#' Inputs:
#'   - mkdata/data/01_keytask.rds
#'   - results/data/06_parameters.rds
#'   - results/data/07_first_stage_vcov.rds   (demand 2SLS SEs; from 07_vcov.R)
#'   - results/data/07_murphy_topel_vcov.rds  (structural SEs; default source)
#'   - results/data/07_bootstrap.rds  (LEGACY; only read when
#'       JMP_STRUCTURAL_SE_SOURCE=draws, produced by legacy/07_bootstrap.R;
#'       optional, falls back to zero SEs if absent)
#'   - mkdata/data/04_estimation_sample.rds
#'   - preamble.R
#'
#' Outputs:
#'   - results/out/tables/08_org_price.tex
#'   - results/out/tables/08_wages_skills_<county>.tex (one per county)
#'   - results/out/tables/08_time_effects.tex
#'   - results/out/tables/08_model_fit.tex
#' =============================================================================

library('data.table')
library('lubridate')
library('stringr')
library('knitr')
library('kableExtra')
library('gmm')

source('config.R')

ensure_directory('results/out/tables')

spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

keytask_path <- file.path(CONFIG$prep_output_dir, "01_keytask.rds")
parameters_path <- file.path("results", "data", "06_parameters.rds")
assert_required_files(c(keytask_path, parameters_path))

key_task <- data.table(readRDS(keytask_path))
key_task[clust == 5, rep_text_cluster := "Nail/Misc."]
key_task[clust == 3, rep_text_cluster := "Blowdry/Style/Etc."]
all_results <- readRDS(parameters_path)
point_estimates <- all_results$coefficients
names(point_estimates) <- all_results$parm_name
point_estimates_demand_flag <- all_results$demand
all_results[, county := str_extract(parm_name, "(?i)(?<=factor\\(county\\))[0-9]*")]
all_results[, quarter_year := str_extract(parm_name, "(?i)(?<=factor\\(quarter_year\\))\\d+\\.*\\d*")]


all_results[, other_part := paste0("factor\\(county\\)", county, "\\:")]
all_results[!is.na(quarter_year), other_part := paste0(other_part, "factor\\(quarter_year\\)", quarter_year)]
all_results[, other_part := str_replace(parm_name, other_part, "")]
all_results[str_detect(parm_name, "task_mix"), task := as.numeric(str_extract(parm_name, "(?i)(?<=task_mix_)[0-9]"))]
all_results[str_detect(parm_name, "B_raw_[0-9]_[0-9]"), task := as.numeric(str_extract(parm_name, "(?i)(?<=B_raw_)[0-9]"))]
all_results[str_detect(parm_name, "E_raw_"), worker_type := as.numeric(str_extract(parm_name, "(?i)(?<=E_raw_)[0-9]"))]
all_results[str_detect(parm_name, "B_raw_[0-9]_[0-9]"), worker_type := as.numeric(str_sub(parm_name, start = -1,))]
all_results <- merge(all_results, key_task[, c("task", "rep_text_cluster")], by = "task", all.x = TRUE)
all_results[, county_name := ifelse(county == "17031", "Cook", ifelse(county == "36061", "New York", "Los Angeles"))]


### create se


# LEGACY: only used when JMP_STRUCTURAL_SE_SOURCE=draws (retired Petrin-Train
# bootstrap, see legacy/07_bootstrap.R). Absent under the Murphy-Topel default,
# in which case the draws branch is skipped. first_stage + MT vcov come from 07_vcov.R.
bootstrap_path <- file.path("results", "data", "07_bootstrap.rds")
first_stage_path <- file.path("results", "data", "07_first_stage_vcov.rds")

all_se <- NULL
if (file.exists(bootstrap_path)) {
  all_bootreps <- data.table(readRDS(bootstrap_path))
  parameter_cols <- names(point_estimates)

  if (identical(ncol(all_bootreps), length(parameter_cols) + 1L) &&
      identical(names(all_bootreps)[1], "iteration")) {
    setnames(all_bootreps, old = names(all_bootreps)[-1], new = parameter_cols)
  }

  missing_parameter_cols <- setdiff(parameter_cols, names(all_bootreps))
  if (length(missing_parameter_cols) > 0L) {
    stop("Bootstrap file is missing coefficient columns: ",
         paste(missing_parameter_cols, collapse = ", "))
  }

  ## Default (CONFIG$bootstrap_se_filter_to_ok = FALSE): compute SEs over ALL
  ## non-error reps and FLAG the soft-reverted / non-converged ones via message,
  ## rather than dropping them or erroring out. status=="error" reps have no
  ## parameter columns and are always excluded. Set
  ## JMP_BOOTSTRAP_SE_FILTER_TO_OK=true to restore the strict ok-only filter.
  ## See docs/bootstrap_slurm.md "Post-hoc filtering in 08".
  if ("status" %in% names(all_bootreps)) {
    error_reps <- all_bootreps[!is.na(status) & status == "error"]
    if (nrow(error_reps) > 0L) {
      message("08: dropping ", nrow(error_reps),
              " status=error replication(s) (no parameters): ",
              paste(head(error_reps$iteration, 10L), collapse = ", "),
              if (nrow(error_reps) > 10L) ", ..." else "")
      all_bootreps <- all_bootreps[is.na(status) | status != "error"]
    }
    flagged <- all_bootreps[!is.na(status) & status != "ok"]
    if (nrow(flagged) > 0L) {
      flag_summary <- flagged[, .N, by = status][order(-N)]
      flag_text <- paste(sprintf("%s=%d", flag_summary$status, flag_summary$N), collapse = ", ")
      if (isTRUE(CONFIG$bootstrap_se_filter_to_ok)) {
        message("08: FILTERING OUT ", nrow(flagged),
                " non-ok replication(s) before SE (JMP_BOOTSTRAP_SE_FILTER_TO_OK=true): ", flag_text)
        all_bootreps <- all_bootreps[status == "ok"]
      } else {
        message("08: KEEPING ", nrow(flagged),
                " flagged (soft-reverted / non-converged) replication(s) in the SE distribution: ",
                flag_text, ". Set JMP_BOOTSTRAP_SE_FILTER_TO_OK=true to exclude them.")
      }
    }
  }

  for (conv_col in intersect(c("wage_convergence", "price_convergence"), names(all_bootreps))) {
    bad_conv <- all_bootreps[!is.na(get(conv_col)) & get(conv_col) != 0L]
    if (nrow(bad_conv) > 0L) {
      message("08: ", nrow(bad_conv), " replication(s) with ", conv_col,
              " != 0 retained in SE distribution: ",
              paste(head(paste0("iteration ", bad_conv$iteration, " (code=", bad_conv[[conv_col]], ")"), 10L),
                    collapse = ", "),
              if (nrow(bad_conv) > 10L) ", ..." else "")
    }
  }

  if (nrow(all_bootreps) < 2L) {
    stop("At least two successful bootstrap replications are required to compute standard errors.")
  }

  all_se <- all_bootreps[, lapply(.SD, stats::sd), .SDcols = parameter_cols]
  all_se <- melt(all_se, measure.vars = parameter_cols,
                 variable.name = "parm_name", value.name = "se",
                 variable.factor = FALSE)
  all_se <- merge(
    data.table(parm_name = parameter_cols, demand = point_estimates_demand_flag),
    all_se,
    by = "parm_name",
    sort = FALSE
  )
}

## Demand-stage SEs are the ANALYTICAL clustered 2SLS SEs in
## results/data/07_first_stage_vcov.rds (produced by 07_vcov.R; formerly by the
## retired 07_bootstrap.R). The structural (wage/price) SEs come from the source
## selected below (Murphy-Topel by default; "draws" only if legacy reps exist).
if (file.exists(first_stage_path)) {
  first_stage <- readRDS(first_stage_path)
  if (is.null(all_se)) {
    all_se <- data.table(parm_name = names(point_estimates),
                         demand = point_estimates_demand_flag,
                         se = NA_real_)
  }
  fs_se <- data.table(parm_name = names(first_stage$se),
                      analytic_se = as.numeric(first_stage$se))
  all_se <- merge(all_se, fs_se, by = "parm_name", all.x = TRUE, sort = FALSE)
  missing_analytic <- all_se[demand == TRUE & is.na(analytic_se), parm_name]
  if (length(missing_analytic) > 0L) {
    stop("07_first_stage_vcov.rds is missing analytical SEs for demand parameters: ",
         paste(head(missing_analytic, 10L), collapse = ", "))
  }
  all_se[demand == TRUE, se := analytic_se]
  all_se[, analytic_se := NULL]
  message("08: demand SEs = analytical clustered 2SLS (cluster = ",
          first_stage$cluster_variable, ", G = ", first_stage$n_clusters,
          "); structural SEs from source '", CONFIG$structural_se_source,
          "' (see the structural-SE message below).")
} else if (!is.null(all_se)) {
  message("08: results/data/07_first_stage_vcov.rds not found; demand SEs fall ",
          "back to the across-rep SD. Re-run 07_vcov.R to generate the ",
          "analytical first-stage SEs.")
}

## Structural-SE source switch (JMP_STRUCTURAL_SE_SOURCE): "draws" (default)
## keeps the across-rep SD from the Petrin-Train replications; "murphy_topel"
## replaces the second-stage SEs with the analytical two-step sandwich from
## 07c_murphy_topel.R (docs/murphy_topel_proposal.md). Price coordinates that
## bind their lower bound have NA Murphy-Topel SEs and keep the draw-based SD.
if (identical(CONFIG$structural_se_source, "murphy_topel")) {
  mt_path <- file.path("results", "data", "07_murphy_topel_vcov.rds")
  if (!file.exists(mt_path)) {
    stop("JMP_STRUCTURAL_SE_SOURCE=murphy_topel but ", mt_path,
         " not found; run 07c_murphy_topel.R first.")
  }
  mt <- readRDS(mt_path)
  if (is.null(all_se)) {
    all_se <- data.table(parm_name = names(point_estimates),
                         demand = point_estimates_demand_flag,
                         se = NA_real_)
  }
  ## Merge on the STRUCTURAL-only SE vector (wage + price block). Its names are
  ## unique, whereas mt$se spans the full system where demand county:quarter FEs
  ## share names with their price/B twins -- a by-name merge on mt$se would
  ## double-match those 33 names and drop/duplicate their structural SEs.
  mt_struct <- if (!is.null(mt$se_structural)) mt$se_structural else mt$se
  mt_se <- data.table(parm_name = names(mt_struct), mt_se = as.numeric(mt_struct))
  all_se <- merge(all_se, mt_se, by = "parm_name", all.x = TRUE, sort = FALSE)
  missing_mt <- all_se[demand == FALSE & is.na(mt_se) &
                         !parm_name %in% mt$binding_price_coords, parm_name]
  if (length(missing_mt) > 0L) {
    stop("07_murphy_topel_vcov.rds is missing SEs for structural parameters: ",
         paste(head(missing_mt, 10L), collapse = ", "))
  }
  both <- all_se[demand == FALSE & !is.na(mt_se) & !is.na(se) & se > 0]
  if (nrow(both) > 0L) {
    message("08: max |MT/draws - 1| over ", nrow(both),
            " structural SEs with both sources: ",
            signif(max(abs(both$mt_se / both$se - 1)), 3))
  }
  all_se[demand == FALSE & !is.na(mt_se), se := mt_se]
  all_se[, mt_se := NULL]
  message("08: structural SEs = Murphy-Topel sandwich (G = ", mt$n_clusters,
          " clusters, k_total = ", mt$k_total, ")",
          if (length(mt$binding_price_coords) > 0L) {
            paste0("; bound-binding coords keep the draw-based SD: ",
                   paste(mt$binding_price_coords, collapse = ", "))
          } else "", ".")
}

if (!is.null(all_se)) {
  all_results <- merge(all_results, all_se, by = c("parm_name", "demand"))
  stopifnot(nrow(all_results) == nrow(all_se))
  ## Parameters without an SE (e.g. bound-binding price coordinates under the
  ## Murphy-Topel source) print "-" rather than a fabricated number.
  all_results[, se_formatted := "-"]
  all_results[!is.na(se),
              se_formatted := str_replace(as.character(paste0("(", format(round(se, 3), nsmall = 3), ")")), " 0.", ".")]
  all_results[, se := NULL]
  setnames(all_results, "se_formatted", "se")
} else {
  all_results[, se := "-", ]
}
## blanks are county-quarter fixed effects in the cost and quality equations.

## format point estimates and se
# flip sign of rho for display
all_results[other_part == "cust_price", coefficients := -coefficients]
all_results[, coefficients := as.character(format(round(coefficients, 3), nsmall = 3)), ]

## melt so se is lower
all_results <- melt(all_results, id.vars = c("parm_name", "other_part", "county_name", "rep_text_cluster", "quarter_year", "worker_type", "demand"), measure.vars = c("se", "coefficients"))
all_results[, variable := as.character(variable)]
setkey(all_results, "county_name", "quarter_year", "other_part", "variable", "rep_text_cluster")
setnames(all_results, "county_name", "County")
## nice names
all_results[other_part == "cust_price", nice_name := "Price Sensitivity"]
all_results[other_part == "org_cost", nice_name := "Reference Org. Cost"]
all_results[other_part == "avg_labor:", nice_name := "Wage Level"]
all_results[str_detect(other_part, "task_mix_[0-9]$"), nice_name := paste("Material Cost", rep_text_cluster)]
all_results[other_part == "" & demand == TRUE, nice_name := "Demand Level"]
all_results[other_part == "" & demand == FALSE, nice_name := "Cost Level"]

## order by county, quarter year,task, coefficient than se

### table 1: org costs and price sensitivity for all markets
output <- all_results[other_part %in% c("cust_price", "org_cost"), c("County", "nice_name", "variable", "value")]
output <- dcast(output, County + variable ~ nice_name)
output[which(!as.logical(1:nrow(output) %% 2)), County := ""]
kable(output[, -"variable"], "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  cat(., file = "results/out/tables/08_org_price.tex")


### table 2: wages and skills (one table per market)


for (cnty in unique(all_results$County)) {
  print(cnty)
  if (is.na(cnty)) next
  output <- all_results[County == cnty & !is.na(worker_type), c("worker_type", "value", "rep_text_cluster", "variable")]
  output[, rep_text_cluster := ifelse(is.na(rep_text_cluster), "Wage", rep_text_cluster)]
  output <- dcast(output, worker_type + variable ~ rep_text_cluster, value.var = "value")
  output[is.na(Wage) & variable == "coefficients", Wage := "-"]
  output[is.na(Wage) & variable == "se", Wage := "-"]
  setcolorder(output, c("worker_type", "variable", "Wage"))
  setnames(output, "worker_type", "Worker Skill Set")
  print(output[, -"variable"])
  kable(output[, -"variable"], "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
    cat(., file = paste0("results/out/tables/08_wages_skills_", gsub(" ", "", cnty), ".tex"))


}

### table 3: material costs, wage time fixed effect, demand time fixed effect
output <- all_results[nice_name %in% c("Demand Level", "Wage Level", "Cost Level") | str_detect(nice_name, "Material Cost"), c("nice_name", "County", "quarter_year", "value", "variable")]
output[is.na(County), County := "All"]
output[, quarter_year := gsub("\\.", "Q", quarter_year)]
output <- dcast(output, County + nice_name + variable ~ quarter_year, value.var = "value")
output[is.na(`2018Q1`), `2018Q1` := "-"]
setnames(output, "nice_name", "Parameter")
kable(output[, -"variable"], "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  cat(., file = "results/out/tables/08_time_effects.tex")




#### display targeted moments and the model output.
rm(list = setdiff(ls(all.names = TRUE), c("point_estimates", "CONFIG")))
source('config.R')  # re-load helper functions wiped by the rm() above
spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
assert_required_files(estimation_sample_path)

estimation_sample <- readRDS(estimation_sample_path)
working_data <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix

source('preamble.R')
estimation_objects <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta <- estimation_objects$beta
mm_1 <- estimation_objects$mm_1
z_mm_1 <- estimation_objects$z_mm_1

## Wage block sits immediately after the demand block; its width is
## (n_worker_types - 1) x n_counties rather than a hardcoded 12.
n_wage_coefs <- (CONFIG$n_worker_types - 1L) * length(CONFIG$counties)
wage_pos <- nrow(beta) + seq_len(n_wage_coefs)
beta_2_subset <- point_estimates[wage_pos]
stopifnot(all(grepl(":avg_labor:E_raw_[0-9]+$", names(beta_2_subset))))

## coefficients for last step
final_coefs <- point_estimates[(nrow(beta) + n_wage_coefs + 1L):length(point_estimates)]


if (get_os() == "windows") {
  clust <- make_windows_solver_cluster(CONFIG)
  on.exit(stopCluster(clust), add = TRUE)
} else {
  clust <- NULL
}

##### initialize

moments_part1 <- data.table(moment_name = colnames(z_mm_1), model_interact = "Log Market Share",
                            raw_moments = colMeans(matrix(estim_matrix$log_rel_mkt, nrow = nrow(z_mm_1), ncol = ncol(z_mm_1), byrow = FALSE) * z_mm_1),
                            model_moments = colMeans(matrix(mm_1 %*% beta, nrow = nrow(z_mm_1), ncol = ncol(z_mm_1), byrow = FALSE) * z_mm_1))


moments_part2 <- eval_moments(theta = beta_2_subset, x = estim_matrix,
                              beta = beta, beta_2_subset = beta_2_subset,
                              config = CONFIG, clust = clust)
moments_part2 <- cbind(data.table(moments_part2), rownames(moments_part2))
colnames(moments_part2) <- c("model_moments", "raw_moments", "moment_name")
moments_part2[, model_interact := "Labor Demand"]


working_data[, wb_2 := beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_2")] * E_raw_2 * avg_labor]
working_data[, wb_3 := beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_3")] * E_raw_3 * avg_labor]
working_data[, wb_4 := beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_4")] * E_raw_4 * avg_labor]
working_data[, wb_5 := beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_5")] * E_raw_5 * avg_labor]
working_data[, gamma_invert := get_gammas(beta_2_subset, estim_matrix,
                                           beta = beta, beta_2_subset = beta_2_subset,
                                           config = CONFIG, clust = clust)]
working_data[, p_adj := cust_price - wb_2 - wb_3 - wb_4 - wb_5 - gamma_invert * s_index * avg_labor + mk_piece / beta[paste0("factor(county)", county, ":cust_price"), ]]
xnam <- as.formula("~avg_labor:factor(county):factor(quarter_year)+factor(quarter_year):factor(county)+factor(quarter_year):(task_mix_2+task_mix_3+task_mix_4+task_mix_5)-1")
mod_mm_2 <- model.matrix(xnam, data = working_data)



moments_part3 <- data.table(model_interact = "Price", moment_name = colnames(mod_mm_2),
                            raw_moments = colMeans(matrix(working_data$p_adj, nrow = nrow(mod_mm_2), ncol = ncol(mod_mm_2), byrow = FALSE) * mod_mm_2),
                            model_moments = colMeans(matrix(mod_mm_2 %*% final_coefs, nrow = nrow(mod_mm_2), ncol = ncol(mod_mm_2), byrow = FALSE) * mod_mm_2))

moment_mat <- rbind(moments_part1, moments_part2, moments_part3)

moment_mat[, county := str_extract(moment_name, "(?i)(?<=factor\\(county\\))[0-9]*")]
moment_mat[, county_name := ifelse(county == "17031", "Chicago", ifelse(county == "36061", "Manhattan", "Los Angeles"))]
moment_mat[, quarter_year := str_extract(moment_name, "(?i)(?<=factor\\(quarter_year\\))\\d+\\.*\\d*")]

moment_mat[, other_part := ""]
moment_mat[!is.na(county), other_part := paste0("factor\\(county\\)", county, "\\:")]
moment_mat[!is.na(quarter_year), other_part := paste0(other_part, "factor\\(quarter_year\\)", quarter_year)]
moment_mat[, other_part := str_replace(moment_name, other_part, "")]
moment_mat[is.na(other_part), other_part := ""]


# drop so not collinear
moment_mat <- moment_mat[!str_detect(other_part, "E_raw_1")]

moment_mat[, other_part := str_replace(other_part, "E_raw_[0-9]", "E_raw")]
moment_mat[, other_part := str_replace(other_part, "B_raw_[0-9]_[0-9]", "B_raw")]
moment_mat[, other_part := str_replace(other_part, "task_mix_[0-9]", "task_mix")]

summary_moments <- moment_mat[, .(count = .N, mean_model = mean(model_moments), mean_data = mean(raw_moments),
                                   r2 = 1 - sum((raw_moments - model_moments)^2) / sum((raw_moments - mean(raw_moments))^2)),
                              by = c("model_interact", "other_part")]

summary_moments[model_interact == "Labor Demand", other_part := "County-Skill Set"]
summary_moments[model_interact == "Price" & other_part == "", other_part := "County-Quarter"]
summary_moments[model_interact == "Log Market Share" & other_part == "", other_part := "County-Quarter"]
summary_moments[model_interact == "Price" & other_part == ":task_mix", other_part := "Quarter-Task Mix"]
summary_moments[model_interact == "Price" & other_part == "avg_labor:E_raw", other_part := "Labor Demand"]
summary_moments[model_interact == "Price" & other_part == "avg_labor:", other_part := "County-Quarter-Labor"]
summary_moments[model_interact == "Log Market Share" & other_part == "dye_instrument", other_part := "County-Dye Instrument"]
summary_moments[model_interact == "Log Market Share" & other_part == "avg_labor:B_raw", other_part := "County-Task Assignments"]
stopifnot(nrow(summary_moments[is.na(other_part) | other_part == "", ]) == 0)

summary_moments[, mean_model := as.character(format(round(mean_model, 2), nsmall = 2)), ]
summary_moments[, mean_data := as.character(format(round(mean_data, 2), nsmall = 2)), ]
summary_moments[, r2 := as.character(format(round(r2, 3), nsmall = 3)), ]
setnames(summary_moments, old = c("model_interact", "other_part", "count", "mean_model", "mean_data", "r2"),
         new = c("Equation", "Instrument", "Count", "Avg. Model", "Avg. Data", "R2"))
kable(summary_moments, "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  cat(., file = "results/out/tables/08_model_fit.tex")
