## Smoke run: replicate the price stage but with no minimum-wage lower bound.
## Uses the existing helpers (no program modifications); only the L-BFGS-B
## lower-bound vector is replaced with -Inf.

library(data.table)
set.seed(4459665)
source('config.R')

## Match the relaxed-tolerance settings of the most recent estimation run so
## get_gammas and the inner solver behave identically to the just-saved
## seeit_bb.rds and 06_parameters.rds.
CONFIG$innertol <- 1e-4
CONFIG$outertol <- 1e-2
CONFIG$obj_tol  <- 1e-2
CONFIG$coarse_innertol <- max(CONFIG$coarse_innertol, CONFIG$innertol)
CONFIG$coarse_outertol <- max(CONFIG$coarse_outertol, CONFIG$outertol)

est <- readRDS(file.path(CONFIG$prep_output_dir, '04_estimation_sample.rds'))
working_data <- data.table(est$working_data)
estim_matrix <- est$estim_matrix
min_wage_levels <- if ('min_wage_levels' %in% names(est)) {
  data.table(est$min_wage_levels)
} else {
  unique(working_data[, .(county, quarter_year, min_wage)])
}

source('preamble.R')

setup <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta <- setup$beta
beta_2 <- setup$beta_2

beta_2_subset <- readRDS(file.path(CONFIG$prep_output_dir, 'seeit_bb.rds'))
names(beta_2_subset) <- rownames(beta_2)[grep(
  'avg_labor:E_raw_[2-9]{1}\\d{0,3}', rownames(beta_2)
)]

clust <- make_windows_solver_cluster(CONFIG)
on.exit(if (!is.null(clust)) parallel::stopCluster(clust), add = TRUE)

cat('Building p_adj via add_price_adjustment...\n')
data <- add_price_adjustment(
  working_data, estim_matrix, beta,
  wage_coefs    = beta_2_subset,
  beta_2_subset = beta_2_subset,
  config = CONFIG, clust = clust
)

task_mix_formula_str <- build_task_mix_sum(CONFIG)
xnam <- as.formula(paste0(
  '~avg_labor:factor(county):factor(quarter_year) + ',
  'factor(quarter_year):factor(county) + ',
  'factor(quarter_year):(', task_mix_formula_str, ') - 1'
))
mod_mm_2 <- model.matrix(xnam, data = data)

obj_final_reg <- function(x) {
  resid_moments <- matrix(
    data$p_adj - mod_mm_2 %*% x,
    nrow = nrow(mod_mm_2), ncol = ncol(mod_mm_2), byrow = FALSE
  ) * mod_mm_2
  sum(colMeans(resid_moments)^2)
}

## Use the same OLS warm-start the production code uses, so we step off from
## the same point — only the bound differs.
first_try_formula <- as.formula(paste0(
  'p_adj ~ avg_labor:factor(county):factor(quarter_year) + ',
  'factor(county):factor(quarter_year) + ',
  'factor(quarter_year):(', task_mix_formula_str, ') - 1'
))
first_try <- coef(lm(first_try_formula, data = data))
starting_final_reg <- as.numeric(first_try)
starting_final_reg[is.na(starting_final_reg)] <- 0

cat('Running unconstrained L-BFGS-B (lower = -Inf everywhere)...\n')
final_reg <- optim(
  starting_final_reg, obj_final_reg,
  lower  = rep(-Inf, length(starting_final_reg)),
  upper  = rep( Inf, length(starting_final_reg)),
  method = 'L-BFGS-B',
  control = list(maxit = 1000000L, trace = 1L,
                 factr = CONFIG$obj_tol / .Machine$double.eps)
)
cat('convergence:', final_reg$convergence, '  message:', final_reg$message,
    '  value:', final_reg$value, '  fn evals:', final_reg$counts['function'], '\n')

coefs <- final_reg$par
names(coefs) <- names(first_try)

## Build wage table: type 1 = labor_coef; types 2..5 = labor_coef + E_raw_k
labor_cq <- data.table(parm_name = names(coefs), labor_coef = coefs)
labor_cq <- labor_cq[grepl('^avg_labor:factor', parm_name)]
labor_cq[, county      := sub('.*county\\)([0-9]+).*', '\\1', parm_name)]
labor_cq[, quarter_year := sub('.*quarter_year\\)([0-9.]+).*', '\\1', parm_name)]

ec <- data.table(parm_name = names(beta_2_subset), e_coef = as.numeric(beta_2_subset))
ec[, county := sub('.*county\\)([0-9]+).*', '\\1', parm_name)]
ec[, k      := as.integer(sub('.*E_raw_([0-9]+).*', '\\1', parm_name))]

type1 <- labor_cq[, .(county, quarter_year, k = 1L, wage = labor_coef)]
oth   <- merge(labor_cq[, .(county, quarter_year, labor_coef)],
               ec[, .(county, k, e_coef)], by = 'county', allow.cartesian = TRUE)
oth[, wage := labor_coef + e_coef]
wages <- rbind(type1, oth[, .(county, quarter_year, k, wage)])
wages_w <- dcast(wages, county + quarter_year ~ k, value.var = 'wage')
setnames(wages_w, as.character(1:5), paste0('type_', 1:5))
setorder(wages_w, county, quarter_year)

cat('\n--- Implied wages (NO min-wage constraint) ---\n')
print(wages_w, row.names = FALSE, digits = 5)

saveRDS(list(coefs = coefs, wages = wages_w, final_reg = final_reg),
        'mkdata/data/smoke_no_minwage.rds')
cat('\nSaved smoke result to mkdata/data/smoke_no_minwage.rds\n')
