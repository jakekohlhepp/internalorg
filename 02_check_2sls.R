#' =============================================================================
#' 2SLS REPLICATION AND NESTED LOGIT EXTENSION
#' =============================================================================
#' Part A: Replicate manual 2SLS (standard logit) using ivreg
#' Part B: Nested logit with outside good in separate nest
#'
#' Both use clustered standard errors at the location_id level.
#'
#' Uses ivreg default factor encoding (all quarter_year levels included).
#'
#' Inputs:  Objects created by preamble.R
#' Outputs: Console tables; results/out/tables/02_2sls_*.tex
#' =============================================================================

library('data.table')
set.seed(4459665)
working_data <- data.table(readRDS('mkdata/data/01_working.rds'))

## setup the data and main functions
source('preamble.R')

if (!requireNamespace("ivreg", quietly = TRUE)) {
  stop("Package 'ivreg' is required. Install with: install.packages('ivreg')")
}
library('ivreg')
library('sandwich')
library('lmtest')
library('stargazer')


#' =============================================================================
#' SHARED FORMULA COMPONENTS
#' =============================================================================

b_raw_cols <- names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]

# Add location_id for clustering
estim_matrix$location_id <- working_data$location_id

# Exogenous terms — use factor(quarter_year) directly (ivreg default encoding)
exog_terms <- c(
  "factor(county):factor(quarter_year)",
  paste0("factor(county):", b_raw_cols)
)
exog_str <- paste(exog_terms, collapse = " + ")


#' =============================================================================
#' PART A: STANDARD LOGIT (REPLICATE MANUAL 2SLS)
#' =============================================================================

cat("\n")
cat("*************************************************************************\n")
cat("*  PART A: STANDARD LOGIT DEMAND (ivreg)                              *\n")
cat("*************************************************************************\n")

demand_formula_std <- as.formula(paste0(
  "log_rel_mkt ~ ", exog_str, " + factor(county):cust_price - 1",
  " | ",
  exog_str, " + factor(county):dye_instrument - 1"
))

cat("Fitting standard logit via ivreg...\n")
iv_std <- ivreg(demand_formula_std, data = estim_matrix)
cat("Done.\n")

# Clustered SEs at location_id level
clust_vcov_std <- vcovCL(iv_std, cluster = estim_matrix$location_id, type = "HC1")
ct_std <- coeftest(iv_std, vcov. = clust_vcov_std)

# Price coefficients
price_rows <- grep("cust_price", rownames(ct_std))
cat("\n--- Standard Logit: Price Coefficients (clustered SEs) ---\n")
cat(sprintf("%-50s %12s %12s %10s %10s\n",
            "Coefficient", "Estimate", "Cluster SE", "t-stat", "p-value"))
cat(paste(rep("-", 94), collapse = ""), "\n")
for (i in price_rows) {
  stars <- ifelse(ct_std[i, 4] < 0.01, "***",
           ifelse(ct_std[i, 4] < 0.05, "**",
           ifelse(ct_std[i, 4] < 0.1, "*", "")))
  cat(sprintf("%-50s %12.6f %12.6f %10.4f %10.4f %s\n",
              rownames(ct_std)[i], ct_std[i, 1], ct_std[i, 2],
              ct_std[i, 3], ct_std[i, 4], stars))
}


#' =============================================================================
#' PART B: NESTED LOGIT (outside good in separate nest, county-specific sigma)
#' =============================================================================
#' Berry (1994) nested logit inversion:
#'   log(s_j/s_0) = alpha_c * p_j + sigma_c * log(s_{j|g}) + x'beta + xi_j
#'
#' where s_{j|g} = s_j / sum_k(s_k) is within-group share (inside nest).
#' sigma_c in [0,1) is a county-specific nesting parameter.
#'
#' Endogenous: county:cust_price (3), county:log_within_share (3) = 6 total
#' Instruments: county:dye_instrument (3), county:org_cost (3) = 6 excluded
#' =============================================================================

cat("\n\n")
cat("*************************************************************************\n")
cat("*  PART B: NESTED LOGIT DEMAND (county-specific sigma)                 *\n")
cat("*************************************************************************\n")

#' ---
#' Construct within-group share
#' ---
# Within-group share: s_{j|g} = s_j / sum_k(s_k) by county-quarter
# Using cust_count shares: equivalent to s_j/(1-s_0) up to county-quarter FE
working_data[, total_inside_share := sum(salon_share_subdiv),
             by = .(county, quarter_year)]
working_data[, within_share := salon_share_subdiv / total_inside_share]
working_data[, log_within_share := log(within_share)]

# Add to estim_matrix
estim_matrix$log_within_share <- working_data$log_within_share

cat("\nWithin-share summary:\n")
cat(sprintf("  Min:    %10.6f\n", min(estim_matrix$log_within_share)))
cat(sprintf("  Median: %10.6f\n", median(estim_matrix$log_within_share)))
cat(sprintf("  Max:    %10.6f\n", max(estim_matrix$log_within_share)))

#' ---
#' Nested logit 2SLS with county-specific sigma
#' ---
#' Endogenous: factor(county):cust_price, factor(county):log_within_share
#' Excluded instruments: factor(county):dye_instrument, factor(county):org_cost
#' (exactly identified: 6 endogenous, 6 excluded instruments)
#' ---

demand_formula_nested <- as.formula(paste0(
  "log_rel_mkt ~ ", exog_str,
  " + factor(county):cust_price + factor(county):log_within_share - 1",
  " | ",
  exog_str,
  " + factor(county):dye_instrument - 1"
))

cat("\nFitting nested logit (county-specific sigma) via ivreg...\n")
iv_nested <- ivreg(demand_formula_nested, data = estim_matrix)
cat("Done.\n")

# Clustered SEs
clust_vcov_nested <- vcovCL(iv_nested, cluster = estim_matrix$location_id, type = "HC1")
ct_nested <- coeftest(iv_nested, vcov. = clust_vcov_nested)

# Nesting parameters (county-specific sigma)
sigma_rows <- grep("log_within_share", rownames(ct_nested))
cat("\n--- Nesting Parameters (county-specific sigma) ---\n")
cat(sprintf("%-55s %12s %12s %10s %10s\n",
            "Coefficient", "Estimate", "Cluster SE", "t-stat", "p-value"))
cat(paste(rep("-", 99), collapse = ""), "\n")
for (i in sigma_rows) {
  stars <- ifelse(ct_nested[i, 4] < 0.01, "***",
           ifelse(ct_nested[i, 4] < 0.05, "**",
           ifelse(ct_nested[i, 4] < 0.1, "*", "")))
  cat(sprintf("%-55s %12.6f %12.6f %10.4f %10.4f %s\n",
              rownames(ct_nested)[i], ct_nested[i, 1], ct_nested[i, 2],
              ct_nested[i, 3], ct_nested[i, 4], stars))

  sigma_hat <- ct_nested[i, 1]
  if (sigma_hat >= 0 && sigma_hat < 1) {
    cat("    -> sigma in [0, 1): consistent with RUM.\n")
  } else if (sigma_hat < 0) {
    cat("    -> WARNING: sigma < 0 (misspecification or weak instruments).\n")
  } else {
    cat("    -> WARNING: sigma >= 1 (inconsistent with RUM).\n")
  }
}

# Price coefficients
price_rows_n <- grep("cust_price", rownames(ct_nested))
cat("\n--- Nested Logit: Price Coefficients (clustered SEs) ---\n")
cat(sprintf("%-55s %12s %12s %10s %10s\n",
            "Coefficient", "Estimate", "Cluster SE", "t-stat", "p-value"))
cat(paste(rep("-", 99), collapse = ""), "\n")
for (i in price_rows_n) {
  stars <- ifelse(ct_nested[i, 4] < 0.01, "***",
           ifelse(ct_nested[i, 4] < 0.05, "**",
           ifelse(ct_nested[i, 4] < 0.1, "*", "")))
  cat(sprintf("%-55s %12.6f %12.6f %10.4f %10.4f %s\n",
              rownames(ct_nested)[i], ct_nested[i, 1], ct_nested[i, 2],
              ct_nested[i, 3], ct_nested[i, 4], stars))
}


#' =============================================================================
#' PART C: SUPPLY EQUATION (OLS, clustered SEs)
#' =============================================================================

cat("\n\n")
cat("*************************************************************************\n")
cat("*  SUPPLY EQUATION: OLS on Adjusted Price                              *\n")
cat("*************************************************************************\n")

# p_adj uses the standard logit ivreg demand estimates
beta_iv_std <- coef(iv_std)
p_adj <- estim_matrix[, "cust_price"]
for (cnty in CONFIG$counties) {
  price_coef <- beta_iv_std[grep(paste0(cnty, ":cust_price"), names(beta_iv_std))]
  p_adj <- p_adj + estim_matrix[, "mk_piece"] * (1 / price_coef) *
    (estim_matrix$county == cnty)
}

# Supply formula using ivreg default factor encoding
task_mix_cols <- names(working_data)[grep("^task_mix", names(working_data))]
e_raw_cols   <- names(working_data)[grep("^E_raw_[0-9]", names(working_data))]

supply_terms <- c(
  "factor(county):org_cost",
  paste0("factor(quarter_year):", task_mix_cols),
  paste0("factor(county):avg_labor:", e_raw_cols),
  "factor(county):factor(quarter_year):avg_labor",
  "factor(county):factor(quarter_year)"
)
supply_formula <- as.formula(paste0(
  "p_adj ~ ", paste(supply_terms, collapse = " + "), " - 1"
))

estim_df <- as.data.frame(estim_matrix)
estim_df$p_adj <- p_adj
ols_supply <- lm(supply_formula, data = estim_df)

# Clustered SEs
clust_vcov_supply <- vcovCL(ols_supply, cluster = estim_df$location_id, type = "HC1")
ct_supply <- coeftest(ols_supply, vcov. = clust_vcov_supply)

# Org cost and worker type coefficients
org_rows <- grep("org_cost", rownames(ct_supply))
e_rows <- grep("E_raw", rownames(ct_supply))

cat("\n--- Org Cost + Worker Type Coefficients (clustered SEs) ---\n")
cat(sprintf("%-55s %12s %12s %10s %10s\n",
            "Coefficient", "Estimate", "Cluster SE", "t-stat", "p-value"))
cat(paste(rep("-", 99), collapse = ""), "\n")
for (i in c(org_rows, e_rows)) {
  stars <- ifelse(ct_supply[i, 4] < 0.01, "***",
           ifelse(ct_supply[i, 4] < 0.05, "**",
           ifelse(ct_supply[i, 4] < 0.1, "*", "")))
  cat(sprintf("%-55s %12.6f %12.6f %10.4f %10.4f %s\n",
              rownames(ct_supply)[i], ct_supply[i, 1], ct_supply[i, 2],
              ct_supply[i, 3], ct_supply[i, 4], stars))
}


#' =============================================================================
#' PART D: SIDE-BY-SIDE COMPARISON (Standard vs Nested Logit)
#' =============================================================================

cat("\n\n")
cat("*************************************************************************\n")
cat("*  COMPARISON: Standard Logit vs Nested Logit                          *\n")
cat("*************************************************************************\n\n")

cat(sprintf("%-50s %15s %15s\n", "", "Standard Logit", "Nested Logit"))
cat(paste(rep("-", 80), collapse = ""), "\n")

# County-specific nesting parameters
for (cnty in CONFIG$counties) {
  pat <- paste0(cnty, ":log_within_share")
  i_nes <- grep(pat, rownames(ct_nested))
  if (length(i_nes) == 1) {
    cat(sprintf("%-50s %15s %12.6f\n",
                paste0("sigma (county ", cnty, ")"), "---",
                ct_nested[i_nes, 1]))
    cat(sprintf("%-50s %15s (%10.6f)\n", "", "",
                ct_nested[i_nes, 2]))
  }
}

# Price coefficients
for (cnty in CONFIG$counties) {
  pat <- paste0(cnty, ":cust_price")
  i_std <- grep(pat, rownames(ct_std))
  i_nes <- grep(pat, rownames(ct_nested))
  if (length(i_std) == 1 && length(i_nes) == 1) {
    cat(sprintf("%-50s %12.6f    %12.6f\n",
                paste0("Price (county ", cnty, ")"),
                ct_std[i_std, 1], ct_nested[i_nes, 1]))
    cat(sprintf("%-50s (%10.6f)    (%10.6f)\n", "",
                ct_std[i_std, 2], ct_nested[i_nes, 2]))
  }
}

cat(paste(rep("-", 80), collapse = ""), "\n")
cat("Clustered SEs in parentheses (location_id level)\n")
cat("Instruments: county:dye_instrument + county:org_cost (nested logit)\n")
cat("Nested logit: exactly identified (6 endo, 6 excluded instruments)\n")


#' =============================================================================
#' PART E: FIRST-STAGE DIAGNOSTICS
#' =============================================================================

cat("\n\n")
cat("*************************************************************************\n")
cat("*  FIRST-STAGE DIAGNOSTICS                                             *\n")
cat("*************************************************************************\n")

# Standard logit diagnostics
cat("\n--- Standard Logit ---\n")
std_diag <- summary(iv_std, diagnostics = TRUE)$diagnostics
print(std_diag)

# Nested logit diagnostics
cat("\n--- Nested Logit ---\n")
nested_diag <- summary(iv_nested, diagnostics = TRUE)$diagnostics
print(nested_diag)


#' =============================================================================
#' PART F: EXPORT LATEX TABLES
#' =============================================================================

ensure_directory("results/out/tables")

# Standard logit table with clustered SEs
stargazer(iv_std,
          type = "latex",
          title = "Demand Equation: Standard Logit 2SLS",
          label = "tab:demand_std",
          dep.var.labels = "log(Share / Outside Share)",
          se = list(sqrt(diag(clust_vcov_std))),
          out = "results/out/tables/02_2sls_demand_std.tex",
          no.space = TRUE,
          notes = "Clustered SEs at location level. Instrument: dye task x PPI.")

# Nested logit table with clustered SEs
stargazer(iv_nested,
          type = "latex",
          title = "Demand Equation: Nested Logit 2SLS",
          label = "tab:demand_nested",
          dep.var.labels = "log(Share / Outside Share)",
          se = list(sqrt(diag(clust_vcov_nested))),
          out = "results/out/tables/02_2sls_demand_nested.tex",
          no.space = TRUE,
          notes = "Clustered SEs at location level. Instruments: dye task x PPI, org cost.")

# Supply table with clustered SEs
stargazer(ols_supply,
          type = "latex",
          title = "Supply Equation: OLS on Adjusted Price",
          label = "tab:supply_ols",
          dep.var.labels = "Adjusted Price",
          se = list(sqrt(diag(clust_vcov_supply))),
          out = "results/out/tables/02_2sls_supply.tex",
          no.space = TRUE,
          notes = "Clustered SEs at location level.")

cat("\n\nLaTeX tables saved to:\n")
cat("  results/out/tables/02_2sls_demand_std.tex\n")
cat("  results/out/tables/02_2sls_demand_nested.tex\n")
cat("  results/out/tables/02_2sls_supply.tex\n")

cat("\n=========================================================================\n")
cat("2SLS estimation complete.\n")
cat("=========================================================================\n")
