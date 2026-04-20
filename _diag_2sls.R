#' Diagnostic: why do manual 2SLS and ivreg coefficients diverge?

library('data.table')
library('ivreg')
set.seed(4459665)
working_data <- data.table(readRDS('mkdata/data/01_working.rds'))
source('preamble.R')

# ---- 1. Rebuild the ivreg formula exactly as in 02_check_2sls.R ----
b_raw_cols <- names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]

exog_terms <- c(
  "factor(county):factor(quarter_year)",
  paste0("factor(county):avg_labor:", b_raw_cols)
)
exog_str <- paste(exog_terms, collapse = " + ")

demand_formula <- as.formula(paste0(
  "log_rel_mkt ~ ", exog_str, " + factor(county):cust_price - 1",
  " | ",
  exog_str, " + factor(county):dye_instrument - 1"
))

iv_fit <- ivreg(demand_formula, data = estim_matrix)

# ---- 2. Compare dimensions ----
cat("=== DIMENSION CHECK ===\n")
cat("Manual mm_1 (regressors): ", nrow(mm_1), "x", ncol(mm_1), "\n")
cat("Manual z_mm_1 (instruments):", nrow(z_mm_1), "x", ncol(z_mm_1), "\n")
cat("ivreg n coefficients:       ", length(coef(iv_fit)), "\n")
cat("Manual beta length:         ", length(beta), "\n")

# ---- 3. Check for intercept in ivreg ----
cat("\n=== INTERCEPT CHECK ===\n")
iv_names <- names(coef(iv_fit))
cat("'(Intercept)' in ivreg coefs:", "(Intercept)" %in% iv_names, "\n")

# ---- 4. Compare column names ----
cat("\n=== NAME COMPARISON ===\n")
manual_names <- rownames(beta)
cat("Manual names NOT in ivreg:\n")
print(setdiff(manual_names, iv_names))
cat("\nivreg names NOT in manual:\n")
print(setdiff(iv_names, manual_names))

# ---- 5. Extract ivreg internal matrices ----
# ivreg stores its model matrices; we can compare directly
iv_X <- model.matrix(iv_fit, component = "regressors")
iv_Z <- model.matrix(iv_fit, component = "instruments")
cat("\n=== ivreg INTERNAL MATRICES ===\n")
cat("ivreg regressors (X): ", nrow(iv_X), "x", ncol(iv_X), "\n")
cat("ivreg instruments (Z):", nrow(iv_Z), "x", ncol(iv_Z), "\n")

cat("\nivreg X colnames NOT in mm_1:\n")
print(setdiff(colnames(iv_X), colnames(mm_1)))
cat("\nmm_1 colnames NOT in ivreg X:\n")
print(setdiff(colnames(mm_1), colnames(iv_X)))

cat("\nivreg Z colnames NOT in z_mm_1:\n")
print(setdiff(colnames(iv_Z), colnames(z_mm_1)))
cat("\nz_mm_1 colnames NOT in ivreg Z:\n")
print(setdiff(colnames(z_mm_1), colnames(iv_Z)))

# ---- 6. If same columns, compare matrices directly ----
common_x <- intersect(colnames(iv_X), colnames(mm_1))
if (length(common_x) > 0) {
  max_diff_x <- max(abs(iv_X[, common_x] - mm_1[, common_x]))
  cat("\n=== MATRIX VALUE CHECK (common X cols) ===\n")
  cat("Max element-wise difference in X:", max_diff_x, "\n")
}

common_z <- intersect(colnames(iv_Z), colnames(z_mm_1))
if (length(common_z) > 0) {
  max_diff_z <- max(abs(iv_Z[, common_z] - z_mm_1[, common_z]))
  cat("Max element-wise difference in Z:", max_diff_z, "\n")
}

# ---- 7. Manually compute 2SLS from ivreg matrices ----
cat("\n=== MANUAL 2SLS FROM ivreg MATRICES ===\n")
ZtZ_inv_iv <- solve(t(iv_Z) %*% iv_Z)
proj_z_iv <- iv_Z %*% ZtZ_inv_iv %*% t(iv_Z)
beta_from_iv_mats <- solve(t(iv_X) %*% proj_z_iv %*% iv_X) %*%
                     (t(iv_X) %*% proj_z_iv %*% estim_matrix[, "log_rel_mkt"])

cat("Max diff (manual-from-ivreg-mats vs ivreg coefs):",
    max(abs(as.numeric(beta_from_iv_mats) - coef(iv_fit)[rownames(beta_from_iv_mats)])), "\n")
cat("Max diff (manual-from-ivreg-mats vs original manual beta):",
    max(abs(as.numeric(beta_from_iv_mats) - as.numeric(beta)[match(rownames(beta_from_iv_mats), rownames(beta))])), "\n")

# ---- 8. If matrices differ, show where ----
if (ncol(iv_X) != ncol(mm_1) || ncol(iv_Z) != ncol(z_mm_1)) {
  cat("\n=== COLUMN COUNT MISMATCH - showing first/last cols ===\n")
  cat("\nFirst 5 mm_1 cols:\n"); print(head(colnames(mm_1), 5))
  cat("Last 5 mm_1 cols:\n"); print(tail(colnames(mm_1), 5))
  cat("\nFirst 5 iv_X cols:\n"); print(head(colnames(iv_X), 5))
  cat("Last 5 iv_X cols:\n"); print(tail(colnames(iv_X), 5))

  cat("\nFirst 5 z_mm_1 cols:\n"); print(head(colnames(z_mm_1), 5))
  cat("Last 5 z_mm_1 cols:\n"); print(tail(colnames(z_mm_1), 5))
  cat("\nFirst 5 iv_Z cols:\n"); print(head(colnames(iv_Z), 5))
  cat("Last 5 iv_Z cols:\n"); print(tail(colnames(iv_Z), 5))
}

# ---- 9. Show a few coefficient values side by side ----
cat("\n=== COEFFICIENT COMPARISON (first 10) ===\n")
cat(sprintf("%-55s %15s %15s\n", "Name", "Manual", "ivreg"))
cat(paste(rep("-", 85), collapse=""), "\n")
for (i in 1:min(10, length(manual_names))) {
  nm <- manual_names[i]
  iv_val <- if (nm %in% iv_names) coef(iv_fit)[nm] else NA
  cat(sprintf("%-55s %15.8f %15s\n", nm, as.numeric(beta[i]),
              ifelse(is.na(iv_val), "NOT FOUND", sprintf("%15.8f", iv_val))))
}
