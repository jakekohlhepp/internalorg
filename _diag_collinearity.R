#' Diagnose why including quarter_year=2018.1 dummies breaks 2SLS

library('data.table')
set.seed(4459665)
working_data <- data.table(readRDS('mkdata/data/01_working.rds'))
source('preamble.R')

b_raw_cols <- names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]

# ---- 1. Build both model matrices ----

# Manual formula (drops 2018.1 — the working version)
manual_fmla <- as.formula(paste0(
  "~factor(county):cust_price + factor(county):factor(quarter_year) + ",
  paste0("factor(county):avg_labor:", b_raw_cols, collapse = " + "), " - 1"
))
X_111 <- model.matrix(manual_fmla, data = estim_matrix)

# ivreg-style formula (includes 2018.1)
# Force all factor levels by building the interaction explicitly
county_f <- factor(estim_matrix$county)
qy_f <- factor(estim_matrix$quarter_year)
# Full interaction with all levels
full_fmla <- as.formula(paste0(
  "~factor(county):cust_price + county_f:qy_f + ",
  paste0("factor(county):avg_labor:", b_raw_cols, collapse = " + "), " - 1"
))
# Use contrasts that keep all levels
estim_matrix$county_f <- county_f
estim_matrix$qy_f <- qy_f
old_contrasts <- options("contrasts")
options(contrasts = c("contr.treatment", "contr.treatment"))
X_114 <- model.matrix(
  ~ county_f:qy_f - 1,
  data = estim_matrix,
  contrasts.arg = list(county_f = diag(nlevels(county_f)),
                       qy_f = diag(nlevels(qy_f)))
)
options(contrasts = old_contrasts[[1]])

# Actually, let's just use ivreg's own matrices since that's what matters
library('ivreg')
exog_terms <- c(
  "factor(county):factor(quarter_year)",
  paste0("factor(county):avg_labor:", b_raw_cols)
)
exog_str <- paste(exog_terms, collapse = " + ")

iv_formula <- as.formula(paste0(
  "log_rel_mkt ~ ", exog_str, " + factor(county):cust_price - 1",
  " | ",
  exog_str, " + factor(county):dye_instrument - 1"
))
iv_fit <- ivreg(iv_formula, data = estim_matrix)
X_iv <- model.matrix(iv_fit, component = "regressors")
Z_iv <- model.matrix(iv_fit, component = "instruments")

cat("=== DIMENSIONS ===\n")
cat("Manual X:  ", ncol(X_111), "cols\n")
cat("ivreg X:   ", ncol(X_iv), "cols\n")
cat("ivreg Z:   ", ncol(Z_iv), "cols\n")

# ---- 2. Rank analysis ----
cat("\n=== RANK ANALYSIS ===\n")
rank_111 <- qr(X_111)$rank
rank_iv_X <- qr(X_iv)$rank
rank_iv_Z <- qr(Z_iv)$rank

cat("Rank of manual X (111 cols):", rank_111, "\n")
cat("Rank of ivreg X (114 cols): ", rank_iv_X, "\n")
cat("Rank of ivreg Z (114 cols): ", rank_iv_Z, "\n")
cat("Rank deficiency in ivreg X: ", ncol(X_iv) - rank_iv_X, "\n")
cat("Rank deficiency in ivreg Z: ", ncol(Z_iv) - rank_iv_Z, "\n")

# ---- 3. Condition numbers ----
cat("\n=== CONDITION NUMBERS ===\n")
sv_111 <- svd(X_111)$d
sv_iv <- svd(X_iv)$d
cat("Manual X condition number:   ", format(max(sv_111)/min(sv_111), digits=6), "\n")
cat("ivreg X condition number:    ", format(max(sv_iv)/min(sv_iv), digits=6), "\n")
cat("\nManual X smallest 5 singular values:\n")
print(tail(sort(sv_111), 5))
cat("ivreg X smallest 5 singular values:\n")
print(tail(sort(sv_iv), 5))
cat("\nManual X LARGEST 5 singular values:\n")
print(head(sort(sv_111, decreasing=TRUE), 5))

# Actually show the tiniest SVs
cat("\nivreg X smallest 10 singular values:\n")
print(sort(sv_iv)[1:min(10, length(sv_iv))])
cat("Manual X smallest 10 singular values:\n")
print(sort(sv_111)[1:min(10, length(sv_111))])

# ---- 4. Check the extra 3 columns specifically ----
cat("\n=== EXTRA COLUMNS ANALYSIS ===\n")
extra_cols <- setdiff(colnames(X_iv), colnames(X_111))
cat("Extra columns in ivreg:\n")
print(extra_cols)

if (length(extra_cols) > 0) {
  # Are the extra columns linearly dependent on the others?
  shared_cols <- intersect(colnames(X_iv), colnames(X_111))
  X_shared <- X_iv[, shared_cols]

  for (col_name in extra_cols) {
    x_extra <- X_iv[, col_name]

    # Project extra column onto shared column space
    proj <- X_shared %*% solve(t(X_shared) %*% X_shared) %*% t(X_shared) %*% x_extra
    residual <- x_extra - proj
    resid_norm <- sqrt(sum(residual^2))
    col_norm <- sqrt(sum(x_extra^2))
    r_squared <- 1 - sum(residual^2) / sum((x_extra - mean(x_extra))^2)

    cat(sprintf("\n%s:\n", col_name))
    cat(sprintf("  Column norm:        %12.6f\n", col_norm))
    cat(sprintf("  Residual norm:      %12.6f\n", resid_norm))
    cat(sprintf("  Ratio (resid/col):  %12.8f\n", resid_norm / col_norm))
    cat(sprintf("  R-squared:          %12.10f\n", r_squared))
    if (r_squared > 0.9999) {
      cat("  >> NEAR-PERFECTLY explained by other columns!\n")
    }
  }
}

# ---- 5. What specifically explains the extra columns? ----
cat("\n=== WHAT EXPLAINS THE 2018.1 DUMMIES? ===\n")
cat("Checking whether county:quarter_year=2018.1 is a linear combination\n")
cat("of the other county:quarter_year dummies...\n\n")

# For each extra column, regress it on subsets of the other columns
# to understand what's driving the collinearity
for (col_name in extra_cols) {
  x_extra <- X_iv[, col_name]

  # Regress on just the other county-quarter FE columns
  qy_cols <- grep("factor\\(quarter_year\\)", colnames(X_iv), value = TRUE)
  qy_cols <- setdiff(qy_cols, extra_cols)  # other quarter dummies only
  X_qy <- X_iv[, qy_cols, drop = FALSE]
  r2_qy <- 1 - sum((x_extra - X_qy %*% solve(t(X_qy) %*% X_qy) %*% t(X_qy) %*% x_extra)^2) /
               sum((x_extra - mean(x_extra))^2)

  # Regress on just county:cust_price columns
  price_cols <- grep("cust_price", colnames(X_iv), value = TRUE)
  X_price <- X_iv[, price_cols, drop = FALSE]
  r2_price <- 1 - sum((x_extra - X_price %*% solve(t(X_price) %*% X_price) %*% t(X_price) %*% x_extra)^2) /
                 sum((x_extra - mean(x_extra))^2)

  # Regress on county:B_raw columns
  braw_cols <- grep("B_raw", colnames(X_iv), value = TRUE)
  X_braw <- X_iv[, braw_cols, drop = FALSE]
  r2_braw <- 1 - sum((x_extra - X_braw %*% solve(t(X_braw) %*% X_braw) %*% t(X_braw) %*% x_extra)^2) /
                sum((x_extra - mean(x_extra))^2)

  # Regress on other quarter FE + county:cust_price
  X_qy_price <- cbind(X_qy, X_price)
  r2_qy_price <- 1 - sum((x_extra - X_qy_price %*% solve(t(X_qy_price) %*% X_qy_price) %*% t(X_qy_price) %*% x_extra)^2) /
                     sum((x_extra - mean(x_extra))^2)

  cat(sprintf("%s:\n", col_name))
  cat(sprintf("  R2 from other county:quarter FE only:   %10.8f\n", r2_qy))
  cat(sprintf("  R2 from county:price only:              %10.8f\n", r2_price))
  cat(sprintf("  R2 from county:B_raw only:              %10.8f\n", r2_braw))
  cat(sprintf("  R2 from other county:quarter + price:   %10.8f\n", r2_qy_price))
}

# ---- 6. Check what the 2018.1 dummy IS ----
cat("\n=== STRUCTURE OF 2018.1 DUMMIES ===\n")
for (col_name in extra_cols) {
  x <- X_iv[, col_name]
  cat(sprintf("%s: sum = %d, n_nonzero = %d, n_obs = %d\n",
              col_name, sum(x), sum(x != 0), length(x)))
}
cat("\nFor comparison, a non-reference quarter dummy:\n")
ref_col <- grep("2018.2", colnames(X_iv), value = TRUE)[1]
x_ref <- X_iv[, ref_col]
cat(sprintf("%s: sum = %d, n_nonzero = %d, n_obs = %d\n",
            ref_col, sum(x_ref), sum(x_ref != 0), length(x_ref)))

# Sum of all quarter dummies for each county
cat("\n=== DO COUNTY QUARTER DUMMIES SUM TO COUNTY INDICATOR? ===\n")
for (cnty in c("17031", "36061", "6037")) {
  cnty_qy_cols <- grep(paste0("county.", cnty, ".*quarter_year\\|county_f", cnty, ".*qy_f"), colnames(X_iv), value = TRUE)
  if (length(cnty_qy_cols) == 0) {
    cnty_qy_cols <- grep(paste0(cnty, ".*quarter_year"), colnames(X_iv), value = TRUE)
  }
  if (length(cnty_qy_cols) > 0) {
    row_sums <- rowSums(X_iv[, cnty_qy_cols, drop = FALSE])
    cat(sprintf("County %s: %d quarter dummies, row sums = {%s}\n",
                cnty, length(cnty_qy_cols),
                paste(unique(row_sums), collapse = ", ")))
    # Is this sum equal to the county indicator?
    county_indicator <- as.numeric(estim_matrix$county == cnty)
    cat(sprintf("  Equals county indicator? %s\n",
                all(row_sums == county_indicator)))
  }
}

# ---- 7. The key insight: check B_raw row sums ----
cat("\n=== B_RAW ROW SUMS ===\n")
braw_mat <- as.matrix(estim_matrix[, grep("^B_raw_[0-9]_", colnames(estim_matrix))])
cat("B_raw columns:", ncol(braw_mat), "\n")
cat("Row sums (unique values):", paste(unique(round(rowSums(braw_mat), 8)), collapse = ", "), "\n")
cat("All row sums = 1?", all(abs(rowSums(braw_mat) - 1) < 1e-10), "\n")

if (all(abs(rowSums(braw_mat) - 1) < 1e-10)) {
  cat("\n>> B_raw rows sum to 1. This means:\n")
  cat("   sum_j [county_i * avg_labor * B_raw_j] = county_i * avg_labor\n")
  cat("   So county:avg_labor is a linear combination of the county:avg_labor:B_raw columns.\n")
  cat("   The county:quarter_year=2018.1 dummy (= county indicator for 2018.1 obs)\n")
  cat("   is NOT directly collinear, but adding it to an already nearly-collinear\n")
  cat("   system can push the condition number to extreme values.\n")
}
