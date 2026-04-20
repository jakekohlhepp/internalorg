library('data.table')
set.seed(4459665)
working_data <- data.table(readRDS('mkdata/data/01_working.rds'))
source('preamble.R')
library('ivreg'); library('sandwich'); library('lmtest')

estim_matrix$location_id <- working_data$location_id
b_raw_cols <- names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]

# Build Hausman-style instrument: mean price in OTHER counties in the same quarter
em <- as.data.table(estim_matrix)
qmean <- em[, .(qy_mean = mean(cust_price), n = .N, qy_sum = sum(cust_price)),
            by = .(quarter_year, county)]
# For each row: leave-own-county-out mean across other counties in that quarter
qall <- em[, .(all_sum = sum(cust_price), all_n = .N), by = quarter_year]
em <- merge(em, qmean[, .(quarter_year, county, own_sum = qy_sum, own_n = n)],
            by = c("quarter_year", "county"))
em <- merge(em, qall, by = "quarter_year")
em[, price_other := (all_sum - own_sum) / (all_n - own_n)]
estim_matrix$price_other <- em$price_other[match(
  paste(estim_matrix$quarter_year, estim_matrix$county, estim_matrix$location_id),
  paste(em$quarter_year, em$county, em$location_id))]
# If match fails, do a simpler assignment: it's the same within (county, quarter)
if (any(is.na(estim_matrix$price_other))) {
  lookup <- unique(em[, .(quarter_year, county, price_other)])
  key <- paste(estim_matrix$quarter_year, estim_matrix$county)
  lkey <- paste(lookup$quarter_year, lookup$county)
  estim_matrix$price_other <- lookup$price_other[match(key, lkey)]
}

exog <- paste0("factor(county) + factor(quarter_year) + ",
               paste0("factor(county):avg_labor:", b_raw_cols, collapse = " + "))
f <- as.formula(paste0("log_rel_mkt ~ ", exog, " + factor(county):cust_price - 1 | ",
                       exog, " + factor(county):price_other - 1"))
iv <- ivreg(f, data = estim_matrix)
vc <- vcovCL(iv, cluster = estim_matrix$location_id, type = "HC1")
ct <- coeftest(iv, vcov. = vc)

cat("\n--- Spec: Q FE + proxy county FE, instr = county:price_other (Hausman) ---\n")
print(ct[grep("cust_price", rownames(ct)), , drop = FALSE])
cat("\nFirst-stage diagnostics:\n")
print(summary(iv, diagnostics = TRUE)$diagnostics)

# Quick look at variation in the instrument after quarter FE
cat("\nSD(price_other) overall and within quarter:\n")
cat(sprintf("  overall:         %10.4f\n", sd(estim_matrix$price_other)))
resid_po <- residuals(lm(price_other ~ factor(quarter_year), data = estim_matrix))
cat(sprintf("  after Q FE:      %10.4f\n", sd(resid_po)))
