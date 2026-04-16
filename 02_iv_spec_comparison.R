#' =============================================================================
#' DEMAND IV SPECIFICATION COMPARISON TABLES
#' =============================================================================
#' Produces side-by-side LaTeX tables comparing alternative fixed-effect
#' parameterizations for the 2SLS demand equation. Two demand models are fit
#' under each specification:
#'
#'   (A) Standard logit:
#'         log(s / s_0) = alpha_c * p + controls + xi
#'       Endogenous: factor(county):cust_price
#'       Instrument: factor(county):dye_instrument  (task_mix_2 * ppi_inputs)
#'
#'   (B) Nested logit (single inside nest containing all salons; the outside
#'       option is "no salon visit"):
#'         log(s / s_0) = alpha_c * p + sigma_c * log(s_{j|g}) + controls + xi
#'       where s_{j|g} = salon_share_subdiv / (1 - outside_share).
#'       Endogenous: factor(county):cust_price, factor(county):log_within_share
#'       Instruments: factor(county):dye_instrument,
#'                    factor(county):hausman_other_price  (leave-own-county-out
#'                    average customer price in the same quarter).
#'
#' Each column of the output table corresponds to a different FE parameterization:
#'
#'   1. "Full CxQ"               Fully saturated county x quarter FE (ivreg
#'                               default). Typically absorbs the instrument's
#'                               identifying variation.
#'   2. "Manual"                 County x quarter FE with the three
#'                               county:2018.1 cells omitted by hand, recovering
#'                               the original manual 2SLS spec in preamble.R.
#'                               Imposes that county-quarter FE are equal
#'                               across counties in 2018.1.
#'   3. "Quarter only; omit 2018.1"
#'                               Additive quarter FE (one reference quarter
#'                               omitted), no county FE, no county x quarter
#'                               interaction. County levels absorbed only by
#'                               the avg_labor x B_raw cost controls.
#'   4. "Quarter only; all qtrs"
#'                               Additive quarter FE including ALL quarters,
#'                               with no constant (the "- 1" on the formula
#'                               suppresses the intercept). Keeps every quarter
#'                               dummy in the design matrix.
#'   5. "County + quarter"       Additive county FE + additive quarter FE
#'                               (one reference quarter omitted).
#'
#' All specifications include county-specific skill-weighted labor controls
#' factor(county):avg_labor:B_raw_j as exogenous cost shifters and estimate
#' county-specific price coefficients. Standard errors are clustered at the
#' location_id level.
#'
#' Inputs:
#'   - mkdata/data/01_working.rds
#'   - Objects created by preamble.R (estim_matrix, CONFIG, counties list, ...)
#'
#' Outputs:
#'   - results/out/tables/02_demand_iv_spec_comparison.tex
#'   - results/out/tables/02_nested_iv_spec_comparison.tex
#' =============================================================================

library("data.table")
set.seed(4459665)
working_data <- data.table(readRDS("mkdata/data/01_working.rds"))

source("preamble.R")

required_packages <- c("ivreg", "sandwich", "lmtest")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "),
       "\nInstall them before running 02_iv_spec_comparison.R.")
}

library("ivreg")
library("sandwich")
library("lmtest")

ensure_directory("results/out/tables")

estim_matrix$location_id <- working_data$location_id

counties <- sort(unique(as.character(estim_matrix$county)))
b_raw_cols <- names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]
qy_levels <- sort(unique(estim_matrix$quarter_year))
ref_quarter <- as.character(qy_levels[1])
non_ref_quarters <- qy_levels[-1]

for (q in non_ref_quarters) {
  qy_name <- paste0("qy_", gsub("\\.", "_", as.character(q)))
  estim_matrix[[qy_name]] <- as.numeric(estim_matrix$quarter_year == q)
}
qy_dummy_names <- paste0("qy_", gsub("\\.", "_", as.character(non_ref_quarters)))
county_qy_terms <- as.vector(outer(paste0("factor(county):"), qy_dummy_names, paste0))

# -----------------------------------------------------------------------------
# Specification definitions
# -----------------------------------------------------------------------------
# Each entry defines one column of the output tables:
#   name       -> internal identifier used to key results
#   label      -> column header in the LaTeX table
#   fe_label   -> "Fixed effects" row entry
#   exog_terms -> RHS terms that appear on BOTH sides of the ivreg formula
#                 (standard + nested). The endogenous price (and within-share)
#                 regressors and instruments are appended by the formula
#                 builders below.
# See the file header for a description of each specification's identifying
# assumptions and what it imposes on the county-quarter FE structure.
spec_definitions <- list(
  list(
    name = "full_ivreg",
    label = "Full CxQ",
    fe_label = "County$\\times$quarter",
    exog_terms = c(
      "factor(county):factor(quarter_year)",
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  ),
  list(
    name = "manual",
    label = "Manual",
    fe_label = "County$\\times$quarter; omit 2018.1",
    exog_terms = c(
      county_qy_terms,
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  ),
  list(
    name = "quarter_only",
    label = "Quarter only; omit 2018.1",
    fe_label = "Quarter FE",
    exog_terms = c(
      unique(str_replace(county_qy_terms, "factor\\(county\\):","")),
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  ),
  list(
    name = "quarter_only_all",
    label = "Quarter only; all qtrs",
    fe_label = "All quarter FE",
    exog_terms = c(
      "factor(quarter_year)",
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  ),
  list(
    name = "county_plus_quarter",
    label = "County + quarter",
    fe_label = "County + quarter",
    exog_terms = c(
      "factor(county)",
      qy_dummy_names,
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  )
)

spec_order <- vapply(spec_definitions, `[[`, character(1), "name")
spec_labels <- setNames(vapply(spec_definitions, `[[`, character(1), "label"), spec_order)
spec_fe_labels <- setNames(vapply(spec_definitions, `[[`, character(1), "fe_label"), spec_order)
spec_terms <- setNames(lapply(spec_definitions, `[[`, "exog_terms"), spec_order)

# -----------------------------------------------------------------------------
# Shared helpers
# -----------------------------------------------------------------------------

build_standard_formula <- function(exog_terms) {
  exog_str <- paste(exog_terms, collapse = " + ")
  as.formula(paste0(
    "log_rel_mkt ~ ", exog_str, " + factor(county):cust_price - 1",
    " | ",
    exog_str, " + factor(county):dye_instrument - 1"
  ))
}

build_nested_formula <- function(exog_terms) {
  exog_str <- paste(exog_terms, collapse = " + ")
  as.formula(paste0(
    "log_rel_mkt ~ ", exog_str,
    " + factor(county):cust_price + factor(county):log_within_share - 1",
    " | ",
    exog_str,
    " + factor(county):dye_instrument + factor(county):hausman_other_price - 1"
  ))
}

scaled_xpzx_condition <- function(fit) {
  x_mat <- model.matrix(fit, component = "regressors")
  z_mat <- model.matrix(fit, component = "instruments")

  x_norms <- sqrt(colSums(x_mat^2))
  z_norms <- sqrt(colSums(z_mat^2))

  x_scaled <- sweep(x_mat, 2, x_norms, "/")
  z_scaled <- sweep(z_mat, 2, z_norms, "/")

  qr_z <- qr(z_scaled)
  q_mat <- qr.Q(qr_z, complete = FALSE)
  q_mat <- q_mat[, seq_len(qr_z$rank), drop = FALSE]
  qx <- crossprod(q_mat, x_scaled)
  xpzx <- crossprod(qx)
  sing_vals <- svd(xpzx, nu = 0, nv = 0)$d

  if (length(sing_vals) == 0 || min(sing_vals) <= .Machine$double.eps) {
    return(Inf)
  }

  max(sing_vals) / min(sing_vals)
}

extract_county_rows <- function(coef_test, suffix) {
  rbindlist(lapply(counties, function(cnty) {
    coef_name <- paste0("factor(county)", cnty, ":", suffix)
    coef_idx <- match(coef_name, rownames(coef_test))
    if (is.na(coef_idx)) {
      data.table(county = cnty, estimate = NA_real_, se = NA_real_, p_value = NA_real_)
    } else {
      data.table(
        county = cnty,
        estimate = unname(coef_test[coef_idx, 1]),
        se = unname(coef_test[coef_idx, 2]),
        p_value = unname(coef_test[coef_idx, 4])
      )
    }
  }))
}

safe_diag_value <- function(diagnostics, row_name, column_name) {
  if (!row_name %in% rownames(diagnostics)) {
    return(NA_real_)
  }
  unname(diagnostics[row_name, column_name])
}

estimate_stars <- function(p_value) {
  if (is.na(p_value)) {
    return("")
  }
  if (p_value < 0.01) {
    return("***")
  }
  if (p_value < 0.05) {
    return("**")
  }
  if (p_value < 0.1) {
    return("*")
  }
  ""
}

format_estimate <- function(estimate, p_value) {
  if (is.na(estimate)) {
    return("--")
  }
  paste0(sprintf("%.4f", estimate), estimate_stars(p_value))
}

format_se <- function(se) {
  if (is.na(se)) {
    return("(--)")
  }
  sprintf("(%.4f)", se)
}

format_stat <- function(x, digits = 2) {
  if (is.na(x)) {
    return("--")
  }
  sprintf(paste0("%.", digits, "f"), x)
}

format_p_value <- function(p_value) {
  if (is.na(p_value)) {
    return("--")
  }
  if (p_value < 1e-4) {
    return("$<$0.0001")
  }
  sprintf("%.4f", p_value)
}

format_int <- function(x) {
  if (is.na(x)) {
    return("--")
  }
  prettyNum(as.integer(round(x)), big.mark = ",", scientific = FALSE)
}

table_row <- function(label, values) {
  paste0(label, " & ", paste(values, collapse = " & "), " \\\\")
}

# -----------------------------------------------------------------------------
# Nested-logit variables and Hausman instrument
# -----------------------------------------------------------------------------

working_data[, row_id := .I]
## warning: because not all salons are sampled, within share should be 1-outside option share.
working_data[, within_share := salon_share_subdiv / (1-outside_share) ]
working_data[, log_within_share := log(within_share)]

quarter_totals <- working_data[, .(
  quarter_sum_price = sum(cust_price),
  quarter_n = .N
), by = quarter_year]
county_quarter_totals <- working_data[, .(
  county_q_sum_price = sum(cust_price),
  county_q_n = .N
), by = .(county, quarter_year)]

working_data <- merge(working_data, quarter_totals,
                      by = "quarter_year", all.x = TRUE, sort = FALSE)
working_data <- merge(working_data, county_quarter_totals,
                      by = c("county", "quarter_year"), all.x = TRUE, sort = FALSE)
setorder(working_data, row_id)

stopifnot(all(working_data$quarter_n > working_data$county_q_n))
working_data[, hausman_other_price :=
               (quarter_sum_price - county_q_sum_price) / (quarter_n - county_q_n)]
stopifnot(all(is.finite(working_data$log_within_share)))
stopifnot(all(is.finite(working_data$hausman_other_price)))

estim_matrix$log_within_share <- working_data$log_within_share
estim_matrix$hausman_other_price <- working_data$hausman_other_price

# -----------------------------------------------------------------------------
# Standard logit comparison table
# -----------------------------------------------------------------------------

fit_standard_spec <- function(spec_name) {
  fit <- ivreg(build_standard_formula(spec_terms[[spec_name]]), data = estim_matrix)
  clust_vcov <- vcovCL(fit, cluster = estim_matrix$location_id, type = "HC1")
  coef_test <- coeftest(fit, vcov. = clust_vcov)
  diagnostics <- summary(fit, diagnostics = TRUE)$diagnostics

  list(
    fit = fit,
    prices = extract_county_rows(coef_test, "cust_price"),
    diagnostics = diagnostics,
    condition_number = scaled_xpzx_condition(fit),
    adj_r2 = summary(fit)$adj.r.squared
  )
}

standard_results <- setNames(lapply(spec_order, fit_standard_spec), spec_order)

standard_header_values <- unname(spec_labels[spec_order])
standard_fe_values <- unname(spec_fe_labels[spec_order])

standard_table_lines <- c(
  "\\begin{table}[!htbp] \\centering",
  "  \\caption{Comparison of Demand IV Specifications}",
  "  \\label{tab:demand_iv_spec_comparison}",
  "  \\begin{tabular}{lccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  table_row("", standard_header_values),
  "\\hline \\\\[-1.8ex]",
  table_row("Fixed effects", standard_fe_values)
)

for (cnty in counties) {
  estimate_cells <- vapply(spec_order, function(spec_name) {
    row <- standard_results[[spec_name]]$prices[county == cnty]
    format_estimate(row$estimate, row$p_value)
  }, character(1))

  se_cells <- vapply(spec_order, function(spec_name) {
    row <- standard_results[[spec_name]]$prices[county == cnty]
    format_se(row$se)
  }, character(1))

  standard_table_lines <- c(
    standard_table_lines,
    table_row(paste0("Price (county ", cnty, ")"), estimate_cells),
    table_row("", se_cells)
  )
}

weak_iv_row_name <- function(cnty, suffix) {
  paste0("Weak instruments (factor(county)", cnty, ":", suffix, ")")
}

standard_weak_iv_rows <- vapply(counties, function(cnty) {
  values <- vapply(spec_order, function(spec_name) {
    format_stat(safe_diag_value(
      standard_results[[spec_name]]$diagnostics,
      weak_iv_row_name(cnty, "cust_price"),
      "statistic"
    ))
  }, character(1))
  table_row(paste0("Weak-IV F (county ", cnty, ")"), values)
}, character(1))

standard_wu_p_values <- vapply(spec_order, function(spec_name) {
  format_p_value(safe_diag_value(standard_results[[spec_name]]$diagnostics,
                                 "Wu-Hausman", "p-value"))
}, character(1))

standard_cond_values <- vapply(spec_order, function(spec_name) {
  format_int(standard_results[[spec_name]]$condition_number)
}, character(1))

standard_adj_r2_values <- vapply(spec_order, function(spec_name) {
  format_stat(standard_results[[spec_name]]$adj_r2, digits = 4)
}, character(1))

standard_obs_values <- vapply(spec_order, function(spec_name) {
  format_int(nobs(standard_results[[spec_name]]$fit))
}, character(1))

standard_table_lines <- c(
  standard_table_lines,
  standard_weak_iv_rows,
  table_row("Wu-Hausman $p$-value", standard_wu_p_values),
  table_row("Scaled $X'P_ZX$ cond.", standard_cond_values),
  table_row("Adj. $R^2$", standard_adj_r2_values),
  table_row("Observations", standard_obs_values),
  "\\hline \\\\[-1.8ex]",
  paste0(
    "\\multicolumn{6}{p{0.92\\linewidth}}{\\footnotesize Notes: Entries are ",
    "2SLS price coefficients with location-level clustered standard errors in ",
    "parentheses. All five specifications use county-specific dye instruments ",
    "and county-specific $avg\\_labor \\times B\\_raw$ controls. The manual ",
    "specification omits the county-specific ", ref_quarter,
    " quarter indicators; the quarter-only omitted-quarter and county-plus-quarter ",
    "specifications omit ", ref_quarter,
    " as the common quarter reference; the all-quarter quarter-only specification ",
    "includes all quarter fixed effects. Weak-IV rows report the diagnostic ",
    "statistics from \\texttt{summary(ivreg, diagnostics = TRUE)}.}\\\\"
  ),
  "\\end{tabular}",
  "\\end{table}"
)

standard_output_path <- "results/out/tables/02_demand_iv_spec_comparison.tex"
writeLines(standard_table_lines, standard_output_path)

# -----------------------------------------------------------------------------
# Nested logit comparison table
# -----------------------------------------------------------------------------

fit_nested_spec <- function(spec_name) {
  fit <- ivreg(build_nested_formula(spec_terms[[spec_name]]), data = estim_matrix)
  clust_vcov <- vcovCL(fit, cluster = estim_matrix$location_id, type = "HC1")
  coef_test <- coeftest(fit, vcov. = clust_vcov)
  diagnostics <- summary(fit, diagnostics = TRUE)$diagnostics

  list(
    fit = fit,
    prices = extract_county_rows(coef_test, "cust_price"),
    sigmas = extract_county_rows(coef_test, "log_within_share"),
    diagnostics = diagnostics,
    condition_number = scaled_xpzx_condition(fit),
    adj_r2 = summary(fit)$adj.r.squared
  )
}

nested_results <- setNames(lapply(spec_order, fit_nested_spec), spec_order)

nested_header_values <- unname(spec_labels[spec_order])
nested_fe_values <- unname(spec_fe_labels[spec_order])

nested_table_lines <- c(
  "\\begin{table}[!htbp] \\centering",
  "  \\caption{Comparison of Nested Logit IV Specifications}",
  "  \\label{tab:nested_iv_spec_comparison}",
  "  \\begin{tabular}{lccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  table_row("", nested_header_values),
  "\\hline \\\\[-1.8ex]",
  table_row("Fixed effects", nested_fe_values)
)

for (cnty in counties) {
  price_cells <- vapply(spec_order, function(spec_name) {
    row <- nested_results[[spec_name]]$prices[county == cnty]
    format_estimate(row$estimate, row$p_value)
  }, character(1))
  price_se_cells <- vapply(spec_order, function(spec_name) {
    row <- nested_results[[spec_name]]$prices[county == cnty]
    format_se(row$se)
  }, character(1))
  sigma_cells <- vapply(spec_order, function(spec_name) {
    row <- nested_results[[spec_name]]$sigmas[county == cnty]
    format_estimate(row$estimate, row$p_value)
  }, character(1))
  sigma_se_cells <- vapply(spec_order, function(spec_name) {
    row <- nested_results[[spec_name]]$sigmas[county == cnty]
    format_se(row$se)
  }, character(1))

  nested_table_lines <- c(
    nested_table_lines,
    table_row(paste0("Price (county ", cnty, ")"), price_cells),
    table_row("", price_se_cells),
    table_row(paste0("Sigma (county ", cnty, ")"), sigma_cells),
    table_row("", sigma_se_cells)
  )
}

nested_weak_price_rows <- vapply(counties, function(cnty) {
  values <- vapply(spec_order, function(spec_name) {
    format_stat(safe_diag_value(
      nested_results[[spec_name]]$diagnostics,
      weak_iv_row_name(cnty, "cust_price"),
      "statistic"
    ))
  }, character(1))
  table_row(paste0("Weak-IV F price (", cnty, ")"), values)
}, character(1))

nested_weak_sigma_rows <- vapply(counties, function(cnty) {
  values <- vapply(spec_order, function(spec_name) {
    format_stat(safe_diag_value(
      nested_results[[spec_name]]$diagnostics,
      weak_iv_row_name(cnty, "log_within_share"),
      "statistic"
    ))
  }, character(1))
  table_row(paste0("Weak-IV F sigma (", cnty, ")"), values)
}, character(1))

nested_wu_p_values <- vapply(spec_order, function(spec_name) {
  format_p_value(safe_diag_value(nested_results[[spec_name]]$diagnostics,
                                 "Wu-Hausman", "p-value"))
}, character(1))

nested_cond_values <- vapply(spec_order, function(spec_name) {
  format_int(nested_results[[spec_name]]$condition_number)
}, character(1))

nested_adj_r2_values <- vapply(spec_order, function(spec_name) {
  format_stat(nested_results[[spec_name]]$adj_r2, digits = 4)
}, character(1))

nested_obs_values <- vapply(spec_order, function(spec_name) {
  format_int(nobs(nested_results[[spec_name]]$fit))
}, character(1))

nested_table_lines <- c(
  nested_table_lines,
  nested_weak_price_rows,
  nested_weak_sigma_rows,
  table_row("Wu-Hausman $p$-value", nested_wu_p_values),
  table_row("Scaled $X'P_ZX$ cond.", nested_cond_values),
  table_row("Adj. $R^2$", nested_adj_r2_values),
  table_row("Observations", nested_obs_values),
  "\\hline \\\\[-1.8ex]",
  paste0(
    "\\multicolumn{6}{p{0.92\\linewidth}}{\\footnotesize Notes: Entries are ",
    "2SLS coefficients with location-level clustered standard errors in ",
    "parentheses. Price rows report county-specific coefficients on \\texttt{cust\\_price}; ",
    "sigma rows report county-specific coefficients on \\texttt{log\\_within\\_share}. ",
    "All five specifications use county-specific dye instruments, a county-specific ",
    "Hausman instrument built from the same-quarter average customer price in the ",
    "other counties, and county-specific $avg\\_labor \\times B\\_raw$ controls. ",
    "The manual specification omits the county-specific ", ref_quarter,
    " quarter indicators; the quarter-only omitted-quarter and county-plus-quarter ",
    "specifications omit ", ref_quarter,
    " as the common quarter reference; the all-quarter quarter-only specification ",
    "includes all quarter fixed effects. Weak-IV rows report the diagnostics from ",
    "\\texttt{summary(ivreg, diagnostics = TRUE)}. In a simple nested-logit interpretation, ",
    "sigma values in $[0,1)$ are the usual RUM-consistent range.}\\\\"
  ),
  "\\end{tabular}",
  "\\end{table}"
)

nested_output_path <- "results/out/tables/02_nested_iv_spec_comparison.tex"
writeLines(nested_table_lines, nested_output_path)

cat("\nDemand IV specification comparison saved to:\n")
cat("  ", standard_output_path, "\n", sep = "")
cat("Nested IV specification comparison saved to:\n")
cat("  ", nested_output_path, "\n", sep = "")


