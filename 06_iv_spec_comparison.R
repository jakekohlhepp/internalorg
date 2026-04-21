#' =============================================================================
#' DEMAND IV SPECIFICATION COMPARISON TABLES
#' =============================================================================
#' Produces three LaTeX tables comparing alternative 2SLS specifications of the
#' demand equation. The model is always a logit (or nested logit), fit by
#' county-specific instrumental-variables regression with standard errors
#' clustered at the location_id level. Tables differ in the set of instruments
#' and fixed-effect parameterizations shown side by side.
#'
#' Demand models
#' -------------
#'   (A) Standard logit:
#'         log(s / s_0) = alpha_c * p + controls + xi
#'       Endogenous: factor(county):cust_price
#'
#'   (B) Nested logit (single inside nest containing all salons; the outside
#'       option is "no salon visit"):
#'         log(s / s_0) = alpha_c * p + sigma_c * log(s_{j|g}) + controls + xi
#'       where s_{j|g} = salon_share_subdiv / (1 - outside_share).
#'       Endogenous: factor(county):cust_price, factor(county):log_within_share
#'
#' Instruments
#' -----------
#'   Dye      factor(county):dye_instrument       (task_mix_2 * ppi_inputs;
#'                                                 input-cost shifter)
#'   Hausman  factor(county):hausman_other_price  (leave-own-county-out mean
#'                                                 cust_price in the same
#'                                                 quarter)
#'
#' Fixed-effect specifications (columns within a table)
#' ----------------------------------------------------
#'   "Full CxQ"               Fully saturated county x quarter FE (ivreg
#'                            default). Typically absorbs the instrument's
#'                            identifying variation.
#'   "Manual"                 County x quarter FE with the three county:2018.1
#'                            cells omitted by hand, recovering the original
#'                            manual 2SLS spec in preamble.R. Imposes that
#'                            county-quarter FE are equal across counties in
#'                            2018.1.
#'   "Quarter only"           Additive quarter FE (reference quarter 2018.1
#'                            omitted); no county FE, no CxQ interaction.
#'                            County-level variation is absorbed only through
#'                            the avg_labor x B_raw cost controls.
#'   "Quarter only; all qtrs" Additive quarter FE including ALL quarters with
#'                            the intercept suppressed ("- 1" on the formula).
#'   "County + quarter"       Additive county FE + additive quarter FE
#'                            (one reference quarter omitted).
#'
#' All specifications include county-specific skill-weighted labor controls
#' factor(county):avg_labor:B_raw_j as exogenous cost shifters and estimate
#' county-specific price coefficients.
#'
#' Tables produced
#' ---------------
#'   Table 1 (06_standard_iv_comparison.tex): standard logit, 4 columns crossing
#'     {Hausman, Dye} x {Manual, Quarter only}. Lets readers see how the price
#'     coefficient moves with the choice of instrument vs. the choice of FE
#'     structure.
#'
#'   Table 2 (06_standard_hausman_fe_comparison.tex): standard logit with the
#'     Hausman instrument, across all 5 FE configurations. Diagnoses whether
#'     results are sensitive to the FE parameterization when the instrument
#'     is held fixed.
#'
#'   Table 3 (06_nested_fe_comparison.tex): nested logit using BOTH the Hausman
#'     and Dye instruments, across all 5 FE configurations. Reports
#'     county-specific price and sigma coefficients together with first-stage
#'     weak-IV F statistics for each endogenous regressor.
#'
#' Inputs:
#'   - mkdata/data/01_working.rds
#'   - Objects created by preamble.R (estim_matrix, CONFIG, counties list, ...)
#'
#' Outputs:
#'   - results/out/tables/06_standard_iv_comparison.tex
#'   - results/out/tables/06_standard_hausman_fe_comparison.tex
#'   - results/out/tables/06_nested_fe_comparison.tex
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
       "\nInstall them before running 06_iv_spec_comparison.R.")
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
# Fixed-effect specification definitions (columns)
# -----------------------------------------------------------------------------
## Each entry defines one FE parameterization. `exog_terms` are the RHS terms
## that appear on BOTH sides of the ivreg formula; the endogenous regressors
## and instruments are appended by the formula builder.
spec_definitions <- list(
  full_ivreg = list(
    label    = "Full CxQ",
    fe_label = "County$\\times$quarter",
    exog_terms = c(
      "factor(county):factor(quarter_year)",
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  ),
  manual = list(
    label    = "Manual",
    fe_label = paste0("County$\\times$quarter; omit ", ref_quarter),
    exog_terms = c(
      county_qy_terms,
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  ),
  quarter_only = list(
    label    = paste0("Quarter only; omit ", ref_quarter),
    fe_label = "Quarter FE",
    exog_terms = c(
      unique(str_replace(county_qy_terms, "factor\\(county\\):", "")),
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  ),
  quarter_only_all = list(
    label    = "Quarter only; all qtrs",
    fe_label = "All quarter FE",
    exog_terms = c(
      "factor(quarter_year)",
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  ),
  county_plus_quarter = list(
    label    = "County + quarter",
    fe_label = "County + quarter",
    exog_terms = c(
      "factor(county)",
      qy_dummy_names,
      paste0("factor(county):avg_labor:", b_raw_cols)
    )
  )
)

# -----------------------------------------------------------------------------
# Formula builder and fitting helpers
# -----------------------------------------------------------------------------

## model_type:   "standard" or "nested"
## instruments:  character vector of instrument base names (without
##               "factor(county):") -- e.g. c("dye_instrument"),
##               c("hausman_other_price"), or
##               c("dye_instrument","hausman_other_price").
build_demand_formula <- function(exog_terms, instruments,
                                 model_type = c("standard", "nested")) {
  model_type <- match.arg(model_type)
  exog_str <- paste(exog_terms, collapse = " + ")
  inst_terms <- paste(paste0("factor(county):", instruments), collapse = " + ")

  if (model_type == "standard") {
    rhs_endog <- "factor(county):cust_price"
  } else {
    rhs_endog <- "factor(county):cust_price + factor(county):log_within_share"
  }

  as.formula(paste0(
    "log_rel_mkt ~ ", exog_str, " + ", rhs_endog, " - 1",
    " | ",
    exog_str, " + ", inst_terms, " - 1"
  ))
}

fit_demand_spec <- function(spec_name, instruments,
                            model_type = c("standard", "nested")) {
  model_type <- match.arg(model_type)
  exog_terms <- spec_definitions[[spec_name]]$exog_terms
  fit <- ivreg(build_demand_formula(exog_terms, instruments, model_type),
               data = estim_matrix)
  clust_vcov <- vcovCL(fit, cluster = estim_matrix$location_id, type = "HC1")
  coef_test <- coeftest(fit, vcov. = clust_vcov)
  diagnostics <- summary(fit, diagnostics = TRUE)$diagnostics

  result <- list(
    fit = fit,
    prices = extract_county_rows(coef_test, "cust_price"),
    diagnostics = diagnostics,
    condition_number = scaled_xpzx_condition(fit),
    adj_r2 = summary(fit)$adj.r.squared
  )

  if (model_type == "nested") {
    result$sigmas <- extract_county_rows(coef_test, "log_within_share")
  }

  result
}

# -----------------------------------------------------------------------------
# Shared helpers
# -----------------------------------------------------------------------------

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

weak_iv_row_name <- function(cnty, suffix) {
  paste0("Weak instruments (factor(county)", cnty, ":", suffix, ")")
}

# -----------------------------------------------------------------------------
# Nested-logit variables and Hausman instrument
# -----------------------------------------------------------------------------

working_data[, row_id := .I]
## warning: because not all salons are sampled, within share should be
## 1 - outside_share.
working_data[, within_share := salon_share_subdiv / (1 - outside_share)]
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
# Generic table builder
# -----------------------------------------------------------------------------
## `columns` is a list where each element describes one column of the table:
##   label      -> main column header
##   fe_label   -> entry in the "Fixed effects" row
##   result     -> output from fit_demand_spec()
##
## `extra_header_rows` is an optional list of additional rows placed between
## the column header and the "Fixed effects" row. Each entry is a list with
##   label   -> row label
##   values  -> character vector (one entry per column)
##
## `has_sigma` controls whether county sigma rows and their first-stage F rows
## are included (only the nested-logit table sets this).
build_iv_table <- function(columns, caption, label,
                           has_sigma = FALSE,
                           extra_header_rows = list(),
                           notes = "") {
  n_cols <- length(columns)
  align <- paste0("l", strrep("c", n_cols))
  total_cols <- n_cols + 1L  # column for row labels + data columns

  header_values <- vapply(columns, `[[`, character(1), "label")
  fe_values <- vapply(columns, `[[`, character(1), "fe_label")

  lines <- c(
    "\\begin{table}[!htbp] \\centering",
    paste0("  \\caption{", caption, "}"),
    paste0("  \\label{", label, "}"),
    paste0("  \\begin{tabular}{", align, "}"),
    "\\\\[-1.8ex]\\hline",
    "\\hline \\\\[-1.8ex]",
    table_row("", header_values)
  )
  for (hdr in extra_header_rows) {
    lines <- c(lines, table_row(hdr$label, hdr$values))
  }
  lines <- c(lines,
    "\\hline \\\\[-1.8ex]",
    table_row("Fixed effects", fe_values)
  )

  # County-specific price rows
  for (cnty in counties) {
    price_cells <- vapply(columns, function(col) {
      row <- col$result$prices[county == cnty]
      format_estimate(row$estimate, row$p_value)
    }, character(1))
    price_se_cells <- vapply(columns, function(col) {
      row <- col$result$prices[county == cnty]
      format_se(row$se)
    }, character(1))
    lines <- c(lines,
      table_row(paste0("Price (county ", cnty, ")"), price_cells),
      table_row("", price_se_cells)
    )

    if (has_sigma) {
      sigma_cells <- vapply(columns, function(col) {
        row <- col$result$sigmas[county == cnty]
        format_estimate(row$estimate, row$p_value)
      }, character(1))
      sigma_se_cells <- vapply(columns, function(col) {
        row <- col$result$sigmas[county == cnty]
        format_se(row$se)
      }, character(1))
      lines <- c(lines,
        table_row(paste0("Sigma (county ", cnty, ")"), sigma_cells),
        table_row("", sigma_se_cells)
      )
    }
  }

  # Weak-IV F rows (price, and also sigma when nested)
  weak_iv_price_rows <- vapply(counties, function(cnty) {
    values <- vapply(columns, function(col) {
      format_stat(safe_diag_value(col$result$diagnostics,
                                  weak_iv_row_name(cnty, "cust_price"),
                                  "statistic"))
    }, character(1))
    table_row(paste0("Weak-IV F price (", cnty, ")"), values)
  }, character(1))
  lines <- c(lines, weak_iv_price_rows)

  if (has_sigma) {
    weak_iv_sigma_rows <- vapply(counties, function(cnty) {
      values <- vapply(columns, function(col) {
        format_stat(safe_diag_value(col$result$diagnostics,
                                    weak_iv_row_name(cnty, "log_within_share"),
                                    "statistic"))
      }, character(1))
      table_row(paste0("Weak-IV F sigma (", cnty, ")"), values)
    }, character(1))
    lines <- c(lines, weak_iv_sigma_rows)
  }

  wu_p_values <- vapply(columns, function(col) {
    format_p_value(safe_diag_value(col$result$diagnostics,
                                   "Wu-Hausman", "p-value"))
  }, character(1))
  cond_values <- vapply(columns, function(col) {
    format_int(col$result$condition_number)
  }, character(1))
  adj_r2_values <- vapply(columns, function(col) {
    format_stat(col$result$adj_r2, digits = 4)
  }, character(1))
  obs_values <- vapply(columns, function(col) {
    format_int(nobs(col$result$fit))
  }, character(1))

  lines <- c(lines,
    table_row("Wu-Hausman $p$-value", wu_p_values),
    table_row("Scaled $X'P_ZX$ cond.", cond_values),
    table_row("Adj. $R^2$", adj_r2_values),
    table_row("Observations", obs_values),
    "\\hline \\\\[-1.8ex]",
    paste0("\\multicolumn{", total_cols,
           "}{p{0.92\\linewidth}}{\\footnotesize ", notes, "}\\\\"),
    "\\end{tabular}",
    "\\end{table}"
  )

  lines
}

# -----------------------------------------------------------------------------
# Table 1: standard logit, {Hausman, Dye} x {Manual, Quarter only}
# -----------------------------------------------------------------------------

table1_columns <- list(
  list(
    label = spec_definitions$manual$label,
    fe_label = spec_definitions$manual$fe_label,
    result = fit_demand_spec("manual", "hausman_other_price", "standard")
  ),
  list(
    label = spec_definitions$quarter_only$label,
    fe_label = spec_definitions$quarter_only$fe_label,
    result = fit_demand_spec("quarter_only", "hausman_other_price", "standard")
  ),
  list(
    label = spec_definitions$manual$label,
    fe_label = spec_definitions$manual$fe_label,
    result = fit_demand_spec("manual", "dye_instrument", "standard")
  ),
  list(
    label = spec_definitions$quarter_only$label,
    fe_label = spec_definitions$quarter_only$fe_label,
    result = fit_demand_spec("quarter_only", "dye_instrument", "standard")
  )
)

table1_instrument_row <- list(
  label = "Instrument",
  values = c("Hausman", "Hausman", "Dye", "Dye")
)

table1_notes <- paste0(
  "Notes: Standard-logit 2SLS price coefficients with location-level clustered ",
  "standard errors in parentheses. Columns cross the two instruments (Hausman ",
  "leave-own-county-out same-quarter price; dye = task\\_mix\\_2 $\\times$ ",
  "PPI) with two fixed-effect parameterizations: ``Manual'' uses county",
  "$\\times$quarter FE with the county:", ref_quarter, " cells dropped, and ",
  "``Quarter only'' uses additive quarter FE with ", ref_quarter, " as the ",
  "reference quarter. All columns also include county-specific $avg\\_labor ",
  "\\times B\\_raw$ cost controls. Weak-IV rows report the diagnostic ",
  "statistics from \\texttt{summary(ivreg, diagnostics = TRUE)}."
)

table1_lines <- build_iv_table(
  columns = table1_columns,
  caption = "Standard Logit: Instrument and Fixed-Effect Comparison",
  label = "tab:standard_iv_comparison",
  has_sigma = FALSE,
  extra_header_rows = list(table1_instrument_row),
  notes = table1_notes
)

table1_output_path <- "results/out/tables/06_standard_iv_comparison.tex"
writeLines(table1_lines, table1_output_path)

# -----------------------------------------------------------------------------
# Table 2: standard logit with Hausman, all 5 FE specifications
# -----------------------------------------------------------------------------

spec_order <- names(spec_definitions)

table2_columns <- lapply(spec_order, function(spec_name) {
  list(
    label = spec_definitions[[spec_name]]$label,
    fe_label = spec_definitions[[spec_name]]$fe_label,
    result = fit_demand_spec(spec_name, "hausman_other_price", "standard")
  )
})

table2_notes <- paste0(
  "Notes: Standard-logit 2SLS price coefficients with location-level clustered ",
  "standard errors in parentheses. All columns use the county-specific Hausman ",
  "instrument (leave-own-county-out same-quarter average customer price) and ",
  "county-specific $avg\\_labor \\times B\\_raw$ controls. Columns vary only in ",
  "the fixed-effect parameterization. The manual specification omits the ",
  "county-specific ", ref_quarter, " quarter indicators; the quarter-only ",
  "omitted-quarter and county-plus-quarter specifications omit ", ref_quarter,
  " as the common quarter reference; the all-quarter quarter-only specification ",
  "includes all quarter fixed effects. Weak-IV rows report the diagnostic ",
  "statistics from \\texttt{summary(ivreg, diagnostics = TRUE)}."
)

table2_lines <- build_iv_table(
  columns = table2_columns,
  caption = "Standard Logit with Hausman Instrument: Fixed-Effect Comparison",
  label = "tab:standard_hausman_fe_comparison",
  has_sigma = FALSE,
  notes = table2_notes
)

table2_output_path <- "results/out/tables/06_standard_hausman_fe_comparison.tex"
writeLines(table2_lines, table2_output_path)

# -----------------------------------------------------------------------------
# Table 3: nested logit with Hausman + Dye, all 5 FE specifications
# -----------------------------------------------------------------------------

table3_columns <- lapply(spec_order, function(spec_name) {
  list(
    label = spec_definitions[[spec_name]]$label,
    fe_label = spec_definitions[[spec_name]]$fe_label,
    result = fit_demand_spec(spec_name,
                             c("dye_instrument", "hausman_other_price"),
                             "nested")
  )
})

table3_notes <- paste0(
  "Notes: Nested-logit 2SLS coefficients with location-level clustered standard ",
  "errors in parentheses. Price rows report county-specific coefficients on ",
  "\\texttt{cust\\_price}; sigma rows report county-specific coefficients on ",
  "\\texttt{log\\_within\\_share}. All columns use the county-specific dye ",
  "instrument and the county-specific Hausman instrument (leave-own-county-out ",
  "same-quarter price) together, plus county-specific $avg\\_labor \\times ",
  "B\\_raw$ controls. Columns vary only in the fixed-effect parameterization. ",
  "The manual specification omits the county-specific ", ref_quarter, " quarter ",
  "indicators; the quarter-only omitted-quarter and county-plus-quarter ",
  "specifications omit ", ref_quarter, " as the common quarter reference; the ",
  "all-quarter quarter-only specification includes all quarter fixed effects. ",
  "Weak-IV rows report the diagnostics from \\texttt{summary(ivreg, ",
  "diagnostics = TRUE)}. In a simple nested-logit interpretation, sigma values ",
  "in $[0,1)$ are the usual RUM-consistent range."
)

table3_lines <- build_iv_table(
  columns = table3_columns,
  caption = "Nested Logit with Hausman + Dye Instruments: Fixed-Effect Comparison",
  label = "tab:nested_fe_comparison",
  has_sigma = TRUE,
  notes = table3_notes
)

table3_output_path <- "results/out/tables/06_nested_fe_comparison.tex"
writeLines(table3_lines, table3_output_path)

cat("\nDemand IV specification comparison tables saved to:\n")
cat("  ", table1_output_path, "\n", sep = "")
cat("  ", table2_output_path, "\n", sep = "")
cat("  ", table3_output_path, "\n", sep = "")
