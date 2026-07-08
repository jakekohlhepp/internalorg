#' =============================================================================
#' STEP 21: Productivity-Weighted Wage-Shock Substitution Patterns
#' =============================================================================
#' Companion to 20_: computes 1% wage-shock substitution elasticities on
#' productivity (rowSums of B * skill) rather than headcount, restricted to the
#' focus quarter and evaluated against cleared equilibrium wages produced by
#' 13_counterfactual_prep.R.
#'
#' Inputs:
#'   - results/data/counterfactuals/13_initial_wages.rds (cleared wages)
#'   - results/data/counterfactuals/13_working_data.rds  (firm panel)
#'   - results/data/06_parameters.rds                    (skill matrix B,
#'                                                        market_parms, rho)
#'
#' Outputs:
#'   - results/out/tables/21_substitute_prod.tex
#' =============================================================================

library('data.table')
library('lubridate')
library('stringr')
library('ggplot2')
library('gridExtra')
library('kableExtra')

source('config.R')
source('utils/counterfactuals_core.R')

ensure_directory('results/out/tables')

ctx <- load_counterfactual_context()
working_data  <- copy(ctx$working_data)
initial_wages <- ctx$initial_wages
market_parms  <- ctx$market_parms
rho           <- ctx$rho

n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
focus_qy       <- get_counterfactual_focus_quarter()

## Per-county productivity-weight matrix. get_prod() below uses this as the weight
## in rowSums(B * new_theta) / rowSums(B), which must match the canonical
## productivity panel the pipeline writes elsewhere: counterfactual_org_outputs(
## with_swept_b = TRUE) forms B * sweep(new_theta, 2, apply(new_theta, 2, min)) and
## 13_counterfactual_prep.R feeds it exactly that. The previous `skills - min(skills)`
## subtracted a single GLOBAL scalar instead of each task-column's minimum, so the
## productivity levels (and the (E_shock - E_base)/E_base ratios in
## 21_substitute_prod.tex) diverged from the 13_ convention.
new_theta <- lapply(CONFIG$counties, function(cnty) {
  skills <- matrix(market_parms[grep(paste0(cnty, ':avg_labor:B'), names(market_parms))],
                   ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  sweep(skills, 2, apply(skills, 2, min))
})
names(new_theta) <- CONFIG$counties

## tild_theta at cleared equilibrium wages for the focus quarter (one slot
## per county). Rebuilt on each wage shock below.
build_tild_cleared <- function(wage_overrides = list()) {
  out <- vector(mode = 'list', length = length(CONFIG$counties))
  names(out) <- CONFIG$counties
  for (cnty in CONFIG$counties) {
    out[[cnty]] <- vector(mode = 'list', length = 1L)
    names(out[[cnty]]) <- as.character(focus_qy)
    cleared_w <- as.numeric(unlist(
      initial_wages[county == cnty & quarter_year == focus_qy,
                    paste0('w', seq_len(n_worker_types)), with = FALSE],
      use.names = FALSE
    ))
    stopifnot(length(cleared_w) == n_worker_types, all(is.finite(cleared_w)))
    if (!is.null(wage_overrides[[cnty]])) {
      cleared_w <- wage_overrides[[cnty]]
    }
    w_mat  <- matrix(cleared_w, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
    skills <- matrix(market_parms[grep(paste0(cnty, ':avg_labor:B'), names(market_parms))],
                     ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
    out[[cnty]][[as.character(focus_qy)]] <- sweep(
      w_mat + (rho[cnty])^(-1) * skills, 2,
      apply(w_mat + (rho[cnty])^(-1) * skills, 2, min)
    )
  }
  out
}

tild_theta <- build_tild_cleared()

innertol <- 1e-08
outertol <- 1e-04

## Productivity-weighted: rowSums(B * theta) / rowSums(B) per worker type.
get_prod <- function(a1, a2, a3, a4, a5, county, quarter_year, gamma) {
  alpha <- c(a1, a2, a3, a4, a5)
  tild_mat <- tild_theta[[county]][[as.character(quarter_year)]]
  if (is.null(tild_mat)) return(rep(NA_real_, n_worker_types))
  if (is.finite(gamma) & gamma > 0) {
    A <- exp(-1 / gamma * tild_mat)
    E <- rep(1 / n_worker_types, n_worker_types)
    A[A >= Inf] <- 1e16
    A[A <= 0]   <- 1e-16
    fxpt <- function(p) {
      C <- colSums(t(A) * alpha / colSums(A * p))
      p * C
    }
    for (i in 1:1000000) {
      E_old <- E
      E <- fxpt(E_old)
      if (all(abs(E - E_old) < innertol)) break
    }
    B <- t(t(A) * alpha / colSums(A * E)) * E
  } else if (!is.na(gamma) && gamma == 0) {
    ## no frictions: cheapest worker takes each task
    B <- matrix(0, ncol = n_task_types, nrow = n_worker_types)
    for (col in seq_len(n_task_types)) {
      B[which.min(tild_mat[, col]), col] <- alpha[col]
    }
  } else {
    ## gamma Inf or NA: max frictions (matches 13_counterfactual_prep)
    B <- matrix(0, ncol = n_task_types, nrow = n_worker_types)
    B[which.min(rowSums(t(t(tild_mat) * alpha))), ] <- alpha
  }
  B[abs(B) < 1e-16] <- 0
  rowSums(B * new_theta[[county]]) / rowSums(B)
}

get_demands <- function(a1, a2, a3, a4, a5, county, quarter_year, gamma) {
  alpha <- c(a1, a2, a3, a4, a5)
  tild_mat <- tild_theta[[county]][[as.character(quarter_year)]]
  if (is.null(tild_mat)) return(rep(NA_real_, n_worker_types))
  if (is.finite(gamma) & gamma > 0) {
    A <- exp(-1 / gamma * tild_mat)
    E <- rep(1 / n_worker_types, n_worker_types)
    A[A >= Inf] <- 1e16
    A[A <= 0]   <- 1e-16
    fxpt <- function(p) {
      C <- colSums(t(A) * alpha / colSums(A * p))
      p * C
    }
    for (i in 1:1000000) {
      E_old <- E
      E <- fxpt(E_old)
      if (all(abs(E - E_old) < innertol)) break
    }
    B <- t(t(A) * alpha / colSums(A * E)) * E
  } else if (!is.na(gamma) && gamma == 0) {
    ## no frictions: cheapest worker takes each task
    B <- matrix(0, ncol = n_task_types, nrow = n_worker_types)
    for (col in seq_len(n_task_types)) {
      B[which.min(tild_mat[, col]), col] <- alpha[col]
    }
  } else {
    ## gamma Inf or NA: max frictions (matches 13_counterfactual_prep)
    B <- matrix(0, ncol = n_task_types, nrow = n_worker_types)
    B[which.min(rowSums(t(t(tild_mat) * alpha))), ] <- alpha
  }
  B[abs(B) < 1e-16] <- 0
  rowSums(B)
}

## Restrict to focus quarter (cleared equilibrium only available here).
working_data <- working_data[quarter_year == focus_qy]

for (i in seq_len(nrow(working_data))) {
  working_data[i, c("l_1", "l_2", "l_3", "l_4", "l_5") :=
                 as.list(get_demands(task_mix_1, task_mix_2, task_mix_3,
                                     task_mix_4, task_mix_5,
                                     county, quarter_year, gamma_invert))]
}

for (i in seq_len(nrow(working_data))) {
  working_data[i, c("E_1", "E_2", "E_3", "E_4", "E_5") :=
                 as.list(get_prod(task_mix_1, task_mix_2, task_mix_3,
                                  task_mix_4, task_mix_5,
                                  county, quarter_year, gamma_invert))]
}

shock_factor <- 1.01
for (w in seq_len(n_worker_types)) {
  wage_overrides <- list()
  for (cnty in CONFIG$counties) {
    cleared_w <- as.numeric(unlist(
      initial_wages[county == cnty & quarter_year == focus_qy,
                    paste0('w', seq_len(n_worker_types)), with = FALSE],
      use.names = FALSE
    ))
    cleared_w[w] <- cleared_w[w] * shock_factor
    wage_overrides[[cnty]] <- cleared_w
  }
  tild_theta <- build_tild_cleared(wage_overrides)
  for (i in seq_len(nrow(working_data))) {
    working_data[i, paste0(c("E_1", "E_2", "E_3", "E_4", "E_5"), "_", w) :=
                   as.list(get_prod(task_mix_1, task_mix_2, task_mix_3,
                                    task_mix_4, task_mix_5,
                                    county, quarter_year, gamma_invert))]
  }
}

tild_theta <- build_tild_cleared()

for (enum in seq_len(n_worker_types)) {
  for (eshock in seq_len(n_worker_types)) {
    working_data[get(paste0("l_", enum)) > 0.01 & get(paste0("l_", eshock)) > 0.01,
                 (paste0("ediff_", enum, "_", eshock)) :=
                   (get(paste0("E_", enum, "_", eshock)) - get(paste0("E_", enum))) /
                     get(paste0("E_", enum))]
  }
}
working_data[is.nan(gamma_invert), gamma_invert := Inf]
working_data[, gamma_quant := ecdf(gamma_invert)(gamma_invert), by = 'county']

quantfunc <- function(x, q) {
  format(round(quantile(x[!is.na(x)], q), digits = 3), nsmall = 3)
}
sub_patterns <- working_data[, lapply(.SD, quantfunc, q = 0), by = c("county"),
                             .SDcols = names(working_data)[grep("ediff", names(working_data))]]
sub_patterns[, stat_name := "Min."]
sub_patterns <- rbind(sub_patterns,
                     working_data[, lapply(.SD, quantfunc, q = 1), by = c("county"),
                                  .SDcols = names(working_data)[grep("ediff", names(working_data))]],
                     fill = TRUE)
sub_patterns[is.na(stat_name), stat_name := "Max."]
sub_patterns <- rbind(sub_patterns,
                     working_data[, lapply(.SD, quantfunc, q = 0.5), by = c("county"),
                                  .SDcols = names(working_data)[grep("ediff", names(working_data))]],
                     fill = TRUE)
sub_patterns[is.na(stat_name), stat_name := "Med."]

sub_patterns <- melt(sub_patterns, id.vars = c("county", "stat_name"),
                     measure = patterns("^ediff"))
sub_patterns[, enum   := str_replace_all(str_extract(variable, "_[0-9]_"), "_", "")]
sub_patterns[, eshock := str_replace_all(str_extract(variable, "_[0-9]$"), "_", "")]
sub_patterns <- dcast(sub_patterns, county + enum ~ eshock + stat_name)

colnames(sub_patterns) <- c("County", "Skill Set", rep(c("Max.", "Med.", "Min."), 5))
setkey(sub_patterns, "County", "Skill Set")
sub_patterns[County == '17031', County := "Cook"]
sub_patterns[County == '36061', County := "New York"]
sub_patterns[County == '6037',  County := "Los Angeles"]

output <- kable(sub_patterns, "latex", align = "c", booktabs = TRUE,
                linesep = c(""), escape = FALSE, caption = NA, label = NA)
output <- add_header_above(output, c(" ", " ",
                                     "Skill Set 1" = 3, "Skill Set 2" = 3,
                                     "Skill Set 3" = 3, "Skill Set 4" = 3,
                                     "Skill Set 5" = 3))
cat(output, file = file.path("results", "out", "tables", "21_substitute_prod.tex"))
