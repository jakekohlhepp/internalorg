## ===========================================================================
## prod_diffusion_best_wage.R
##
## Productivity estimates for the LA (6037) REORG diffusion counterfactual,
## evaluated at the "best" wage vector saved as the diffusion warm start (the
## multistart point that passes the labor-weighted gate: lw|r|=0.0058, with
## worker-5 sacrificed at r5=+1.006).
##
## Recomputes the reorg get_prod() panel at that wage vector exactly as
## 14_counterfactual_diffusion.R does (fresh org solve, swept B, Lambert-W
## prices), aggregates it with 18_counterfactual_summary.R's summarize_prod_panel
## (parse-evaled, not copied), and reports levels + % change vs the Initial
## baseline (13_prod_initial.rds) -- the same s-index / labor-productivity /
## revenue-per-labor metrics 18 tabulates.
##
## Run at the cap1e6 recipe so productivity is consistent with how the point's
## residual was evaluated (worker-5 is inner-solve-sensitive).
## Output: prod_diffusion_best_wage.rds
## ===========================================================================
suppressPackageStartupMessages({
  library(data.table); library(SQUAREM); library(stringr)
})
source("config.R")
source("utils/counterfactuals_core.R")

LA <- "6037"
QY <- get_counterfactual_focus_quarter(); stopifnot(length(QY) == 1L)
n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
innertol <- CONFIG$counterfactual_innertol
outertol <- CONFIG$counterfactual_outertol
task_mix_cols   <- get_task_mix_cols(CONFIG)
e_field_names   <- counterfactual_e_field_names(CONFIG)
b_field_names   <- counterfactual_b_field_names(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)
market_input_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP")

## ---- reuse 18's summarize_prod_panel verbatim (parse-eval the def) ---------
ex18 <- parse("18_counterfactual_summary.R")
got <- FALSE
for (e in ex18) {
  if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
      is.symbol(e[[2]]) && identical(as.character(e[[2]]), "summarize_prod_panel")) {
    eval(e, globalenv()); got <- TRUE; break
  }
}
stopifnot("could not extract summarize_prod_panel from 18" = got)

## ---- context + diffusion shock --------------------------------------------
ctx <- load_counterfactual_context(config = CONFIG)
working_data <- copy(ctx$working_data)
market_parms <- ctx$market_parms
rho          <- ctx$rho
total_labor  <- copy(ctx$total_labor)
improve_it <- function(x) sort(x)[pmax(frank(x, ties.method = "first") - 1, 1)]
working_data[, gamma_invert := improve_it(gamma_invert), by = c("county", "quarter_year")]

## ---- get_prod (reorg), mirroring 14_counterfactual_diffusion.R ------------
build_market_matrices <- function(wage_guess, cnty) {
  new_theta <- matrix(market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
                      ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  w_mat <- matrix(wage_guess, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  ntt <- w_mat + (rho[cnty])^(-1) * new_theta
  ntt <- sweep(ntt, 2, apply(ntt, 2, min))
  list(new_theta = new_theta, new_tild_theta = ntt)
}
apply_pricing <- function(counter_res, cnty, qy) {
  counter_res[, Q := q_endog * avg_labor + qual_exo]
  counter_res[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
  counter_res[, newprice := counterfactual_best_response_prices(
    cust_price, Q, C, weight, rho[cnty], outertol, paste(cnty, qy))]
  counter_res[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[cnty])]
  counter_res
}
get_prod_reorg <- function(wage_guess, cnty, qy) {
  cr <- copy(working_data[county == cnty & quarter_year == qy, ..market_input_cols])
  mats <- build_market_matrices(wage_guess, cnty)
  of <- c("c_endog", "q_endog", "s_index", e_field_names, b_field_names)
  cr[, (of) := counterfactual_org_outputs(
      cost_matrix = mats$new_tild_theta, alpha = as.numeric(.SD), gamma = gamma_invert,
      wage_guess = wage_guess, new_theta = mats$new_theta, innertol = innertol,
      with_s_index = TRUE, with_swept_b = TRUE, config = CONFIG)[of],
    by = c("location_id"), .SDcols = task_mix_cols]
  apply_pricing(cr, cnty, qy)
}
labor_gap <- function(cr) {
  ntl <- cr[, setNames(lapply(seq_len(n_worker_types), function(idx)
    sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)), tot_field_names)]
  as.numeric(counterfactual_labor_gap(ntl, total_labor, LA, QY))
}

## ---- best wage vector (read straight from the saved warm start) ------------
warm <- as.data.table(readRDS(counterfactual_data_path("14_warm_start_wages_diffusion.rds")))
best_wages <- as.numeric(unlist(warm[as.character(county) == LA & sol_type == "reorg" &
  as.character(quarter_year) == as.character(QY), paste0("w", seq_len(n_worker_types)), with = FALSE]))
stopifnot(length(best_wages) == n_worker_types, all(is.finite(best_wages)))
cat(sprintf("best reorg wages (LA %s): [%s]\n", QY, paste(signif(best_wages, 8), collapse = ", ")))
cat(sprintf("inner: innertol=%.1e outertol=%.1e fp_cap=%g\n",
            innertol, outertol, CONFIG$counterfactual_fixedpoint_max_iter))

## ---- compute reorg panel + residual at the best wages ----------------------
cr <- get_prod_reorg(best_wages, LA, QY)
r  <- labor_gap(cr)
cat(sprintf("labor-clearing residual: max|r|=%.6g  lw|r|=%.6g\n  components=[%s]\n\n",
            max(abs(r)), counterfactual_residual_norm_lw(r),
            paste(signif(r, 4), collapse = ", ")))

panel_reorg <- data.table(county = LA, quarter_year = QY, sol_type = "reorg", cr)
summ_reorg  <- summarize_prod_panel(panel_reorg, "Management Diffusion")
firm_reorg  <- summ_reorg$firm[county == LA]
type_reorg  <- summ_reorg$type[county == LA]

## ---- baseline (Initial) for LA --------------------------------------------
base_panel <- as.data.table(readRDS(counterfactual_data_path("13_prod_initial.rds")))
summ_base  <- summarize_prod_panel(base_panel, "Initial")
firm_base  <- summ_base$firm[county == LA]
type_base  <- summ_base$type[county == LA]
cat("baseline LA firm rows (sol_type / s_avg / labor_prod / rev_per_labor):\n")
print(firm_base[, .(sol_type, s_avg, labor_prod, rev_per_labor)])
fb <- firm_base[1]   # baseline is per-county; take the single baseline row

## ---- report ---------------------------------------------------------------
fr <- firm_reorg[1]
pct <- function(new, old) (new - old) / old
cat("\n================ LA REORG DIFFUSION PRODUCTIVITY (best wage vector) ================\n")
cat(sprintf("%-18s %14s %14s %12s\n", "metric", "reorg", "baseline", "%change"))
cat(sprintf("%-18s %14.6g %14.6g %11.2f%%\n", "s-index (wtd)", fr$s_avg, fb$s_avg, 100 * pct(fr$s_avg, fb$s_avg)))
cat(sprintf("%-18s %14.6g %14.6g %11.2f%%\n", "labor productivity", fr$labor_prod, fb$labor_prod, 100 * pct(fr$labor_prod, fb$labor_prod)))
cat(sprintf("%-18s %14.6g %14.6g %11.2f%%\n", "revenue/labor", fr$rev_per_labor, fb$rev_per_labor, 100 * pct(fr$rev_per_labor, fb$rev_per_labor)))

cat("\n---- per-worker-type productivity (reorg vs baseline, %change) ----\n")
wt <- as.character(seq_len(n_worker_types))
cat(sprintf("%-8s %14s %14s %12s\n", "type", "reorg", "baseline", "%change"))
for (k in wt) {
  nr <- as.numeric(type_reorg[[k]]); nb <- as.numeric(type_base[[k]])
  cat(sprintf("worker_%-2s %14.6g %14.6g %11.2f%%\n", k, nr, nb, 100 * pct(nr, nb)))
}

out <- list(best_wages = best_wages, residual = r,
            residual_max = max(abs(r)), residual_lw = counterfactual_residual_norm_lw(r),
            firm_reorg = fr, firm_baseline = fb,
            type_reorg = type_reorg, type_baseline = type_base,
            pct = list(sindex = pct(fr$s_avg, fb$s_avg),
                       labor_prod = pct(fr$labor_prod, fb$labor_prod),
                       rev_per_labor = pct(fr$rev_per_labor, fb$rev_per_labor)))
saveRDS(out, "prod_diffusion_best_wage.rds")
cat("\nSaved: prod_diffusion_best_wage.rds\n")
