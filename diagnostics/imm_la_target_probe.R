## LA immigration target-type probe.
## Why did the LA immigration impact flip sign across estimation vintages?
## Old manuscript & mid-June: realloc negative / reorg positive; Jul 4 run:
## realloc +0.018 / reorg -0.005. Between vintages BOTH the estimates changed
## (dye-IV demand, org-cost x avg_labor) AND the shock design changed
## (91c48dd: target LA type 1 -> derived lowest-wage type 4; magnitude
## 10%-of-total -> 5%-of-total; old manuscript: 10% of the type's own labor).
## This probe holds the CURRENT estimates fixed and re-solves the LA cell
## under the alternative shock designs, so target-type/magnitude channels are
## separated from the estimation channel:
##   A) target=4, delta = 5% of total county labor   [replicates 16_: validation]
##   B) target=1, delta = 5% of total county labor   [old target, current magnitude]
##   C) target=1, delta = 10% of type-1 labor        [old-manuscript-style shock]
## Writes only diagnostics/imm_la_target_probe_results.rds; results/ untouched.
source("config.R")
source("utils/counterfactuals_core.R")

innertol <- CONFIG$counterfactual_innertol
outertol <- CONFIG$counterfactual_outertol

ctx <- load_counterfactual_context()
working_data  <- ctx$working_data
initial_wages <- ctx$initial_wages
market_parms  <- ctx$market_parms
total_labor0  <- copy(ctx$total_labor)
rho           <- ctx$rho

n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
task_mix_cols  <- get_task_mix_cols(CONFIG)
e_field_names  <- counterfactual_e_field_names(CONFIG)
b_field_names  <- counterfactual_b_field_names(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)

LA <- "6037"
qy <- get_counterfactual_focus_quarter(); stopifnot(length(qy) == 1L)

cat("Derived lowest-wage targets (current baseline):\n")
print(counterfactual_lowest_wage_types(initial_wages))

market_input_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP"
)

## --- market machinery (mirrors 16_counterfactual_immigration.R) -------------
build_market_matrices <- function(wage_guess, cnty) {
  new_theta <- matrix(
    market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
    ncol = n_task_types, nrow = n_worker_types, byrow = FALSE
  )
  w_mat <- matrix(wage_guess, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  new_tild_theta <- w_mat + (rho[cnty])^(-1) * new_theta
  new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))
  list(new_theta = new_theta, new_tild_theta = new_tild_theta)
}

apply_pricing <- function(counter_res, cnty, qy) {
  counter_res[, Q := q_endog * avg_labor + qual_exo]
  counter_res[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
  counter_res[, newprice := counterfactual_best_response_prices(
    cust_price, Q, C, weight, rho[cnty], outertol, paste(cnty, qy)
  )]
  counter_res[, new_share := counterfactual_logit_shares(
    Q, newprice, weight, rho[cnty]
  )]
  counter_res
}

new_total_labor_from <- function(counter_res) {
  counter_res[, setNames(
    lapply(seq_len(n_worker_types), function(idx) {
      sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)
    }),
    tot_field_names
  )]
}

wcols <- paste0("w", seq_len(n_worker_types))
w_base <- as.numeric(unlist(
  initial_wages[county == LA & quarter_year == qy, ..wcols], use.names = FALSE))
cat("LA baseline wages:", sprintf("%.2f", w_base), " argmin =", which.min(w_base), "\n")

## --- baseline org structure (raw B at baseline wages), LA only --------------
orig_LA <- local({
  cr <- copy(working_data[county == LA & quarter_year == qy, ..market_input_cols])
  mats <- build_market_matrices(w_base, LA)
  out_fields <- c("c_endog", "q_endog", "s_index", e_field_names, b_field_names)
  cr[, (out_fields) := counterfactual_org_outputs(
      cost_matrix = mats$new_tild_theta, alpha = as.numeric(.SD),
      gamma = gamma_invert, wage_guess = w_base, new_theta = mats$new_theta,
      innertol = innertol, with_s_index = TRUE, with_b = TRUE, config = CONFIG
    ), by = "location_id", .SDcols = task_mix_cols]
  apply_pricing(cr, LA, qy)
})

saved_B_for <- function(loc) {
  matrix(as.numeric(orig_LA[location_id == loc, .SD, .SDcols = b_field_names]),
         byrow = FALSE, nrow = n_worker_types, ncol = n_task_types)
}

## --- generic solve + panel builders ------------------------------------------
solve_cell <- function(counter_res, wage_guess, mode, out_fields,
                       with_s_index = FALSE, with_swept_b = FALSE) {
  mats <- build_market_matrices(wage_guess, LA)
  if (mode == "reorg") {
    counter_res[, (out_fields) := counterfactual_org_outputs(
        cost_matrix = mats$new_tild_theta, alpha = as.numeric(.SD),
        gamma = gamma_invert, wage_guess = wage_guess, new_theta = mats$new_theta,
        innertol = innertol, with_s_index = with_s_index,
        with_swept_b = with_swept_b, config = CONFIG
      ), by = "location_id", .SDcols = task_mix_cols]
  } else {
    counter_res[, (out_fields) := counterfactual_org_outputs_from_b(
        B = saved_B_for(location_id), alpha = as.numeric(.SD),
        gamma = gamma_invert, wage_guess = wage_guess, new_theta = mats$new_theta,
        with_s_index = with_s_index, with_swept_b = with_swept_b, config = CONFIG
      ), by = "location_id", .SDcols = task_mix_cols]
  }
  apply_pricing(counter_res, LA, qy)
}

eval_gap <- function(wage_guess, tl_scen, mode) {
  cr <- copy(working_data[county == LA & quarter_year == qy, ..market_input_cols])
  cr <- solve_cell(cr, wage_guess, mode, c("c_endog", "q_endog", e_field_names))
  ntl <- new_total_labor_from(cr)
  stopifnot(nrow(ntl) == 1)
  counterfactual_labor_gap(ntl, tl_scen, LA, qy)
}

prod_panel <- function(wage_guess, mode) {
  cr <- copy(working_data[county == LA & quarter_year == qy, ..market_input_cols])
  solve_cell(cr, wage_guess, mode,
             c("c_endog", "q_endog", "s_index", e_field_names, b_field_names),
             with_s_index = TRUE, with_swept_b = TRUE)
}

## --- 18-style aggregation -----------------------------------------------------
base_panel <- local({
  p <- as.data.table(readRDS(counterfactual_data_path("13_prod_initial.rds")))
  p <- p[, !duplicated(names(p)), with = FALSE]
  p[as.character(county) == LA]
})
agg <- function(panel, nf) {
  p <- copy(panel)
  p[, tot_prod := Reduce(`+`, .SD), .SDcols = b_field_names]
  p[, mult := avg_labor * CSPOP * new_share * weight]
  p[, ns := Reduce(`+`, lapply(seq_len(n_worker_types),
                               function(k) get(e_field_names[k]) * nf[k]))]
  list(prod_nat = p[, sum(tot_prod * mult * ns) / sum(mult * ns)],
       prod_inc = p[, sum(tot_prod * mult) / sum(mult)],
       s_avg    = p[, weighted.mean(s_index, mult)])
}
base_ag <- agg(base_panel, rep(1, n_worker_types))
cat(sprintf("Baseline: prod=%.4f s_avg=%.4f\n", base_ag$prod_inc, base_ag$s_avg))

## --- scenario driver -----------------------------------------------------------
run_scenario <- function(name, target_idx, delta_rule) {
  tl <- copy(total_labor0)
  base <- as.numeric(tl[county == LA & quarter_year == qy, ..tot_field_names])
  delta <- if (delta_rule == "5pct_total") 0.05 * sum(base) else 0.10 * base[target_idx]
  col <- tot_field_names[target_idx]
  tl[county == LA & quarter_year == qy, (col) := get(col) + delta]
  nf <- rep(1, n_worker_types)
  nf[target_idx] <- base[target_idx] / (base[target_idx] + delta)
  cat(sprintf("\n================ %s: target=%d rule=%s delta=%.4g (%.1f%% of type, %.1f%% of total) ================\n",
              name, target_idx, delta_rule, delta,
              100 * delta / base[target_idx], 100 * delta / sum(base)))

  sol_rl <- counterfactual_solve_wage_market(
    function(w) eval_gap(w, tl, "realloc"), w_base,
    label = paste0("probe-", name, "-realloc"),
    target_tol = CONFIG$counterfactual_wage_tol)
  cat(sprintf("[%s realloc] converged=%s residual=%.4g\n",
              name, isTRUE(sol_rl$converged), sol_rl$residual))
  cat("  wage chg:", sprintf("%+.3f", (sol_rl$par - w_base) / w_base), "\n")

  sol_rg <- counterfactual_solve_wage_market(
    function(w) eval_gap(w, tl, "reorg"), sol_rl$par,
    label = paste0("probe-", name, "-reorg"),
    additional_starts = list(w_base),
    target_tol = CONFIG$counterfactual_wage_tol)
  cat(sprintf("[%s reorg] converged=%s residual=%.4g\n",
              name, isTRUE(sol_rg$converged), sol_rg$residual))
  cat("  wage chg:", sprintf("%+.3f", (sol_rg$par - w_base) / w_base), "\n")

  a_rl <- agg(prod_panel(sol_rl$par, "realloc"), nf)
  a_rg <- agg(prod_panel(sol_rg$par, "reorg"),   nf)
  res <- data.table(
    scenario = name, target = target_idx, rule = delta_rule, delta = delta,
    conv_rl = isTRUE(sol_rl$converged), resid_rl = sol_rl$residual,
    conv_rg = isTRUE(sol_rg$converged), resid_rg = sol_rg$residual,
    dprod_nat_rl = a_rl$prod_nat / base_ag$prod_nat - 1,
    dprod_nat_rg = a_rg$prod_nat / base_ag$prod_nat - 1,
    dprod_inc_rl = a_rl$prod_inc / base_ag$prod_inc - 1,
    dprod_inc_rg = a_rg$prod_inc / base_ag$prod_inc - 1,
    ds_rl = a_rl$s_avg / base_ag$s_avg - 1,
    ds_rg = a_rg$s_avg / base_ag$s_avg - 1)
  for (k in seq_len(n_worker_types)) {
    res[, (paste0("wchg_rl_w", k)) := (sol_rl$par[k] - w_base[k]) / w_base[k]]
    res[, (paste0("wchg_rg_w", k)) := (sol_rg$par[k] - w_base[k]) / w_base[k]]
  }
  print(res[, .(scenario, dprod_nat_rl, dprod_nat_rg, dprod_inc_rl,
                dprod_inc_rg, ds_rl, ds_rg)], digits = 4)
  res
}

out <- list()
for (spec in list(list("A_t4_5pctTotal", 4L, "5pct_total"),
                  list("B_t1_5pctTotal", 1L, "5pct_total"),
                  list("C_t1_10pctType", 1L, "10pct_type"))) {
  nm <- spec[[1]]
  out[[nm]] <- tryCatch(run_scenario(nm, spec[[2]], spec[[3]]),
                        error = function(e) {
                          cat(sprintf("[%s] FAILED: %s\n", nm, conditionMessage(e)))
                          NULL
                        })
  if (!is.null(out[[nm]])) {
    saveRDS(rbindlist(out[!sapply(out, is.null)]),
            "diagnostics/imm_la_target_probe_results.rds")
  }
}

cat("\n================ FINAL SUMMARY ================\n")
final <- rbindlist(out[!sapply(out, is.null)])
print(final[, .(scenario, target, rule, dprod_nat_rl, dprod_nat_rg,
                ds_rl, ds_rg, resid_rl, resid_rg)], digits = 4)
cat("Done.\n")
