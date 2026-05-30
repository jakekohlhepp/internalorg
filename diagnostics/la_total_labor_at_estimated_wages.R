## Total labor by worker type (skill set) in LA at the ESTIMATED wages.
## Wages are fixed at parm_wage_vec (the wage levels implied by 06 estimates:
## avg_labor:county:quarter base + :avg_labor:E differentials). At those fixed
## wages we solve (a) the per-firm task-assignment fixed point and (b) the
## best-response pricing, then aggregate labor demand by worker type:
##   L_k = sum_firms weight * new_share * CSPOP * E_k * avg_labor
## Hardened inner solve (innertol=1e-12, SQUAREM cap=1e6) to avoid the LA
## near-corner degeneracy. Read-only.
suppressPackageStartupMessages({ library(data.table) })
source("config.R"); source("utils/counterfactuals_core.R")

CFG <- CONFIG
CFG$counterfactual_innertol <- 1e-12
CFG$counterfactual_fixedpoint_max_iter <- 1000000L
innertol <- CFG$counterfactual_innertol
outertol <- 1e-8

n_w <- CFG$n_worker_types; n_t <- CFG$n_task_types
task_mix_cols   <- get_task_mix_cols(CFG)
e_field_names   <- counterfactual_e_field_names(CFG)
tot_field_names <- counterfactual_tot_labor_field_names(CFG)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()

ctx <- load_counterfactual_context(config = CFG)
working_data <- ctx$working_data
market_parms <- ctx$market_parms
rho <- ctx$rho

market_input_cols <- c(
  "location_id","county","quarter_year","gamma_invert","avg_labor",
  task_mix_cols,"qual_exo","cost_exo","weight","cust_price","CSPOP")
la <- copy(working_data[county==LA & quarter_year==QY, ..market_input_cols])
cat("LA firms at ", QY, ": ", nrow(la), "\n", sep="")

## estimated wage levels (parm_wage_vec), exactly as 13_counterfactual_prep.R
w1 <- market_parms[paste0("avg_labor:factor(county)", LA, ":factor(quarter_year)", QY)]
diffs <- market_parms[grep(paste0(LA, ":avg_labor:E"), names(market_parms))]
wage_vec <- as.numeric(c(w1, w1 + diffs))
cat("Estimated wages (w1..w", n_w, "): ", paste(sprintf("%.4f", wage_vec), collapse=", "), "\n", sep="")
stopifnot(length(wage_vec)==n_w, all(is.finite(wage_vec)))

new_theta <- matrix(market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
                    ncol=n_t, nrow=n_w, byrow=FALSE)
w_mat <- matrix(wage_vec, ncol=n_t, nrow=n_w, byrow=FALSE)
new_tild_theta <- sweep(w_mat + (rho[LA])^(-1)*new_theta, 2, apply(w_mat + (rho[LA])^(-1)*new_theta, 2, min))

## (a) per-firm task assignment best response (inner fixed point) + endog c/q
out_fields <- c("c_endog","q_endog", e_field_names)
la[, (out_fields) := counterfactual_org_outputs(
    cost_matrix=new_tild_theta, alpha=as.numeric(.SD), gamma=gamma_invert,
    wage_guess=wage_vec, new_theta=new_theta, innertol=innertol, config=CFG
  )[c("c_endog","q_endog", e_field_names)],
  by=c("location_id"), .SDcols=task_mix_cols]

## (b) best-response pricing
la[, Q := q_endog*avg_labor + qual_exo]
la[, C := pmax(c_endog*avg_labor + cost_exo, 0)]
la[, newprice := counterfactual_best_response_prices(cust_price, Q, C, weight, rho[LA], outertol, paste(LA,QY))]
la[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[LA])]

## aggregate labor by worker type (skill set)
L <- sapply(seq_len(n_w), function(k)
  sum(la$weight * la$new_share * la$CSPOP * la[[e_field_names[k]]] * la$avg_labor))

## data-anchored observed totals (E_raw based) for comparison
data_tot <- as.numeric(as.matrix(compute_counterfactual_total_labor(working_data, CFG)[
  county==LA & quarter_year==QY, .SD, .SDcols=tot_field_names]))

cat("\n=== LA total labor by skill set (worker type) at ESTIMATED wages ===\n")
cat(sprintf("%-9s %18s %18s %12s\n", "skill", "L_model(est wages)", "L_data(observed)", "model/data"))
for (k in seq_len(n_w))
  cat(sprintf("type %d   %18.2f %18.2f %12.3f\n", k, L[k], data_tot[k], L[k]/data_tot[k]))
cat(sprintf("%-9s %18.2f %18.2f\n", "TOTAL", sum(L), sum(data_tot)))

res <- data.table(skill_type=seq_len(n_w), wage=wage_vec,
                  L_model_est_wages=L, L_data_observed=data_tot, ratio=L/data_tot)
saveRDS(res, "smoke_la_total_labor_est_wages.rds")
fwrite(res, "results/data/la_total_labor_est_wages.csv")
cat("\nSaved: results/data/la_total_labor_est_wages.csv\n")
