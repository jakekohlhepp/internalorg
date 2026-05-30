## How much marginal-cost truncation is going on? For each focus-quarter firm,
## solve the inner assignment at the baseline wages and compute
##   C = c_endog * avg_labor + cost_exo
## then count firms where C < 0 (the value that gets pmax'd to 0 in the wage
## solver). Compares production (Pass 1+2 residuals as in current
## 13_working_data) and Pass-1-only (recomputed from scratch).
suppressPackageStartupMessages({ library(data.table) })
source("config.R"); source("utils/counterfactuals_core.R")

CFG <- CONFIG; CFG$counterfactual_innertol <- 1e-12; CFG$counterfactual_fixedpoint_max_iter <- 1000000L
n_w <- CFG$n_worker_types; n_t <- CFG$n_task_types
task_mix_cols <- get_task_mix_cols(CFG); e_field_names <- counterfactual_e_field_names(CFG)
QY <- get_counterfactual_focus_quarter()
mic <- c("location_id","county","quarter_year","gamma_invert","avg_labor",
         task_mix_cols,"qual_exo","cost_exo","weight","cust_price","CSPOP")

ctx <- load_counterfactual_context(config = CFG)
wd_prod <- copy(ctx$working_data); market_parms <- ctx$market_parms; rho <- ctx$rho
init <- ctx$initial_wages

## --- Pass-1-only: recompute cost_exo from scratch (mirrors smoke_13_single_pass_only.R) ---
all_results <- read_counterfactual_parameters(CFG)
parm_supply <- all_results[demand==FALSE & !grepl("E_raw_[0-9]$", parm_name), ]$coefficients
parm_names_s <- all_results[demand==FALSE & !grepl("E_raw_[0-9]$", parm_name), ]$parm_name
names(parm_supply) <- parm_names_s
wd_p1 <- copy(wd_prod)
## wb_k = wage-bill term using E_raw and the avg_labor:E_raw_k coef
for (k in 2:n_w) {
  wd_p1[, (paste0("wb_",k)) := market_parms[paste0("factor(county)",county,":avg_labor:E_raw_",k)] *
                                get(paste0("E_raw_",k)) * avg_labor]
}
wb_cols <- paste0("wb_", 2:n_w)
wb_total <- Reduce("+", lapply(wb_cols, function(col) wd_p1[[col]]))
wd_p1[, org_cost := ifelse(is.finite(gamma_invert), gamma_invert*s_index*avg_labor, 0)]
xnam_s <- as.formula(paste0(
  "~avg_labor:factor(county):factor(quarter_year)+factor(quarter_year):factor(county)+",
  "factor(quarter_year):(", build_task_mix_sum(CFG), ")-1"))
mm_2 <- model.matrix(xnam_s, data = wd_p1)
mm_2[, -grep("^avg_labor:", colnames(mm_2))] <- 0
parm_supply_v <- mm_2 %*% parm_supply
wd_p1[, cost_exo := cust_price - wb_total - org_cost +
        mk_piece / market_parms[paste0("factor(county)",county,":cust_price")] - parm_supply_v]

## --- per-firm C count at the saved baseline wages, both residual sets ---
summarize <- function(label, working_data, init_wages) {
  cat("\n=== ", label, " (cost_exo: ", attr(working_data,"label"), ") ===\n", sep="")
  out <- list()
  for (cnty in CFG$counties) {
    wages <- as.numeric(unlist(init_wages[county==cnty & quarter_year==QY, paste0("w",seq_len(n_w)), with=FALSE]))
    if (!all(is.finite(wages))) next
    la <- copy(working_data[county==cnty & quarter_year==QY, ..mic])
    new_theta <- matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))],ncol=n_t,nrow=n_w,byrow=FALSE)
    w_mat <- matrix(wages,ncol=n_t,nrow=n_w,byrow=FALSE)
    ntt <- sweep(w_mat + (rho[cnty])^(-1)*new_theta, 2, apply(w_mat + (rho[cnty])^(-1)*new_theta,2,min))
    of <- c("c_endog","q_endog", e_field_names)
    la[, (of) := counterfactual_org_outputs(cost_matrix=ntt, alpha=as.numeric(.SD), gamma=gamma_invert,
          wage_guess=wages, new_theta=new_theta, innertol=CFG$counterfactual_innertol, config=CFG)[c("c_endog","q_endog",e_field_names)],
       by=c("location_id"), .SDcols=task_mix_cols]
    la[, C_raw := c_endog*avg_labor + cost_exo]
    n_total <- nrow(la); n_neg <- sum(la$C_raw < 0)
    neg <- la[C_raw < 0]
    cat(sprintf("  %s  n=%d  n(C<0)=%d (%.1f%%)  min_C=%.3g  median_neg_C=%s  sum_neg_|C|=%.3g  weight_share_neg=%.1f%%\n",
        cnty, n_total, n_neg, 100*n_neg/n_total, min(la$C_raw),
        if(n_neg>0) sprintf("%.3g", median(neg$C_raw)) else "-",
        if(n_neg>0) sum(abs(neg$C_raw)) else 0,
        if(n_neg>0) 100*sum(neg$weight)/sum(la$weight) else 0))
    out[[cnty]] <- la[, .(location_id, county, weight, c_endog, avg_labor, cost_exo, C_raw, was_truncated = C_raw<0)]
  }
  rbindlist(out, fill=TRUE)
}
attr(wd_prod,"label") <- "Pass 1+2 (production)"
attr(wd_p1,  "label") <- "Pass 1 only (no model-consistent overlay)"

d_prod <- summarize("Production cost_exo at 13_initial_wages",   wd_prod, init)
d_p1   <- summarize("Pass-1-only cost_exo at 13_initial_wages",  wd_p1,   init)

saveRDS(list(production=d_prod, pass1=d_p1), "smoke_cost_truncation.rds")
fwrite(d_prod, "results/data/cost_truncation_prod.csv")
fwrite(d_p1,   "results/data/cost_truncation_pass1only.csv")
cat("\nSaved: results/data/cost_truncation_{prod,pass1only}.csv\n")
