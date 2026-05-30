## Correctness test for the parallel + adaptive-cap + reorder changes.
## Confirms:
##   1. counterfactual_assignment with adaptive cap (cheap=1000 -> full)
##      returns the same E as a single direct call at the full cap.
##   2. counterfactual_apply_per_firm in parallel returns the same per-firm
##      output as sequential by-group iteration.
## Both checked at the LA baseline (52 firms) at the saved 13_initial_wages.
suppressPackageStartupMessages({ library(data.table); library(SQUAREM) })
source("config.R"); source("utils/counterfactuals_core.R")

CFG <- CONFIG
CFG$counterfactual_innertol <- 1e-10
CFG$counterfactual_fixedpoint_max_iter <- 1000000L

n_w <- CFG$n_worker_types; n_t <- CFG$n_task_types
task_mix_cols <- get_task_mix_cols(CFG); e_field_names <- counterfactual_e_field_names(CFG)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()

ctx <- load_counterfactual_context(config = CFG)
wd <- copy(ctx$working_data); market_parms <- ctx$market_parms; rho <- ctx$rho
init <- ctx$initial_wages
mic <- c("location_id","county","quarter_year","gamma_invert","avg_labor",
         task_mix_cols,"qual_exo","cost_exo","weight","cust_price","CSPOP")
la <- copy(wd[county==LA & quarter_year==QY, ..mic])

wages <- as.numeric(unlist(init[county==LA & quarter_year==QY, paste0("w",seq_len(n_w)), with=FALSE]))
new_theta <- matrix(market_parms[grep(paste0(LA,":avg_labor:B"),names(market_parms))],ncol=n_t,nrow=n_w,byrow=FALSE)
w_mat <- matrix(wages,ncol=n_t,nrow=n_w,byrow=FALSE)
ntt <- sweep(w_mat + (rho[LA])^(-1)*new_theta, 2, apply(w_mat + (rho[LA])^(-1)*new_theta,2,min))

## --- (1) Adaptive cap equivalence: new (cheap=1000) vs reference (cheap=0) ---
CFG_REF <- CFG; CFG_REF$counterfactual_fixedpoint_max_iter_cheap <- 0L   # disable adaptive
CFG_NEW <- CFG; CFG_NEW$counterfactual_fixedpoint_max_iter_cheap <- 1000L

solve_one <- function(alpha, gamma, CFG_use) {
  counterfactual_org_outputs(
    cost_matrix=ntt, alpha=alpha, gamma=gamma, wage_guess=wages,
    new_theta=new_theta, innertol=CFG_use$counterfactual_innertol, config=CFG_use)
}

ref_seq <- lapply(seq_len(nrow(la)), function(i) solve_one(as.numeric(la[i,..task_mix_cols]), la$gamma_invert[i], CFG_REF))
new_seq <- lapply(seq_len(nrow(la)), function(i) solve_one(as.numeric(la[i,..task_mix_cols]), la$gamma_invert[i], CFG_NEW))

extract_E <- function(lst) sapply(lst, function(r) sapply(e_field_names, function(f) r[[f]]))
E_ref <- extract_E(ref_seq); E_new <- extract_E(new_seq)
max_dE_adaptive <- max(abs(E_new - E_ref))
c_endog_ref <- sapply(ref_seq, function(r) r$c_endog)
c_endog_new <- sapply(new_seq, function(r) r$c_endog)
max_dc_adaptive <- max(abs(c_endog_new - c_endog_ref))
cat("=== TEST 1: Adaptive cap (cheap=1000 -> full) vs reference (cheap=0) ===\n")
cat(sprintf("  max|E_new - E_ref| over 52 firms: %.3g\n", max_dE_adaptive))
cat(sprintf("  max|c_endog_new - c_endog_ref|:   %.3g\n", max_dc_adaptive))

## --- (2) Parallel vs sequential per-firm helper ---
la_seq <- copy(la); la_par <- copy(la)
solve_org_wrap <- function(alpha, gamma) solve_one(alpha, gamma, CFG_NEW)

## Force sequential by setting SLURM_CPUS_PER_TASK=1 in this scope
.cores_saved <- Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1")
Sys.setenv(SLURM_CPUS_PER_TASK = "1")
la_seq <- counterfactual_apply_per_firm(la_seq, solve_org_wrap,
  output_fields=c("c_endog","q_endog",e_field_names), alpha_cols=task_mix_cols, config=CFG_NEW)
Sys.setenv(SLURM_CPUS_PER_TASK = .cores_saved)
la_par <- counterfactual_apply_per_firm(la_par, solve_org_wrap,
  output_fields=c("c_endog","q_endog",e_field_names), alpha_cols=task_mix_cols, config=CFG_NEW)

n_cores <- Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1")
cat("\n=== TEST 2: Parallel (cores=", n_cores, ") vs sequential per-firm helper ===\n", sep="")
diffs_par <- sapply(c("c_endog","q_endog",e_field_names), function(fld)
  max(abs(la_par[[fld]] - la_seq[[fld]])))
print(diffs_par)
max_par_diff <- max(diffs_par)
cat(sprintf("  max abs diff over all output fields: %.3g\n", max_par_diff))

## --- Verdict ---
ok_adaptive <- max_dE_adaptive < 1e-8 && max_dc_adaptive < 1e-6
ok_parallel <- max_par_diff < 1e-12
cat("\n================================================\n")
cat(sprintf("ADAPTIVE OK (matches reference < 1e-8 on E): %s\n", ok_adaptive))
cat(sprintf("PARALLEL OK (matches sequential < 1e-12):    %s\n", ok_parallel))
cat(sprintf("OVERALL: %s\n", if (ok_adaptive && ok_parallel) "PASS" else "FAIL"))
