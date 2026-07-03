## ===========================================================================
## prod_diffusion_compare.R
##
## LA (6037) diffusion productivity for THREE wage solutions, all vs the Initial
## baseline, aggregated with 18_counterfactual_summary.R's summarize_prod_panel:
##   - realloc      : reallocation-only (baseline B), production cleared wages
##   - reorg_floor  : reorganization at the coord_descent floor wages
##                    (lw|r|~0.012, worker-5 BOUNDED at +0.31) -- the production point
##   - reorg_best   : reorganization at the lw-gate-clearing wages we saved as the
##                    warm start (lw|r|~0.006, worker-5 SACRIFICED at +1.006)
##
## realloc + reorg_floor are read straight from the production panel
## results/data/counterfactuals/14_prod_diffusion.rds; only reorg_best is
## recomputed (fresh org solve, swept B) at the saved wage vector.
## Output: prod_diffusion_compare.rds
## ===========================================================================
suppressPackageStartupMessages({ library(data.table); library(SQUAREM); library(stringr) })
source("config.R"); source("utils/counterfactuals_core.R")

LA <- "6037"; QY <- get_counterfactual_focus_quarter(); stopifnot(length(QY) == 1L)
n_worker_types <- CONFIG$n_worker_types; n_task_types <- CONFIG$n_task_types
innertol <- CONFIG$counterfactual_innertol; outertol <- CONFIG$counterfactual_outertol
task_mix_cols   <- get_task_mix_cols(CONFIG)
e_field_names   <- counterfactual_e_field_names(CONFIG)
b_field_names   <- counterfactual_b_field_names(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)
market_input_cols <- c("location_id","county","quarter_year","gamma_invert","avg_labor",
                       task_mix_cols,"qual_exo","cost_exo","weight","cust_price","CSPOP")

## ---- 18's summarize_prod_panel (parse-eval the def) ------------------------
ex18 <- parse("18_counterfactual_summary.R")
for (e in ex18) if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]),"<-") &&
    is.symbol(e[[2]]) && identical(as.character(e[[2]]),"summarize_prod_panel")) { eval(e, globalenv()); break }
stopifnot(exists("summarize_prod_panel"))

## ---- context + diffusion shock (for reorg_best recompute + residual target) -
ctx <- load_counterfactual_context(config = CONFIG)
working_data <- copy(ctx$working_data); market_parms <- ctx$market_parms
rho <- ctx$rho; total_labor <- copy(ctx$total_labor)
improve_it <- function(x) sort(x)[pmax(frank(x, ties.method = "first") - 1, 1)]
working_data[, gamma_invert := improve_it(gamma_invert), by = c("county","quarter_year")]

## ---- fresh reorg get_prod (mirrors 14) ------------------------------------
build_market_matrices <- function(w, cnty) {
  nt <- matrix(market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
               ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  wm <- matrix(w, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  ntt <- wm + (rho[cnty])^(-1) * nt; ntt <- sweep(ntt, 2, apply(ntt, 2, min))
  list(new_theta = nt, new_tild_theta = ntt)
}
apply_pricing <- function(cr, cnty, qy) {
  cr[, Q := q_endog * avg_labor + qual_exo]; cr[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
  cr[, newprice := counterfactual_best_response_prices(cust_price, Q, C, weight, rho[cnty], outertol, paste(cnty, qy))]
  cr[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[cnty])]; cr
}
get_prod_reorg <- function(w, cnty, qy) {
  cr <- copy(working_data[county == cnty & quarter_year == qy, ..market_input_cols])
  mats <- build_market_matrices(w, cnty); of <- c("c_endog","q_endog","s_index", e_field_names, b_field_names)
  cr[, (of) := counterfactual_org_outputs(cost_matrix = mats$new_tild_theta, alpha = as.numeric(.SD),
      gamma = gamma_invert, wage_guess = w, new_theta = mats$new_theta, innertol = innertol,
      with_s_index = TRUE, with_swept_b = TRUE, config = CONFIG)[of],
    by = c("location_id"), .SDcols = task_mix_cols]
  apply_pricing(cr, cnty, qy)
}
labor_gap <- function(cr) {
  ntl <- cr[, setNames(lapply(seq_len(n_worker_types), function(i)
    sum(weight * new_share * CSPOP * get(e_field_names[i]) * avg_labor)), tot_field_names)]
  as.numeric(counterfactual_labor_gap(ntl, total_labor, LA, QY))
}

## ---- wage vectors ----------------------------------------------------------
solved <- as.data.table(readRDS(counterfactual_data_path("14_wages_diffusion.rds")))
warm   <- as.data.table(readRDS(counterfactual_data_path("14_warm_start_wages_diffusion.rds")))
getw <- function(dt, st) as.numeric(unlist(dt[as.character(county)==LA & sol_type==st &
  as.character(quarter_year)==as.character(QY), paste0("w", seq_len(n_worker_types)), with = FALSE]))
w_realloc     <- getw(solved, "realloc")
w_reorg_floor <- getw(solved, "reorg")
w_reorg_best  <- getw(warm,   "reorg")
cat(sprintf("realloc      wages: [%s]\n", paste(signif(w_realloc,7), collapse=", ")))
cat(sprintf("reorg_floor  wages: [%s]\n", paste(signif(w_reorg_floor,7), collapse=", ")))
cat(sprintf("reorg_best   wages: [%s]\n\n", paste(signif(w_reorg_best,7), collapse=", ")))

## ---- panels: read production realloc + reorg(floor); recompute reorg(best) --
prod <- as.data.table(readRDS(counterfactual_data_path("14_prod_diffusion.rds")))
prod <- prod[, !duplicated(names(prod)), with = FALSE]
pan_realloc <- prod[as.character(county)==LA & sol_type=="realloc"]
pan_rfloor  <- prod[as.character(county)==LA & sol_type=="reorg"]
cr_best     <- get_prod_reorg(w_reorg_best, LA, QY)
pan_rbest   <- data.table(county = LA, quarter_year = QY, sol_type = "reorg", cr_best)

## ---- summarize + baseline --------------------------------------------------
S <- function(panel, tag) { s <- summarize_prod_panel(copy(panel), tag)
  list(firm = s$firm[as.character(county)==LA][1], type = s$type[as.character(county)==LA][1]) }
sc <- list(realloc = S(pan_realloc,"realloc"), reorg_floor = S(pan_rfloor,"reorg_floor"),
           reorg_best = S(pan_rbest,"reorg_best"))
base <- S(as.data.table(readRDS(counterfactual_data_path("13_prod_initial.rds"))), "Initial")
resid <- list(realloc = labor_gap(pan_realloc), reorg_floor = labor_gap(pan_rfloor), reorg_best = labor_gap(cr_best))

## ---- report ----------------------------------------------------------------
fb <- base$firm; nm <- c("realloc","reorg_floor","reorg_best")
pct <- function(n,o) 100*(n-o)/o
cat("================= LA DIFFUSION PRODUCTIVITY (levels) =================\n")
cat(sprintf("%-16s %12s %12s %12s %12s\n","metric","baseline", nm[1], nm[2], nm[3]))
for (m in c("s_avg","labor_prod","rev_per_labor"))
  cat(sprintf("%-16s %12.5g %12.5g %12.5g %12.5g\n", m, fb[[m]],
              sc$realloc$firm[[m]], sc$reorg_floor$firm[[m]], sc$reorg_best$firm[[m]]))

cat("\n================= % CHANGE vs baseline =================\n")
cat(sprintf("%-16s %12s %12s %12s\n","metric", nm[1], nm[2], nm[3]))
for (m in c("s_avg","labor_prod","rev_per_labor"))
  cat(sprintf("%-16s %11.2f%% %11.2f%% %11.2f%%\n", m,
              pct(sc$realloc$firm[[m]],fb[[m]]), pct(sc$reorg_floor$firm[[m]],fb[[m]]), pct(sc$reorg_best$firm[[m]],fb[[m]])))

cat("\n================= labor-clearing residual (context) =================\n")
cat(sprintf("%-12s %12s %12s\n","scenario","max|r|","lw|r|"))
for (k in nm) cat(sprintf("%-12s %12.5g %12.5g\n", k, max(abs(resid[[k]])), counterfactual_residual_norm_lw(resid[[k]])))

cat("\n================= per-worker-type productivity (% change vs baseline) =================\n")
cat(sprintf("%-9s %12s %12s %12s\n","type", nm[1], nm[2], nm[3]))
for (k in as.character(seq_len(n_worker_types))) {
  ob <- as.numeric(base$type[[k]])
  cat(sprintf("worker_%-2s %11.2f%% %11.2f%% %11.2f%%\n", k,
              pct(as.numeric(sc$realloc$type[[k]]),ob), pct(as.numeric(sc$reorg_floor$type[[k]]),ob),
              pct(as.numeric(sc$reorg_best$type[[k]]),ob)))
}

saveRDS(list(wages=list(realloc=w_realloc, reorg_floor=w_reorg_floor, reorg_best=w_reorg_best),
             firm=lapply(sc,`[[`,"firm"), type=lapply(sc,`[[`,"type"),
             baseline_firm=fb, baseline_type=base$type, residual=resid),
        "prod_diffusion_compare.rds")
cat("\nSaved: prod_diffusion_compare.rds\n")
