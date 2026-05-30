## FALSE-BASIN AUDIT across every counterfactual cell.
##
## For the baseline (13_) and each counterfactual (14_ diffusion, 15_ salestax,
## 16_ immigration, 17_ merger), county in {17031, 36061, 6037}, sol_type in
## {reorg, realloc}, this re-evaluates the SAVED wage vector through the cell's
## own labor-clearing residual at two inner-solve settings:
##   production:  innertol = 1e-8,  fixedpoint_max_iter = 1e5  (current defaults)
##   hardened:    innertol = 1e-12, fixedpoint_max_iter = 1e6  (convergence study)
## A large change in max|resid| between the two => the saved solve sits in a
## false basin (inner contraction pseudo-converged at the loose setting).
##
## reorg + baseline re-run the inner SQUAREM assignment (sensitive). realloc uses
## the fixed baseline B (orig_struct), so its sensitivity enters only through the
## baseline B, which is rebuilt at each setting here.
##
## Output: smoke_audit_false_basins_all_cells.rds (gitignored)
suppressPackageStartupMessages({ library(data.table) })

source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types
n_t <- CONFIG$n_task_types
task_mix_cols   <- get_task_mix_cols(CONFIG)
e_field_names   <- counterfactual_e_field_names(CONFIG)
b_field_names   <- counterfactual_b_field_names(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)
QY <- get_counterfactual_focus_quarter()

market_input_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP"
)

improve_it <- function(x) {
  rankhold <- frank(x, ties.method = "first")
  sort(x)[pmax(rankhold - 1, 1)]
}

saved_wage_files <- list(
  diffusion   = "14_wages_diffusion.rds",
  salestax    = "15_wages_salestax.rds",
  immigration = "16_wages_immigration.rds",
  merger      = "17_wages_merger.rds"
)

get_saved_wages <- function(file, cnty, sol) {
  tab <- as.data.table(readRDS(counterfactual_data_path(file, CONFIG)))
  row <- tab[county == cnty & quarter_year == QY & sol_type == sol]
  if (nrow(row) != 1) return(NULL)
  w <- as.numeric(unlist(row[, paste0("w", seq_len(n_w)), with = FALSE]))
  if (!all(is.finite(w)) || any(w <= 0)) return(NULL)
  w
}

## Build everything needed at a given inner-solve setting.
build_for_setting <- function(innertol, maxiter) {
  CFG <- CONFIG
  CFG$counterfactual_innertol <- innertol
  CFG$fixedpoint_max_iter <- as.integer(maxiter)
  outertol <- 1e-8   # tight price loop (study showed price loop is innocent either way)

  ctx <- load_counterfactual_context(config = CFG)
  working_data  <- ctx$working_data
  initial_wages <- ctx$initial_wages
  total_labor0  <- ctx$total_labor
  market_parms  <- ctx$market_parms
  rho0          <- ctx$rho

  build_market_matrices <- function(wage_guess, cnty, rho) {
    new_theta <- matrix(market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
                        ncol = n_t, nrow = n_w, byrow = FALSE)
    w_mat <- matrix(wage_guess, ncol = n_t, nrow = n_w, byrow = FALSE)
    ntt <- w_mat + (rho[cnty])^(-1) * new_theta
    ntt <- sweep(ntt, 2, apply(ntt, 2, min))
    list(new_theta = new_theta, new_tild_theta = ntt)
  }
  apply_pricing <- function(cr, cnty, rho) {
    cr[, Q := q_endog * avg_labor + qual_exo]
    cr[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
    cr[, newprice := counterfactual_best_response_prices(
      cust_price, Q, C, weight, rho[cnty], outertol, paste(cnty, QY))]
    cr[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[cnty])]
    cr
  }
  new_total_labor_from <- function(cr) {
    cr[, setNames(lapply(seq_len(n_w), function(idx)
      sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)),
      tot_field_names)]
  }
  solve_org_fresh <- function(alpha, gamma, mats, wage_guess,
                              with_s_index = FALSE, with_b = FALSE) {
    counterfactual_org_outputs(
      cost_matrix = mats$new_tild_theta, alpha = alpha, gamma = gamma,
      wage_guess = wage_guess, new_theta = mats$new_theta, innertol = innertol,
      with_s_index = with_s_index, with_b = with_b, config = CFG)
  }
  solve_org_from_saved <- function(loc, alpha, gamma, mats, wage_guess,
                                   saved_struct, saved_b_cols) {
    saved_B <- matrix(as.numeric(saved_struct[location_id == loc, .SD, .SDcols = saved_b_cols]),
                      byrow = FALSE, nrow = n_w, ncol = n_t)
    counterfactual_org_outputs_from_b(
      B = saved_B, alpha = alpha, gamma = gamma, wage_guess = wage_guess,
      new_theta = mats$new_theta, config = CFG)
  }

  ## baseline structure snapshot (fixed B for realloc), built at baseline wages
  ## with UN-shocked working_data.
  get_everything <- function(wage_guess, cnty, qy) {
    cr <- copy(working_data[county == cnty & quarter_year == qy, ..market_input_cols])
    mats <- build_market_matrices(wage_guess, cnty, rho0)
    of <- c("c_endog", "q_endog", "s_index", e_field_names, b_field_names)
    cr[, (of) := solve_org_fresh(as.numeric(.SD), gamma_invert, mats, wage_guess,
                                 with_s_index = TRUE, with_b = TRUE),
       by = c("location_id"), .SDcols = task_mix_cols]
    apply_pricing(cr, cnty, rho0)
  }
  orig_struct <- build_counterfactual_structure_snapshot(get_everything, initial_wages,
                                                         config = CFG)

  ## residual for a fresh-assignment (reorg/baseline) solve
  resid_reorg <- function(cnty, wage_guess, wd, total_labor, rho) {
    cr <- copy(wd[county == cnty & quarter_year == QY, ..market_input_cols])
    mats <- build_market_matrices(wage_guess, cnty, rho)
    of <- c("c_endog", "q_endog", e_field_names)
    cr[, (of) := solve_org_fresh(as.numeric(.SD), gamma_invert, mats, wage_guess),
       by = c("location_id"), .SDcols = task_mix_cols]
    cr <- apply_pricing(cr, cnty, rho)
    ntl <- new_total_labor_from(cr)
    as.numeric(counterfactual_labor_gap(ntl, total_labor, cnty, QY))
  }
  ## residual for a fixed-B (realloc) solve
  resid_realloc <- function(cnty, wage_guess, wd, total_labor, rho) {
    cr <- copy(wd[county == cnty & quarter_year == QY,
                  c(market_input_cols, e_field_names), with = FALSE])
    mats <- build_market_matrices(wage_guess, cnty, rho)
    sbc <- grep("^B_", colnames(orig_struct[[cnty]]), value = TRUE)
    of <- c("c_endog", "q_endog", e_field_names)
    cr[, (of) := solve_org_from_saved(location_id, as.numeric(.SD), gamma_invert,
                                      mats, wage_guess, orig_struct[[cnty]], sbc),
       by = c("location_id"), .SDcols = task_mix_cols]
    cr <- apply_pricing(cr, cnty, rho)
    ntl <- new_total_labor_from(cr)
    as.numeric(counterfactual_labor_gap(ntl, total_labor, cnty, QY))
  }

  list(working_data = working_data, initial_wages = initial_wages,
       total_labor0 = total_labor0, rho0 = rho0,
       resid_reorg = resid_reorg, resid_realloc = resid_realloc)
}

## Apply each scenario's shock to copies of (working_data, total_labor, rho).
apply_shock <- function(scenario, wd, total_labor, rho) {
  wd <- copy(wd); total_labor <- copy(total_labor); rho <- rho
  if (scenario == "baseline") {
    ## no shock
  } else if (scenario == "diffusion") {
    wd[quarter_year == QY, gamma_invert := improve_it(gamma_invert),
       by = c("county", "quarter_year")]
  } else if (scenario == "merger") {
    wd[, weight := weight / 2]
  } else if (scenario == "salestax") {
    rho["36061"] <- rho["36061"] * 1.08 / 1.04
    rho["6037"]  <- rho["6037"]  * 1.04
    rho["17031"] <- rho["17031"] * 1.04
  } else if (scenario == "immigration") {
    tgt <- c("6037" = 1L, "36061" = 2L, "17031" = 4L)
    for (cn in names(tgt)) {
      base <- as.numeric(as.matrix(total_labor[county == cn & quarter_year == QY,
                                               .SD, .SDcols = tot_field_names]))
      delta <- 0.05 * sum(base)
      total_labor[county == cn & quarter_year == QY,
                  (tot_field_names[tgt[[cn]]]) := get(tot_field_names[tgt[[cn]]]) + delta]
    }
  }
  list(wd = wd, total_labor = total_labor, rho = rho)
}

## ---------------------------------------------------------------- run
settings <- list(
  production = list(innertol = 1e-8,  maxiter = 1e5),
  hardened   = list(innertol = 1e-12, maxiter = 1e6)
)

results <- list()
for (sname in names(settings)) {
  s <- settings[[sname]]
  cat("==================== building setting: ", sname,
      " (innertol=", s$innertol, ", maxiter=", s$maxiter, ") ====================\n", sep = "")
  env <- build_for_setting(s$innertol, s$maxiter)

  for (cnty in CONFIG$counties) {
    ## baseline (13): saved wages = initial_wages, target = total_labor0, no shock
    bw <- as.numeric(unlist(env$initial_wages[county == cnty & quarter_year == QY,
                                              paste0("w", seq_len(n_w)), with = FALSE]))
    if (all(is.finite(bw)) && all(bw > 0)) {
      r <- tryCatch(env$resid_reorg(cnty, bw, env$working_data, env$total_labor0, env$rho0),
                    error = function(e) rep(NA_real_, n_w))
      results[[length(results) + 1]] <- data.table(
        setting = sname, scenario = "baseline", county = cnty, sol_type = "baseline",
        max_abs = max(abs(r)), bind_worker = which.max(abs(r)),
        r1 = r[1], r2 = r[2], r3 = r[3], r4 = r[4], r5 = r[5])
    }
  }

  for (scenario in names(saved_wage_files)) {
    shk <- apply_shock(scenario, env$working_data, env$total_labor0, env$rho0)
    for (cnty in CONFIG$counties) {
      for (sol in c("reorg", "realloc")) {
        w <- get_saved_wages(saved_wage_files[[scenario]], cnty, sol)
        if (is.null(w)) next
        fn <- if (sol == "reorg") env$resid_reorg else env$resid_realloc
        r <- tryCatch(fn(cnty, w, shk$wd, shk$total_labor, shk$rho),
                      error = function(e) rep(NA_real_, n_w))
        results[[length(results) + 1]] <- data.table(
          setting = sname, scenario = scenario, county = cnty, sol_type = sol,
          max_abs = max(abs(r)), bind_worker = which.max(abs(r)),
          r1 = r[1], r2 = r[2], r3 = r[3], r4 = r[4], r5 = r[5])
      }
    }
  }
}

out <- rbindlist(results, fill = TRUE)

## reshape: production vs hardened side by side
wide <- dcast(out, scenario + county + sol_type ~ setting,
              value.var = c("max_abs", "bind_worker"))
wide[, delta_max_abs := abs(max_abs_hardened - max_abs_production)]
wide[, false_basin := is.finite(delta_max_abs) & delta_max_abs > 1e-3]
setorder(wide, -delta_max_abs)

cat("\n==================== FALSE-BASIN AUDIT (sorted by |delta max_abs|) ====================\n")
print(wide[, .(scenario, county, sol_type,
               max_abs_production, max_abs_hardened, delta_max_abs,
               bind_production = bind_worker_production,
               bind_hardened = bind_worker_hardened, false_basin)],
      nrows = 100)

cat("\nCells flagged as FALSE BASIN (|delta| > 1e-3): ",
    sum(wide$false_basin, na.rm = TRUE), " / ", nrow(wide), "\n", sep = "")
cat("Cells that CLEAR (<=1e-2) under hardened tol: ",
    sum(wide$max_abs_hardened <= 1e-2, na.rm = TRUE), " / ", nrow(wide), "\n", sep = "")

saveRDS(list(long = out, wide = wide), "smoke_audit_false_basins_all_cells.rds")
cat("\nSaved: smoke_audit_false_basins_all_cells.rds\n")
