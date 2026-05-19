## compute counterfactuals
## Store only the essentials of the equilibrium:
### 1. Equilibrium wages (to get equilibrium again)
### 2. org Structures (because these require contraction to recover)
## 

## Compute the following objects
## 1. Consumer surplus
## 2. specialization (s-index)
## 3. Specialization (max fraction of time spent on one task)
## 4. sum of theta*B at each firm
## 5. marginal cost of each firm.

## Structure:
### First loop: wages
### Second loop: best-response pricing (contract on lambert's w). 
#### convergence not for sure. but if we end up converging, this is a best response to best responses.
### Third loop: best-response org structure. convergence guaranteed.
source("config.R")
source("utils/counterfactuals_core.R")

innertol <- CONFIG$counterfactual_innertol
outertol <- CONFIG$counterfactual_outertol

load_counterfactual_packages()
ensure_counterfactual_dirs()
spec_log<-function(x)  ifelse(x==0 | is.nan(x),0,log(x))

n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
e_field_names <- counterfactual_e_field_names(CONFIG)
e_raw_cols    <- get_E_raw_cols(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)


## need to get back employee count
staff_task<-data.table(read_first_existing_rds(
  c(project_path(CONFIG$prep_output_dir, "01_staff_task.rds"),
    legacy_counterfactual_data_path("01_00_staff_task.rds")),
  "counterfactual staff-task input"
))
has_emps<-unique(staff_task[, c("emps", "location_id", "quarter_year")])


## market parameters
working_data<-read_first_existing_rds(
  c(project_path("results", "data", "12_data_for_counterfactuals.rds"),
    legacy_counterfactual_data_path("02_06_data_for_counterfactuals.rds")),
  "counterfactual working-data input"
)
working_data<-merge(working_data, has_emps, by=c("location_id", "quarter_year"), all.x=TRUE)
stopifnot(nrow(working_data[is.na(emps)])==0)
rm(has_emps)
rm(staff_task)



## wage adjusted skills

all_results<-read_counterfactual_parameters()
market_parms<-get_counterfactual_market_parms(all_results)
guess_setup<-build_counterfactual_initial_guess(working_data, market_parms)
tild_theta<-build_counterfactual_tild_theta(working_data, market_parms, guess_setup$rho)


##### Initial State: Compute Total Labor
cex<-readRDS('mkdata/data/cex_outside.rds')
cex[, outside_share:=nohc_count /count_sample ]
cex<-cex[str_detect(PSU,"S")>0,]
cex<-unique(cex)
stopifnot(nrow(cex)==uniqueN(cex[,c("quarter_year", "PSU")]))

# get psu to county xwalk
cex<-merge(cex,data.table(readRDS("mkdata/data/county_msa_xwalk.rds")),by="PSU", all.x=TRUE, allow.cartesian = TRUE)
stopifnot(nrow(cex)==uniqueN(cex[,c("quarter_year", "county")]))
cex[, county:=as.character(county)]

### some firms were not in the estimation sample originally
## for these we need to plug in B_raw as the computed B

for (col in gsub("^B_raw_","",names(working_data)[grep("^B_raw_", names(working_data))])) working_data[,(paste0("B_raw_",col))  := ifelse(is.na(get(paste0("B_raw_",col))),get(paste0("B_",col)),get(paste0("B_raw_",col))) ]




addition_info<-read_first_existing_rds(
  c(project_path(CONFIG$prep_output_dir, "01_staff_task_full.rds"),
    legacy_counterfactual_data_path("01_00_staff_task_full.rds")),
  "counterfactual staff-task full input"
)
addition_info<-unique(addition_info[,.SD, .SDcols=c("location_id", "quarter_year", "cust_count", "CSPOP", "revenue")])
working_data<-merge(working_data, addition_info, by=c("location_id", "quarter_year"), all.x=TRUE)
working_data<-merge(working_data,cex[,c("county", "quarter_year", "outside_share") ], by=c("county", "quarter_year"),all.x=TRUE )
working_data[, salon_share_subdiv:=cust_count/CSPOP]
# weight is balanced by qcew firm size distributions for 2021Q2
size_classes<-c(50398,14240,6663,2270+29+44+1+1)
working_data[emps<5, size_class:=1]
working_data[emps>=5 & emps<=9, size_class:=2]
working_data[emps>9 & emps<=19, size_class:=3]
working_data[emps>19, size_class:=4]
stopifnot(nrow(working_data[emps>99,])==0)

working_data[, tot_inclass:= uniqueN(location_id) , by=c("quarter_year", "county", "size_class") ]
working_data[, size_mult:=size_classes[size_class]/tot_inclass]


#working_data[, weight:=size_mult*(1-outside_share)/(sum(size_mult*salon_share_subdiv)), by=c("county","quarter_year")]
working_data[, weight:=(1-outside_share)/(sum(salon_share_subdiv)), by=c("county","quarter_year")]
stopifnot(nrow(working_data[is.na(weight) | is.infinite(weight)])==0)

# check weight
checkit<-working_data[, .(tot=sum(weight*salon_share_subdiv), outside_share=unique(outside_share)), by=c("county", "quarter_year")]
checkit[,shouldadd:=tot+outside_share]
stopifnot(nrow(checkit[round(shouldadd,15)!=1,])==0)
rm(checkit)

# other misc variables - need to make to get quality-cost residuals
working_data[, avg_labor:=tot_duration/cust_count/60]
working_data[, log_rel_mkt:=log(salon_share_subdiv/outside_share)]
working_data[, cust_price:=revenue/cust_count]
working_data[, mk_piece:=1/(1-salon_share_subdiv)]


## for this portion only, we obtain org_cost by 
working_data[, org_cost:=ifelse(is.finite(gamma_invert),gamma_invert*s_index*avg_labor,0)]

## Compute amount of each type.
get_demands <- function(alpha_vec, county, quarter_year, gamma){
  cost_matrix <- tild_theta[[county]][[as.character(quarter_year)]]
  if (is.finite(gamma) & gamma > 0 & !is.na(gamma)){
    A <- exp(-1 / gamma * cost_matrix)
    E <- rep(1 / n_worker_types, n_worker_types)
    A[A >= Inf] <- CONFIG$numeric_ceiling
    A[A <= 0]   <- CONFIG$numeric_floor
    fxpt <- function(p){
      C <- colSums(t(A) * alpha_vec / colSums(A * p))
      p * C
    }
    for (i in seq_len(CONFIG$fixedpoint_max_iter)){
      E_old <- E
      E <- fxpt(E_old)
      if (all(abs(E - E_old) < innertol)) break
    }
    B <- t(t(A) * alpha_vec / colSums(A * E)) * E
  } else if (is.infinite(gamma) | is.na(gamma) | is.nan(gamma)){
    # max frictions
    B <- matrix(0, ncol = n_task_types, nrow = n_worker_types)
    B[which.min(rowSums(t(t(cost_matrix) * alpha_vec))), ] <- alpha_vec
  } else {
    # no frictions
    B <- matrix(0, ncol = n_task_types, nrow = n_worker_types)
    for (col in seq_len(n_task_types)){
      B[which.min(cost_matrix[, col]), col] <- alpha_vec[col]
    }
  }

  E <- rowSums(B)
  B[abs(B) < CONFIG$B_zero_threshold] <- 0
  Brel <- t(t(B / E) / alpha_vec)
  c(E, sum(B * spec_log(Brel)))
}
task_mix_cols <- get_task_mix_cols(CONFIG)
for (i in seq_len(nrow(working_data))){
  alpha_vec <- as.numeric(working_data[i, ..task_mix_cols])
  working_data[i, c(e_field_names, "s_pred") := as.list(get_demands(
    alpha_vec,
    county,
    quarter_year,
    gamma_invert
  ))]
}
# fill in labor demands for firms not in regression estimation sample.
# to get labor demand we use raw shares for firms in estimation sample and we use model shares for others.

for (idx in seq_len(n_worker_types)){
  raw_col <- e_raw_cols[idx]
  pred_col <- e_field_names[idx]
  working_data[is.na(get(raw_col)), (raw_col) := get(pred_col)]
}


# labor demand is then weights times market share times avg_labor times cspop
total_labor <- compute_counterfactual_total_labor(working_data, CONFIG)
total_labor_orig <- copy(total_labor)

# get quality and marginal cost heterogeneity (residual)
# instead of writing out the entire multiplication I rebuild 
# the feature matrix

# only missing should be infinite gammas



parm_demand<-all_results[demand==TRUE,]$coefficients
parm_supply<-all_results[demand==FALSE & !str_detect(parm_name, "E_raw_[0-9]$"),]$coefficients
wb_cols <- paste0("wb_", 2:n_worker_types)
for (idx in 2:n_worker_types){
  raw_col <- paste0("E_raw_", idx)
  wb_col  <- paste0("wb_", idx)
  working_data[, (wb_col) := market_parms[paste0(
    "factor(county)", county, ":avg_labor:", raw_col
  )] * get(raw_col) * avg_labor]
}

## get exogenous cost and demand shifters
## these are everything net of org cost, markups, 
xnam<-c("factor(county):cust_price","factor(county):factor(quarter_year)",paste0("factor(county):avg_labor:",names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]))
xnam <- as.formula(paste0("~",paste0(xnam, collapse="+"),"-1"))
mm_1<-model.matrix(xnam, data=working_data)
mm_1[,-c(grep(":cust_price$",colnames(mm_1)),grep("B_raw_",colnames(mm_1)))]<-0
working_data[,qual_exo:=log_rel_mkt- mm_1%*%parm_demand]
working_data[, mean_qual_exo:=sum(qual_exo*(service_mix_id=="11111"))/sum(service_mix_id=="11111"), by=c("county", "quarter_year")]


xnam <- as.formula(paste0(
  "~avg_labor:factor(county):factor(quarter_year)+",
  "factor(quarter_year):factor(county)+",
  "factor(quarter_year):(", build_task_mix_sum(CONFIG), ")-1"
))
mod_mm_2<-model.matrix(xnam, data=working_data)
mod_mm_2[,-grep("^avg_labor:",colnames(mod_mm_2))]<-0
wb_total <- Reduce("+", lapply(wb_cols, function(col) working_data[[col]]))
working_data[, cost_exo:=cust_price-wb_total-org_cost+mk_piece/market_parms[paste0("factor(county)",county, ":cust_price")] - mod_mm_2%*%parm_supply ]
working_data[, mean_cost_exo:=sum(cost_exo*(service_mix_id=="11111"))/sum(service_mix_id=="11111"), by=c("county", "quarter_year")]


## ----------------------------------------------------------------------------
## Model-consistent residual adjustment for the focus quarter. Replaces the
## data-anchored qual_exo / cost_exo (built immediately above from B_raw,
## E_raw, s_index) with residuals that absorb the gap to model-implied
## B, E, s solved at the supply-regression "parm wages". The identity
## (prices/shares fixed) collapses to:
##   qual_exo += (q_endog_data - q_endog_model) * avg_labor
##   cost_exo += (c_endog_data - c_endog_model) * avg_labor
## See smoke_qual_cost_exo_endog_vs_data.R for the derivation; smoke runs
## (smoke_13_with_model_exo) showed this overlay clears labor markets at
## 2021.2 for all three counties.
## Only focus-quarter rows are adjusted because the wage solver below and all
## 14_-19_ counterfactuals operate exclusively on the focus quarter.
## ----------------------------------------------------------------------------
focus_qy <- get_counterfactual_focus_quarter()

new_theta_by_county <- lapply(CONFIG$counties, function(cnty) {
  matrix(market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
         ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
})
names(new_theta_by_county) <- CONFIG$counties

parm_wage_vec <- function(cnty, qy) {
  w1 <- market_parms[paste0(
    "avg_labor:factor(county)", cnty, ":factor(quarter_year)", qy
  )]
  diffs <- market_parms[grep(paste0(cnty, ":avg_labor:E"), names(market_parms))]
  c(w1, w1 + diffs)
}

## B_raw columns are named `B_raw_<task>_<worker>`; flatten in the same order
## as `as.vector(theta_demand)` (column-major over a (worker x task) matrix).
b_raw_cols_ordered <- as.vector(vapply(
  seq_len(n_task_types),
  function(t) paste0("B_raw_", t, "_", seq_len(n_worker_types)),
  character(n_worker_types)
))
stopifnot(all(b_raw_cols_ordered %in% names(working_data)))

working_data[, q_endog_model := NA_real_]
working_data[, c_endog_model := NA_real_]

for (cnty_ in CONFIG$counties) {
  wage_vec <- parm_wage_vec(cnty_, as.character(focus_qy))
  if (any(!is.finite(wage_vec))) {
    message("[", cnty_, " ", focus_qy,
            "] skipping residual adjustment (non-finite parm wage).")
    next
  }
  new_theta <- new_theta_by_county[[cnty_]]
  w_mat <- matrix(wage_vec, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  tild  <- w_mat + (guess_setup$rho[cnty_])^(-1) * new_theta
  tild  <- sweep(tild, 2, apply(tild, 2, min), FUN = "-")

  rows <- working_data[, which(county == cnty_ & quarter_year == focus_qy)]
  for (i in rows) {
    alpha <- as.numeric(working_data[i, ..task_mix_cols])
    out <- counterfactual_org_outputs(
      cost_matrix  = tild,
      alpha        = alpha,
      gamma        = working_data$gamma_invert[i],
      wage_guess   = wage_vec,
      new_theta    = new_theta,
      innertol     = innertol,
      with_s_index = FALSE,
      with_b       = FALSE,
      config       = CONFIG
    )
    set(working_data, i, "q_endog_model", out$q_endog)
    set(working_data, i, "c_endog_model", out$c_endog)
  }
}

## Data-side q_endog / c_endog using B_raw, E_raw, s_index, gamma_invert.
working_data[, q_endog_data := NA_real_]
working_data[, c_endog_data := NA_real_]
for (cnty_ in CONFIG$counties) {
  rows <- working_data[, which(county == cnty_ & quarter_year == focus_qy)]
  if (length(rows) == 0L) next
  theta_demand  <- new_theta_by_county[[cnty_]]
  wage_vec_data <- parm_wage_vec(cnty_, as.character(focus_qy))
  if (any(!is.finite(wage_vec_data))) next
  B_mat <- as.matrix(working_data[rows, ..b_raw_cols_ordered])
  q_vec <- as.numeric(B_mat %*% as.vector(theta_demand))
  E_mat <- as.matrix(working_data[rows, ..e_raw_cols])
  g_vec <- working_data$gamma_invert[rows]
  s_vec <- working_data$s_index[rows]
  c_vec <- as.numeric(E_mat %*% wage_vec_data) +
    ifelse(is.finite(g_vec), g_vec * s_vec, 0)
  set(working_data, rows, "q_endog_data", q_vec)
  set(working_data, rows, "c_endog_data", c_vec)
}

adjust_rows <- working_data[, which(quarter_year == focus_qy &
                                      !is.na(q_endog_model) &
                                      !is.na(q_endog_data))]
working_data[adjust_rows,
             qual_exo := qual_exo + (q_endog_data - q_endog_model) * avg_labor]
working_data[adjust_rows,
             cost_exo := cost_exo + (c_endog_data - c_endog_model) * avg_labor]

cat(sprintf(
  "Model-consistent residual adjustment applied to %d rows at focus_qy=%s.\n",
  length(adjust_rows), as.character(focus_qy)
))

working_data[, mean_qual_exo := sum(qual_exo * (service_mix_id == "11111")) /
               sum(service_mix_id == "11111"),
             by = c("county", "quarter_year")]
working_data[, mean_cost_exo := sum(cost_exo * (service_mix_id == "11111")) /
               sum(service_mix_id == "11111"),
             by = c("county", "quarter_year")]

working_data[, c("q_endog_model", "c_endog_model",
                 "q_endog_data",  "c_endog_data") := NULL]


## trim

#### solve counterfactual

# start guess at estimated wages, overridable by warm-starts from prior smoke
# runs (see compile_warm_start_wages.R).
initial_guess<-guess_setup$initial_guess
rho<-guess_setup$rho
warm_table <- read_counterfactual_warm_starts()


solve_wages_market_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP"
)

solve_wages <- function(wage_guess){
  counter_res <- copy(working_data[county == cnty & quarter_year == qy,
                                   ..solve_wages_market_cols])

  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  ## sweep here, because it improves numerical performance. undo when calcing quality.
  new_theta <- matrix(market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
                      ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  w_mat <- matrix(wage_guess, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  new_tild_theta <- w_mat + (rho[cnty])^(-1) * new_theta
  new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))

  ## solve internal org via the shared counterfactuals_core helper.
  solve_org <- function(alpha, gamma){
    counterfactual_org_outputs(
      cost_matrix = new_tild_theta,
      alpha = alpha,
      gamma = gamma,
      wage_guess = wage_guess,
      new_theta = new_theta,
      innertol = innertol,
      with_s_index = FALSE,
      with_b = FALSE,
      config = CONFIG
    )
  }

  counter_res[, c("c_endog", "q_endog", e_field_names) :=
                solve_org(as.numeric(.SD), gamma_invert),
              by = c("location_id"),
              .SDcols = task_mix_cols]
  counter_res[, Q := q_endog * avg_labor + qual_exo]
  counter_res[, C := c_endog * avg_labor + cost_exo]
  # do not allow negative costs.
  counter_res[C < 0, C := 0]

  counter_res[, newprice := counterfactual_best_response_prices(
    cust_price, Q, C, weight, rho[cnty], outertol, paste(cnty, qy)
  )]
  counter_res[, new_share := counterfactual_logit_shares(
    Q, newprice, weight, rho[cnty]
  )]

  new_total_labor <- counter_res[, setNames(
    lapply(seq_len(n_worker_types), function(idx) {
      sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)
    }),
    tot_field_names
  )]

  stopifnot(nrow(new_total_labor) == 1)
  counterfactual_labor_gap(new_total_labor, total_labor, cnty, qy)
}

solve_wage_abs<-function(x){
  return(sum(abs(solve_wages(x))))
}

## Same body as solve_wages through the labor-aggregation step, but returns
## the per-worker-type aggregate (the new_total_labor row) rather than the
## clearing gap. Used after the solver loop to record the baseline-
## equilibrium labor amounts (which downstream counterfactuals must target
## instead of the data anchor for markets where the full-5D fallback was needed).
eval_total_labor <- function(wage_guess){
  counter_res <- copy(working_data[county == cnty & quarter_year == qy,
                                   ..solve_wages_market_cols])
  new_theta <- matrix(market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
                      ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  w_mat <- matrix(wage_guess, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  new_tild_theta <- w_mat + (rho[cnty])^(-1) * new_theta
  new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))

  solve_org <- function(alpha, gamma){
    counterfactual_org_outputs(
      cost_matrix = new_tild_theta,
      alpha = alpha,
      gamma = gamma,
      wage_guess = wage_guess,
      new_theta = new_theta,
      innertol = innertol,
      with_s_index = FALSE,
      with_b = FALSE,
      config = CONFIG
    )
  }

  counter_res[, c("c_endog", "q_endog", e_field_names) :=
                solve_org(as.numeric(.SD), gamma_invert),
              by = c("location_id"),
              .SDcols = task_mix_cols]
  counter_res[, Q := q_endog * avg_labor + qual_exo]
  counter_res[, C := c_endog * avg_labor + cost_exo]
  counter_res[C < 0, C := 0]

  counter_res[, newprice := counterfactual_best_response_prices(
    cust_price, Q, C, weight, rho[cnty], outertol, paste(cnty, qy)
  )]
  counter_res[, new_share := counterfactual_logit_shares(
    Q, newprice, weight, rho[cnty]
  )]

  out <- counter_res[, setNames(
    lapply(seq_len(n_worker_types), function(idx) {
      sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)
    }),
    tot_field_names
  )]
  stopifnot(nrow(out) == 1)
  as.numeric(out)
}


res_wages <- new_counterfactual_wages_grid(
  unique(total_labor_orig$quarter_year),
  include_solution_type = FALSE
)

## Per-market log of which markets fell through to the full-5D best-effort
## fall-back (and the resulting per-component residuals there). Empty list ==
## every market cleared in 5D via BBsolve.
full5d_fallback_log <- list()

for (cnty in CONFIG$counties) {
  for (qy in get_counterfactual_focus_quarter()) {
    print(paste("*******", cnty, qy, "- BBsolve baseline solve"))
    start <- initial_guess[grep(paste0("^", cnty, "-", qy), names(initial_guess))]
    warm <- counterfactual_warm_start_for(warm_table, cnty, as.character(qy))
    if (!is.null(warm)) {
      message("[", cnty, " ", qy, "] using smoke-derived warm-start wages.")
      start <- warm
    }

    ## Tier 1: try to solve the full 5-equation labor-clearing system with
    ## BBsolve (NM=FALSE then NM=TRUE, keep whichever has smaller max-abs).
    quad_wages <- counterfactual_solve_wage_market_bbsolve(
      solve_wages,
      start,
      label = paste("baseline", cnty, qy),
      target_tol = CONFIG$counterfactual_wage_tol
    )

    if (!isTRUE(quad_wages$converged)) {
      ## Full-5D fallback. NEVER drops a residual. Tries nleqslv (Broyden/dbldog
      ## and Newton/dbldog) and minimizer-based attempts (L-BFGS-B and
      ## Nelder-Mead on SSR) over all five residuals. Keeps the best result
      ## (lowest max-abs residual norm) across attempts.
      message("[", cnty, " ", qy, "] full 5D BBsolve did not converge ",
              "(max_abs=", signif(quad_wages$residual, 4),
              "); trying nleqslv and minimizer fallbacks on full 5D system.")

      target_at_market <- as.numeric(
        total_labor[county == cnty & quarter_year == qy,
                    .SD, .SDcols = tot_field_names]
      )

      fb_result <- counterfactual_full_5d_retry(
        solve_wages,
        start_par = quad_wages$par,
        label = paste("baseline", cnty, qy),
        target_tol = CONFIG$counterfactual_wage_tol
      )

      if (counterfactual_better_result(fb_result, quad_wages)) {
        quad_wages <- fb_result
      }

      if (!isTRUE(quad_wages$converged)) {
        full5d_fallback_log[[paste(cnty, qy)]] <- list(
          county = cnty, quarter_year = qy,
          strategy = "full_5d_best_effort",
          method = quad_wages$method,
          residual_norm = quad_wages$residual,
          residual_components = quad_wages$residual_components,
          wages = quad_wages$par,
          target_labor = target_at_market
        )
        warning("[", cnty, " ", qy, "] no full-5D method cleared target_tol=",
                signif(CONFIG$counterfactual_wage_tol, 4),
                "; keeping best attempt (method=", quad_wages$method,
                ", residual=", signif(quad_wages$residual, 4), ").",
                call. = FALSE)
      }
    }

    counterfactual_store_wage_solution(res_wages, quad_wages, cnty, qy)
  }
}

cat("\nFull-5D fall-back summary (markets that did not clear under BBsolve):\n")
if (length(full5d_fallback_log) == 0L) {
  cat("  (none; every market converged under BBsolve in 5D)\n")
} else {
  for (entry in full5d_fallback_log) {
    cat(sprintf(
      "  county=%s qy=%s  strategy=%s  best_method=%s  max_abs_resid=%.4g\n",
      entry$county, as.character(entry$quarter_year),
      entry$strategy, entry$method, entry$residual_norm
    ))
    cat("    residual_components: ",
        paste(signif(entry$residual_components, 3), collapse = ", "), "\n", sep = "")
    cat("    wages:               ",
        paste(signif(entry$wages, 4), collapse = ", "), "\n", sep = "")
  }
}

## Compute and persist the BASELINE-EQUILIBRIUM total labor (model_labor
## evaluated at each market's final wages). For markets that cleared
## under Tier 1, this equals the data anchor by construction. For markets
## that fell through to the full-5D fallback (Tier 2), this differs from
## the data anchor: the fallback optimum lives at wages where model_labor
## does not equal the data target, and downstream counterfactuals (14_-17_)
## must target *this* equilibrium labor amount, not the unreachable data
## target.
baseline_total_labor <- copy(total_labor)
for (cnty in CONFIG$counties) {
  for (qy in get_counterfactual_focus_quarter()) {
    final_wages <- as.numeric(unlist(
      res_wages[county == cnty & quarter_year == qy,
                paste0("w", seq_len(n_worker_types)), with = FALSE],
      use.names = FALSE
    ))
    if (length(final_wages) != n_worker_types ||
        any(!is.finite(final_wages)) ||
        any(final_wages <= 0)) {
      next
    }
    model_tot <- eval_total_labor(final_wages)
    baseline_total_labor[county == cnty & quarter_year == qy,
                          (tot_field_names) := as.list(model_tot)]
  }
}
cat("\nBaseline-equilibrium total labor (model_labor at final wages):\n")
for (cnty in CONFIG$counties) {
  for (qy in get_counterfactual_focus_quarter()) {
    data_row  <- as.numeric(total_labor[county == cnty & quarter_year == qy,
                                          .SD, .SDcols = tot_field_names])
    eq_row    <- as.numeric(baseline_total_labor[county == cnty & quarter_year == qy,
                                                  .SD, .SDcols = tot_field_names])
    pct <- 100 * (eq_row - data_row) / pmax(abs(data_row), .Machine$double.eps)
    cat(sprintf("  %s %s\n", cnty, as.character(qy)))
    for (k in seq_len(n_worker_types)) {
      cat(sprintf("    type=%d  data=%-12.4g  baseline_eq=%-12.4g  pct=%+.2f%%\n",
                  k, data_row[k], eq_row[k], pct[k]))
    }
  }
}

save_counterfactual_rds(
  res_wages,
  "13_initial_wages.rds",
  legacy_filename = "13_initial_wages.rds"
)
save_counterfactual_rds(
  working_data,
  "13_working_data.rds",
  legacy_filename = "13_working_data.rds"
)
save_counterfactual_rds(
  baseline_total_labor,
  "13_total_labor.rds",
  legacy_filename = NULL
)

## ----------------------------------------------------------------------------
## Baseline-equilibrium firm-level productivity panel.
## Loops over each (county, quarter) at the cleared wages from res_wages and
## records the firm-level B (swept by min-theta), E, s_index, c_endog, q_endog,
## Q, C, newprice, new_share. Downstream 18_counterfactual_summary.R reads
## this as the "Initial" baseline against which 14_/15_/16_/17_ counterfactual
## panels are compared.
##
## Previously this panel was written by 15_counterfactual_sales_tax.R at the
## end of its run, which meant any 15 failure (timeout or non-convergence on
## the sales-tax perturbation) left the baseline panel stale. The panel
## logically belongs to 13 because it depends only on the baseline wages.
## ----------------------------------------------------------------------------
b_field_names <- counterfactual_b_field_names(CONFIG)
prod_initial_market_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP"
)

compute_initial_prod_panel <- function(cnty, qy, wage_vec) {
  counter_res <- copy(working_data[county == cnty & quarter_year == qy,
                                   ..prod_initial_market_cols])
  if (nrow(counter_res) == 0L) return(counter_res)

  new_theta <- matrix(market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
                      ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  w_mat <- matrix(wage_vec, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  new_tild_theta <- w_mat + (rho[cnty])^(-1) * new_theta
  new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))

  output_fields <- c("c_endog", "q_endog", "s_index", e_field_names, b_field_names)
  counter_res[, (output_fields) := counterfactual_org_outputs(
    cost_matrix = new_tild_theta,
    alpha = as.numeric(.SD),
    gamma = gamma_invert,
    wage_guess = wage_vec,
    new_theta = new_theta,
    innertol = innertol,
    with_s_index = TRUE,
    with_swept_b = TRUE,
    config = CONFIG
  ), by = c("location_id"), .SDcols = task_mix_cols]

  counter_res[, Q := q_endog * avg_labor + qual_exo]
  counter_res[, C := c_endog * avg_labor + cost_exo]
  counter_res[C < 0, C := 0]
  counter_res[, newprice := counterfactual_best_response_prices(
    cust_price, Q, C, weight, rho[cnty], outertol, paste(cnty, qy)
  )]
  counter_res[, new_share := counterfactual_logit_shares(
    Q, newprice, weight, rho[cnty]
  )]
  counter_res[, sol_type := NA_character_]
  counter_res
}

prod_initial_panel <- data.table()
for (cnty in CONFIG$counties) {
  for (qy in get_counterfactual_focus_quarter()) {
    final_wages <- as.numeric(unlist(
      res_wages[county == cnty & quarter_year == qy,
                paste0("w", seq_len(n_worker_types)), with = FALSE],
      use.names = FALSE
    ))
    if (length(final_wages) != n_worker_types ||
        any(!is.finite(final_wages)) ||
        any(final_wages <= 0)) {
      message("[", cnty, " ", qy, "] skipping initial prod panel (bad wages).")
      next
    }
    panel <- compute_initial_prod_panel(cnty, qy, final_wages)
    if (nrow(panel) == 0L) next
    prod_initial_panel <- rbind(prod_initial_panel, panel, fill = TRUE)
  }
}

save_counterfactual_rds(
  prod_initial_panel,
  "13_prod_initial.rds",
  legacy_filename = "15_prod_initial.rds"
)



