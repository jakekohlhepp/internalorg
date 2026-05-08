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


## trim

#### solve counterfactual

# start guess at estimated wages.
initial_guess<-guess_setup$initial_guess
rho<-guess_setup$rho


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


res_wages <- new_counterfactual_wages_grid(
  unique(total_labor_orig$quarter_year),
  include_solution_type = FALSE
)

for (cnty in CONFIG$counties) {
  for (qy in get_counterfactual_focus_quarter()) {
    print(paste("*******", cnty, qy, "- shared baseline solve"))
    start <- initial_guess[grep(paste0("^", cnty, "-", qy), names(initial_guess))]
    quad_wages <- counterfactual_solve_wage_market(
      solve_wages,
      start,
      label = paste("baseline", cnty, qy),
      target_tol = CONFIG$counterfactual_wage_tol
    )
    counterfactual_store_wage_solution(res_wages, quad_wages, cnty, qy)
  }
}
save_counterfactual_rds(
  res_wages,
  "05_00_initial_wages.rds",
  legacy_filename = "05_00_initial_wages.rds"
)
save_counterfactual_rds(
  working_data,
  "05_00_working_data.rds",
  legacy_filename = "05_00_working_data.rds"
)



