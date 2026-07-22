## Immigration counterfactual.
## Boosts total labor supply (summed across all worker types) by 5%, with
## the entire increase concentrated in the lowest-wage worker type per
## county. Then re-solves wages and writes the productivity panel.
##
source("config.R")
source("utils/counterfactuals_core.R")

innertol <- CONFIG$counterfactual_innertol
outertol <- CONFIG$counterfactual_outertol

counterfactual_context <- load_counterfactual_context()
working_data    <- counterfactual_context$working_data
initial_wages   <- counterfactual_context$initial_wages
all_results     <- counterfactual_context$all_results
market_parms    <- counterfactual_context$market_parms
total_labor     <- counterfactual_context$total_labor
total_labor_orig<- counterfactual_context$total_labor_orig
initial_guess   <- counterfactual_context$initial_guess

## Scenario-specific warm-start wages (per county × sol_type) from a prior run.
warm_table <- read_counterfactual_warm_start_table("16_warm_start_wages_immigration.rds")
if (!is.null(warm_table)) {
  message("[16] loaded immigration warm-start table: ", nrow(warm_table), " rows.")
}
rho             <- counterfactual_context$rho
tild_theta      <- counterfactual_context$tild_theta

n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
task_mix_cols  <- get_task_mix_cols(CONFIG)
e_field_names  <- counterfactual_e_field_names(CONFIG)
b_field_names  <- counterfactual_b_field_names(CONFIG)
spec_log <- counterfactual_spec_log

market_input_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP"
)

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
    counterfactual_tot_labor_field_names(CONFIG)
  )]
}

solve_org_fresh <- function(alpha, gamma, mats, wage_guess,
                            with_s_index = FALSE,
                            with_b = FALSE, with_swept_b = FALSE) {
  counterfactual_org_outputs(
    cost_matrix  = mats$new_tild_theta,
    alpha        = alpha,
    gamma        = gamma,
    wage_guess   = wage_guess,
    new_theta    = mats$new_theta,
    innertol     = innertol,
    with_s_index = with_s_index,
    with_b       = with_b,
    with_swept_b = with_swept_b,
    config       = CONFIG
  )
}

solve_org_from_saved <- function(loc, alpha, gamma, mats, wage_guess,
                                 saved_struct, saved_b_cols,
                                 with_s_index = FALSE,
                                 with_b = FALSE, with_swept_b = FALSE) {
  saved_B <- matrix(
    as.numeric(saved_struct[location_id == loc, .SD, .SDcols = saved_b_cols]),
    byrow = FALSE,
    nrow = n_worker_types, ncol = n_task_types
  )
  counterfactual_org_outputs_from_b(
    B            = saved_B,
    alpha        = alpha,
    gamma        = gamma,
    wage_guess   = wage_guess,
    new_theta    = mats$new_theta,
    with_s_index = with_s_index,
    with_b       = with_b,
    with_swept_b = with_swept_b,
    config       = CONFIG
  )
}

get_everything <- function(wage_guess, cnty, qy) {
  counter_res <- copy(working_data[county == cnty & quarter_year == qy,
                                   ..market_input_cols])
  mats <- build_market_matrices(wage_guess, cnty)
  output_fields <- c("c_endog", "q_endog", "s_index", e_field_names, b_field_names)

  counter_res[, (output_fields) := solve_org_fresh(
      as.numeric(.SD), gamma_invert, mats, wage_guess,
      with_s_index = TRUE, with_b = TRUE
    ),
    by = c("location_id"),
    .SDcols = task_mix_cols
  ]

  apply_pricing(counter_res, cnty, qy)
}

orig_struct <- build_counterfactual_structure_snapshot(get_everything, initial_wages)


## Increase labor supply for the lowest-baseline-wage worker type in each county.
## By default the increase is 5% of total county labor.
##
## Environment overrides:
##   JMP_IMM_SHOCK_FRAC       shock fraction
##   JMP_IMM_SHOCK_BASE       "county" for total labor or "target" for target labor
##   JMP_IMM_TARGET_OVERRIDE  explicit county-to-worker-type mappings
##   JMP_IMM_OUT_SUFFIX       suffix for robustness output files
imm_shock_frac <- as.numeric(Sys.getenv("JMP_IMM_SHOCK_FRAC", unset = "0.05"))
imm_shock_base <- tolower(Sys.getenv("JMP_IMM_SHOCK_BASE", unset = "county"))
stopifnot(is.finite(imm_shock_frac), imm_shock_frac > 0,
          imm_shock_base %in% c("county", "target"))
imm_out_suffix <- Sys.getenv("JMP_IMM_OUT_SUFFIX", unset = "")

tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)
add_immigrants_to_target <- function(cnty, target_idx, qy = 2021.2) {
  base <- as.numeric(as.matrix(total_labor[
    county == cnty & quarter_year == qy,
    .SD, .SDcols = tot_field_names
  ]))
  delta <- if (identical(imm_shock_base, "target")) {
    imm_shock_frac * base[target_idx]
  } else {
    imm_shock_frac * sum(base)
  }
  message(sprintf(
    "[16] %s: target SS%d, delta = %.4g (%.2f%% of county labor, %.1f%% of target's own)",
    cnty, target_idx, delta, 100 * delta / sum(base), 100 * delta / base[target_idx]))
  target_col <- tot_field_names[target_idx]
  total_labor[county == cnty & quarter_year == qy,
              (target_col) := get(target_col) + delta]
}
imm_target_types <- counterfactual_lowest_wage_types(initial_wages)

## Optional per-county target override (robustness for argmin near-ties).
imm_target_override <- Sys.getenv("JMP_IMM_TARGET_OVERRIDE", unset = "")
if (nzchar(imm_target_override)) {
  for (kv in strsplit(imm_target_override, ",")[[1]]) {
    parts <- strsplit(trimws(kv), "=")[[1]]
    stopifnot(length(parts) == 2L)
    cnty_o <- parts[1]; type_o <- as.integer(parts[2])
    stopifnot(cnty_o %in% names(imm_target_types),
              type_o >= 1L, type_o <= CONFIG$n_worker_types)
    message(sprintf("[16] TARGET OVERRIDE %s: SS%d -> SS%d",
                    cnty_o, imm_target_types[[cnty_o]], type_o))
    imm_target_types[[cnty_o]] <- type_o
  }
}

message("[16] immigration shock: frac=", imm_shock_frac, " base=", imm_shock_base,
        if (nzchar(imm_out_suffix)) paste0(" suffix=", imm_out_suffix) else "")
message("[16] immigration target types (lowest baseline wage per county): ",
        paste(names(imm_target_types), imm_target_types, sep = "->",
              collapse = ", "))
for (cnty in names(imm_target_types)) {
  add_immigrants_to_target(cnty, imm_target_types[[cnty]])
}

res_wages <- new_counterfactual_wages_grid(unique(total_labor_orig$quarter_year))


solve_reloc <- function(wage_guess) {
  counter_res <- copy(working_data[county == cnty & quarter_year == qy,
                                   c(market_input_cols, e_field_names),
                                   with = FALSE])
  mats <- build_market_matrices(wage_guess, cnty)
  saved_b_cols <- grep("^B_", colnames(orig_struct[[cnty]]), value = TRUE)
  output_fields <- c("c_endog", "q_endog", e_field_names)

  counter_res[, (output_fields) := solve_org_from_saved(
      location_id, as.numeric(.SD), gamma_invert, mats, wage_guess,
      orig_struct[[cnty]], saved_b_cols
    ),
    by = c("location_id"),
    .SDcols = task_mix_cols
  ]

  counter_res <- apply_pricing(counter_res, cnty, qy)
  new_total_labor <- new_total_labor_from(counter_res)
  stopifnot(nrow(new_total_labor) == 1)
  counterfactual_labor_gap(new_total_labor, total_labor, cnty, qy)
}

solve_wages <- function(wage_guess) {
  counter_res <- copy(working_data[county == cnty & quarter_year == qy,
                                   ..market_input_cols])
  mats <- build_market_matrices(wage_guess, cnty)
  output_fields <- c("c_endog", "q_endog", e_field_names)

  counter_res[, (output_fields) := solve_org_fresh(
      as.numeric(.SD), gamma_invert, mats, wage_guess
    ),
    by = c("location_id"),
    .SDcols = task_mix_cols
  ]

  counter_res <- apply_pricing(counter_res, cnty, qy)
  new_total_labor <- new_total_labor_from(counter_res)
  stopifnot(nrow(new_total_labor) == 1)
  counterfactual_labor_gap(new_total_labor, total_labor, cnty, qy)
}

solve_wage_abs <- function(x) sum(abs(solve_wages(x)))

for (cnty in CONFIG$counties) {
  for (qy in get_counterfactual_focus_quarter()) {
    print(paste("*******", cnty, qy, "- shared", "immigration", "realloc solve"))
    baseline_start <- as.numeric(unlist(initial_wages[county == cnty & quarter_year == qy,
                                             paste0("w", seq_len(n_worker_types)),
                                             with = FALSE], use.names = FALSE))
    warm_realloc <- counterfactual_warm_start_by_sol_type(warm_table, cnty, qy,
                                                          "realloc", n_worker_types)
    if (!is.null(warm_realloc)) {
      message("[16] immigration realloc ", cnty, " ", qy, ": using warm-start wages.")
      start <- warm_realloc
      additional <- list(baseline_start)
    } else {
      start <- baseline_start
      additional <- list()
    }
    quad_wages <- counterfactual_solve_wage_market(
      solve_reloc, start,
      label = paste("immigration", "realloc", cnty, qy),
      target_tol = CONFIG$counterfactual_wage_tol,
      additional_starts = additional
    )
    counterfactual_store_wage_solution(res_wages, quad_wages, cnty, qy, "realloc")
  }
}

for (cnty in CONFIG$counties) {
  for (qy in get_counterfactual_focus_quarter()) {
    print(paste("*******", cnty, qy, "- shared", "immigration", "reorg solve"))
    realloc_start <- as.numeric(unlist(
      res_wages[county == cnty & quarter_year == qy & sol_type == "realloc",
                paste0("w", seq_len(n_worker_types)), with = FALSE],
      use.names = FALSE))
    baseline_start <- as.numeric(unlist(
      initial_wages[county == cnty & quarter_year == qy,
                    paste0("w", seq_len(n_worker_types)), with = FALSE],
      use.names = FALSE))
    warm_reorg <- counterfactual_warm_start_by_sol_type(warm_table, cnty, qy,
                                                        "reorg", n_worker_types)
    if (!is.null(warm_reorg)) {
      message("[16] immigration reorg ", cnty, " ", qy, ": using warm-start wages.")
      primary <- warm_reorg
      additional <- list(realloc_start, baseline_start)
    } else {
      primary <- realloc_start
      additional <- list(baseline_start)
    }
    quad_wages <- counterfactual_solve_wage_market(
      solve_wages, primary,
      label = paste("immigration", "reorg", cnty, qy),
      additional_starts = additional,
      target_tol = CONFIG$counterfactual_wage_tol
    )
    counterfactual_store_wage_solution(res_wages, quad_wages, cnty, qy, "reorg")
  }
}
save_counterfactual_rds(
  res_wages,
  paste0("16_wages_immigration", imm_out_suffix, ".rds"),
  legacy_filename = paste0("16_wages_immigration", imm_out_suffix, ".rds")
)


get_prod <- function(wage_guess, cnty, qy, stype) {
  counter_res <- copy(working_data[county == cnty & quarter_year == qy,
                                   ..market_input_cols])
  mats <- build_market_matrices(wage_guess, cnty)
  output_fields <- c("c_endog", "q_endog", "s_index", e_field_names, b_field_names)

  if (stype == "reorg") {
    counter_res[, (output_fields) := solve_org_fresh(
        as.numeric(.SD), gamma_invert, mats, wage_guess,
        with_s_index = TRUE, with_swept_b = TRUE
      ),
      by = c("location_id"),
      .SDcols = task_mix_cols
    ]
  } else if (stype == "realloc") {
    saved_b_cols <- grep("^B_", colnames(orig_struct[[cnty]]), value = TRUE)
    counter_res[, (output_fields) := solve_org_from_saved(
        location_id, as.numeric(.SD), gamma_invert, mats, wage_guess,
        orig_struct[[cnty]], saved_b_cols,
        with_s_index = TRUE, with_swept_b = TRUE
      ),
      by = c("location_id"),
      .SDcols = task_mix_cols
    ]
  } else {
    stopifnot(FALSE)
  }

  apply_pricing(counter_res, cnty, qy)
}


prod_data_new <- data.table()
for (cnty in unique(res_wages[is.finite(w1), ]$county)) {
  for (qy in unique(res_wages[is.finite(w1), ]$quarter_year)) {
    for (sol in unique(res_wages[is.finite(w1), ]$sol_type)) {
      wage_vec <- as.numeric(res_wages[county == cnty & quarter_year == qy & sol_type == sol,
                                       paste0("w", seq_len(n_worker_types)), with = FALSE])
      prod_data_new <- rbind(
        prod_data_new,
        data.table(county = cnty, quarter_year = qy, sol_type = sol,
                   get_prod(wage_vec, cnty, qy, sol))
      )
    }
  }
}
save_counterfactual_rds(
  prod_data_new,
  paste0("16_prod_immigration", imm_out_suffix, ".rds"),
  legacy_filename = paste0("16_prod_immigration", imm_out_suffix, ".rds")
)
