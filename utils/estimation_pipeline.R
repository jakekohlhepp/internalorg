rank_aware_solve <- function(a, b, tolerance = sqrt(.Machine$double.eps),
                             context = "linear system", warn = TRUE) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  decomp <- svd(a)

  if (length(decomp$d) == 0 || max(decomp$d) == 0) {
    stop("Cannot solve ", context, ": coefficient matrix has zero rank.")
  }

  threshold <- max(dim(a)) * max(decomp$d) * tolerance
  keep <- decomp$d > threshold
  rank <- sum(keep)

  if (rank == 0) {
    stop("Cannot solve ", context, ": coefficient matrix has zero numerical rank.")
  }

  full_rank <- min(dim(a))
  if (warn && rank < full_rank) {
    warning(
      context, " is rank deficient (rank ", rank, " of ", full_rank,
      "); using SVD minimum-norm solution.",
      call. = FALSE
    )
  }

  u_keep <- decomp$u[, keep, drop = FALSE]
  v_keep <- decomp$v[, keep, drop = FALSE]
  scaled_rhs <- sweep(crossprod(u_keep, b), 1, decomp$d[keep], "/")
  solution <- v_keep %*% scaled_rhs

  rownames(solution) <- colnames(a)
  colnames(solution) <- colnames(b)
  solution
}

rank_aware_projection <- function(z, x, tolerance = sqrt(.Machine$double.eps),
                                  context = "projection") {
  z %*% rank_aware_solve(
    crossprod(z),
    crossprod(z, x),
    tolerance = tolerance,
    context = paste(context, "normal equations")
  )
}

rank_aware_2sls <- function(x, z, y, tolerance = sqrt(.Machine$double.eps),
                            context = "2SLS") {
  x_hat <- rank_aware_projection(z, x, tolerance, context)
  coef <- rank_aware_solve(
    crossprod(x, x_hat),
    crossprod(x_hat, as.matrix(y)),
    tolerance = tolerance,
    context = paste(context, "second stage")
  )
  rownames(coef) <- colnames(x)
  coef
}

rank_aware_ols <- function(x, y, tolerance = sqrt(.Machine$double.eps),
                           context = "OLS") {
  coef <- rank_aware_solve(
    crossprod(x),
    crossprod(x, as.matrix(y)),
    tolerance = tolerance,
    context = context
  )
  rownames(coef) <- colnames(x)
  coef
}

build_estimation_setup_rank_aware <- function(working_data, estim_matrix,
                                              config = CONFIG,
                                              tolerance = sqrt(.Machine$double.eps)) {
  stopifnot(all(!is.na(estim_matrix)))

  xnam <- c(
    "factor(county):cust_price",
    "factor(county):factor(quarter_year)",
    paste0("factor(county):avg_labor:", names(working_data)[grep("^B_raw_[0-9]_", names(working_data))])
  )
  xnam <- as.formula(paste0("~", paste0(xnam, collapse = "+"), "-1"))
  mm_1 <- model.matrix(xnam, data = estim_matrix)

  xnam2 <- paste0("factor(quarter_year):", names(working_data)[grep("^task_mix", names(working_data))])
  xnam3 <- paste0("factor(county):avg_labor:", names(working_data)[grep("^E_raw_[0-9]", names(working_data))])
  xnam4 <- c(xnam2, xnam3)
  xnam4 <- as.formula(paste0(
    "~factor(county):mk_piece+factor(county):org_cost+",
    paste0(xnam4, collapse = "+"),
    "+factor(county):factor(quarter_year):avg_labor+factor(county):factor(quarter_year)-1"
  ))
  mm_2 <- model.matrix(xnam4, data = estim_matrix)

  xnam <- c(
    "hausman_other_price",
    "factor(quarter_year)",
    paste0("avg_labor:", names(working_data)[grep("^B_raw_[0-9]_", names(working_data))])
  )
  xnam <- as.formula(paste0("~", paste0(paste0("factor(county):", xnam), collapse = "+"), "-1"))
  z_mm_1 <- model.matrix(xnam, data = estim_matrix)

  xnam2 <- paste0("factor(quarter_year):", names(working_data)[grep("^task_mix", names(working_data))])
  xnam3 <- paste0("factor(county):avg_labor:", names(working_data)[grep("^E_raw_[0-9]", names(working_data))])
  xnam4 <- c(xnam2, xnam3)
  xnam4 <- as.formula(paste0(
    "~factor(county):org_cost+",
    paste0(xnam4, collapse = "+"),
    "+factor(county):factor(quarter_year):avg_labor+factor(county):factor(quarter_year)-1"
  ))
  z_mm_2 <- model.matrix(xnam4, data = estim_matrix)

  e_raw_cols_no_first <- paste0("E_raw_", 2:config$n_worker_types)
  e_match <- data.frame(as.matrix(estim_matrix[, c(e_raw_cols_no_first, "s_index")]),
                        factor(estim_matrix[, "county"]))
  e_col_names <- paste0("E_", 2:config$n_worker_types)
  colnames(e_match) <- c(e_col_names, "s_index", "county")
  e_mat <- model.matrix(build_E_formula(2:config$n_worker_types, include_s_index = TRUE),
                        data = e_match)

  beta <- rank_aware_2sls(
    mm_1,
    z_mm_1,
    estim_matrix[, "log_rel_mkt"],
    tolerance = tolerance,
    context = "demand IV estimator"
  )

  p_adj <- estim_matrix[, "cust_price"]
  for (cnty in config$counties) {
    price_idx <- grep(paste0(cnty, ":cust_price"), rownames(beta))
    if (length(price_idx) != 1) {
      stop("Expected one price coefficient for county ", cnty,
           "; found ", length(price_idx), ".")
    }
    p_adj <- p_adj + estim_matrix[, "mk_piece"] *
      (1 / beta[price_idx, 1]) *
      (estim_matrix$county == cnty)
  }

  beta_2 <- rank_aware_ols(
    z_mm_2,
    p_adj,
    tolerance = tolerance,
    context = "price adjustment estimator"
  )

  list(
    beta = beta,
    beta_2 = beta_2,
    mm_1 = mm_1,
    mm_2 = mm_2,
    z_mm_1 = z_mm_1,
    z_mm_2 = z_mm_2,
    e_mat = e_mat,
    diagnostics = list(
      mm_1_rank = qr(mm_1, tol = tolerance)$rank,
      z_mm_1_rank = qr(z_mm_1, tol = tolerance)$rank,
      z_mm_2_rank = qr(z_mm_2, tol = tolerance)$rank
    )
  )
}

build_demand_iv_formula <- function(data, config = CONFIG) {
  b_cols <- names(data)[grep("^B_raw_[0-9]_", names(data))]
  exog_terms <- c(
    "factor(county):factor(quarter_year)",
    paste0("factor(county):avg_labor:", b_cols)
  )

  as.formula(paste0(
    "log_rel_mkt ~ ",
    paste0(exog_terms, collapse = " + "),
    " - 1 | factor(county):cust_price | factor(county):hausman_other_price"
  ))
}

fit_demand_parameters <- function(working_data, config = CONFIG, weights = NULL) {
  if (!requireNamespace("ivreg", quietly = TRUE)) {
    stop("Package 'ivreg' is required to fit demand parameters.")
  }

  data <- as.data.frame(working_data)
  weights <- normalize_moment_weights(weights, nrow(data))
  fit <- if (is.null(weights)) {
    ivreg::ivreg(build_demand_iv_formula(data, config), data = data)
  } else {
    data$.jmp_weight <- weights
    ivreg::ivreg(build_demand_iv_formula(data, config), data = data, weights = .jmp_weight)
  }
  beta <- as.matrix(stats::coef(fit), ncol = 1)
  rownames(beta) <- names(stats::coef(fit))
  beta
}

extract_wage_start <- function(parameters, config = CONFIG) {
  if (is.data.frame(parameters) && all(c("parm_name", "coefficients") %in% names(parameters))) {
    starting <- parameters$coefficients
    names(starting) <- parameters$parm_name
  } else {
    starting <- parameters
  }

  wage_terms <- paste0(":avg_labor:E_raw_", 2:config$n_worker_types, "$")
  wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, names(starting)))
  beta_2_subset <- starting[wage_idx]
  names(beta_2_subset) <- names(starting)[wage_idx]

  if (length(beta_2_subset) == 0) {
    stop("Could not find wage starting values in supplied parameters.")
  }

  beta_2_subset
}

add_price_adjustment <- function(working_data, estim_matrix, beta, wage_coefs,
                                 beta_2_subset, config = CONFIG, clust = NULL,
                                 solver_state = NULL) {
  data <- data.table::copy(data.table::as.data.table(working_data))

  for (i in 2:config$n_worker_types) {
    col_name <- paste0("wb_", i)
    e_raw_col <- paste0("E_raw_", i)
    data[, (col_name) := wage_coefs[paste0(
      "factor(county)", county, ":avg_labor:", e_raw_col
    )] * get(e_raw_col) * avg_labor]
  }

  data[, gamma_invert := get_gammas(
    wage_coefs,
    estim_matrix,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = config,
    clust = clust,
    solver_state = solver_state
  )]

  wb_cols <- paste0("wb_", 2:config$n_worker_types)
  wb_sum <- Reduce("+", lapply(wb_cols, function(col) data[[col]]))
  data[, p_adj := cust_price - wb_sum - gamma_invert * s_index +
         mk_piece / beta[paste0("factor(county)", county, ":cust_price"), ]]

  data
}

fit_price_parameters <- function(working_data, estim_matrix, beta, wage_coefs,
                                 beta_2_subset, min_wage_levels, config = CONFIG,
                                 clust = NULL, weights = NULL,
                                 solver_state = NULL) {
  data <- add_price_adjustment(
    working_data,
    estim_matrix,
    beta,
    wage_coefs,
    beta_2_subset,
    config,
    clust,
    solver_state
  )
  weights <- normalize_moment_weights(weights, nrow(data))
  if (!is.null(weights)) {
    data[, .jmp_weight := weights]
  }

  task_mix_formula_str <- build_task_mix_sum(config)
  first_try_formula <- as.formula(paste0(
    "p_adj ~ avg_labor:factor(county):factor(quarter_year) + ",
    "factor(county):factor(quarter_year) + ",
    "factor(quarter_year):(", task_mix_formula_str, ") - 1"
  ))
  first_try_fit <- if (is.null(weights)) {
    stats::lm(first_try_formula, data = data)
  } else {
    stats::lm(first_try_formula, data = data, weights = data$.jmp_weight)
  }
  first_try <- stats::coef(first_try_fit)

  temp <- data.table::data.table(V1 = names(first_try), position = seq_along(first_try))
  temp[, quarter_year := as.numeric(stringr::str_extract(V1, "\\b\\d{4}\\.\\d\\b"))][
    , county := as.numeric(gsub(
      "factor\\(county\\)",
      "",
      stringr::str_extract(V1, "factor\\(county\\)\\s*(\\d+)")
    ))]
  temp <- temp[grep("^avg_labor", V1), ]

  temp2 <- data.table::data.table(V1 = names(wage_coefs), value = wage_coefs)
  temp2 <- rbind(
    temp2,
    data.table::data.table(
      V1 = unique(stringr::str_replace(names(wage_coefs), "E_raw_[0-9]+", "E_raw_1")),
      value = 0
    )
  )
  temp2[, county := as.numeric(gsub(
    "factor\\(county\\)",
    "",
    stringr::str_extract(V1, "factor\\(county\\)\\s*(\\d+)")
  ))]

  min_wage_bounds <- data.table::copy(data.table::as.data.table(min_wage_levels))
  min_wage_bounds[, county := as.numeric(county)]
  temp <- merge(temp2, temp, by = "county", allow.cartesian = TRUE)
  temp <- merge(temp, min_wage_bounds, by = c("county", "quarter_year"), all.x = TRUE)
  stopifnot(nrow(temp[is.na(min_wage)]) == 0)

  temp_bounds <- temp[, .(lb = max(min_wage - value)), by = "position"]
  lower_bound <- rep(-Inf, length(first_try))
  lower_bound[temp_bounds$position] <- temp_bounds$lb

  starting_final_reg <- lower_bound + 10
  starting_final_reg[is.infinite(starting_final_reg)] <- 0

  xnam <- as.formula(paste0(
    "~avg_labor:factor(county):factor(quarter_year) + ",
    "factor(quarter_year):factor(county) + ",
    "factor(quarter_year):(", task_mix_formula_str, ") - 1"
  ))
  mod_mm_2 <- stats::model.matrix(xnam, data = data)

  obj_final_reg <- function(x) {
    residual_moments <- matrix(
      data$p_adj - mod_mm_2 %*% x,
      nrow = nrow(mod_mm_2),
      ncol = ncol(mod_mm_2),
      byrow = FALSE
    ) * mod_mm_2
    sum(weighted_col_means(residual_moments, weights)^2)
  }

  final_reg <- stats::optim(
    starting_final_reg,
    obj_final_reg,
    lower = lower_bound,
    upper = rep(Inf, length(starting_final_reg)),
    method = "L-BFGS-B",
    control = list(
      maxit = solver_value(config, "price_optimizer_maxit", 1000000L),
      trace = solver_value(config, "price_optimizer_trace", 3L),
      factr = config$obj_tol / .Machine$double.eps
    )
  )
  coef_vect2 <- final_reg$par
  names(coef_vect2) <- names(first_try)

  list(
    coefficients = coef_vect2,
    result = final_reg,
    working_data = data,
    lower_bound = lower_bound,
    starting_values = starting_final_reg
  )
}

estimate_structural_parameters <- function(working_data, estim_matrix, min_wage_levels,
                                           config = CONFIG, clust = NULL,
                                           weights = NULL, starting_parameters = NULL,
                                           beta = NULL, beta_2_subset = NULL,
                                           skip_structural_optimizer = config$skip_structural_optimizer,
                                           solver_state = NULL) {
  if (is.null(beta)) {
    beta <- fit_demand_parameters(working_data, config, weights)
  }

  if (is.null(beta_2_subset)) {
    if (is.null(starting_parameters)) {
      stop("starting_parameters or beta_2_subset must be supplied.")
    }
    beta_2_subset <- extract_wage_start(starting_parameters, config)
  }

  if (!isTRUE(skip_structural_optimizer)) {
    wage_result <- estimate_wage_parameters(
      beta_2_subset,
      estim_matrix,
      beta,
      beta_2_subset,
      config = config,
      clust = clust,
      solver_state = solver_state,
      moment_weights = weights
    )
  } else {
    wage_result <- list(par = beta_2_subset, convergence = NA_integer_)
  }

  wage_coefs <- wage_result$par
  names(wage_coefs) <- names(beta_2_subset)

  price_result <- fit_price_parameters(
    working_data,
    estim_matrix,
    beta,
    wage_coefs,
    beta_2_subset,
    min_wage_levels,
    config = config,
    clust = clust,
    weights = weights,
    solver_state = solver_state
  )

  coefficients <- c(as.numeric(beta), wage_coefs, price_result$coefficients)
  parameter_table <- data.table::data.table(
    demand = c(
      rep(TRUE, length(rownames(beta))),
      rep(FALSE, length(c(wage_coefs, price_result$coefficients)))
    ),
    parm_name = c(rownames(beta), names(wage_coefs), names(price_result$coefficients)),
    coefficients = coefficients
  )

  list(
    parameter_table = parameter_table,
    beta = beta,
    beta_2_subset = beta_2_subset,
    wage_coefficients = wage_coefs,
    price_coefficients = price_result$coefficients,
    wage_result = wage_result,
    price_result = price_result
  )
}

make_windows_solver_cluster <- function(config = CONFIG) {
  if (!identical(get_os(), "windows") || !isTRUE(config$pl_on)) {
    return(NULL)
  }

  clust <- parallel::makeCluster(get_core_count(config))
  worker_exports <- c(
    "CONFIG",
    "get_initial_E",
    "get_task_mix_cols",
    "get_E_raw_cols",
    "get_os",
    "get_core_count",
    "build_E_formula",
    "build_task_mix_sum",
    "build_cost_matrix",
    "compute_corner_solution",
    "spec_log",
    "normalize_worker_share",
    "solver_flag",
    "solver_value",
    "ensure_rcpp_equilibrium_solver",
    "solve_equilibrium",
    "bisection",
    "find_gamma_for_sindex",
    "get_worker_allocation"
  )
  parallel::clusterExport(clust, worker_exports, envir = .GlobalEnv)
  invisible(parallel::clusterEvalQ(clust, {
    library("data.table")
    library("SQUAREM")
    library("BB")
    NULL
  }))
  clust
}
