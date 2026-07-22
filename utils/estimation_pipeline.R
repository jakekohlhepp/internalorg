rank_aware_direct_solve_diagnostics <- function(a, b, solution) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  solution <- as.matrix(solution)

  residual <- a %*% solution - b
  denom <- norm(a, type = "F") * norm(solution, type = "F") +
    norm(b, type = "F")
  relative_residual <- if (denom > 0) {
    norm(residual, type = "F") / denom
  } else {
    norm(residual, type = "F")
  }

  list(
    method = "solve",
    reciprocal_condition = tryCatch(rcond(a), error = function(e) NA_real_),
    relative_residual = relative_residual
  )
}

rank_aware_solve <- function(a, b, tolerance = sqrt(.Machine$double.eps),
                             context = "linear system", warn = TRUE) {
  a <- as.matrix(a)
  b <- as.matrix(b)

  ## Prefer a direct solve for full-rank systems whose columns have very
  ## different scales. Use the rank-aware SVD fallback only when the direct
  ## solve fails or its residual is unacceptable.
  direct <- tryCatch(
    suppressWarnings(solve(a, b)),
    error = function(e) e
  )
  if (!inherits(direct, "error")) {
    direct <- as.matrix(direct)
    rownames(direct) <- colnames(a)
    colnames(direct) <- colnames(b)
    diagnostics <- rank_aware_direct_solve_diagnostics(a, b, direct)

    residual_tolerance <- sqrt(tolerance)
    if (is.finite(diagnostics$relative_residual) &&
        diagnostics$relative_residual <= residual_tolerance) {
      if (warn &&
          is.finite(diagnostics$reciprocal_condition) &&
          diagnostics$reciprocal_condition < tolerance) {
        warning(
          context, " direct solve is ill-conditioned (rcond = ",
          signif(diagnostics$reciprocal_condition, 3),
          ") but has a small scaled residual (",
          signif(diagnostics$relative_residual, 3),
          "); retaining solve() result.",
          call. = FALSE
        )
      }
      return(direct)
    }

    if (warn) {
      warning(
        context, " direct solve returned a large scaled residual (",
        signif(diagnostics$relative_residual, 3),
        "); falling back to SVD minimum-norm solution.",
        call. = FALSE
      )
    }
  }

  ## SVD min-norm fallback for genuinely rank-deficient systems.
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

#' Cluster-robust (CR1) asymptotic variance-covariance matrix of the 2SLS
#' estimator produced by rank_aware_2sls().
#'
#'   V = c * (X'PzX)^{-1} [ sum_g s_g s_g' ] (X'PzX)^{-1}
#'
#' with Xhat = Pz X, score sums s_g = Xhat_g' u_g over cluster g, residuals
#' u = y - X beta evaluated at the 2SLS coefficients on the ORIGINAL X (not
#' Xhat), and the Stata-style small-sample factor
#' c = G/(G-1) * (N-1)/(N-K). This matches sandwich::vcovCL(fit, cluster,
#' type = "HC1") on an equivalent full-rank ivreg fit (validated in
#' tests/testthat/test_bootstrap_draws.R).
#' When return_scores = TRUE the result additionally carries the per-cluster
#' score sums (`scores`, G x K with cluster ids as rownames) and the
#' first-stage influence rows (`influence`, G x K, g-th row = (A^{-1} s_g)'),
#' which satisfy vcov == adjustment * crossprod(influence). These are the
#' psi_beta building blocks of the Murphy-Topel sandwich (07c_murphy_topel.R).
cluster_vcov_2sls <- function(x, z, y, beta, cluster,
                              tolerance = sqrt(.Machine$double.eps),
                              context = "2SLS clustered vcov",
                              return_scores = FALSE) {
  x <- as.matrix(x)
  z <- as.matrix(z)
  y <- as.numeric(y)
  cluster <- as.character(cluster)
  stopifnot(
    nrow(x) == length(y),
    nrow(x) == nrow(z),
    nrow(x) == length(cluster),
    !anyNA(cluster),
    nrow(beta) == ncol(x)
  )

  x_hat <- rank_aware_projection(z, x, tolerance, context)
  bread <- crossprod(x, x_hat)
  residuals <- y - as.numeric(x %*% beta)
  scores <- rowsum(x_hat * residuals, group = cluster)
  meat <- crossprod(scores)

  n_obs <- nrow(x)
  n_clusters <- nrow(scores)
  k_rank <- qr(x, tol = tolerance)$rank
  if (n_clusters < 2L) {
    stop(context, ": at least 2 clusters are required; got ", n_clusters, ".")
  }
  if (n_clusters < ncol(x)) {
    warning(
      context, ": fewer clusters (", n_clusters, ") than parameters (",
      ncol(x), "); the clustered vcov is singular with rank <= ", n_clusters,
      ". Draws from it stay confined to a ", n_clusters,
      "-dimensional subspace.",
      call. = FALSE
    )
  }
  adjustment <- (n_clusters / (n_clusters - 1)) * ((n_obs - 1) / (n_obs - k_rank))

  bread_inv_meat <- rank_aware_solve(
    bread, meat,
    tolerance = tolerance,
    context = paste(context, "bread")
  )
  vcov <- adjustment * t(rank_aware_solve(
    bread, t(bread_inv_meat),
    tolerance = tolerance,
    context = paste(context, "bread")
  ))
  vcov <- (vcov + t(vcov)) / 2
  dimnames(vcov) <- list(colnames(x), colnames(x))

  variances <- diag(vcov)
  if (any(variances < -tolerance * max(abs(variances)))) {
    stop(context, ": vcov has materially negative diagonal entries; ",
         "the bread solve is unreliable at this conditioning.")
  }
  se <- setNames(sqrt(pmax(variances, 0)), colnames(x))

  out <- list(
    vcov = vcov,
    se = se,
    n_obs = n_obs,
    n_clusters = n_clusters,
    rank = k_rank,
    adjustment = adjustment
  )
  if (isTRUE(return_scores)) {
    colnames(scores) <- colnames(x)
    influence <- t(rank_aware_solve(
      bread, t(scores),
      tolerance = tolerance,
      context = paste(context, "influence")
    ))
    dimnames(influence) <- dimnames(scores)
    out$scores <- scores
    out$influence <- influence
  }
  out
}

#' Murphy-Topel sandwich for the triangular three-stage system
#' (beta -> wage -> price); see docs/murphy_topel_proposal.md.
#'
#' Solves the block-triangular joint-GMM influence functions per cluster g:
#'   psi_b,g = A^{-1} s1_g                      (passed in as psi_b rows)
#'   psi_w,g = -J_2w^{-1} (s2_g + J_2b psi_b,g)
#'   psi_p,g = (MtM)^{-1} (s3_g + J_3b psi_b,g + J_3w psi_w,g)
#' and returns V = adjustment * crossprod(cbind(psi_b, psi_w, psi_p)).
#' All score matrices are per-cluster SUMS (G rows, aligned rownames);
#' Jacobian blocks are derivatives of the SUMMED moments. MtM = M'M is the
#' NEGATIVE of the price-moment Jacobian in p (g3 is linear in p).
assemble_triangular_mt_vcov <- function(psi_b, s2, s3,
                                        J_2w, J_2b, J_3w, J_3b, MtM,
                                        adjustment = 1,
                                        tolerance = sqrt(.Machine$double.eps)) {
  stopifnot(
    identical(rownames(psi_b), rownames(s2)),
    identical(rownames(psi_b), rownames(s3)),
    ncol(s2) == nrow(J_2w), ncol(s2) == ncol(J_2w),
    nrow(J_2b) == ncol(s2), ncol(J_2b) == ncol(psi_b),
    nrow(J_3w) == ncol(s3), ncol(J_3w) == ncol(s2),
    nrow(J_3b) == ncol(s3), ncol(J_3b) == ncol(psi_b),
    nrow(MtM) == ncol(s3), ncol(MtM) == ncol(s3)
  )

  ## psi_w,g' = -(s2_g + J_2b psi_b,g)' (J_2w^{-1})'
  rhs_w <- s2 + psi_b %*% t(J_2b)
  psi_w <- -t(solve(J_2w, t(rhs_w)))
  colnames(psi_w) <- colnames(s2)

  rhs_p <- s3 + psi_b %*% t(J_3b) + psi_w %*% t(J_3w)
  psi_p <- t(rank_aware_solve(MtM, t(rhs_p), tolerance = tolerance,
                              context = "Murphy-Topel price block"))
  colnames(psi_p) <- colnames(s3)

  psi <- cbind(psi_b, psi_w, psi_p)
  vcov <- adjustment * crossprod(psi)
  vcov <- (vcov + t(vcov)) / 2
  list(
    vcov = vcov,
    se = setNames(sqrt(pmax(diag(vcov), 0)), colnames(psi)),
    psi = psi
  )
}

#' Draw first-stage parameter vectors from N(beta, vcov) for the Petrin-Train
#' second-stage procedure (07_bootstrap.R). Returns an n_draws x K matrix with
#' columns named by rownames(beta). The caller owns the RNG seed. vcov may be
#' rank-deficient (fewer clusters than parameters); MASS::mvrnorm handles PSD
#' matrices via its eigendecomposition.
draw_first_stage_parameters <- function(beta, vcov, n_draws) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required to draw first-stage parameters.")
  }
  parm_names <- rownames(beta)
  stopifnot(
    n_draws >= 1L,
    length(parm_names) == nrow(vcov),
    identical(parm_names, rownames(vcov)),
    identical(parm_names, colnames(vcov))
  )
  draws <- MASS::mvrnorm(n = n_draws, mu = as.numeric(beta), Sigma = vcov)
  if (n_draws == 1L) {
    draws <- matrix(draws, nrow = 1L)
  }
  colnames(draws) <- parm_names
  draws
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
    "dye_instrument",
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

  ## Excluded instruments must retain variation after partialling out the
  ## exogenous controls; otherwise the 2SLS model is not identified.
  exog_cols <- intersect(colnames(mm_1), colnames(z_mm_1))
  excluded_inst_cols <- setdiff(colnames(z_mm_1), colnames(mm_1))
  if (length(excluded_inst_cols) > 0L && length(exog_cols) > 0L) {
    exog_only <- z_mm_1[, exog_cols, drop = FALSE]
    excluded_only <- z_mm_1[, excluded_inst_cols, drop = FALSE]
    proj <- rank_aware_projection(
      exog_only, excluded_only,
      tolerance = tolerance,
      context = "instrument identification check"
    )
    inst_residual <- excluded_only - proj
    inst_residual_norms <- sqrt(colSums(inst_residual^2))
    inst_norms <- sqrt(colSums(excluded_only^2))
    relative_residual <- ifelse(inst_norms > 0,
                                inst_residual_norms / inst_norms, 0)
    collinear <- relative_residual < sqrt(tolerance)
    if (any(collinear)) {
      stop(
        "Excluded instrument column(s) lie in the column space of the ",
        "exogenous controls (no identifying variation after partialling ",
        "out fixed effects and other exog controls): ",
        paste(excluded_inst_cols[collinear], collapse = ", "),
        ". This typically indicates the FE specification is too saturated ",
        "relative to the variation in the instrument. Relative residual norms: ",
        paste0(excluded_inst_cols[collinear], "=",
               signif(relative_residual[collinear], 3),
               collapse = ", "),
        "."
      )
    }
  }

  beta <- rank_aware_2sls(
    mm_1,
    z_mm_1,
    estim_matrix[, "log_rel_mkt"],
    tolerance = tolerance,
    context = "demand IV estimator"
  )

  ## Optional workers-as-rows monotonicity restriction on the per-county skill
  ## matrix B[task, worker]. Replaces the unconstrained 2SLS beta with the
  ## constrained QP solution, searching all n_w! permutations of worker types
  ## per county and picking the one that minimizes the constrained 2SLS
  ## criterion. The wage / price stages downstream consume this constrained
  ## beta unchanged (they do not optimize over beta).
  monotone_fit <- NULL
  monotone_mode <- if (is.null(config$skill_monotone_orientation)) "none" else config$skill_monotone_orientation
  if (identical(monotone_mode, "workers_rows")) {
    monotone_fit <- search_workers_perm(
      X = mm_1,
      Z = z_mm_1,
      y = estim_matrix[, "log_rel_mkt"],
      beta_names = rownames(beta),
      counties = config$counties,
      county_vec = as.character(estim_matrix[, "county"]),
      n_t = config$n_task_types,
      n_w = config$n_worker_types,
      tolerance = tolerance,
      context = "demand IV monotone (workers_rows)"
    )
    beta <- monotone_fit$beta
  } else if (!identical(monotone_mode, "none")) {
    stop("Unsupported config$skill_monotone_orientation: ", monotone_mode)
  }

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
    skill_monotone_fit = monotone_fit,
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
    " - 1 | factor(county):cust_price | factor(county):dye_instrument"
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

is_uniform_weights <- function(weights, tolerance = sqrt(.Machine$double.eps)) {
  if (is.null(weights)) {
    return(TRUE)
  }
  weights <- as.numeric(weights)
  if (length(weights) == 0 || anyNA(weights) || any(!is.finite(weights))) {
    return(FALSE)
  }
  max(abs(weights - weights[1])) <= tolerance
}

build_weighted_estimation_setup <- function(working_data, estim_matrix,
                                            config = CONFIG, weights = NULL) {
  if (is_uniform_weights(weights)) {
    return(build_estimation_setup(working_data, estim_matrix, config = config))
  }

  beta <- fit_demand_parameters(working_data, config, weights)
  unweighted_setup <- build_estimation_setup(working_data, estim_matrix, config = config)
  unweighted_setup$beta <- beta
  unweighted_setup
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
  data[, p_adj := cust_price - wb_sum - gamma_invert * s_index * avg_labor +
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

  ## Exact-solution fast path: when every lower bound is slack, the moment
  ## system weighted_col_means((p_adj - M x) * M) = 0 is exactly the
  ## (weighted) OLS normal equations, so solve it in closed form and keep it
  ## if it is feasible. Only a genuinely bound-constrained problem falls
  ## through to the bounded L-BFGS-B search below. (On the current sample the
  ## unconstrained OLS violates ~19 of the 36 min-wage bounds, so the solver
  ## path still runs there; the fast path covers reweighted/bootstrap samples
  ## and future data where the bounds turn out slack.)
  w_sqrt <- if (is.null(weights)) NULL else sqrt(weights)
  ols_x <- if (is.null(w_sqrt)) mod_mm_2 else mod_mm_2 * w_sqrt
  ols_y <- if (is.null(w_sqrt)) data$p_adj else data$p_adj * w_sqrt
  ols_coef <- tryCatch(
    as.numeric(rank_aware_ols(ols_x, ols_y, context = "price stage OLS fast path")),
    error = function(e) {
      warning("price stage OLS fast path failed (", conditionMessage(e),
              "); using bounded L-BFGS-B.", call. = FALSE)
      NULL
    }
  )
  ols_feasible <- !is.null(ols_coef) && all(is.finite(ols_coef)) &&
    all(ols_coef >= lower_bound)

  if (ols_feasible) {
    final_reg <- list(
      par = ols_coef,
      value = obj_final_reg(ols_coef),
      counts = c(`function` = 1L, gradient = NA_integer_),
      convergence = 0L,
      message = "exact OLS solution (all lower bounds slack)"
    )
    message("fit_price_parameters: unconstrained OLS satisfies every lower ",
            "bound; using the exact solution (moment objective ",
            signif(final_reg$value, 3), ").")
  } else {
    if (!is.null(ols_coef)) {
      n_violated <- sum(is.finite(lower_bound) & ols_coef < lower_bound)
      message("fit_price_parameters: unconstrained OLS violates ", n_violated,
              " lower bound(s); falling back to bounded L-BFGS-B.")
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
  }
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
  moment_weights <- if (is_uniform_weights(weights)) NULL else weights

  if (is.null(beta)) {
    estimation_setup <- build_weighted_estimation_setup(
      working_data,
      estim_matrix,
      config = config,
      weights = moment_weights
    )
    beta <- estimation_setup$beta
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
      moment_weights = moment_weights
    )
  } else {
    wage_result <- list(par = beta_2_subset, convergence = NA_integer_)
  }

  wage_coefs <- wage_result$par
  names(wage_coefs) <- names(beta_2_subset)

  # The wage solver writes a finite last_moment_norm into solver_state. If that
  # is left in place, get_solver_tolerances() flips fit_price_parameters'
  # get_gammas() calls onto coarse tolerances, making gamma_invert (and hence
  # the L-BFGS-B objective) depend on the wage trajectory rather than only on
  # (wage_coefs, beta, data). Reset to Inf so the price step always uses fine
  # tolerances; warm-start gamma/E hints in the cache stay valid.
  if (!is.null(solver_state)) {
    solver_state$last_moment_norm <- Inf
  }
  default_state <- get_default_solver_state()
  if (!is.null(default_state)) {
    default_state$last_moment_norm <- Inf
  }

  price_result <- fit_price_parameters(
    working_data,
    estim_matrix,
    beta,
    wage_coefs,
    beta_2_subset,
    min_wage_levels,
    config = config,
    clust = clust,
    weights = moment_weights,
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
