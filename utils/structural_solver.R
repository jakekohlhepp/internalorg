#' =============================================================================
#' Shared structural solver accelerators
#' =============================================================================
#'
#' This file overrides the baseline equilibrium and GMM helpers defined in
#' preamble.R with speed-oriented implementations. The API remains compatible
#' with the existing scripts, but the functions also expose warm starts and
#' reusable row-preparation helpers for counterfactual solvers.
#'
#' Main speedups:
#'   1. Precompute row-level task shares, counties, and observed worker shares.
#'   2. Collapse repeated row inputs within an objective evaluation.
#'   3. Warm-start gamma and worker shares across optimizer evaluations.
#'   4. Cache bisection function values so each gamma is evaluated once.
#'   5. Optionally solve wage parameters county-by-county.
#'   6. Optionally use an Rcpp plain fixed-point solver when explicitly enabled.
#'

normalize_worker_share <- function(E, config = CONFIG) {
  if (is.null(E) || length(E) != config$n_worker_types || any(!is.finite(E))) {
    E <- get_initial_E(config)
  }

  if (length(E) != config$n_worker_types) {
    E <- rep(1 / config$n_worker_types, config$n_worker_types)
  }

  E <- pmax(as.numeric(E), config$numeric_floor)
  E / sum(E)
}

make_solver_state <- function() {
  state <- new.env(parent = emptyenv())
  state$gamma_by_key <- new.env(parent = emptyenv())
  state$E_by_key <- new.env(parent = emptyenv())
  state$last_moment_norm <- Inf
  state$n_objective_calls <- 0L
  state
}

get_default_solver_state <- local({
  state <- make_solver_state()
  function(reset = FALSE) {
    if (isTRUE(reset)) {
      state <<- make_solver_state()
    }
    state
  }
})

solver_flag <- function(config, name, default = FALSE) {
  value <- config[[name]]
  if (is.null(value)) {
    return(default)
  }
  isTRUE(value)
}

solver_value <- function(config, name, default) {
  value <- config[[name]]
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(default)
  }
  value
}

format_solver_key <- function(county, alpha, s_index, config = CONFIG) {
  digits <- solver_value(config, "solver_cache_round_digits", 12L)
  rounded <- round(c(alpha, s_index), digits)
  paste(
    county,
    paste(format(rounded, scientific = FALSE, trim = TRUE), collapse = ","),
    sep = "|"
  )
}

prepare_equilibrium_rows <- function(x, config = CONFIG) {
  data <- as.data.frame(x)
  task_mix_cols <- get_task_mix_cols(config)
  e_raw_cols <- paste0("E_raw_", 2:config$n_worker_types)

  missing_cols <- setdiff(c("county", "s_index", task_mix_cols, e_raw_cols), names(data))
  if (length(missing_cols) > 0) {
    stop("Equilibrium rows missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  alpha <- as.matrix(data[, task_mix_cols, drop = FALSE])
  storage.mode(alpha) <- "double"
  county <- as.character(data[["county"]])
  s_index <- as.numeric(data[["s_index"]])
  observed_E <- as.matrix(data[, e_raw_cols, drop = FALSE])
  storage.mode(observed_E) <- "double"

  row_key <- vapply(
    seq_len(nrow(data)),
    function(i) format_solver_key(county[i], alpha[i, ], s_index[i], config),
    character(1)
  )

  if (solver_flag(config, "use_solver_cache", TRUE)) {
    unique_key <- unique(row_key)
    unique_id <- match(row_key, unique_key)
  } else {
    unique_key <- paste0(row_key, "|row=", seq_along(row_key))
    unique_id <- seq_along(row_key)
  }

  list(
    n = nrow(data),
    alpha = alpha,
    county = county,
    s_index = s_index,
    observed_E = observed_E,
    row_key = row_key,
    unique_key = unique_key,
    unique_id = unique_id,
    unique_first_row = match(unique_key, unique_key[unique_id])
  )
}

get_solver_tolerances <- function(config = CONFIG, state = NULL) {
  innertol <- config$innertol
  outertol <- config$outertol

  if (
    solver_flag(config, "use_staged_solver_tolerances", TRUE) &&
      !is.null(state) &&
      is.finite(state$last_moment_norm) &&
      state$last_moment_norm > solver_value(config, "staged_tolerance_switch_norm", 1e-3)
  ) {
    innertol <- max(innertol, solver_value(config, "coarse_innertol", innertol))
    outertol <- max(outertol, solver_value(config, "coarse_outertol", outertol))
  }

  list(innertol = innertol, outertol = outertol)
}

ensure_rcpp_equilibrium_solver <- local({
  compiled <- FALSE
  compile_failed <- FALSE

  function() {
    if (compiled) {
      return(TRUE)
    }
    if (compile_failed || !requireNamespace("Rcpp", quietly = TRUE)) {
      return(FALSE)
    }

    ok <- tryCatch({
      Rcpp::cppFunction('
        Rcpp::List solve_equilibrium_plain_cpp(
          Rcpp::NumericMatrix cost,
          Rcpp::NumericVector alpha,
          double gamma,
          Rcpp::NumericVector E,
          double tol,
          int max_iter,
          double floor_value,
          double ceiling_value,
          double zero_threshold
        ) {
          int n_w = cost.nrow();
          int n_t = cost.ncol();
          Rcpp::NumericMatrix A(n_w, n_t);
          double max_log_A = R_NegInf;

          for (int i = 0; i < n_w; ++i) {
            for (int j = 0; j < n_t; ++j) {
              double value = -cost(i, j) / gamma;
              if (value > max_log_A) max_log_A = value;
              A(i, j) = value;
            }
          }

          for (int i = 0; i < n_w; ++i) {
            for (int j = 0; j < n_t; ++j) {
              double value = std::exp(A(i, j) - max_log_A);
              if (value < floor_value) value = floor_value;
              if (value > ceiling_value) value = ceiling_value;
              A(i, j) = value;
            }
          }

          bool converged = false;
          int iter = 0;
          Rcpp::NumericVector E_next(n_w);
          Rcpp::NumericVector col_sums(n_t);

          for (iter = 0; iter < max_iter; ++iter) {
            for (int j = 0; j < n_t; ++j) {
              double total = 0.0;
              for (int i = 0; i < n_w; ++i) total += A(i, j) * E[i];
              if (total < floor_value) total = floor_value;
              col_sums[j] = total;
            }

            double norm_total = 0.0;
            for (int i = 0; i < n_w; ++i) {
              double factor = 0.0;
              for (int j = 0; j < n_t; ++j) factor += A(i, j) * alpha[j] / col_sums[j];
              double value = E[i] * factor;
              if (!R_finite(value) || value < floor_value) value = floor_value;
              E_next[i] = value;
              norm_total += value;
            }

            double max_diff = 0.0;
            for (int i = 0; i < n_w; ++i) {
              E_next[i] = E_next[i] / norm_total;
              double diff = std::fabs(E_next[i] - E[i]);
              if (diff > max_diff) max_diff = diff;
              E[i] = E_next[i];
            }

            if (max_diff < tol) {
              converged = true;
              break;
            }
          }

          for (int j = 0; j < n_t; ++j) {
            double total = 0.0;
            for (int i = 0; i < n_w; ++i) total += A(i, j) * E[i];
            if (total < floor_value) total = floor_value;
            col_sums[j] = total;
          }

          Rcpp::NumericMatrix B(n_w, n_t);
          double entropy = 0.0;
          for (int i = 0; i < n_w; ++i) {
            double E_safe = E[i] < floor_value ? floor_value : E[i];
            for (int j = 0; j < n_t; ++j) {
              double alpha_safe = alpha[j] < floor_value ? floor_value : alpha[j];
              double value = A(i, j) * E[i] * alpha[j] / col_sums[j];
              if (std::fabs(value) < zero_threshold) value = 0.0;
              B(i, j) = value;
              if (value != 0.0) {
                double rel = value / E_safe / alpha_safe;
                if (rel > 0.0 && R_finite(rel)) entropy += value * std::log(rel);
              }
            }
          }

          return Rcpp::List::create(
            Rcpp::_["E"] = E,
            Rcpp::_["B"] = B,
            Rcpp::_["A"] = A,
            Rcpp::_["entropy"] = entropy,
            Rcpp::_["converged"] = converged,
            Rcpp::_["iterations"] = iter + 1
          );
        }
      ')
      TRUE
    }, error = function(e) FALSE)

    compiled <<- isTRUE(ok)
    compile_failed <<- !isTRUE(ok)
    compiled
  }
})

#' Solve for equilibrium worker allocation given gamma
#'
#' This override keeps the original return contract but accepts E_start. E_start
#' is deliberately generic so the same routine can be reused by counterfactual
#' solvers that trace equilibria over nearby wage or task-mix states.
solve_equilibrium <- function(cost_matrix, alpha, gamma, innertol, config = CONFIG,
                              E_start = NULL) {
  n_w <- nrow(cost_matrix)
  n_t <- ncol(cost_matrix)
  E <- normalize_worker_share(E_start, config)

  if (
    solver_flag(config, "use_rcpp_equilibrium", FALSE) &&
      ensure_rcpp_equilibrium_solver()
  ) {
    cpp_result <- tryCatch(
      solve_equilibrium_plain_cpp(
        cost_matrix,
        as.numeric(alpha),
        gamma,
        E,
        innertol,
        config$fixedpoint_max_iter,
        config$numeric_floor,
        config$numeric_ceiling,
        config$B_zero_threshold
      ),
      error = function(e) NULL
    )
    if (!is.null(cpp_result) && all(is.finite(cpp_result$E))) {
      return(cpp_result)
    }
  }

  log_A <- -cost_matrix / gamma
  log_A_max <- max(log_A)
  A <- exp(log_A - log_A_max)
  A <- pmax(A, config$numeric_floor)
  A <- pmin(A, config$numeric_ceiling)
  A_t <- t(A)

  fxpt <- function(p) {
    col_sums <- as.vector(A_t %*% p)
    col_sums <- pmax(col_sums, config$numeric_floor)
    task_scales <- alpha / col_sums
    update_factors <- as.vector(A %*% task_scales)
    p * update_factors
  }

  result <- tryCatch(
    squarem(
      E,
      fixptfn = fxpt,
      control = list(maxiter = config$fixedpoint_max_iter, tol = innertol)
    ),
    error = function(e) NULL
  )

  if (!is.null(result) && all(is.finite(result$par))) {
    E <- result$par
    converged <- (result$convergence == 0)
    iterations <- if (!is.null(result$fpevals)) result$fpevals else NA_integer_
  } else {
    converged <- FALSE
    iterations <- config$fixedpoint_max_iter
    for (iter in seq_len(config$fixedpoint_max_iter)) {
      E_next <- fxpt(E)
      if (!all(is.finite(E_next))) {
        break
      }
      E_next <- normalize_worker_share(E_next, config)
      if (max(abs(E_next - E)) < innertol) {
        E <- E_next
        converged <- TRUE
        iterations <- iter
        break
      }
      E <- E_next
    }
  }

  E <- normalize_worker_share(E, config)
  col_sums <- as.vector(A_t %*% E)
  col_sums <- pmax(col_sums, config$numeric_floor)
  B <- A * E
  B <- t(t(B) * alpha / col_sums)
  B[abs(B) < config$B_zero_threshold] <- 0

  E_safe <- pmax(E, config$numeric_floor)
  alpha_safe <- pmax(alpha, config$numeric_floor)
  B_rel <- B / E_safe
  B_rel <- t(t(B_rel) / alpha_safe)
  entropy <- sum(B * spec_log(B_rel))

  list(
    E = E,
    B = B,
    A = A,
    converged = converged,
    entropy = entropy,
    iterations = iterations
  )
}

#' Bisection with bracket adjustment and memoized function values
bisection <- function(f, a, b, n, xtol, ftol, config = CONFIG, start = NULL) {
  cache <- new.env(parent = emptyenv())
  eval_count <- 0L

  eval_f <- function(x) {
    key <- format(signif(x, 16), scientific = TRUE)
    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }
    value <- f(x)
    assign(key, value, envir = cache)
    eval_count <<- eval_count + 1L
    value
  }

  use_warm_bracket <- FALSE
  if (!is.null(start) && is.finite(start) && start > 0) {
    width <- max(xtol * 4, 0.25 * max(abs(start), 1))
    a_warm <- max(a, start - width)
    b_warm <- min(max(b, start + width), start + width)
    fa_warm <- eval_f(a_warm)
    fb_warm <- eval_f(b_warm)
    expand <- 0L

    while (
      is.finite(fa_warm) && is.finite(fb_warm) &&
        sign(fa_warm) == sign(fb_warm) &&
        expand < 8L
    ) {
      width <- width * 2
      a_warm <- max(a, start - width)
      b_warm <- start + width
      fa_warm <- eval_f(a_warm)
      fb_warm <- eval_f(b_warm)
      expand <- expand + 1L
    }

    if (is.finite(fa_warm) && is.finite(fb_warm) && sign(fa_warm) != sign(fb_warm)) {
      a_prime <- a_warm
      b_prime <- b_warm
      fa <- fa_warm
      fb <- fb_warm
      use_warm_bracket <- TRUE
    }
  }

  if (!use_warm_bracket) {
    a_prime <- a
    b_prime <- b

    if (eval_f(config$bisection_low_test_point) > 0) {
      a_prime <- config$bisection_high_start_if_positive
    }

    fa <- eval_f(a_prime)
    while (is.nan(fa)) {
      a_prime <- a_prime + config$bisection_nan_step
      stopifnot(b_prime > a_prime)
      fa <- eval_f(a_prime)
    }

    while (fa > 0) {
      a_prime <- a_prime - config$bisection_lower_step
      stopifnot(a_prime > 0)
      fa <- eval_f(a_prime)
    }

    fb <- eval_f(b_prime)
    while (fb < 0) {
      b_prime <- b_prime + config$bisection_upper_step
      fb <- eval_f(b_prime)
    }
  }

  stopifnot(b_prime > a_prime)
  c_mid <- (a_prime + b_prime) / 2
  fc <- eval_f(c_mid)

  for (i in seq_len(n)) {
    c_mid <- (a_prime + b_prime) / 2
    fc <- eval_f(c_mid)

    conv <- abs(fc) < ftol || ((b_prime - a_prime) / 2) < xtol
    if (conv) {
      return(list(root = c_mid, val = fc, conv = TRUE, eval_count = eval_count))
    }

    if (sign(fc) == sign(fa)) {
      a_prime <- c_mid
      fa <- fc
    } else {
      b_prime <- c_mid
      fb <- fc
    }
  }

  list(
    root = c_mid,
    val = fc,
    conv = abs(fc) < ftol || ((b_prime - a_prime) / 2) < xtol,
    eval_count = eval_count
  )
}

find_gamma_for_sindex <- function(cost_matrix, alpha, s_index, innertol, outertol,
                                  config = CONFIG, gamma_start = NULL,
                                  E_start = NULL) {
  corner <- compute_corner_solution(cost_matrix, alpha, config)

  if (is.na(corner$entropy_bound) || is.nan(corner$entropy_bound)) {
    corner$entropy_bound <- Inf
  }

  if (s_index > corner$entropy_bound) {
    return(list(
      gamma = 0,
      E = corner$E,
      B = corner$B,
      is_corner = TRUE,
      converged = TRUE,
      entropy = corner$entropy_bound,
      bisection_evals = 0L
    ))
  }

  use_fixedpoint_warm_start <- solver_flag(config, "use_fixedpoint_warm_starts", FALSE)
  last_E <- if (use_fixedpoint_warm_start) normalize_worker_share(E_start, config) else NULL
  last_eq <- NULL
  objective <- function(gamma) {
    eq <- solve_equilibrium(cost_matrix, alpha, gamma, innertol, config, E_start = last_E)
    if (use_fixedpoint_warm_start && all(is.finite(eq$E))) {
      last_E <<- eq$E
      last_eq <<- eq
    }
    s_index - eq$entropy
  }

  result <- bisection(
    f = objective,
    a = config$bisection_lower,
    b = config$bisection_upper,
    n = config$bisection_max_iter,
    xtol = outertol,
    ftol = outertol,
    config = config,
    start = gamma_start
  )

  if (!result$conv) {
    warning("Bisection did not converge for s_index = ", s_index,
            ". Final gamma = ", result$root, ", residual = ", result$val)
  }

  gamma_final <- result$root
  eq_final <- solve_equilibrium(cost_matrix, alpha, gamma_final, innertol, config, E_start = last_E)

  list(
    gamma = gamma_final,
    E = eq_final$E,
    B = eq_final$B,
    is_corner = FALSE,
    converged = result$conv,
    entropy = eq_final$entropy,
    bisection_evals = result$eval_count
  )
}

get_worker_allocation <- function(cost_matrix, alpha, s_index, innertol, outertol,
                                  config = CONFIG, gamma_start = NULL,
                                  E_start = NULL) {
  result <- find_gamma_for_sindex(
    cost_matrix,
    alpha,
    s_index,
    innertol,
    outertol,
    config,
    gamma_start = gamma_start,
    E_start = E_start
  )
  result$E[2:config$n_worker_types]
}

solve_worker_rows <- function(tild_theta, rows, config = CONFIG, clust = NULL,
                              state = NULL, return_full_E = FALSE,
                              return_gamma = FALSE) {
  if (is.null(state) && solver_flag(config, "use_solver_warm_starts", TRUE)) {
    state <- get_default_solver_state()
  }

  tolerances <- get_solver_tolerances(config, state)
  n_unique <- length(rows$unique_key)
  n_E <- if (return_full_E) config$n_worker_types else config$n_worker_types - 1L

  jobs <- vector("list", n_unique)
  for (u in seq_len(n_unique)) {
    row_idx <- rows$unique_first_row[u]
    key <- rows$unique_key[u]
    gamma_start <- NULL
    E_start <- NULL
    if (!is.null(state) && solver_flag(config, "use_solver_warm_starts", TRUE)) {
      if (exists(key, envir = state$gamma_by_key, inherits = FALSE)) {
        gamma_start <- get(key, envir = state$gamma_by_key, inherits = FALSE)
      }
      if (exists(key, envir = state$E_by_key, inherits = FALSE)) {
        E_start <- get(key, envir = state$E_by_key, inherits = FALSE)
      }
    }

    jobs[[u]] <- list(
      unique_id = u,
      key = key,
      county = rows$county[row_idx],
      alpha = rows$alpha[row_idx, ],
      s_index = rows$s_index[row_idx],
      gamma_start = gamma_start,
      E_start = E_start
    )
  }

  solve_one <- function(job, worker_config, worker_tild_theta, worker_tolerances) {
    result <- find_gamma_for_sindex(
      worker_tild_theta[[job$county]],
      job$alpha,
      job$s_index,
      worker_tolerances$innertol,
      worker_tolerances$outertol,
      worker_config,
      gamma_start = job$gamma_start,
      E_start = job$E_start
    )

    E_out <- if (return_full_E) result$E else result$E[2:worker_config$n_worker_types]
    list(
      unique_id = job$unique_id,
      key = job$key,
      E = E_out,
      E_full = result$E,
      gamma = result$gamma
    )
  }

  worker_config <- config
  if (!is.null(clust)) {
    worker_config$use_rcpp_equilibrium <- FALSE
  }

  if (solver_flag(config, "pl_on", TRUE)) {
    if (get_os() == "windows") {
      results <- parLapply(clust, jobs, solve_one, worker_config, tild_theta, tolerances)
    } else {
      results <- mclapply(jobs, solve_one, worker_config, tild_theta, tolerances,
                          mc.cores = get_core_count(config))
    }
  } else {
    results <- lapply(jobs, solve_one, worker_config, tild_theta, tolerances)
  }

  E_unique <- matrix(NA_real_, nrow = n_unique, ncol = n_E)
  gamma_unique <- rep(NA_real_, n_unique)

  for (result in results) {
    E_unique[result$unique_id, ] <- result$E
    gamma_unique[result$unique_id] <- result$gamma

    if (!is.null(state) && solver_flag(config, "use_solver_warm_starts", TRUE)) {
      assign(result$key, result$gamma, envir = state$gamma_by_key)
      assign(result$key, result$E_full, envir = state$E_by_key)
    }
  }

  E_rows <- E_unique[rows$unique_id, , drop = FALSE]
  if (return_gamma) {
    return(list(E = E_rows, gamma = gamma_unique[rows$unique_id]))
  }
  E_rows
}

objective_gmm <- function(theta, x, beta, beta_2_subset, config = CONFIG, clust = NULL,
                          solver_state = NULL) {
  rows <- prepare_equilibrium_rows(x, config)
  pre_parms <- theta
  names(pre_parms) <- names(beta_2_subset)
  tild_theta <- build_cost_matrix(pre_parms, beta, config)

  temp_res <- solve_worker_rows(
    tild_theta,
    rows,
    config,
    clust,
    state = solver_state,
    return_full_E = FALSE
  )

  E_match <- data.frame(
    temp_res - rows$observed_E,
    factor(rows$county)
  )
  E_col_names <- paste0("E_", 2:config$n_worker_types)
  colnames(E_match) <- c(E_col_names, "county")
  E_mat <- model.matrix(build_E_formula(2:config$n_worker_types, include_s_index = FALSE), data = E_match)

  state <- solver_state
  if (is.null(state) && solver_flag(config, "use_solver_warm_starts", TRUE)) {
    state <- get_default_solver_state()
  }
  if (!is.null(state)) {
    state$n_objective_calls <- state$n_objective_calls + 1L
    state$last_moment_norm <- sum(colMeans(E_mat)^2)
  }

  E_mat
}

eval_moments <- function(theta, x, beta, beta_2_subset, config = CONFIG, clust = NULL,
                         solver_state = NULL) {
  rows <- prepare_equilibrium_rows(x, config)
  pre_parms <- theta
  names(pre_parms) <- names(beta_2_subset)
  tild_theta <- build_cost_matrix(pre_parms, beta, config)

  temp_res <- solve_worker_rows(
    tild_theta,
    rows,
    config,
    clust,
    state = solver_state,
    return_full_E = TRUE
  )

  E_raw_all_cols <- get_E_raw_cols(config)
  E_all_names <- paste0("E_", 1:config$n_worker_types)

  data <- as.data.frame(x)
  E_match <- data.frame(as.matrix(data[, E_raw_all_cols, drop = FALSE]), factor(rows$county))
  colnames(E_match) <- c(E_all_names, "county")
  E_raw <- model.matrix(build_E_formula(1:config$n_worker_types, include_s_index = FALSE), data = E_match)

  E_match <- data.frame(temp_res, factor(rows$county))
  colnames(E_match) <- c(E_all_names, "county")
  E_model <- model.matrix(build_E_formula(1:config$n_worker_types, include_s_index = FALSE), data = E_match)

  cbind(colMeans(E_model), colMeans(E_raw))
}

get_gammas <- function(theta, x, beta, beta_2_subset, config = CONFIG, clust = NULL,
                       solver_state = NULL) {
  rows <- prepare_equilibrium_rows(x, config)
  pre_parms <- theta
  names(pre_parms) <- names(beta_2_subset)
  tild_theta <- build_cost_matrix(pre_parms, beta, config)

  solved <- solve_worker_rows(
    tild_theta,
    rows,
    config,
    clust,
    state = solver_state,
    return_full_E = FALSE,
    return_gamma = TRUE
  )

  solved$gamma
}

estimate_wage_parameters_joint <- function(start, x, beta, beta_2_subset, config = CONFIG,
                                           clust = NULL, solver_state = NULL) {
  objective_vect <- function(parms) {
    colMeans(objective_gmm(
      theta = parms,
      x = x,
      beta = beta,
      beta_2_subset = beta_2_subset,
      config = config,
      clust = clust,
      solver_state = solver_state
    ))
  }

  BBsolve(
    start,
    objective_vect,
    control = list(trace = TRUE, tol = config$obj_tol, maxit = config$structural_optimizer_maxit)
  )
}

estimate_wage_parameters_by_county <- function(start, x, beta, beta_2_subset, config = CONFIG,
                                               clust = NULL, solver_state = NULL) {
  data <- as.data.frame(x)
  full_par <- start
  names(full_par) <- names(beta_2_subset)
  county_results <- list()
  rounds <- max(1L, solver_value(config, "county_optimizer_rounds", 1L))

  for (round_id in seq_len(rounds)) {
    for (cnty in config$counties) {
      county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
      par_idx <- grepl(county_pattern, names(full_par), fixed = TRUE)
      row_idx <- as.character(data$county) == cnty

      if (!any(par_idx) || !any(row_idx)) {
        next
      }

      x_county <- data[row_idx, , drop = FALSE]
      objective_county <- function(parms) {
        candidate <- full_par
        candidate[par_idx] <- parms
        moments <- colMeans(objective_gmm(
          theta = candidate,
          x = x_county,
          beta = beta,
          beta_2_subset = beta_2_subset,
          config = config,
          clust = clust,
          solver_state = solver_state
        ))
        as.numeric(moments)
      }

      result <- BBsolve(
        full_par[par_idx],
        objective_county,
        control = list(trace = TRUE, tol = config$obj_tol, maxit = config$structural_optimizer_maxit)
      )
      full_par[par_idx] <- result$par
      county_results[[paste(round_id, cnty, sep = ":")]] <- result
    }
  }

  final_moments <- colMeans(objective_gmm(
    theta = full_par,
    x = x,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = config,
    clust = clust,
    solver_state = solver_state
  ))

  list(
    par = full_par,
    convergence = as.integer(sum(final_moments^2) > config$obj_tol),
    county_results = county_results,
    final_moments = final_moments,
    objective = sum(final_moments^2),
    mode = "county"
  )
}

#' Estimate wage parameters with the configured outer solver.
#'
#' The default county mode exploits separability: rows from a county depend only
#' on that county's four wage parameters. Joint mode remains available for exact
#' parity checks or future specifications with cross-county coupling.
estimate_wage_parameters <- function(start, x, beta, beta_2_subset, config = CONFIG,
                                     clust = NULL, solver_state = NULL) {
  mode <- tolower(solver_value(config, "wage_optimizer_mode", "county"))
  if (is.null(solver_state) && solver_flag(config, "use_solver_warm_starts", TRUE)) {
    solver_state <- get_default_solver_state(reset = TRUE)
  }

  if (identical(mode, "joint")) {
    return(estimate_wage_parameters_joint(start, x, beta, beta_2_subset, config, clust, solver_state))
  }

  if (identical(mode, "county")) {
    return(estimate_wage_parameters_by_county(start, x, beta, beta_2_subset, config, clust, solver_state))
  }

  stop("Unknown wage_optimizer_mode: ", mode, ". Use 'county' or 'joint'.")
}
