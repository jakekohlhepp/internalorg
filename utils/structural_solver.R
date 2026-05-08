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

build_worker_moment_matrix <- function(E_values, county, config = CONFIG,
                                       worker_indices = 2:config$n_worker_types) {
  E_values <- as.matrix(E_values)
  storage.mode(E_values) <- "double"
  county <- as.character(county)

  out <- matrix(
    0,
    nrow = length(county),
    ncol = length(config$counties) * length(worker_indices)
  )

  col_names <- character(ncol(out))
  col_id <- 1L
  for (worker in worker_indices) {
    worker_col <- match(worker, worker_indices)
    for (cnty in config$counties) {
      out[county == cnty, col_id] <- E_values[county == cnty, worker_col]
      col_names[col_id] <- paste0("county", cnty, ":E_", worker)
      col_id <- col_id + 1L
    }
  }

  colnames(out) <- col_names
  out
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

  E_mat <- build_worker_moment_matrix(
    temp_res - rows$observed_E,
    rows$county,
    config,
    worker_indices = 2:config$n_worker_types
  )

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

structural_bound_diagnostics <- function(theta, x, beta, beta_2_subset,
                                         config = CONFIG, weights = NULL) {
  rows <- prepare_equilibrium_rows(x, config)
  pre_parms <- theta
  names(pre_parms) <- names(beta_2_subset)
  tild_theta <- build_cost_matrix(pre_parms, beta, config)

  unique_bound <- numeric(length(rows$unique_key))
  for (u in seq_along(rows$unique_key)) {
    row_idx <- rows$unique_first_row[u]
    corner <- compute_corner_solution(
      tild_theta[[rows$county[row_idx]]],
      rows$alpha[row_idx, ],
      config
    )
    unique_bound[u] <- corner$entropy_bound
  }

  bounds <- unique_bound[rows$unique_id]
  violation <- pmax(rows$s_index - bounds, 0)
  row_weights <- normalize_moment_weights(weights, rows$n)
  if (is.null(row_weights)) {
    row_weights <- rep(1 / rows$n, rows$n)
  }

  diagnostics <- data.table::data.table(
    county = rows$county,
    s_index = rows$s_index,
    s_bound = bounds,
    violation = violation,
    weight = row_weights
  )

  tol <- solver_value(config, "structural_bound_guard_tol", 1e-10)
  diagnostics[, .(
    n = .N,
    positive_s = sum(s_index > tol, na.rm = TRUE),
    above_bound = sum(violation > tol, na.rm = TRUE),
    above_bound_share = mean(violation > tol, na.rm = TRUE),
    positive_bound_share = mean(s_bound > tol, na.rm = TRUE),
    mean_s_index = sum(s_index * weight) / sum(weight),
    mean_s_bound = sum(s_bound * weight) / sum(weight),
    mean_violation = sum(violation * weight) / sum(weight),
    max_violation = max(violation, na.rm = TRUE)
  ), by = county]
}

structural_bound_moments <- function(theta, x, beta, beta_2_subset,
                                     config = CONFIG, weights = NULL) {
  diagnostics <- structural_bound_diagnostics(
    theta,
    x,
    beta,
    beta_2_subset,
    config,
    weights
  )
  out <- diagnostics$mean_violation
  names(out) <- paste0("county", diagnostics$county, ":s_bound_violation")
  out
}

structural_wage_objective_score <- function(moment_vector, theta, x, beta,
                                            beta_2_subset, config = CONFIG,
                                            weights = NULL) {
  moment_score <- sum(as.numeric(moment_vector)^2)
  if (!solver_flag(config, "structural_bound_guard_enabled", TRUE)) {
    return(moment_score)
  }

  bound_moments <- structural_bound_moments(
    theta,
    x,
    beta,
    beta_2_subset,
    config,
    weights
  )
  moment_score + solver_value(config, "structural_bound_guard_weight", 10) *
    sum(as.numeric(bound_moments)^2)
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
  E_raw <- build_worker_moment_matrix(
    as.matrix(data[, E_raw_all_cols, drop = FALSE]),
    rows$county,
    config,
    worker_indices = 1:config$n_worker_types
  )

  E_model <- build_worker_moment_matrix(
    temp_res,
    rows$county,
    config,
    worker_indices = 1:config$n_worker_types
  )

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

normalize_moment_weights <- function(weights, n) {
  if (is.null(weights)) {
    return(NULL)
  }

  weights <- as.numeric(weights)
  if (length(weights) != n) {
    stop("Moment weights length (", length(weights), ") does not match row count (", n, ").")
  }
  if (anyNA(weights) || any(!is.finite(weights)) || any(weights < 0)) {
    stop("Moment weights must be finite, non-missing, and non-negative.")
  }

  weight_sum <- sum(weights)
  if (!is.finite(weight_sum) || weight_sum <= 0) {
    stop("Moment weights must have a positive finite sum.")
  }

  weights / weight_sum
}

weighted_col_means <- function(x, weights = NULL) {
  x <- as.matrix(x)
  weights <- normalize_moment_weights(weights, nrow(x))

  if (is.null(weights)) {
    return(colMeans(x))
  }

  colSums(sweep(x, MARGIN = 1, weights, `*`))
}

estimate_wage_parameters_joint <- function(start, x, beta, beta_2_subset, config = CONFIG,
                                           clust = NULL, solver_state = NULL,
                                           moment_weights = NULL) {
  objective_vect <- function(parms) {
    weighted_col_means(objective_gmm(
      theta = parms,
      x = x,
      beta = beta,
      beta_2_subset = beta_2_subset,
      config = config,
      clust = clust,
      solver_state = solver_state
    ), moment_weights)
  }

  BBsolve(
    start,
    objective_vect,
    control = list(trace = TRUE, tol = config$obj_tol, maxit = config$structural_optimizer_maxit)
  )
}

estimate_wage_parameters_by_county <- function(start, x, beta, beta_2_subset, config = CONFIG,
                                               clust = NULL, solver_state = NULL,
                                               moment_weights = NULL) {
  data <- as.data.frame(x)
  full_par <- start
  names(full_par) <- names(beta_2_subset)
  county_results <- list()
  rounds <- max(1L, solver_value(config, "county_optimizer_rounds", 1L))
  moment_weights <- normalize_moment_weights(moment_weights, nrow(data))

  for (round_id in seq_len(rounds)) {
    for (cnty in config$counties) {
      county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
      par_idx <- grepl(county_pattern, names(full_par), fixed = TRUE)
      row_idx <- as.character(data$county) == cnty

      if (!any(par_idx) || !any(row_idx)) {
        next
      }

      x_county <- data[row_idx, , drop = FALSE]
      county_weights <- if (is.null(moment_weights)) NULL else moment_weights[row_idx]
      objective_county <- function(parms) {
        candidate <- full_par
        candidate[par_idx] <- parms
        moments <- weighted_col_means(objective_gmm(
          theta = candidate,
          x = x_county,
          beta = beta,
          beta_2_subset = beta_2_subset,
          config = config,
          clust = clust,
          solver_state = solver_state
        ), county_weights)
        county_moments <- grepl(paste0("county", cnty, ":E_"), names(moments), fixed = TRUE)
        as.numeric(moments[county_moments])
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

  final_moments <- weighted_col_means(objective_gmm(
    theta = full_par,
    x = x,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = config,
    clust = clust,
    solver_state = solver_state
  ), moment_weights)

  list(
    par = full_par,
    convergence = as.integer(sum(final_moments^2) > config$obj_tol),
    county_results = county_results,
    final_moments = final_moments,
    objective = sum(final_moments^2),
    mode = "county"
  )
}

#' Per-county wage solver that minimizes the bound-guarded ssq via optim
#' Nelder-Mead with parameter scaling.
#'
#' Use case: when the per-county wage moment system has flat or saturated
#' regions (e.g. LA where worker 3 gets priced out), nleqslv (a root-finder)
#' stalls with termcd = 6 and returns extreme parameter values trying to
#' satisfy the moment equations one-by-one. A minimizer on ||g||^2 sees a
#' useful descent direction even at non-zero floors and finds clean
#' interior solutions. Empirically (tests/wage_stage_minimizer_check.R) the
#' LA wage block goes from ssq = 0.147 with extreme wages under nleqslv to
#' ssq = 0.002 with sensible wages (all under 200) under this routine.
#'
#' Parscale defaults assume the wage parameters are O(50). Tune via
#' ``min_optim_parscale_wage`` config entry if your scale differs.
estimate_wage_parameters_min_optim <- function(start, x, beta, beta_2_subset,
                                               config = CONFIG, clust = NULL,
                                               solver_state = NULL,
                                               moment_weights = NULL) {
  data <- as.data.frame(x)
  full_par <- start
  names(full_par) <- names(beta_2_subset)
  initial_full_par <- full_par
  county_results <- list()
  moment_weights <- normalize_moment_weights(moment_weights, nrow(data))
  ## Keep the per-salon solver warm-starts ON: each Nelder-Mead step is a
  ## small perturbation of the previous one, so the equilibrium solver
  ## converges much faster from the cached gamma/E values. Disabling them
  ## (as the nleqslv path does) made per-county runtime ~6x longer in
  ## practice.
  objective_config <- config

  parscale_w_default <- solver_value(config, "min_optim_parscale_wage", 20)
  parscale_by_county <- solver_value(config, "min_optim_parscale_wage_by_county", list())
  maxit_per_county <- solver_value(config, "min_optim_maxit", 5000L)
  reltol <- solver_value(config, "min_optim_reltol", config$obj_tol)
  strict_obj_tol <- solver_value(config, "obj_tol", 1e-6)
  max_restarts <- solver_value(config, "min_optim_max_restarts", 3L)
  restart_jitter <- solver_value(config, "min_optim_restart_jitter", 0.05)
  multistart_n_by_county <- solver_value(config, "min_optim_n_multistarts_by_county", list())
  multistart_scale_by_county <- solver_value(config, "min_optim_multistart_scale_by_county", list())

  start_full_moments <- weighted_col_means(objective_gmm(
    theta = initial_full_par,
    x = x,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = objective_config,
    clust = clust,
    solver_state = NULL
  ), moment_weights)
  start_full_objective <- sum(start_full_moments^2)
  start_full_score <- structural_wage_objective_score(
    start_full_moments,
    initial_full_par,
    x,
    beta,
    beta_2_subset,
    objective_config,
    moment_weights
  )

  for (cnty in config$counties) {
    county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
    par_idx <- grepl(county_pattern, names(full_par), fixed = TRUE)
    row_idx <- as.character(data$county) == cnty
    if (!any(par_idx) || !any(row_idx)) next

    x_county <- data[row_idx, , drop = FALSE]
    county_weights <- if (is.null(moment_weights)) NULL else moment_weights[row_idx]

    objective_county_ssq <- function(parms) {
      candidate <- full_par
      candidate[par_idx] <- parms
      moments <- tryCatch(
        weighted_col_means(objective_gmm(
          theta = candidate,
          x = x_county,
          beta = beta,
          beta_2_subset = beta_2_subset,
          config = objective_config,
          clust = clust,
          solver_state = NULL
        ), weights = county_weights),
        error = function(e) NULL
      )
      if (is.null(moments)) {
        return(solver_value(config, "optimizer_failure_penalty", 1e6))
      }
      county_moments <- grepl(paste0("county", cnty, ":E_"),
                              names(moments), fixed = TRUE)
      v <- as.numeric(moments[county_moments])
      if (anyNA(v) || any(!is.finite(v))) {
        return(solver_value(config, "optimizer_failure_penalty", 1e6))
      }
      sum(v^2)
    }

    objective_county_vec <- function(parms) {
      candidate <- full_par
      candidate[par_idx] <- parms
      moments <- weighted_col_means(objective_gmm(
        theta = candidate,
        x = x_county,
        beta = beta,
        beta_2_subset = beta_2_subset,
        config = objective_config,
        clust = clust,
        solver_state = NULL
      ), weights = county_weights)
      county_moments <- grepl(paste0("county", cnty, ":E_"),
                              names(moments), fixed = TRUE)
      as.numeric(moments[county_moments])
    }

    start_par <- full_par[par_idx]
    start_objective <- objective_county_ssq(start_par)
    start_candidate <- full_par
    start_candidate[par_idx] <- start_par
    start_score <- structural_wage_objective_score(
      objective_county_vec(start_par),
      start_candidate,
      x_county,
      beta,
      beta_2_subset,
      objective_config,
      county_weights
    )

    cnty_key <- as.character(cnty)
    parscale_w <- if (!is.null(parscale_by_county[[cnty_key]])) {
      parscale_by_county[[cnty_key]]
    } else {
      parscale_w_default
    }
    parscale_vec <- rep(parscale_w, length(start_par))

    n_multistarts <- if (!is.null(multistart_n_by_county[[cnty_key]])) {
      as.integer(multistart_n_by_county[[cnty_key]])
    } else {
      1L
    }
    multistart_scale <- if (!is.null(multistart_scale_by_county[[cnty_key]])) {
      multistart_scale_by_county[[cnty_key]]
    } else {
      5 * parscale_w
    }
    multistart_starts <- list(start_par)
    if (n_multistarts > 1L) {
      for (.ms_i in 2:n_multistarts) {
        rand_par <- stats::runif(length(start_par), -multistart_scale, multistart_scale)
        names(rand_par) <- names(start_par)
        multistart_starts[[.ms_i]] <- rand_par
      }
    }

    best_result <- NULL
    for (ms_idx in seq_along(multistart_starts)) {
      attempt_par <- multistart_starts[[ms_idx]]
      restart_count <- 0L
      repeat {
        result <- tryCatch(
          stats::optim(
            par = attempt_par,
            fn = objective_county_ssq,
            method = "Nelder-Mead",
            control = list(
              maxit = maxit_per_county,
              reltol = reltol,
              parscale = parscale_vec,
              trace = solver_value(config, "min_optim_trace", 0L)
            )
          ),
          error = function(e) {
            warning("optim Nelder-Mead failed for county ", cnty, ": ",
                    conditionMessage(e), call. = FALSE)
            list(par = attempt_par, value = start_objective, convergence = -1L)
          }
        )

        if (identical(as.integer(result$convergence), 10L) &&
            restart_count < max_restarts) {
          restart_count <- restart_count + 1L
          jitter <- pmax(abs(result$par), parscale_w) *
            stats::runif(length(result$par), -restart_jitter, restart_jitter)
          attempt_par <- result$par + jitter
          message(sprintf(
            paste0("Nelder-Mead degenerate simplex (code 10) for county %s ",
                   "(start %d/%d) at ssq=%.6g; restarting attempt %d/%d ",
                   "with jitter."),
            cnty, ms_idx, n_multistarts, result$value,
            restart_count, max_restarts
          ))
          next
        }
        break
      }
      result$restart_count <- restart_count
      result$parscale <- parscale_w
      result$multistart_idx <- ms_idx

      if (n_multistarts > 1L) {
        message(sprintf(
          "Multi-start %d/%d for county %s: ssq=%.6g (convergence=%d, restarts=%d).",
          ms_idx, n_multistarts, cnty, result$value,
          result$convergence, restart_count
        ))
      }
      if (is.null(best_result) || result$value < best_result$value) {
        best_result <- result
      }
    }
    result <- best_result
    result$multistart_count <- n_multistarts

    if (result$value > strict_obj_tol) {
      stop(sprintf(
        paste0("Wage solver did not reach obj_tol for county %s ",
               "(convergence=%d, restarts=%d, multistarts=%d, parscale=%g): ",
               "best objective %.6g exceeds obj_tol=%.6g."),
        cnty, result$convergence, result$restart_count,
        n_multistarts, parscale_w, result$value, strict_obj_tol
      ), call. = FALSE)
    }

    final_par <- result$par
    final_moments_county <- objective_county_vec(final_par)
    final_objective_county <- sum(final_moments_county^2)
    final_candidate <- full_par
    final_candidate[par_idx] <- final_par
    final_score_county <- structural_wage_objective_score(
      final_moments_county,
      final_candidate,
      x_county,
      beta,
      beta_2_subset,
      objective_config,
      county_weights
    )

    accepted <- is.finite(final_score_county) &&
      final_score_county <= start_score * (1 + sqrt(.Machine$double.eps))
    result$x <- final_par
    result$start_objective <- start_objective
    result$final_objective <- final_objective_county
    result$start_score <- start_score
    result$final_score <- final_score_county
    result$accepted <- accepted

    if (accepted) {
      full_par[par_idx] <- final_par
    } else {
      warning(
        "Rejected min_optim wage update for county ", cnty,
        " because it did not improve the bound-guarded objective. ",
        "start=", signif(start_objective, 6),
        ", candidate=", signif(final_objective_county, 6),
        ", start_score=", signif(start_score, 6),
        ", candidate_score=", signif(final_score_county, 6),
        call. = FALSE
      )
      result$x <- start_par
    }
    county_results[[as.character(cnty)]] <- result
  }

  final_moments <- weighted_col_means(objective_gmm(
    theta = full_par,
    x = x,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = objective_config,
    clust = clust,
    solver_state = NULL
  ), moment_weights)
  final_objective <- sum(final_moments^2)
  final_score <- structural_wage_objective_score(
    final_moments,
    full_par,
    x,
    beta,
    beta_2_subset,
    objective_config,
    moment_weights
  )
  global_accepted <- is.finite(final_score) &&
    final_score <= start_full_score * (1 + sqrt(.Machine$double.eps))

  if (!global_accepted) {
    warning(
      "Rejected min_optim wage solve because the full bound-guarded ",
      "objective worsened. start=", signif(start_full_objective, 6),
      ", candidate=", signif(final_objective, 6),
      ", start_score=", signif(start_full_score, 6),
      ", candidate_score=", signif(final_score, 6),
      call. = FALSE
    )
    full_par <- initial_full_par
    final_moments <- start_full_moments
    final_objective <- start_full_objective
    final_score <- start_full_score
  }

  list(
    par = full_par,
    convergence = if (
      global_accepted &&
        all(vapply(county_results,
                   function(r) is.finite(r$convergence) && r$convergence == 0L,
                   logical(1)))
    ) 0L else 1L,
    county_results = county_results,
    final_moments = final_moments,
    objective = final_objective,
    score = final_score,
    start_objective = start_full_objective,
    start_score = start_full_score,
    global_accepted = global_accepted,
    mode = "min_optim"
  )
}

#' Per-county particle-swarm wage solver with NM polish.
#'
#' Use case: when the per-county wage moment surface has multiple basins
#' that local Nelder-Mead (even with multi-start) cannot escape. Empirical
#' case in this project: NYC has a wide plateau at ssq ~0.025 near all-large-
#' negative wages that NM dead-ends in; PSO with a wide search box finds a
#' basin at ssq ~0.001 with a manuscript-like sign pattern.
#'
#' For each county the swarm initialises with one particle pinned at the
#' seed start and the rest drawn uniformly in
#' [-pso_search_halfwidth, +pso_search_halfwidth]^d, runs n_iter velocity
#' updates, then polishes the global best with the configured optim method
#' (default Nelder-Mead). The strict-exit guard at obj_tol fires the same
#' way as in min_optim mode.
estimate_wage_parameters_pso <- function(start, x, beta, beta_2_subset,
                                         config = CONFIG, clust = NULL,
                                         solver_state = NULL,
                                         moment_weights = NULL) {
  data <- as.data.frame(x)
  full_par <- start
  names(full_par) <- names(beta_2_subset)
  initial_full_par <- full_par
  county_results <- list()
  moment_weights <- normalize_moment_weights(moment_weights, nrow(data))
  objective_config <- config

  parscale_w_default <- solver_value(config, "min_optim_parscale_wage", 20)
  parscale_by_county <- solver_value(config, "min_optim_parscale_wage_by_county", list())
  strict_obj_tol_default <- solver_value(config, "pso_strict_obj_tol", 0.01)
  strict_obj_tol_by_county <- solver_value(config, "pso_strict_obj_tol_by_county", list())
  reltol <- solver_value(config, "min_optim_reltol", config$obj_tol)
  polish_maxit <- solver_value(config, "min_optim_maxit", 5000L)
  polish_method <- solver_value(config, "pso_polish_method", "Nelder-Mead")

  n_particles  <- solver_value(config, "pso_n_particles", 40L)
  n_iter       <- solver_value(config, "pso_n_iter", 100L)
  halfwidth_default <- solver_value(config, "pso_search_halfwidth", 2000)
  halfwidth_by_county <- solver_value(config, "pso_search_halfwidth_by_county", list())
  w_inertia <- solver_value(config, "pso_w", 0.7)
  c1 <- solver_value(config, "pso_c1", 1.5)
  c2 <- solver_value(config, "pso_c2", 1.5)
  pso_trace <- solver_value(config, "min_optim_trace", 0L)

  start_full_moments <- weighted_col_means(objective_gmm(
    theta = initial_full_par, x = x, beta = beta,
    beta_2_subset = beta_2_subset, config = objective_config,
    clust = clust, solver_state = NULL
  ), moment_weights)
  start_full_objective <- sum(start_full_moments^2)
  start_full_score <- structural_wage_objective_score(
    start_full_moments, initial_full_par, x, beta, beta_2_subset,
    objective_config, moment_weights
  )

  ## Vanilla PSO: x_i^{t+1} = x_i^t + v_i^{t+1};
  ##   v_i^{t+1} = w*v_i^t + c1*r1*(pbest_i - x_i) + c2*r2*(gbest - x_i)
  pso_solve <- function(fn, lower, upper, seed_particle,
                        n_particles, n_iter, w, c1, c2, trace, label) {
    d <- length(lower)
    X <- matrix(stats::runif(n_particles * d,
                             rep(lower, each = n_particles),
                             rep(upper, each = n_particles)),
                n_particles, d)
    if (!is.null(seed_particle) && length(seed_particle) == d) X[1, ] <- seed_particle
    V <- matrix(stats::runif(n_particles * d,
                             -abs(upper - lower), abs(upper - lower)),
                n_particles, d) * 0.1
    fX <- apply(X, 1, fn)
    pbest <- X; fpbest <- fX
    gi <- which.min(fpbest); gbest <- X[gi, ]; fgbest <- fpbest[gi]
    for (t in seq_len(n_iter)) {
      r1 <- matrix(stats::runif(n_particles * d), n_particles, d)
      r2 <- matrix(stats::runif(n_particles * d), n_particles, d)
      V <- w * V +
        c1 * r1 * (pbest - X) +
        c2 * r2 * (matrix(gbest, n_particles, d, byrow = TRUE) - X)
      X <- X + V
      X <- pmax(pmin(X, matrix(upper, n_particles, d, byrow = TRUE)),
                matrix(lower, n_particles, d, byrow = TRUE))
      fX <- apply(X, 1, fn)
      upd <- fX < fpbest
      pbest[upd, ] <- X[upd, ]; fpbest[upd] <- fX[upd]
      gi <- which.min(fpbest)
      if (fpbest[gi] < fgbest) { gbest <- pbest[gi, ]; fgbest <- fpbest[gi] }
      if (trace > 0L && (t %% 10L == 0L || t == 1L)) {
        message(sprintf("  PSO %s iter %3d: gbest ssq=%.6g", label, t, fgbest))
      }
    }
    list(par = gbest, value = fgbest, n_iter = n_iter, n_particles = n_particles)
  }

  for (cnty in config$counties) {
    cnty_key <- as.character(cnty)
    parscale_w <- if (!is.null(parscale_by_county[[cnty_key]])) {
      parscale_by_county[[cnty_key]]
    } else {
      parscale_w_default
    }
    halfwidth <- if (!is.null(halfwidth_by_county[[cnty_key]])) {
      halfwidth_by_county[[cnty_key]]
    } else {
      halfwidth_default
    }

    county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
    par_idx <- grepl(county_pattern, names(full_par), fixed = TRUE)
    row_idx <- as.character(data$county) == cnty
    if (!any(par_idx) || !any(row_idx)) next

    x_county <- data[row_idx, , drop = FALSE]
    county_weights <- if (is.null(moment_weights)) NULL else moment_weights[row_idx]

    objective_county_ssq <- function(parms) {
      candidate <- full_par
      candidate[par_idx] <- parms
      moments <- tryCatch(
        weighted_col_means(objective_gmm(
          theta = candidate, x = x_county, beta = beta,
          beta_2_subset = beta_2_subset, config = objective_config,
          clust = clust, solver_state = NULL
        ), weights = county_weights),
        error = function(e) NULL
      )
      if (is.null(moments)) {
        return(solver_value(config, "optimizer_failure_penalty", 1e6))
      }
      cm <- grepl(paste0("county", cnty, ":E_"), names(moments), fixed = TRUE)
      v <- as.numeric(moments[cm])
      if (anyNA(v) || any(!is.finite(v))) {
        return(solver_value(config, "optimizer_failure_penalty", 1e6))
      }
      sum(v^2)
    }

    objective_county_vec <- function(parms) {
      candidate <- full_par
      candidate[par_idx] <- parms
      moments <- weighted_col_means(objective_gmm(
        theta = candidate, x = x_county, beta = beta,
        beta_2_subset = beta_2_subset, config = objective_config,
        clust = clust, solver_state = NULL
      ), weights = county_weights)
      cm <- grepl(paste0("county", cnty, ":E_"), names(moments), fixed = TRUE)
      as.numeric(moments[cm])
    }

    start_par <- full_par[par_idx]
    start_objective <- objective_county_ssq(start_par)
    start_candidate <- full_par
    start_candidate[par_idx] <- start_par
    start_score <- structural_wage_objective_score(
      objective_county_vec(start_par), start_candidate, x_county,
      beta, beta_2_subset, objective_config, county_weights
    )

    d <- length(start_par)
    lower <- rep(-halfwidth, d); upper <- rep(halfwidth, d)
    message(sprintf(
      "PSO county %s: %d particles x %d iter, search box +/- %g, seed-as-particle.",
      cnty, n_particles, n_iter, halfwidth
    ))
    pso_res <- pso_solve(objective_county_ssq, lower, upper,
                         seed_particle = as.numeric(start_par),
                         n_particles = n_particles, n_iter = n_iter,
                         w = w_inertia, c1 = c1, c2 = c2, trace = pso_trace,
                         label = cnty_key)

    polish <- tryCatch(
      stats::optim(
        par = pso_res$par, fn = objective_county_ssq,
        method = polish_method,
        control = list(parscale = rep(parscale_w, d),
                       maxit = polish_maxit, reltol = reltol,
                       trace = pso_trace)
      ),
      error = function(e) {
        warning("PSO polish failed for county ", cnty, ": ",
                conditionMessage(e), call. = FALSE)
        list(par = pso_res$par, value = pso_res$value, convergence = -1L)
      }
    )

    if (polish$value < pso_res$value) {
      result_par <- polish$par
      result_value <- polish$value
      polish_used <- TRUE
    } else {
      result_par <- pso_res$par
      result_value <- pso_res$value
      polish_used <- FALSE
    }

    result <- list(
      par = result_par, value = result_value, x = result_par,
      pso_value = pso_res$value, polish_value = polish$value,
      polish_used = polish_used, polish_convergence = polish$convergence,
      pso_n_iter = n_iter, pso_n_particles = n_particles,
      halfwidth = halfwidth, parscale = parscale_w,
      start_objective = start_objective, final_objective = result_value,
      start_score = start_score
    )
    message(sprintf(
      "PSO county %s done: pso=%.6g  polish=%.6g  best=%.6g",
      cnty, pso_res$value, polish$value, result_value
    ))

    strict_tol <- if (!is.null(strict_obj_tol_by_county[[cnty_key]])) {
      strict_obj_tol_by_county[[cnty_key]]
    } else {
      strict_obj_tol_default
    }
    if (result_value > strict_tol) {
      stop(sprintf(
        paste0("PSO+polish did not reach pso_strict_obj_tol for county %s ",
               "(pso=%.6g, polish=%.6g, halfwidth=%g, parscale=%g): ",
               "best objective %.6g exceeds pso_strict_obj_tol=%.6g."),
        cnty, pso_res$value, polish$value, halfwidth, parscale_w,
        result_value, strict_tol
      ), call. = FALSE)
    }

    final_moments_county <- objective_county_vec(result_par)
    final_objective_county <- sum(final_moments_county^2)
    final_candidate <- full_par
    final_candidate[par_idx] <- result_par
    final_score_county <- structural_wage_objective_score(
      final_moments_county, final_candidate, x_county,
      beta, beta_2_subset, objective_config, county_weights
    )
    accepted <- is.finite(final_score_county) &&
      final_score_county <= start_score * (1 + sqrt(.Machine$double.eps))
    result$final_score <- final_score_county
    result$accepted <- accepted

    if (accepted) {
      full_par[par_idx] <- result_par
    } else {
      warning(
        "Rejected PSO wage update for county ", cnty,
        " because it did not improve the bound-guarded objective. ",
        "start=", signif(start_objective, 6),
        ", candidate=", signif(final_objective_county, 6),
        ", start_score=", signif(start_score, 6),
        ", candidate_score=", signif(final_score_county, 6),
        call. = FALSE
      )
      result$x <- start_par
    }
    county_results[[cnty_key]] <- result
  }

  final_moments <- weighted_col_means(objective_gmm(
    theta = full_par, x = x, beta = beta, beta_2_subset = beta_2_subset,
    config = objective_config, clust = clust, solver_state = NULL
  ), moment_weights)
  final_objective <- sum(final_moments^2)
  final_score <- structural_wage_objective_score(
    final_moments, full_par, x, beta, beta_2_subset,
    objective_config, moment_weights
  )
  global_accepted <- is.finite(final_score) &&
    final_score <= start_full_score * (1 + sqrt(.Machine$double.eps))
  if (!global_accepted) {
    warning("Rejected PSO wage solve because the full bound-guarded ",
            "objective worsened.", call. = FALSE)
    full_par <- initial_full_par
    final_moments <- start_full_moments
    final_objective <- start_full_objective
    final_score <- start_full_score
  }

  list(
    par = full_par,
    convergence = if (global_accepted) 0L else 1L,
    county_results = county_results,
    final_moments = final_moments,
    objective = final_objective,
    score = final_score,
    start_objective = start_full_objective,
    start_score = start_full_score,
    global_accepted = global_accepted,
    mode = "pso"
  )
}

estimate_wage_parameters_nleqslv <- function(start, x, beta, beta_2_subset,
                                             config = CONFIG, clust = NULL,
                                             solver_state = NULL,
                                             moment_weights = NULL) {
  if (!requireNamespace("nleqslv", quietly = TRUE)) {
    stop("Package 'nleqslv' is required for wage_optimizer_mode='nleqslv'.")
  }
  data <- as.data.frame(x)
  full_par <- start
  names(full_par) <- names(beta_2_subset)
  initial_full_par <- full_par
  county_results <- list()
  moment_weights <- normalize_moment_weights(moment_weights, nrow(data))
  objective_config <- config
  objective_config$use_solver_warm_starts <- FALSE
  objective_config$use_fixedpoint_warm_starts <- FALSE
  objective_config$use_staged_solver_tolerances <- FALSE

  start_full_moments <- weighted_col_means(objective_gmm(
    theta = initial_full_par,
    x = x,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = objective_config,
    clust = clust,
    solver_state = NULL
  ), moment_weights)
  start_full_objective <- sum(start_full_moments^2)
  start_full_score <- structural_wage_objective_score(
    start_full_moments,
    initial_full_par,
    x,
    beta,
    beta_2_subset,
    objective_config,
    moment_weights
  )

  for (cnty in config$counties) {
    county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
    par_idx <- grepl(county_pattern, names(full_par), fixed = TRUE)
    row_idx <- as.character(data$county) == cnty
    if (!any(par_idx) || !any(row_idx)) next

    x_county <- data[row_idx, , drop = FALSE]
    county_weights <- if (is.null(moment_weights)) NULL else moment_weights[row_idx]
    objective_county <- function(parms) {
      candidate <- full_par
      candidate[par_idx] <- parms
      moments <- tryCatch(
        weighted_col_means(objective_gmm(
          theta = candidate,
                x = x_county,
                beta = beta,
                beta_2_subset = beta_2_subset,
                config = objective_config,
                clust = clust,
                solver_state = NULL
        ), weights = county_weights),
        error = function(e) {
          rep(solver_value(config, "optimizer_failure_penalty", 1e6),
              length(beta_2_subset))
        }
      )
      county_moments <- grepl(paste0("county", cnty, ":E_"),
                              names(moments), fixed = TRUE)
      out <- as.numeric(moments[county_moments])
      if (length(out) != sum(par_idx) || anyNA(out) || any(!is.finite(out))) {
        out <- rep(solver_value(config, "optimizer_failure_penalty", 1e6),
                   sum(par_idx))
      }
      out
    }

    start_par <- full_par[par_idx]
    start_moments <- objective_county(start_par)
    start_objective <- sum(start_moments^2)
    start_candidate <- full_par
    start_candidate[par_idx] <- start_par
    start_score <- structural_wage_objective_score(
      start_moments,
      start_candidate,
      x_county,
      beta,
      beta_2_subset,
      objective_config,
      county_weights
    )

    result <- nleqslv::nleqslv(
      x      = start_par,
      fn     = objective_county,
      method = solver_value(config, "nleqslv_method", "Broyden"),
      global = solver_value(config, "nleqslv_global", "dbldog"),
      control = list(
        xtol  = config$obj_tol,
        ftol  = config$obj_tol,
        maxit = solver_value(config, "nleqslv_maxit", 200L),
        trace = solver_value(config, "nleqslv_trace", 1L)
      )
    )

    final_moments_county <- objective_county(result$x)
    final_objective_county <- sum(final_moments_county^2)
    final_candidate <- full_par
    final_candidate[par_idx] <- result$x
    final_score_county <- structural_wage_objective_score(
      final_moments_county,
      final_candidate,
      x_county,
      beta,
      beta_2_subset,
      objective_config,
      county_weights
    )
    accepted <- is.finite(final_score_county) &&
      final_score_county <= start_score * (1 + sqrt(.Machine$double.eps))
    result$start_objective <- start_objective
    result$final_objective <- final_objective_county
    result$start_score <- start_score
    result$final_score <- final_score_county
    result$accepted <- accepted

    if (accepted) {
      full_par[par_idx] <- result$x
    } else {
      warning(
        "Rejected nleqslv wage update for county ", cnty,
        " because it did not improve the bound-guarded weighted objective. ",
        "start=", signif(start_objective, 6),
        ", candidate=", signif(final_objective_county, 6),
        ", start_score=", signif(start_score, 6),
        ", candidate_score=", signif(final_score_county, 6),
        call. = FALSE
      )
      result$x <- start_par
    }
    county_results[[as.character(cnty)]] <- result
  }

  final_moments <- weighted_col_means(objective_gmm(
    theta = full_par,
    x = x,
    beta = beta,
    beta_2_subset = beta_2_subset,
    config = objective_config,
    clust = clust,
    solver_state = NULL
  ), moment_weights)
  final_objective <- sum(final_moments^2)
  final_score <- structural_wage_objective_score(
    final_moments,
    full_par,
    x,
    beta,
    beta_2_subset,
    objective_config,
    moment_weights
  )
  global_accepted <- is.finite(final_score) &&
    final_score <= start_full_score * (1 + sqrt(.Machine$double.eps))

  if (!global_accepted) {
    warning(
      "Rejected nleqslv wage solve because the full bound-guarded weighted objective worsened. ",
      "start=", signif(start_full_objective, 6),
      ", candidate=", signif(final_objective, 6),
      ", start_score=", signif(start_full_score, 6),
      ", candidate_score=", signif(final_score, 6),
      call. = FALSE
    )
    full_par <- initial_full_par
    final_moments <- start_full_moments
    final_objective <- start_full_objective
    final_score <- start_full_score
  }

  list(
    par = full_par,
    convergence = if (
      global_accepted &&
        all(vapply(county_results, function(r) r$termcd == 1, logical(1)))
    ) 0L else 1L,
    county_results = county_results,
    final_moments = final_moments,
    objective = final_objective,
    score = final_score,
    start_objective = start_full_objective,
    start_score = start_full_score,
    global_accepted = global_accepted,
    mode = "nleqslv"
  )
}

#' Estimate wage parameters with the configured outer solver.
#'
#' Modes:
#'   - "nleqslv" (Broyden + double dogleg root-finder; the default kept for
#'     backwards compatibility).
#'   - "min_optim" (per-county optim Nelder-Mead minimizer on ||g||^2 with
#'     parameter scaling). Recommended when the moment system has flat or
#'     saturated regions where a root-finder gets stuck. See
#'     ``docs/la_wage_moment_floor.md`` for the empirical case.
#'   - "pso" (per-county vanilla particle swarm + NM polish on ||g||^2).
#'     Use when the moment surface has multiple basins that local solvers
#'     dead-end in. NYC's wage block at the manuscript-style basin is the
#'     empirical case here.
#'   - "county" (BBsolve per county; original paper-draft solver).
#'   - "joint"  (BBsolve over the full wage vector; original paper-draft).
estimate_wage_parameters <- function(start, x, beta, beta_2_subset, config = CONFIG,
                                     clust = NULL, solver_state = NULL,
                                     moment_weights = NULL) {
  mode <- tolower(solver_value(config, "wage_optimizer_mode", "nleqslv"))
  if (is.null(solver_state) && solver_flag(config, "use_solver_warm_starts", TRUE)) {
    solver_state <- get_default_solver_state(reset = TRUE)
  }

  if (identical(mode, "nleqslv")) {
    return(estimate_wage_parameters_nleqslv(
      start, x, beta, beta_2_subset, config, clust, solver_state, moment_weights
    ))
  }

  if (identical(mode, "min_optim")) {
    return(estimate_wage_parameters_min_optim(
      start, x, beta, beta_2_subset, config, clust, solver_state, moment_weights
    ))
  }

  if (identical(mode, "pso")) {
    return(estimate_wage_parameters_pso(
      start, x, beta, beta_2_subset, config, clust, solver_state, moment_weights
    ))
  }

  if (identical(mode, "joint")) {
    return(estimate_wage_parameters_joint(
      start, x, beta, beta_2_subset, config, clust, solver_state, moment_weights
    ))
  }

  if (identical(mode, "county")) {
    return(estimate_wage_parameters_by_county(
      start, x, beta, beta_2_subset, config, clust, solver_state, moment_weights
    ))
  }

  stop("Unknown wage_optimizer_mode: ", mode,
       ". Use 'nleqslv', 'min_optim', 'pso', 'county', or 'joint'.")
}
