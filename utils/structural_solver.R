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

#' Snapshot the mutable solver state as a plain list. Used by the parallel
#' wage-stage paths: forked children export their final state so the parent
#' can reproduce, exactly, the cache a serial run would have left behind
#' (the price stage consumes these warm starts via get_gammas()).
export_solver_state <- function(state) {
  list(
    gamma = as.list(state$gamma_by_key),
    E = as.list(state$E_by_key),
    last_moment_norm = state$last_moment_norm,
    n_objective_calls = state$n_objective_calls
  )
}

#' Write a snapshot's cache entries back into a live solver state. When
#' `key_prefix` is given, only keys starting with it are imported -- the
#' county-parallel path uses this to import exactly the entries a county's
#' fork wrote (solver keys are "<county>|..."), leaving other counties'
#' entries to their own forks. Does NOT touch last_moment_norm or
#' n_objective_calls; callers restore those explicitly.
import_solver_state_entries <- function(state, snapshot, key_prefix = NULL) {
  keep <- function(keys) {
    if (is.null(key_prefix)) keys else keys[startsWith(keys, key_prefix)]
  }
  for (key in keep(names(snapshot$gamma))) {
    assign(key, snapshot$gamma[[key]], envir = state$gamma_by_key)
  }
  for (key in keep(names(snapshot$E))) {
    assign(key, snapshot$E[[key]], envir = state$E_by_key)
  }
  invisible(state)
}

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
    ## deliberately NOT capped at b: the expansion loop below also grows the
    ## bracket past b when needed (was written min(max(b, .), .) == identity)
    b_warm <- start + width
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
      if (b_prime <= a_prime) {
        return(list(root = NA_real_, val = NaN, conv = FALSE,
                    eval_count = eval_count, bracket_failure = TRUE))
      }
      fa <- eval_f(a_prime)
    }

    while (fa > 0) {
      a_prime <- a_prime - config$bisection_lower_step
      if (a_prime <= 0) {
        return(list(root = NA_real_, val = NaN, conv = FALSE,
                    eval_count = eval_count, bracket_failure = TRUE))
      }
      fa <- eval_f(a_prime)
    }

    fb <- eval_f(b_prime)
    while (fb < 0) {
      b_prime <- b_prime + config$bisection_upper_step
      fb <- eval_f(b_prime)
    }
  }

  if (b_prime <= a_prime) {
    return(list(root = NA_real_, val = NaN, conv = FALSE,
                eval_count = eval_count, bracket_failure = TRUE))
  }
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

  if (isTRUE(result$bracket_failure)) {
    return(list(
      gamma = NA_real_,
      E = rep(NaN, config$n_worker_types),
      B = matrix(NaN, nrow = nrow(cost_matrix), ncol = ncol(cost_matrix)),
      is_corner = FALSE,
      converged = FALSE,
      entropy = NaN,
      bisection_evals = result$eval_count,
      bracket_failure = TRUE
    ))
  }

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

  ## mclapply/parLapply return `try-error` (atomic character with attributes)
  ## for jobs whose worker process errored; the downstream `result$unique_id`
  ## then triggers "$ operator is invalid for atomic vectors". Convert any
  ## such failure into a stop() the caller can catch.
  bad <- vapply(results,
                function(r) inherits(r, "try-error") || !is.list(r),
                logical(1))
  if (any(bad)) {
    first_bad <- which(bad)[1L]
    msg <- if (inherits(results[[first_bad]], "try-error")) {
      paste(attr(results[[first_bad]], "condition")$message,
            "(job index ", first_bad, ")")
    } else {
      paste0("inner solver returned non-list of class ",
             paste(class(results[[first_bad]]), collapse = "/"),
             " for job index ", first_bad)
    }
    warning("solve_worker_rows: ", sum(bad), " of ", length(results),
            " inner solves failed; filling NaN. First failure: ", msg,
            call. = FALSE)
    for (i in which(bad)) {
      results[[i]] <- list(
        unique_id = jobs[[i]]$unique_id,
        key = jobs[[i]]$key,
        E = rep(NaN, n_E),
        E_full = rep(NaN, config$n_worker_types),
        gamma = NaN
      )
    }
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

#' Interior-share penalty terms for one county (see config.R
#' `wage_interior_penalty_*` and docs/wage_interior_penalty_proposal.md).
#' `moment_means` are the county-subset weighted column means of the wage
#' moment matrix (E_model - E_obs, types 2..n_w in ascending order), so
#' `moment_means + obs_means` recovers the county-mean MODEL shares.
#' Enforces interiority ONLY: every type's county-mean model share must stay
#' at or above the absolute floor `wage_interior_penalty_min_share`; the
#' observed shares enter solely to recover the model share from the moment
#' means. Exactly zero when all types are interior; grows like log(share)^2
#' as a type goes numerically extinct.
#'
#' Type 1 is the omitted reference type: it has no moment and no coefficient, so
#' `moment_means`/`obs_means` cover only types 2..n_w. Its model share is still
#' pinned by adding-up (shares sum to 1), so recover it as `1 - sum(shares_2..n)`
#' and hold it to the same floor. Without this the penalty is blind to the base
#' type going extinct -- the failure mode that priced New York's type 1 out
#' entirely (model share 0.000 vs 0.235 observed) while the penalty read exactly
#' zero, because types 2..5 had simply absorbed its mass and all looked healthy.
wage_interior_penalty_county <- function(moment_means, obs_means, config = CONFIG) {
  raw <- as.numeric(moment_means) + as.numeric(obs_means)
  raw_1 <- 1 - sum(raw)
  share <- pmax(c(raw_1, raw), config$numeric_floor)
  floor_k <- max(
    solver_value(config, "wage_interior_penalty_min_share", 1e-3),
    config$numeric_floor
  )
  gap <- pmax(0, log(floor_k) - log(share))
  sum(gap^2)
}

#' County-subset weighted means of the observed shares E_raw_2..E_raw_n,
#' the `obs_means` argument of wage_interior_penalty_county().
wage_interior_obs_means <- function(x, config = CONFIG, weights = NULL) {
  data <- as.data.frame(x)
  w <- normalize_moment_weights(weights, nrow(data))
  if (is.null(w)) w <- rep(1 / nrow(data), nrow(data))
  vapply(
    2:config$n_worker_types,
    function(k) sum(w * data[[paste0("E_raw_", k)]]),
    numeric(1)
  )
}

#' Full-vector wrapper used by the accept/revert gates: splits a (possibly
#' joint, all-county) wage moment-mean vector by county, undoes the
#' off-county zero dilution (means over all of x -> county means via the
#' county weight fraction), and sums the per-county penalties. Unnamed
#' vectors are only accepted when x contains a single county (the per-county
#' gate calls), in which ascending-type order is assumed.
wage_interior_penalty_terms <- function(moment_vector, x, config = CONFIG,
                                        weights = NULL) {
  data <- as.data.frame(x)
  w <- normalize_moment_weights(weights, nrow(data))
  if (is.null(w)) w <- rep(1 / nrow(data), nrow(data))
  counties_in_x <- unique(as.character(data$county))
  k_range <- 2:config$n_worker_types

  if (is.null(names(moment_vector))) {
    if (length(counties_in_x) == 1L && length(moment_vector) == length(k_range)) {
      names(moment_vector) <- paste0("county", counties_in_x, ":E_", k_range)
    } else {
      warning("wage_interior_penalty_terms: unnamed multi-county moment vector; ",
              "penalty skipped.", call. = FALSE)
      return(0)
    }
  }

  total <- 0
  for (cnty in counties_in_x) {
    row_idx <- as.character(data$county) == cnty
    w_frac <- sum(w[row_idx])
    if (w_frac <= 0) next
    nms <- paste0("county", cnty, ":E_", k_range)
    if (!all(nms %in% names(moment_vector))) next
    m_county <- as.numeric(moment_vector[nms]) / w_frac
    if (anyNA(m_county) || any(!is.finite(m_county))) next
    obs_means <- vapply(
      k_range,
      function(k) sum(w[row_idx] * data[row_idx, paste0("E_raw_", k)]) / w_frac,
      numeric(1)
    )
    total <- total + wage_interior_penalty_county(m_county, obs_means, config)
  }
  total
}

structural_wage_objective_score <- function(moment_vector, theta, x, beta,
                                            beta_2_subset, config = CONFIG,
                                            weights = NULL) {
  moment_score <- sum(as.numeric(moment_vector)^2)
  if (solver_flag(config, "wage_interior_penalty_enabled", FALSE)) {
    moment_score <- moment_score +
      solver_value(config, "wage_interior_penalty_weight", 1) *
      wage_interior_penalty_terms(moment_vector, x, config, weights)
  }
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
  strict_obj_tol <- solver_value(config, "obj_tol", 0.01)
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
    interior_pen_on <- solver_flag(objective_config, "wage_interior_penalty_enabled", FALSE)
    interior_pen_weight <- solver_value(objective_config, "wage_interior_penalty_weight", 1)
    county_obs_means <- if (interior_pen_on) {
      wage_interior_obs_means(x_county, objective_config, county_weights)
    } else NULL

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
      ssq <- sum(v^2)
      if (interior_pen_on) {
        ssq <- ssq + interior_pen_weight *
          wage_interior_penalty_county(v, county_obs_means, objective_config)
      }
      ssq
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
    result$strict_tol <- strict_obj_tol
    result$candidate_value <- result$value

    if (result$value > strict_obj_tol) {
      warning(sprintf(
        paste0("Wage solver did not reach obj_tol for county %s ",
               "(convergence=%d, restarts=%d, multistarts=%d, parscale=%g): ",
               "best objective %.6g exceeds obj_tol=%.6g. ",
               "Continuing (absolute-tol gate is soft); ",
               "rep will be stored with wage_convergence != 0."),
        cnty, result$convergence, result$restart_count,
        n_multistarts, parscale_w, result$value, strict_obj_tol
      ), call. = FALSE)
    }

    candidate_par <- result$par
    final_moments_county <- objective_county_vec(candidate_par)
    final_objective_county <- sum(final_moments_county^2)
    final_candidate <- full_par
    final_candidate[par_idx] <- candidate_par
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
    result$candidate_par <- candidate_par
    result$candidate_objective <- final_objective_county
    result$candidate_score <- final_score_county
    result$start_objective <- start_objective
    result$start_score <- start_score
    result$accepted <- accepted

    if (accepted) {
      full_par[par_idx] <- candidate_par
      result$par <- candidate_par
      result$x <- candidate_par
      result$value <- final_objective_county
      result$final_objective <- final_objective_county
      result$final_score <- final_score_county
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
      result$par <- start_par
      result$x <- start_par
      result$value <- start_objective
      result$final_objective <- start_objective
      result$final_score <- start_score
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

  use_reltol_gate <- identical(solver_value(config, "wage_convergence_gate", "obj_tol"), "reltol")
  county_converged <- vapply(
    county_results,
    function(r) {
      if (use_reltol_gate) {
        ## Reltol gate (bootstrap): NM convergence==0 (reltol) OR ==10
        ## (degenerate simplex) both mean "optimizer stopped at a local
        ## min and can't improve." Drop the absolute obj_tol requirement.
        isTRUE(r$accepted) &&
          is.finite(r$convergence) && r$convergence %in% c(0L, 10L)
      } else {
        ## obj_tol gate (estimation default): keep strict reltol convergence
        ## AND the absolute objective threshold. Unchanged behavior.
        isTRUE(r$accepted) &&
          is.finite(r$convergence) && r$convergence == 0L &&
          is.finite(r$final_objective) &&
          is.finite(r$strict_tol) &&
          r$final_objective <= r$strict_tol
      }
    },
    logical(1)
  )

  list(
    par = full_par,
    convergence = if (global_accepted && all(county_converged)) 0L else 1L,
    county_results = county_results,
    final_moments = final_moments,
    objective = final_objective,
    score = final_score,
    start_objective = start_full_objective,
    start_score = start_full_score,
    global_accepted = global_accepted,
    county_converged = county_converged,
    mode = "min_optim"
  )
}

#' Vanilla particle-swarm minimiser shared by the estimation and
#' counterfactual wage stages.
#'
#'   x_i^{t+1} = x_i^t + v_i^{t+1};
#'   v_i^{t+1} = w*v_i^t + c1*r1*(pbest_i - x_i) + c2*r2*(gbest - x_i).
#'
#' Particles initialise uniformly in [lower, upper]^d. If `seed_particle` is
#' supplied, it replaces the first particle (warm start); pass NULL for a
#' cold start (all particles random). Returns the global-best location and
#' value after `n_iter` velocity updates.
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

#' Per-county particle-swarm wage solver with NM polish.
#'
#' Use case: when the per-county wage moment surface has multiple basins
#' that local Nelder-Mead (even with multi-start) cannot escape. Empirical
#' case in this project: NYC has a wide plateau at ssq ~0.025 near all-large-
#' negative wages that NM dead-ends in; PSO with a wide search box finds a
#' basin at ssq ~0.001 with a manuscript-like sign pattern.
#'
#' For each county the swarm initialises with all particles drawn uniformly
#' in [-pso_search_halfwidth, +pso_search_halfwidth]^d (cold start; no
#' particle pinned at the seed -- anchoring at a poor seed collapses the
#' swarm to that basin), runs n_iter velocity updates, then runs the
#' configured optim polisher (default Nelder-Mead) twice: once from the PSO
#' global best, once from the seed start. The county solution is the lowest
#' of (raw PSO, polish-from-PSO, polish-from-seed). The strict-exit guard
#' (obj_tol) fires the same way as in min_optim mode.
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
  strict_obj_tol <- solver_value(config, "obj_tol", 0.01)
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
  pso_seed_offset <- solver_value(config, "pso_seed_offset", 0L)
  if (length(pso_seed_offset) == 0L || is.na(pso_seed_offset)) {
    pso_seed_offset <- 0L
  }
  pso_iteration_seed <- 0L
  if (!is.null(config$slurm_array_task_id) && !is.na(config$slurm_array_task_id)) {
    pso_iteration_seed <- config$slurm_array_task_id
  } else if (!is.null(config$bootstrap_iteration) && !is.na(config$bootstrap_iteration)) {
    pso_iteration_seed <- config$bootstrap_iteration
  }
  pso_county_order <- setNames(seq_along(config$counties), as.character(config$counties))

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

  ## Per-county solve, extracted verbatim from the historical sequential loop
  ## so the serial path and the county-parallel path share one implementation.
  ## A county's moments depend only on its own parameter slice
  ## (build_cost_matrix() selects wage parameters by county pattern and the
  ## objective is evaluated on that county's rows alone), so passing the
  ## pre-loop joint vector (parallel path) or the sequentially updated one
  ## (serial path) as `full_par_in` yields bit-identical results. Returns NULL
  ## for counties with no parameters/rows, else
  ## list(cnty_key, par_idx, result); the caller applies
  ## `full_par[par_idx] <- result$par` (a rejected candidate carries the
  ## unchanged seed slice, so unconditional assignment is safe).
  solve_pso_county <- function(cnty, full_par_in, cfg_obj = objective_config) {
    full_par <- full_par_in
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
    if (!any(par_idx) || !any(row_idx)) return(NULL)

    x_county <- data[row_idx, , drop = FALSE]
    county_weights <- if (is.null(moment_weights)) NULL else moment_weights[row_idx]
    interior_pen_on <- solver_flag(cfg_obj, "wage_interior_penalty_enabled", FALSE)
    interior_pen_weight <- solver_value(cfg_obj, "wage_interior_penalty_weight", 1)
    county_obs_means <- if (interior_pen_on) {
      wage_interior_obs_means(x_county, cfg_obj, county_weights)
    } else NULL

    objective_county_ssq <- function(parms) {
      candidate <- full_par
      candidate[par_idx] <- parms
      moments <- tryCatch(
        weighted_col_means(objective_gmm(
          theta = candidate, x = x_county, beta = beta,
          beta_2_subset = beta_2_subset, config = cfg_obj,
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
      ssq <- sum(v^2)
      if (interior_pen_on) {
        ssq <- ssq + interior_pen_weight *
          wage_interior_penalty_county(v, county_obs_means, cfg_obj)
      }
      ssq
    }

    objective_county_vec <- function(parms) {
      candidate <- full_par
      candidate[par_idx] <- parms
      moments <- tryCatch(
        weighted_col_means(objective_gmm(
          theta = candidate, x = x_county, beta = beta,
          beta_2_subset = beta_2_subset, config = cfg_obj,
          clust = clust, solver_state = NULL
        ), weights = county_weights),
        error = function(e) NULL
      )
      if (is.null(moments)) {
        ## Match the moment width that the success path produces so callers
        ## (structural_wage_objective_score, sum(...^2)) see NA, not a length
        ## mismatch. The accepted-check downstream uses is.finite(), so NA
        ## triggers the existing rejection branch back to the warm start.
        n_moments_per_county <- config$n_worker_types - 1L
        return(rep(NA_real_, n_moments_per_county))
      }
      cm <- grepl(paste0("county", cnty, ":E_"), names(moments), fixed = TRUE)
      as.numeric(moments[cm])
    }

    start_par <- full_par[par_idx]
    start_objective <- objective_county_ssq(start_par)
    start_candidate <- full_par
    start_candidate[par_idx] <- start_par
    start_score <- structural_wage_objective_score(
      objective_county_vec(start_par), start_candidate, x_county,
      beta, beta_2_subset, cfg_obj, county_weights
    )

    d <- length(start_par)
    lower <- rep(-halfwidth, d); upper <- rep(halfwidth, d)
    pso_seed <- as.integer((
      as.numeric(pso_seed_offset) +
        1000003 * as.numeric(pso_iteration_seed) +
        9973 * as.numeric(pso_county_order[[cnty_key]])
    ) %% .Machine$integer.max)
    if (is.na(pso_seed) || pso_seed <= 0L) {
      pso_seed <- 1L
    }
    set.seed(pso_seed)
    if (solver_flag(cfg_obj, "pso_skip_swarm", FALSE)) {
      ## L4 polish-only re-solve (wage_fallback_repso_polish_only): the cold
      ## swarm is deterministic given pso_seed, so re-running it reproduces
      ## the previous round bit-for-bit; only the polish from the improved
      ## seed start below adds information. Inf-valued placeholders keep the
      ## candidate bookkeeping and convergence gates on their existing paths.
      message(sprintf(
        "PSO county %s: swarm skipped (pso_skip_swarm); polishing from seed start only.",
        cnty
      ))
      pso_res <- list(par = as.numeric(start_par), value = Inf,
                      n_iter = 0L, n_particles = 0L)
      polish_pso <- list(par = pso_res$par, value = Inf, convergence = NA_integer_)
    } else {
    message(sprintf(
      "PSO county %s: %d particles x %d iter, search box +/- %g, cold start, rng seed %d.",
      cnty, n_particles, n_iter, halfwidth, pso_seed
    ))
    ## Cold PSO: do not anchor any particle at the seed. Anchoring at a
    ## bad-basin seed (as NYC's was previously) collapses the swarm to that
    ## basin and prevents discovery of better basins elsewhere. Independent
    ## NM polishes from both the PSO best and the seed below ensure the
    ## seed's solution is never *worsened* by switching to PSO mode.
    pso_res <- pso_solve(objective_county_ssq, lower, upper,
                         seed_particle = NULL,
                         n_particles = n_particles, n_iter = n_iter,
                         w = w_inertia, c1 = c1, c2 = c2, trace = pso_trace,
                         label = cnty_key)

    polish_pso <- tryCatch(
      stats::optim(
        par = pso_res$par, fn = objective_county_ssq,
        method = polish_method,
        control = list(parscale = rep(parscale_w, d),
                       maxit = polish_maxit, reltol = reltol,
                       trace = pso_trace)
      ),
      error = function(e) {
        warning("PSO->polish failed for county ", cnty, ": ",
                conditionMessage(e), call. = FALSE)
        list(par = pso_res$par, value = pso_res$value, convergence = -1L)
      }
    )
    }

    polish_seed <- tryCatch(
      stats::optim(
        par = as.numeric(start_par), fn = objective_county_ssq,
        method = polish_method,
        control = list(parscale = rep(parscale_w, d),
                       maxit = polish_maxit, reltol = reltol,
                       trace = pso_trace)
      ),
      error = function(e) {
        warning("seed->polish failed for county ", cnty, ": ",
                conditionMessage(e), call. = FALSE)
        list(par = as.numeric(start_par), value = start_objective,
             convergence = -1L)
      }
    )

    ## Third polish: L-BFGS-B from the best NM polish. Gradient-based and
    ## strictly bounded to the PSO search box, so it can finish when NM
    ## degenerates without overshooting into non-finite regions.
    bfgs_start <- if (is.finite(polish_pso$value) &&
                     (!is.finite(polish_seed$value) ||
                      polish_pso$value <= polish_seed$value)) {
      polish_pso$par
    } else {
      polish_seed$par
    }
    bfgs_start <- pmax(pmin(as.numeric(bfgs_start), upper), lower)
    polish_bfgs <- tryCatch(
      stats::optim(
        par = bfgs_start, fn = objective_county_ssq,
        method = "L-BFGS-B",
        lower = lower, upper = upper,
        control = list(parscale = rep(parscale_w, d),
                       maxit = polish_maxit,
                       factr = 1e7,
                       trace = pso_trace)
      ),
      error = function(e) {
        warning("BFGS polish failed for county ", cnty, ": ",
                conditionMessage(e), call. = FALSE)
        list(par = bfgs_start, value = NA_real_, convergence = -1L)
      }
    )

    candidates <- list(
      pso         = list(par = pso_res$par,     value = pso_res$value),
      polish_pso  = list(par = polish_pso$par,  value = polish_pso$value),
      polish_seed = list(par = polish_seed$par, value = polish_seed$value),
      polish_bfgs = list(par = polish_bfgs$par, value = polish_bfgs$value)
    )
    cand_values <- vapply(candidates, function(x) x$value, numeric(1))
    best_label <- names(cand_values)[which.min(cand_values)]
    result_par <- candidates[[best_label]]$par
    result_value <- candidates[[best_label]]$value

    strict_tol <- strict_obj_tol

    result <- list(
      par = result_par, value = result_value, x = result_par,
      candidate_par = result_par, candidate_value = result_value,
      pso_value = pso_res$value,
      polish_pso_value = polish_pso$value,
      polish_seed_value = polish_seed$value,
      polish_bfgs_value = polish_bfgs$value,
      polish_pso_convergence = polish_pso$convergence,
      polish_seed_convergence = polish_seed$convergence,
      polish_bfgs_convergence = polish_bfgs$convergence,
      source = best_label,
      pso_n_iter = n_iter, pso_n_particles = n_particles,
      halfwidth = halfwidth, parscale = parscale_w,
      pso_seed = pso_seed, strict_tol = strict_tol,
      start_objective = start_objective, final_objective = result_value,
      start_score = start_score
    )
    message(sprintf(
      "PSO county %s done: pso=%.6g  polish_pso=%.6g  polish_seed=%.6g  polish_bfgs=%.6g  best=%.6g (%s)",
      cnty, pso_res$value, polish_pso$value, polish_seed$value,
      polish_bfgs$value, result_value, best_label
    ))

    if (result_value > strict_tol) {
      warning(sprintf(
        paste0("PSO+polish did not reach obj_tol for county %s ",
               "(pso=%.6g, polish_pso=%.6g, polish_seed=%.6g, polish_bfgs=%.6g, ",
               "halfwidth=%g, parscale=%g): best objective %.6g exceeds ",
               "obj_tol=%.6g. Continuing (absolute-tol gate is soft); ",
               "rep will be stored with wage_convergence != 0."),
        cnty, pso_res$value, polish_pso$value, polish_seed$value, polish_bfgs$value,
        halfwidth, parscale_w, result_value, strict_tol
      ), call. = FALSE)
    }

    final_candidate <- full_par
    final_candidate[par_idx] <- result_par
    final_moments_county <- objective_county_vec(result_par)
    final_objective_county <- sum(final_moments_county^2)
    final_score_county <- tryCatch(
      structural_wage_objective_score(
        final_moments_county, final_candidate, x_county,
        beta, beta_2_subset, cfg_obj, county_weights
      ),
      error = function(e) {
        warning("PSO post-check structural_wage_objective_score failed for county ",
                cnty, ": ", conditionMessage(e), " -- treating candidate as rejected.",
                call. = FALSE)
        NA_real_
      }
    )
    accepted <- is.finite(final_score_county) &&
      final_score_county <= start_score * (1 + sqrt(.Machine$double.eps))
    result$candidate_objective <- final_objective_county
    result$candidate_score <- final_score_county
    result$accepted <- accepted

    if (accepted) {
      result$par <- result_par
      result$x <- result_par
      result$value <- final_objective_county
      result$final_objective <- final_objective_county
      result$final_score <- final_score_county
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
      result$par <- start_par
      result$x <- start_par
      result$value <- start_objective
      result$final_objective <- start_objective
      result$final_score <- start_score
    }
    list(cnty_key = cnty_key, par_idx = par_idx, result = result)
  }

  county_parallel <- solver_flag(config, "wage_pso_county_parallel", FALSE) &&
    !identical(get_os(), "windows") &&
    solver_flag(config, "pl_on", TRUE) &&
    length(config$counties) > 1L &&
    get_core_count(config) > 1L

  if (!county_parallel) {
    for (cnty in config$counties) {
      county_out <- solve_pso_county(cnty, full_par)
      if (is.null(county_out)) next
      full_par[county_out$par_idx] <- county_out$result$par
      county_results[[county_out$cnty_key]] <- county_out$result
    }
  } else {
    ## Fork one child per county. Bit-identity with the sequential loop rests
    ## on: (i) exact county separability of the objective; (ii) per-county RNG
    ## reseeding (set.seed(pso_seed) above); (iii) warm-start cache keys being
    ## county-prefixed, so the children's cache writes are disjoint; (iv) the
    ## staged-tolerance handoff check below. The only cross-county coupling in
    ## the sequential loop is get_solver_tolerances()' coarse/fine branch at a
    ## county's FIRST objective call, which reads the previous county's final
    ## moment norm; children fork before any county runs and therefore see the
    ## pre-loop norm. Whenever the branch these two norms select differs, the
    ## county is re-run in-process with the exact serial incoming state.
    solver_state_env <- if (solver_flag(config, "use_solver_warm_starts", TRUE)) {
      get_default_solver_state()
    } else {
      NULL
    }
    pre_norm <- if (!is.null(solver_state_env)) solver_state_env$last_moment_norm else Inf
    pre_calls <- if (!is.null(solver_state_env)) solver_state_env$n_objective_calls else 0L

    ## Cores per child proportional to the county's row count, not an even
    ## split: the stage's wall time is bounded by the slowest county, each
    ## county's per-evaluation cost scales with its own rows, and the counties
    ## are badly imbalanced (LA holds ~55% of the estimation sample, Cook 15%),
    ## so an even split leaves most of the allocation idle once the small
    ## counties finish. Job 59321331: 16 of 24 cores sat idle for hours while
    ## LA ground on alone with 8. Scheduling-only -- worker counts never enter
    ## the numerics (inner jobs are built before dispatch and results written
    ## back in job order), so results are unchanged to the bit.
    county_row_n <- vapply(as.character(config$counties),
                           function(cn) sum(as.character(data$county) == cn),
                           numeric(1))
    county_child_cores <- if (sum(county_row_n) > 0) {
      pmax(1L, as.integer(round(get_core_count(config) *
                                  county_row_n / sum(county_row_n))))
    } else {
      rep(max(1L, get_core_count(config) %/% length(config$counties)),
          length(config$counties))
    }
    names(county_child_cores) <- as.character(config$counties)

    county_child <- function(cnty) {
      warn_msgs <- character(0)
      cfg_child <- objective_config
      cfg_child$core_count <- county_child_cores[[as.character(cnty)]]
      out <- withCallingHandlers(
        tryCatch(
          solve_pso_county(cnty, full_par, cfg_obj = cfg_child),
          error = function(e) {
            structure(list(message = conditionMessage(e)),
                      class = "pso_county_child_error")
          }
        ),
        warning = function(w) {
          warn_msgs <<- c(warn_msgs, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      list(
        out = out,
        state = if (!is.null(solver_state_env)) {
          export_solver_state(get_default_solver_state())
        } else {
          NULL
        },
        rng = if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
          get(".Random.seed", envir = globalenv(), inherits = FALSE)
        } else {
          NULL
        },
        warnings = warn_msgs
      )
    }

    children <- parallel::mclapply(
      config$counties,
      county_child,
      mc.cores = min(length(config$counties), get_core_count(config)),
      mc.preschedule = FALSE
    )
    names(children) <- as.character(config$counties)

    staged_active <- !is.null(solver_state_env) &&
      solver_flag(config, "use_staged_solver_tolerances", TRUE)
    switch_norm <- solver_value(config, "staged_tolerance_switch_norm", 1e-3)
    coarse_branch <- function(norm) is.finite(norm) && norm > switch_norm

    expected_norm <- pre_norm
    forked_call_delta <- 0L
    last_rng <- NULL

    for (cnty in config$counties) {
      cnty_key <- as.character(cnty)
      child <- children[[cnty_key]]
      child_ok <- is.list(child) && !inherits(child$out, "pso_county_child_error")
      handoff_ok <- !staged_active ||
        identical(coarse_branch(expected_norm), coarse_branch(pre_norm))

      if (child_ok && handoff_ok) {
        for (msg in child$warnings) warning(msg, call. = FALSE)
        county_out <- child$out
        if (is.null(county_out)) next
        full_par[county_out$par_idx] <- county_out$result$par
        county_results[[county_out$cnty_key]] <- county_out$result
        if (!is.null(solver_state_env) && !is.null(child$state)) {
          import_solver_state_entries(solver_state_env, child$state,
                                      key_prefix = paste0(cnty_key, "|"))
          forked_call_delta <- forked_call_delta +
            (child$state$n_objective_calls - pre_calls)
          expected_norm <- child$state$last_moment_norm
        }
        if (!is.null(child$rng)) last_rng <- child$rng
      } else {
        if (!child_ok) {
          message("[pso county-parallel] child for county ", cnty_key,
                  " failed (",
                  if (is.list(child)) child$out$message else "worker process died",
                  "); re-running this county in-process.")
        } else {
          message("[pso county-parallel] staged-tolerance handoff for county ",
                  cnty_key, " differs from serial order; re-running this ",
                  "county in-process for bit-identical results.")
        }
        if (!is.null(solver_state_env)) {
          solver_state_env$last_moment_norm <- expected_norm
        }
        county_out <- solve_pso_county(cnty, full_par)
        if (is.null(county_out)) next
        last_rng <- NULL
        full_par[county_out$par_idx] <- county_out$result$par
        county_results[[county_out$cnty_key]] <- county_out$result
        if (!is.null(solver_state_env)) {
          expected_norm <- solver_state_env$last_moment_norm
        }
      }
    }

    ## Leave the parent process exactly as the sequential loop would have:
    ## warm-start cache entries were merged per county above; the norm is the
    ## last-run county's final norm; the RNG state is the last forked county's
    ## final state (a NULL last_rng means the last county ran in-process, so
    ## the parent RNG already advanced serially).
    if (!is.null(solver_state_env)) {
      solver_state_env$last_moment_norm <- expected_norm
      solver_state_env$n_objective_calls <-
        solver_state_env$n_objective_calls + forked_call_delta
    }
    if (!is.null(last_rng)) {
      assign(".Random.seed", last_rng, envir = globalenv())
    }
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

  use_reltol_gate <- identical(solver_value(config, "wage_convergence_gate", "obj_tol"), "reltol")
  county_converged <- vapply(
    county_results,
    function(r) {
      if (!isTRUE(r$accepted)) return(FALSE)
      if (use_reltol_gate) {
        ## Converged when the winning region's NM polish stopped making
        ## progress: optim convergence==0 (reltol satisfied) OR ==10
        ## (degenerate simplex on a flat local min -- the common exit on the
        ## bootstrap reweighted wage surface; bootstrap log diagnostic across
        ## 308 reps showed NM exiting at iter 100-900 with 100s of SHRINKs,
        ## i.e. simplex collapsed before reltol was numerically satisfied).
        ## Both codes mean "the optimizer found a local min and can't improve."
        polish_min <- suppressWarnings(min(c(
          r$polish_pso_value, r$polish_seed_value, r$polish_bfgs_value
        ), na.rm = TRUE))
        polish_improved <- is.finite(polish_min) && is.finite(r$pso_value) &&
          polish_min <= r$pso_value
        ## Accept when any polish formally converged (code 0/10), OR when the
        ## best polish strictly improved over the PSO best. The improvement
        ## test catches the dominant remaining wage_nonconv mode: hard reps
        ## where 3 independent polishes all exhaust maxit but did refine the
        ## PSO solution -- which is what we actually need for bootstrap
        ## inference (a refined local min in the right basin), not whether
        ## the optimizer's stopping rule formally fired.
        (is.finite(r$polish_pso_convergence)  && r$polish_pso_convergence  %in% c(0L, 10L)) ||
          (is.finite(r$polish_seed_convergence) && r$polish_seed_convergence %in% c(0L, 10L)) ||
          (is.finite(r$polish_bfgs_convergence) && r$polish_bfgs_convergence == 0L) ||
          polish_improved
      } else {
        is.finite(r$final_objective) &&
          is.finite(r$strict_tol) &&
          r$final_objective <= r$strict_tol
      }
    },
    logical(1)
  )

  list(
    par = full_par,
    convergence = if (global_accepted && all(county_converged)) 0L else 1L,
    county_results = county_results,
    final_moments = final_moments,
    objective = final_objective,
    score = final_score,
    start_objective = start_full_objective,
    start_score = start_full_score,
    global_accepted = global_accepted,
    county_converged = county_converged,
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
#'   - "min_optim_warm" (identical to "min_optim" -- per-county Nelder-Mead +
#'     parameter scaling + degenerate-simplex restarts -- but WITHOUT the
#'     per-county random multistart. Each county does a single solve
#'     warm-started from `start`. Intended for the bootstrap: every rep
#'     already warm-starts from the 06 point estimate (the good basin), so a
#'     single local solve tracks that basin under the rep's reweighting
#'     instead of cold-searching for it as "pso" does -- avoiding the global
#'     search that stalls/time-outs bootstrap reps. It will NOT escape to a
#'     different basin, which is the intended behaviour for a warm-started
#'     bootstrap (and the reason it is distinct from "min_optim").)
#'   - "pso" (per-county vanilla particle swarm + NM polish on ||g||^2).
#'     Use when the moment surface has multiple basins that local solvers
#'     dead-end in. NYC's wage block at the manuscript-style basin is the
#'     empirical case here.
#'   - "county" (BBsolve per county; original paper-draft solver).
#'   - "joint"  (BBsolve over the full wage vector; original paper-draft).
#' Per-mode wage-solver dispatcher. Picks the underlying solver based on
#' `config$wage_optimizer_mode` and returns its result unchanged. The public
#' entry point `estimate_wage_parameters()` (defined below) wraps this with
#' the post-solve fallback layers in `utils/wage_fallbacks.R`.
estimate_wage_parameters_dispatch <- function(start, x, beta, beta_2_subset,
                                              config = CONFIG, clust = NULL,
                                              solver_state = NULL,
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

  ## "min_optim" minus step-3 (per-county random multistart): a single
  ## warm-started Nelder-Mead solve per county, keeping the parameter scaling
  ## (incl. the NYC parscale override) and the degenerate-simplex restart.
  ## Emptying `min_optim_n_multistarts_by_county` makes every county use
  ## `n_multistarts = 1L` (the seed start only); parscale and restart settings
  ## are untouched. See the dispatcher doc comment for the rationale.
  if (identical(mode, "min_optim_warm")) {
    cfg <- config
    cfg$min_optim_n_multistarts_by_county <- list()
    return(estimate_wage_parameters_min_optim(
      start, x, beta, beta_2_subset, cfg, clust, solver_state, moment_weights
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
       ". Use 'nleqslv', 'min_optim', 'min_optim_warm', 'pso', 'county', or 'joint'.")
}

#' Public wage-stage entry point. Layered fallback architecture:
#'   Layer 5 (joint multistart, opt-in) wraps:
#'     Layer 4 (re-PSO loop) wrapping:
#'       Layer 3 (per-county multistart on flagged counties) wrapping:
#'         Layer 2 (per-county Hessian probe, diagnostic) wrapping:
#'           Layer 1 (per-county tight polish) wrapping:
#'             the existing per-mode dispatcher above.
#'
#' In bootstrap mode (config$bootstrap_iteration is not NA) all layers are
#' bypassed by default; see config.R wage_fallback_skip_in_bootstrap.
estimate_wage_parameters <- function(start, x, beta, beta_2_subset, config = CONFIG,
                                     clust = NULL, solver_state = NULL,
                                     moment_weights = NULL) {
  if (wage_fallback_in_bootstrap_skip(config)) {
    return(estimate_wage_parameters_dispatch(
      start, x, beta, beta_2_subset, config, clust, solver_state, moment_weights
    ))
  }

  joint_k <- as.integer(solver_value(config, "wage_fallback_joint_multistart_k", 0L))
  if (!is.na(joint_k) && joint_k > 1L) {
    return(estimate_wage_parameters_with_joint_multistart(
      start, x, beta, beta_2_subset, config, clust, solver_state,
      moment_weights, K = joint_k
    ))
  }

  result <- estimate_wage_parameters_dispatch(
    start, x, beta, beta_2_subset, config, clust, solver_state, moment_weights
  )
  apply_wage_fallback_layers(result, x, beta, beta_2_subset, config,
                             clust, solver_state, moment_weights)
}
