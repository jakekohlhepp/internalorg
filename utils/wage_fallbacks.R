## ===========================================================================
## utils/wage_fallbacks.R
##
## Layered post-solve fallbacks for the structural wage stage.
## Each layer targets weak or non-local solutions left by the primary solver.
##
## Layer 1 -- per-county post-PSO polish.
## Layer 2 -- per-county slice-Hessian diagnostic.
## Layer 3 -- per-county multistart for flagged counties.
## Layer 4 -- re-PSO from the improved joint vector.
## Layer 5 -- optional joint multistart.
##
## Bootstrap execution can disable the ladder through configuration.
## ===========================================================================

#' TRUE if we are running inside a bootstrap rep AND the user has not
#' overridden the bootstrap-skip flag.
wage_fallback_in_bootstrap_skip <- function(config = CONFIG) {
  if (!solver_flag(config, "wage_fallback_skip_in_bootstrap", TRUE)) {
    return(FALSE)
  }
  iter <- config$bootstrap_iteration
  array_iter <- config$slurm_array_task_id
  (!is.null(iter)       && !is.na(iter)) ||
    (!is.null(array_iter) && !is.na(array_iter))
}

#' Per-county wage parameter index pattern: `factor(county)<c>:avg_labor:E_raw_*`.
wage_fallback_county_par_idx <- function(cnty, par_names) {
  county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
  grepl(county_pattern, par_names, fixed = TRUE)
}

#' Build the per-county slice objective `ssq(parms) = sum_v moments_county^2`
#' (plus the interior-share penalty when enabled, so the ladder searches and
#' diagnoses the SAME criterion the solvers minimize and the gates accept on)
#' where `parms` is the 4-vector of `:avg_labor:E_raw_2..5` entries for county
#' `cnty`, and the rest of the parameter vector is held at `full_par`.
wage_fallback_make_slice_objective <- function(cnty, full_par, par_idx, x_county,
                                               beta, beta_2_subset, config,
                                               clust, county_weights) {
  interior_pen_on <- solver_flag(config, "wage_interior_penalty_enabled", FALSE)
  interior_pen_weight <- solver_value(config, "wage_interior_penalty_weight", 1)
  county_obs_means <- if (interior_pen_on) {
    wage_interior_obs_means(x_county, config, county_weights)
  } else NULL
  function(parms) {
    candidate <- full_par
    candidate[par_idx] <- parms
    moments <- tryCatch(
      weighted_col_means(objective_gmm(
        theta = candidate, x = x_county, beta = beta,
        beta_2_subset = beta_2_subset, config = config,
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
        wage_interior_penalty_county(v, county_obs_means, config)
    }
    ssq
  }
}

wage_fallback_parscale_for <- function(cnty, config) {
  cnty_key <- as.character(cnty)
  by_county <- solver_value(config, "min_optim_parscale_wage_by_county", list())
  if (!is.null(by_county[[cnty_key]])) {
    return(by_county[[cnty_key]])
  }
  solver_value(config, "min_optim_parscale_wage", 20)
}

wage_fallback_log <- function(msg, config) {
  if (solver_flag(config, "wage_fallback_verbose", TRUE)) {
    message("[wage_fallbacks] ", msg)
  }
}

#' Format a per-county wage parameter vector (E_raw_2..n) for logging.
wage_fallback_format_county_par <- function(par_vec) {
  paste(sprintf("%.3f", as.numeric(par_vec)), collapse = ", ")
}

#' Write a checkpoint of the current joint wage vector to disk so a timed-out
#' or crashed run can resume from the best-known L1/L3/L4 vector. Atomic via
#' rename-from-tmp. Path defaults to <prep_output_dir>/06_wage_fb_checkpoint.rds.
wage_fallback_checkpoint_write <- function(full_par, config, label) {
  path <- solver_value(config, "wage_fallback_checkpoint_path",
                       file.path(config$prep_output_dir,
                                 "06_wage_fb_checkpoint.rds"))
  if (is.null(path) || !nzchar(path) || identical(tolower(path), "none")) {
    return(invisible())
  }
  tryCatch({
    tmp <- paste0(path, ".tmp")
    saveRDS(list(par = full_par, label = label, timestamp = Sys.time()), tmp)
    file.rename(tmp, path)
    wage_fallback_log(sprintf("checkpoint written (%s) -> %s", label, path), config)
  }, error = function(e) {
    wage_fallback_log(sprintf("checkpoint write failed (%s): %s",
                              label, conditionMessage(e)), config)
  })
}

#' Bound-guarded score at a candidate (theta, x) using the same combinator the
#' existing per-county acceptance check uses (sum(g^2) + lambda*sum(bound^2)).
#' Returns NA_real_ if the score cannot be evaluated (e.g. the structural
#' bound moments themselves return NA at the candidate).
wage_fallback_bound_guarded_score <- function(par_vec, x, beta, beta_2_subset,
                                              config, clust, county_weights, cnty) {
  moments <- tryCatch(
    weighted_col_means(objective_gmm(
      theta = par_vec, x = x, beta = beta,
      beta_2_subset = beta_2_subset, config = config,
      clust = clust, solver_state = NULL
    ), weights = county_weights),
    error = function(e) NULL
  )
  if (is.null(moments)) return(NA_real_)
  cm <- grepl(paste0("county", cnty, ":E_"), names(moments), fixed = TRUE)
  v <- as.numeric(moments[cm])
  if (anyNA(v) || any(!is.finite(v))) return(NA_real_)
  tryCatch(
    structural_wage_objective_score(
      v, par_vec, x, beta, beta_2_subset, config, county_weights
    ),
    error = function(e) NA_real_
  )
}

## ---------------------------------------------------------------------------
## Layer 1: per-county tight Nelder-Mead polish.
## ---------------------------------------------------------------------------
#' Re-polish a single county's wage slice from its current value in `full_par`
#' under reltol = wage_fallback_post_polish_reltol (default 1e-4). Returns
#' list(par, value, improved, before_ssq, after_ssq).
wage_fallback_layer1_polish_one <- function(cnty, full_par, x_county, beta,
                                            beta_2_subset, config, clust,
                                            county_weights) {
  par_idx <- wage_fallback_county_par_idx(cnty, names(full_par))
  if (!any(par_idx)) {
    return(list(par = numeric(0), value = NA_real_, improved = FALSE,
                before_ssq = NA_real_, after_ssq = NA_real_, skipped = TRUE))
  }

  fn <- wage_fallback_make_slice_objective(cnty, full_par, par_idx, x_county,
                                           beta, beta_2_subset, config,
                                           clust, county_weights)
  parscale_w <- wage_fallback_parscale_for(cnty, config)
  reltol     <- solver_value(config, "wage_fallback_post_polish_reltol", 1e-4)
  maxit      <- solver_value(config, "wage_fallback_post_polish_maxit", 2000L)
  threshold  <- solver_value(config, "wage_fallback_post_polish_threshold", 0.01)

  start_par <- as.numeric(full_par[par_idx])
  before_ssq <- fn(start_par)

  opt <- tryCatch(
    stats::optim(par = start_par, fn = fn, method = "Nelder-Mead",
                 control = list(maxit = maxit, reltol = reltol,
                                parscale = rep(parscale_w, length(start_par)))),
    error = function(e) NULL
  )
  if (is.null(opt) || !is.finite(opt$value)) {
    return(list(par = start_par, value = before_ssq, improved = FALSE,
                before_ssq = before_ssq, after_ssq = before_ssq,
                rejected_reason = "optim_failed", skipped = FALSE))
  }

  ## Slice-objective acceptance: the polished point must be lower by more than
  ## the relative threshold to even consider acceptance.
  ssq_improved <- is.finite(before_ssq) && opt$value < before_ssq &&
    (before_ssq - opt$value) > threshold * max(abs(before_ssq), 1e-12)

  ## Bound-guarded acceptance: mirror the existing per-county check in
  ## estimate_wage_parameters_pso. If the polished point's score is NA (bound
  ## violation) or worsens vs the seed score, reject and revert. This catches
  ## the May 15 failure mode where PSO+polish found a lower slice ssq but the
  ## structural bound moments returned NA at the candidate (-> rejection).
  candidate <- full_par
  candidate[par_idx] <- opt$par
  start_candidate <- full_par
  start_candidate[par_idx] <- start_par
  start_score <- wage_fallback_bound_guarded_score(
    start_candidate, x_county, beta, beta_2_subset, config, clust, county_weights, cnty
  )
  candidate_score <- wage_fallback_bound_guarded_score(
    candidate, x_county, beta, beta_2_subset, config, clust, county_weights, cnty
  )
  bound_ok <- is.finite(candidate_score) &&
    (is.na(start_score) ||
       candidate_score <= start_score * (1 + sqrt(.Machine$double.eps)))

  improved <- isTRUE(ssq_improved) && isTRUE(bound_ok)

  reason <- if (improved) NA_character_
            else if (!ssq_improved) "ssq_not_improved"
            else "bound_guard_rejected"

  list(
    par        = if (improved) opt$par else start_par,
    value      = if (improved) opt$value else before_ssq,
    improved   = improved,
    before_ssq = before_ssq,
    after_ssq  = opt$value,
    start_score = start_score,
    candidate_score = candidate_score,
    convergence = opt$convergence,
    rejected_reason = reason,
    skipped    = FALSE
  )
}

## ---------------------------------------------------------------------------
## Layer 2: per-county slice-Hessian eigenvalue probe.
## ---------------------------------------------------------------------------
#' Compute the per-county Hessian at the current point and classify the
#' verdict using 06b's exact rule. Returns list(eigenvalues, verdict, hessian).
wage_fallback_layer2_hessian_one <- function(cnty, full_par, x_county, beta,
                                             beta_2_subset, config, clust,
                                             county_weights) {
  par_idx <- wage_fallback_county_par_idx(cnty, names(full_par))
  if (!any(par_idx)) {
    return(list(eigenvalues = numeric(0), verdict = NA_character_,
                hessian = NULL, skipped = TRUE))
  }
  if (!requireNamespace("numDeriv", quietly = TRUE)) {
    wage_fallback_log("numDeriv not installed; skipping Hessian probe.", config)
    return(list(eigenvalues = numeric(0), verdict = NA_character_,
                hessian = NULL, skipped = TRUE))
  }
  fn <- wage_fallback_make_slice_objective(cnty, full_par, par_idx, x_county,
                                           beta, beta_2_subset, config,
                                           clust, county_weights)
  x_cur <- as.numeric(full_par[par_idx])
  ## numDeriv's `d` is relative; keep it small so the Hessian remains local.
  fd_rel <- 1e-2
  H <- tryCatch(
    numDeriv::hessian(fn, x_cur,
                      method.args = list(d = fd_rel, eps = 1e-4,
                                         zero.tol = 1e-12, r = 4, v = 2)),
    error = function(e) NULL
  )
  if (is.null(H)) {
    return(list(eigenvalues = rep(NA_real_, length(x_cur)),
                verdict = "hessian_failed", hessian = NULL, skipped = FALSE))
  }
  eig <- eigen(H, symmetric = TRUE)
  tol <- solver_value(config, "wage_fallback_hessian_neg_eig_tol", 1e-9)
  npos <- sum(eig$values >  tol)
  nneg <- sum(eig$values < -tol)
  nzer <- sum(abs(eig$values) <= tol)
  verdict <- if (nneg > 0) "saddle"
             else if (nzer > 0 || min(eig$values) < 1e-9) "ridge"
             else "local_min"
  list(eigenvalues = eig$values, verdict = verdict, hessian = H,
       eigenvectors = eig$vectors, skipped = FALSE)
}

## ---------------------------------------------------------------------------
## Layer 3: per-county random multistart for flagged counties.
## ---------------------------------------------------------------------------
#' K random starts uniform in [x* - scale*parscale, x* + scale*parscale]^d,
#' polish each, return the best by final ssq.
wage_fallback_layer3_multistart_one <- function(cnty, full_par, x_county, beta,
                                                beta_2_subset, config, clust,
                                                county_weights, K = NULL,
                                                scale_mult = NULL) {
  par_idx <- wage_fallback_county_par_idx(cnty, names(full_par))
  if (!any(par_idx)) {
    return(list(par = numeric(0), value = NA_real_, improved = FALSE, skipped = TRUE))
  }
  K          <- if (is.null(K))          solver_value(config, "wage_fallback_multistart_k", 4L) else K
  scale_mult <- if (is.null(scale_mult)) solver_value(config, "wage_fallback_multistart_scale_mult", 5) else scale_mult
  parscale_w <- wage_fallback_parscale_for(cnty, config)
  reltol     <- solver_value(config, "wage_fallback_post_polish_reltol", 1e-4)
  maxit      <- solver_value(config, "wage_fallback_post_polish_maxit", 2000L)

  fn <- wage_fallback_make_slice_objective(cnty, full_par, par_idx, x_county,
                                           beta, beta_2_subset, config,
                                           clust, county_weights)
  x_cur <- as.numeric(full_par[par_idx])
  d <- length(x_cur)
  span <- scale_mult * parscale_w

  ## Use a seed derived from county + iteration so runs are reproducible.
  cnty_int <- as.integer(suppressWarnings(as.numeric(cnty)))
  if (is.na(cnty_int)) cnty_int <- 0L
  iter_int <- as.integer(suppressWarnings(as.numeric(
    config$bootstrap_iteration %||% config$slurm_array_task_id %||% 0L
  )))
  if (is.na(iter_int)) iter_int <- 0L
  seed <- (cnty_int * 7919L + iter_int * 977L + 13L) %% .Machine$integer.max
  if (seed <= 0L) seed <- 1L
  set.seed(seed)

  before_ssq <- fn(x_cur)
  start_candidate <- full_par
  start_candidate[par_idx] <- x_cur
  start_score <- wage_fallback_bound_guarded_score(
    start_candidate, x_county, beta, beta_2_subset, config, clust, county_weights, cnty
  )

  best_par <- x_cur
  best_val <- before_ssq
  best_score <- start_score
  improved <- FALSE
  for (k in seq_len(K)) {
    perturb <- runif(d, -span, span)
    s <- x_cur + perturb
    opt <- tryCatch(
      stats::optim(par = s, fn = fn, method = "Nelder-Mead",
                   control = list(maxit = maxit, reltol = reltol,
                                  parscale = rep(parscale_w, d))),
      error = function(e) NULL
    )
    if (is.null(opt) || !is.finite(opt$value) || opt$value >= best_val) next
    cand_full <- full_par
    cand_full[par_idx] <- opt$par
    cand_score <- wage_fallback_bound_guarded_score(
      cand_full, x_county, beta, beta_2_subset, config, clust, county_weights, cnty
    )
    if (!is.finite(cand_score)) next
    if (is.na(best_score) || cand_score <= best_score * (1 + sqrt(.Machine$double.eps))) {
      best_val <- opt$value
      best_par <- opt$par
      best_score <- cand_score
      improved <- TRUE
    }
  }
  list(par = best_par, value = best_val, improved = improved,
       before_ssq = before_ssq, after_ssq = best_val,
       start_score = start_score, candidate_score = best_score,
       K = K, scale_mult = scale_mult, skipped = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---------------------------------------------------------------------------
## Apply Layers 1-4 around a freshly-solved wage result.
## ---------------------------------------------------------------------------
#' Modify `result` in place (returning a copy) by running:
#'   - Layer 0 short circuit: skip any county whose slice ssq is already below
#'     `wage_fallback_converged_ssq_tol`
#'   - Layer 1 polish per county (tight reltol)
#'   - Layer 2 Hessian probe per county (diagnostic log)
#'   - Layer 3 multistart per flagged county
#'   - Layer 4 re-PSO if any county changed
#' Stops when no county's slice ssq improves by more than
#' `wage_fallback_repso_min_improvement` (relative) or after
#' `wage_fallback_repso_max_iter` re-PSO rounds.
apply_wage_fallback_layers <- function(result, x, beta, beta_2_subset, config,
                                       clust, solver_state, moment_weights) {
  full_par <- result$par
  data <- as.data.frame(x)

  l1_enable <- solver_flag(config, "wage_fallback_post_polish_enable", TRUE)
  l2_enable <- solver_flag(config, "wage_fallback_hessian_enable", TRUE)
  l3_enable <- solver_flag(config, "wage_fallback_multistart_enable", TRUE)
  l3_only_flagged <- solver_flag(config, "wage_fallback_multistart_only_flagged", TRUE)
  l4_max_iter <- solver_value(config, "wage_fallback_repso_max_iter", 2L)
  l4_min_improve <- solver_value(config, "wage_fallback_repso_min_improvement", 0.01)
  converged_tol <- solver_value(config, "wage_fallback_converged_ssq_tol", 1e-8)

  if (!any(c(l1_enable, l2_enable, l3_enable)) && l4_max_iter <= 0L) {
    result$fallback_disabled <- TRUE
    return(result)
  }

  layer_report <- list()
  for (repso_iter in seq_len(max(1L, l4_max_iter + 1L))) {
    iter_report <- list(iter = repso_iter)
    any_improvement <- FALSE

    for (cnty in config$counties) {
      cnty_key <- as.character(cnty)
      row_idx <- as.character(data$county) == cnty
      if (!any(row_idx)) next
      x_county <- data[row_idx, , drop = FALSE]
      county_weights <- if (is.null(moment_weights)) NULL else moment_weights[row_idx]

      cnty_record <- list(cnty = cnty_key)

      ## Layer 0: skip counties already at machine-zero. One slice evaluation
      ## here replaces an L1 polish + L2 Hessian probe + (possibly) an L3
      ## multistart that cannot improve on a converged point.
      if (is.finite(converged_tol) && converged_tol > 0) {
        par_idx <- wage_fallback_county_par_idx(cnty, names(full_par))
        cur_ssq <- tryCatch(
          wage_fallback_make_slice_objective(
            cnty, full_par, par_idx, x_county, beta, beta_2_subset,
            config, clust, county_weights
          )(full_par[par_idx]),
          error = function(e) NA_real_
        )
        if (is.finite(cur_ssq) && cur_ssq < converged_tol) {
          wage_fallback_log(sprintf(
            "L0 county %s: slice ssq %.6g < %.6g; already converged, skipping ladder.",
            cnty_key, cur_ssq, converged_tol), config)
          cnty_record$layer0 <- list(skipped_ladder = TRUE, ssq = cur_ssq,
                                     tol = converged_tol)
          iter_report[[cnty_key]] <- cnty_record
          next
        }
        cnty_record$layer0 <- list(skipped_ladder = FALSE, ssq = cur_ssq,
                                   tol = converged_tol)
      }

      ## Layer 1: post-PSO polish.
      if (l1_enable) {
        l1 <- wage_fallback_layer1_polish_one(
          cnty, full_par, x_county, beta, beta_2_subset, config, clust, county_weights
        )
        if (!isTRUE(l1$skipped) && l1$improved) {
          par_idx <- wage_fallback_county_par_idx(cnty, names(full_par))
          full_par[par_idx] <- l1$par
          wage_fallback_log(sprintf(
            "L1 county %s polish: %.6g -> %.6g (accepted); wages (E_raw_2..) = %s",
            cnty_key, l1$before_ssq, l1$after_ssq,
            wage_fallback_format_county_par(l1$par)), config)
          any_improvement <- TRUE
          wage_fallback_checkpoint_write(
            full_par, config,
            sprintf("L4 round %d post-L1 county %s", repso_iter, cnty_key))
        }
        cnty_record$layer1 <- l1
      }

      ## Layer 2: Hessian probe (diagnostic).
      verdict <- NA_character_
      if (l2_enable) {
        l2 <- wage_fallback_layer2_hessian_one(
          cnty, full_par, x_county, beta, beta_2_subset, config, clust, county_weights
        )
        verdict <- l2$verdict
        if (!isTRUE(l2$skipped)) {
          wage_fallback_log(sprintf(
            "L2 county %s verdict: %s (eigs: %s)", cnty_key, verdict,
            paste(signif(l2$eigenvalues, 4), collapse = ", ")), config)
        }
        cnty_record$layer2 <- l2
      }

      ## Layer 3: multistart on flagged counties.
      if (l3_enable) {
        run_l3 <- if (l3_only_flagged && l2_enable) {
          isTRUE(verdict %in% c("saddle", "ridge"))
        } else {
          TRUE
        }
        if (run_l3) {
          l3 <- wage_fallback_layer3_multistart_one(
            cnty, full_par, x_county, beta, beta_2_subset, config, clust, county_weights
          )
          if (!isTRUE(l3$skipped) && l3$improved) {
            par_idx <- wage_fallback_county_par_idx(cnty, names(full_par))
            full_par[par_idx] <- l3$par
            wage_fallback_log(sprintf(
              "L3 county %s multistart: %.6g -> %.6g (accepted, K=%d); wages (E_raw_2..) = %s",
              cnty_key, l3$before_ssq, l3$after_ssq, l3$K,
              wage_fallback_format_county_par(l3$par)), config)
            any_improvement <- TRUE
            wage_fallback_checkpoint_write(
              full_par, config,
              sprintf("L4 round %d post-L3 county %s", repso_iter, cnty_key))
          }
          cnty_record$layer3 <- l3
        }
      }

      iter_report[[cnty_key]] <- cnty_record
    }
    layer_report[[repso_iter]] <- iter_report

    if (!any_improvement || repso_iter > l4_max_iter) break

    ## Layer 4: re-PSO from the improved warm start. Pass the updated
    ## full_par as the new wage start; everything else identical. In pso mode
    ## the optional polish-only variant skips the deterministic re-swarm
    ## (identical seed => identical trajectory) and re-polishes from the
    ## improved seed only; see config.R wage_fallback_repso_polish_only.
    if (repso_iter <= l4_max_iter) {
      l4_config <- config
      l4_polish_only <- solver_flag(config, "wage_fallback_repso_polish_only", FALSE) &&
        identical(tolower(solver_value(config, "wage_optimizer_mode", "nleqslv")), "pso")
      if (l4_polish_only) {
        l4_config$pso_skip_swarm <- TRUE
      }
      wage_fallback_log(sprintf(
        "L4 round %d: re-running wage solver from improved warm start%s.",
        repso_iter,
        if (l4_polish_only) " (polish-only, swarm skipped)" else ""), config)
      tryCatch({
        new_result <- estimate_wage_parameters_dispatch(
          start = full_par, x = x, beta = beta, beta_2_subset = beta_2_subset,
          config = l4_config, clust = clust, solver_state = solver_state,
          moment_weights = moment_weights
        )
        full_par <- new_result$par
        result <- new_result
        wage_fallback_checkpoint_write(
          full_par, config,
          sprintf("L4 round %d completed re-PSO", repso_iter))
      }, error = function(e) {
        warning("L4 re-PSO failed (", conditionMessage(e),
                "); keeping pre-re-PSO vector.", call. = FALSE)
      })
    }
  }

  result$par <- full_par
  result$fallback_layers <- layer_report
  result
}

## ---------------------------------------------------------------------------
## Layer 5: joint multistart.
## ---------------------------------------------------------------------------
#' Run the wage solver (with layers 1-4) K times from K perturbed warm
#' starts and return the run with the lowest final objective. Each start
#' jitters the seed wage vector by a random uniform multiplier in
#' [-jitter, +jitter] (relative to each entry, with a floor for entries
#' near zero so multiplicative perturbation does not collapse to zero).
estimate_wage_parameters_with_joint_multistart <- function(start, x, beta,
                                                           beta_2_subset, config,
                                                           clust, solver_state,
                                                           moment_weights, K) {
  jitter <- solver_value(config, "wage_fallback_joint_multistart_jitter", 0.1)
  wage_fallback_log(sprintf(
    "L5 joint multistart with K=%d, jitter=%.3g.", K, jitter), config)

  ## First start: the original warm start (no perturbation).
  starts <- vector("list", K)
  starts[[1]] <- start
  if (K > 1L) {
    set.seed(20260519L)
    for (k in seq.int(2L, K)) {
      noise <- runif(length(start), 1 - jitter, 1 + jitter)
      floor_abs <- pmax(abs(start), 1) * jitter
      starts[[k]] <- start * noise + sign(runif(length(start), -1, 1)) * floor_abs
      names(starts[[k]]) <- names(start)
    }
  }

  ## Inner runner: dispatch + layers 1-4 (NOT recursive into joint multistart).
  inner_config <- config
  inner_config$wage_fallback_joint_multistart_k <- 0L

  run_start <- function(k, cfg = inner_config) {
    res <- tryCatch(
      estimate_wage_parameters_dispatch(
        start = starts[[k]], x = x, beta = beta, beta_2_subset = beta_2_subset,
        config = cfg, clust = clust, solver_state = solver_state,
        moment_weights = moment_weights
      ),
      error = function(e) {
        warning("L5 start ", k, " dispatcher errored: ", conditionMessage(e),
                call. = FALSE)
        NULL
      }
    )
    if (is.null(res)) return(NULL)
    apply_wage_fallback_layers(res, x, beta, beta_2_subset, cfg,
                               clust, solver_state, moment_weights)
  }

  ## Parallel path: each start is a pure function of its start vector -- the
  ## dispatcher resets the global solver cache on entry and every RNG consumer
  ## (per-county PSO seeds, L3 multistart seeds) reseeds deterministically --
  ## so K forked starts reproduce the sequential loop bit-for-bit. Gated on
  ## is.null(solver_state): a caller-supplied live state is NOT reset by the
  ## dispatcher, which couples consecutive starts and forces the serial loop.
  l5_parallel <- solver_flag(config, "wage_fallback_joint_multistart_parallel", FALSE) &&
    !identical(get_os(), "windows") &&
    solver_flag(config, "pl_on", TRUE) &&
    K > 1L &&
    get_core_count(config) > 1L &&
    is.null(solver_state)

  best <- NULL
  best_val <- Inf
  if (!l5_parallel) {
    for (k in seq_len(K)) {
      wage_fallback_log(sprintf("L5 start %d/%d.", k, K), config)
      res <- run_start(k)
      if (is.null(res)) next
      val <- if (is.numeric(res$objective)) res$objective else
             if (is.numeric(res$value))     res$value     else Inf
      if (is.finite(val) && val < best_val) {
        best_val <- val
        best <- res
      }
    }
  } else {
    n_forks <- min(K, get_core_count(config))
    wage_fallback_log(sprintf(
      "L5 running %d starts in parallel forks (%d at a time).", K, n_forks), config)

    l5_child <- function(k) {
      warn_msgs <- character(0)
      cfg_child <- inner_config
      cfg_child$core_count <- max(1L, get_core_count(config) %/% n_forks)
      wage_fallback_log(sprintf("L5 start %d/%d.", k, K), config)
      res <- withCallingHandlers(
        run_start(k, cfg_child),
        warning = function(w) {
          warn_msgs <<- c(warn_msgs, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      list(
        res = res,
        state = export_solver_state(get_default_solver_state()),
        rng = if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
          get(".Random.seed", envir = globalenv(), inherits = FALSE)
        } else {
          NULL
        },
        warnings = warn_msgs
      )
    }

    children <- parallel::mclapply(seq_len(K), l5_child,
                                   mc.cores = n_forks, mc.preschedule = FALSE)

    for (k in seq_len(K)) {
      ch <- children[[k]]
      if (!is.list(ch)) {
        wage_fallback_log(sprintf(
          "L5 start %d/%d fork failed; re-running in-process.", k, K), config)
        res <- run_start(k)
      } else {
        for (msg in ch$warnings) warning(msg, call. = FALSE)
        res <- ch$res
      }
      if (is.null(res)) next
      val <- if (is.numeric(res$objective)) res$objective else
             if (is.numeric(res$value))     res$value     else Inf
      if (is.finite(val) && val < best_val) {
        best_val <- val
        best <- res
      }
    }

    ## Restore the parent-process side effects a serial loop would have left:
    ## the global solver cache and RNG state of start K (the price stage
    ## consumes the cache as warm starts via get_gammas()). If start K's fork
    ## failed, its in-process re-run above already left both in place.
    last_child <- children[[K]]
    if (is.list(last_child)) {
      if (solver_flag(config, "use_solver_warm_starts", TRUE)) {
        state <- get_default_solver_state(reset = TRUE)
        import_solver_state_entries(state, last_child$state)
        state$last_moment_norm <- last_child$state$last_moment_norm
        state$n_objective_calls <- last_child$state$n_objective_calls
      }
      if (!is.null(last_child$rng)) {
        assign(".Random.seed", last_child$rng, envir = globalenv())
      }
    }
  }

  if (is.null(best)) {
    stop("L5 joint multistart: all ", K, " starts failed.")
  }
  best$l5_starts_run <- K
  best$l5_best_objective <- best_val
  best
}
