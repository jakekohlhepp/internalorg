## Constrained demand-IV utilities for the workers-as-rows monotonicity
## restriction on the per-county skill matrix B[task, worker]. Replaces the
## closed-form rank_aware_2sls solve with a quadratic program when monotonicity
## is imposed; falls back to the unconstrained estimate when no row of the
## constraint set binds.
##
## Numerical strategy: rescale columns of X to unit L2 norm before forming the
## Hessian H = X' P_Z X (the demand-IV crossproduct is documented in
## utils/estimation_pipeline.R as ill-conditioned, rcond ~ 5e-11, due to
## scale mismatch between county-quarter FE crossproducts and avg_labor:B_raw
## crossproducts). A small ridge sqrt(eps) * max(diag(H)) is added so
## quadprog::solve.QP accepts a positive-definite Dmat.

if (!requireNamespace("quadprog", quietly = TRUE)) {
  stop("Package 'quadprog' is required for the constrained demand IV step.")
}

enumerate_permutations <- function(x) {
  x <- as.integer(x)
  if (length(x) == 1L) return(matrix(x, 1L, 1L))
  out <- NULL
  for (i in seq_along(x)) {
    sub <- enumerate_permutations(x[-i])
    out <- rbind(out, cbind(x[i], sub, deparse.level = 0))
  }
  out
}

solve_constrained_2sls <- function(X, Z, y, weights = NULL,
                                   A_ineq = NULL, b_ineq = NULL,
                                   tolerance = sqrt(.Machine$double.eps),
                                   context = "constrained 2SLS") {
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  y <- as.matrix(y)
  if (ncol(X) == 0L) stop(context, ": X has zero columns.")

  if (!is.null(weights)) {
    w <- as.numeric(weights)
    if (any(!is.finite(w)) || any(w < 0)) {
      stop(context, ": weights must be non-negative finite numbers.")
    }
    w_sqrt <- sqrt(w)
    X <- X * w_sqrt
    Z <- Z * w_sqrt
    y <- y * w_sqrt
  }

  beta_unc <- rank_aware_2sls(X, Z, y, tolerance = tolerance,
                              context = paste(context, "unconstrained"))

  ZZ <- crossprod(Z)
  obj_at <- function(beta) {
    resid <- y - X %*% beta
    Zr <- crossprod(Z, resid)
    proj_coef <- rank_aware_solve(ZZ, Zr, tolerance = tolerance,
                                  context = paste(context, "objective projection"),
                                  warn = FALSE)
    as.numeric(crossprod(resid, Z %*% proj_coef))
  }
  obj_unc <- obj_at(beta_unc)

  if (is.null(A_ineq) || NROW(A_ineq) == 0L) {
    return(list(beta = beta_unc, objective = obj_unc,
                status = "unconstrained", ridge = 0))
  }

  A_ineq <- as.matrix(A_ineq)
  b_ineq <- if (is.null(b_ineq)) rep(0, nrow(A_ineq)) else as.numeric(b_ineq)
  if (length(b_ineq) != nrow(A_ineq)) {
    stop(context, ": b_ineq length (", length(b_ineq),
         ") does not match A_ineq nrow (", nrow(A_ineq), ").")
  }

  cons_slack <- as.numeric(A_ineq %*% beta_unc) - b_ineq
  cons_tol <- max(tolerance, tolerance * max(abs(beta_unc)))
  if (all(cons_slack >= -cons_tol)) {
    return(list(beta = beta_unc, objective = obj_unc,
                status = "constraints_slack", ridge = 0))
  }

  col_norms <- sqrt(colSums(X^2))
  col_norms[col_norms == 0] <- 1
  X_s <- sweep(X, 2, col_norms, "/")
  A_s <- sweep(A_ineq, 2, col_norms, "/")

  ZX_s <- crossprod(Z, X_s)
  Zy <- crossprod(Z, y)
  beta_first_X <- rank_aware_solve(ZZ, ZX_s, tolerance = tolerance,
                                   context = paste(context, "first stage X"),
                                   warn = FALSE)
  X_hat_s <- Z %*% beta_first_X
  H <- crossprod(X_s, X_hat_s)
  H <- (H + t(H)) / 2

  beta_first_y <- rank_aware_solve(ZZ, Zy, tolerance = tolerance,
                                   context = paste(context, "first stage y"),
                                   warn = FALSE)
  y_hat <- Z %*% beta_first_y
  d_lin <- as.numeric(crossprod(X_s, y_hat))

  p <- ncol(X_s)
  diag_H <- diag(H)
  base_ridge <- sqrt(.Machine$double.eps) * max(abs(diag_H))
  if (!is.finite(base_ridge) || base_ridge <= 0) base_ridge <- sqrt(.Machine$double.eps)

  ridge <- base_ridge
  qp <- NULL
  ridge_used <- ridge
  for (attempt in 1:6) {
    Dmat <- 2 * (H + ridge * diag(p))
    qp <- tryCatch(
      quadprog::solve.QP(Dmat = Dmat, dvec = 2 * d_lin,
                         Amat = t(A_s), bvec = b_ineq, meq = 0),
      error = function(e) e
    )
    if (!inherits(qp, "error")) {
      ridge_used <- ridge
      break
    }
    ridge <- ridge * 1e3
  }
  if (inherits(qp, "error")) {
    stop(context, ": quadprog::solve.QP failed even after ridge escalation: ",
         conditionMessage(qp))
  }

  beta_s_sol <- qp$solution
  beta_full <- beta_s_sol / col_norms
  beta_full <- matrix(beta_full, ncol = 1)
  rownames(beta_full) <- colnames(X)

  list(beta = beta_full, objective = obj_at(beta_full),
       status = "qp_solved", ridge = ridge_used)
}

build_workers_monotone_constraints <- function(beta_names, n_t, n_w, perm) {
  beta_names <- as.character(beta_names)
  perm <- as.integer(perm)
  if (length(perm) != n_w || !setequal(perm, seq_len(n_w))) {
    stop("perm must be a permutation of 1:", n_w)
  }

  pos_by_tw <- matrix(NA_integer_, nrow = n_t, ncol = n_w)
  for (t in seq_len(n_t)) {
    for (w in seq_len(n_w)) {
      pat <- paste0("avg_labor:B_raw_", t, "_", w, "$")
      idx <- grep(pat, beta_names)
      if (length(idx) != 1L) {
        stop("Could not uniquely match B_raw_", t, "_", w,
             " in beta_names (got ", length(idx), " hits).")
      }
      pos_by_tw[t, w] <- idx
    }
  }

  n_constraints <- (n_w - 1L) * n_t
  A <- matrix(0, nrow = n_constraints, ncol = length(beta_names))
  r <- 0L
  for (t in seq_len(n_t)) {
    for (i in seq_len(n_w - 1L)) {
      r <- r + 1L
      A[r, pos_by_tw[t, perm[i + 1L]]] <-  1
      A[r, pos_by_tw[t, perm[i]]]     <- -1
    }
  }
  A
}

search_workers_perm_per_county <- function(X_c, Z_c, y_c, beta_names_c,
                                           n_t, n_w, weights = NULL, perms = NULL,
                                           tolerance = sqrt(.Machine$double.eps),
                                           context = "per-county workers monotone") {
  if (is.null(perms)) perms <- enumerate_permutations(seq_len(n_w))
  n_perms <- nrow(perms)
  obj_vec <- rep(NA_real_, n_perms)
  best_idx <- NA_integer_
  best <- list(objective = Inf, beta = NULL, perm = NULL,
               status = NA_character_, ridge = NA_real_)

  for (k in seq_len(n_perms)) {
    A_k <- build_workers_monotone_constraints(beta_names_c, n_t, n_w, perms[k, ])
    fit <- solve_constrained_2sls(X_c, Z_c, y_c, weights = weights,
                                  A_ineq = A_k, b_ineq = rep(0, nrow(A_k)),
                                  tolerance = tolerance,
                                  context = paste(context, "perm", k))
    obj_vec[k] <- fit$objective
    if (fit$objective < best$objective) {
      best <- list(objective = fit$objective, beta = fit$beta,
                   perm = perms[k, ], status = fit$status, ridge = fit$ridge)
      best_idx <- k
    }
  }

  list(beta = best$beta, perm = best$perm, objective = best$objective,
       obj_by_perm = obj_vec, perms = perms, best_idx = best_idx,
       status = best$status, ridge = best$ridge)
}

search_workers_perm <- function(X, Z, y, beta_names, counties, county_vec,
                                n_t, n_w, weights = NULL,
                                tolerance = sqrt(.Machine$double.eps),
                                context = "workers monotone search") {
  beta_names <- as.character(beta_names)
  county_vec <- as.character(county_vec)
  if (length(county_vec) != nrow(X)) {
    stop("county_vec length (", length(county_vec),
         ") does not match nrow(X) (", nrow(X), ").")
  }

  beta_full <- rep(NA_real_, length(beta_names))
  names(beta_full) <- beta_names
  perms_by_county <- list()
  obj_by_county <- list()
  obj_by_perm_by_county <- list()
  status_by_county <- list()
  ridge_by_county <- list()
  best_idx_by_county <- list()

  for (cnty in counties) {
    cnty_str <- as.character(cnty)
    pat <- paste0("^factor\\(county\\)", cnty_str, ":")
    cols_c <- grep(pat, beta_names)
    if (length(cols_c) == 0L) {
      stop("No columns of beta_names match county ", cnty_str)
    }
    z_cols_c <- grep(pat, colnames(Z))
    if (length(z_cols_c) == 0L) {
      stop("No columns of Z match county ", cnty_str)
    }
    rows_c <- which(county_vec == cnty_str)
    if (length(rows_c) == 0L) {
      stop("No rows in county_vec match county ", cnty_str)
    }
    X_c <- X[rows_c, cols_c, drop = FALSE]
    Z_c <- Z[rows_c, z_cols_c, drop = FALSE]
    y_c <- if (is.matrix(y)) y[rows_c, , drop = FALSE] else as.matrix(y[rows_c], ncol = 1)
    w_c <- if (is.null(weights)) NULL else as.numeric(weights)[rows_c]

    res <- search_workers_perm_per_county(
      X_c, Z_c, y_c, beta_names[cols_c],
      n_t = n_t, n_w = n_w, weights = w_c,
      tolerance = tolerance,
      context = paste(context, cnty_str)
    )
    beta_full[cols_c] <- as.numeric(res$beta)
    perms_by_county[[cnty_str]] <- res$perm
    obj_by_county[[cnty_str]] <- res$objective
    obj_by_perm_by_county[[cnty_str]] <- res$obj_by_perm
    status_by_county[[cnty_str]] <- res$status
    ridge_by_county[[cnty_str]] <- res$ridge
    best_idx_by_county[[cnty_str]] <- res$best_idx
  }

  if (any(is.na(beta_full))) {
    stop("Constrained beta has NA entries; some columns of beta_names had no county match.")
  }

  beta_mat <- matrix(beta_full, ncol = 1, dimnames = list(beta_names, NULL))

  list(beta = beta_mat,
       perms_by_county = perms_by_county,
       obj_by_county = obj_by_county,
       obj_by_perm_by_county = obj_by_perm_by_county,
       status_by_county = status_by_county,
       ridge_by_county = ridge_by_county,
       best_idx_by_county = best_idx_by_county,
       perms = enumerate_permutations(seq_len(n_w)))
}
