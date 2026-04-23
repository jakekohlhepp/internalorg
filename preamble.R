library('lubridate')
library('stringr')
library('zoo')
library('lessR')
library('stats')
library('parallel')
library('xtable')
library('gmm')
library('readxl')
library('nloptr')
library('SQUAREM')
library('BB')

source('config.R')

#' =============================================================================
#' CORE MODEL FUNCTIONS
#' =============================================================================

#' Build cost matrix (tild_theta) for each county
#'
#' Constructs the effective cost matrix that combines wage premiums and skill
#' requirements. The cost matrix is normalized so the minimum in each column is 0.
#'
#' @param theta Named vector of wage premium parameters (coefficients on E_raw)
#' @param beta Named vector/matrix of demand-side estimates (from IV regression)
#' @param config Configuration list (default: CONFIG)
#' @return Named list of cost matrices, one per county
#'
#' @details
#' The cost matrix for worker type i and task type j is:
#'   tild_theta[i,j] = w[i,j] + skills[i,j] / rho
#' where:
#'   - w[i,j] is the wage premium for worker type i (0 for type 1, from theta for 2+)
#'   - skills[i,j] is the skill coefficient from beta
#'   - rho is the price sensitivity (negative, so 1/rho flips sign)
#'
#' The matrix is then column-normalized by subtracting the column minimum,
#' so min_i(tild_theta[i,j]) = 0 for all j.
#'
build_cost_matrix <- function(theta, beta, config = CONFIG) {

  n_counties <- length(config$counties)
  n_w <- config$n_worker_types
  n_t <- config$n_task_types

  tild_theta <- vector(mode = 'list', length = n_counties)
  names(tild_theta) <- config$counties

  for (cnty in config$counties) {

    # Extract wage premium parameters for this county
    # Pattern: "factor(county)XXXXX:avg_labor:E_raw_Y"
    # First worker type (E_raw_1) is normalized to 0
    w_pattern <- paste0(cnty, ":avg_labor:E")
    w_params <- theta[grep(w_pattern, names(theta))]

    # Build wage matrix: first column is zeros, rest from parameters
    # byrow=FALSE fills column by column, so c(0, w_params) becomes first column
    # then repeats for subsequent columns (wages don't vary by task)
    w_mat <- matrix(c(0, w_params), ncol = n_t, nrow = n_w, byrow = FALSE)

    # Extract skill parameters for this county
    # Pattern: "factor(county)XXXXX:avg_labor:B_raw_Y_Z"
    skill_pattern <- paste0(cnty, ":avg_labor:B")
    skill_params <- beta[grep(skill_pattern, rownames(beta))]
    skills <- matrix(skill_params, ncol = n_t, nrow = n_w, byrow = FALSE)

    # Extract price sensitivity (rho < 0 typically)
    rho_pattern <- paste0(cnty, ":cust_price$")
    rho <- beta[grep(rho_pattern, rownames(beta))]

    # Construct cost matrix: wage + skill/rho
    # Note: rho is typically negative, so 1/rho is negative,
    # meaning higher skill (positive) reduces effective cost
    tild_theta[[cnty]] <- w_mat + (1/rho) * skills

    # Normalize: subtract column minimum so min cost per task = 0
    col_mins <- apply(tild_theta[[cnty]], 2, min)
    tild_theta[[cnty]] <- sweep(tild_theta[[cnty]], 2, col_mins, "-")
  }

  return(tild_theta)
}

#' Solve for equilibrium worker allocation given gamma
#'
#' Uses Sinkhorn-like fixed-point iteration to find the equilibrium assignment
#' matrix B and worker type distribution E, given a regularization parameter gamma.
#'
#' @param cost_matrix Cost matrix (tild_theta) for a single county [n_workers x n_tasks]
#' @param alpha Task mix vector (shares of each task type) [n_tasks]
#' @param gamma Regularization parameter (entropy weight)
#' @param innertol Convergence tolerance for fixed-point iteration
#' @param config Configuration list (default: CONFIG)
#' @return List with components:
#'   - E: Worker type distribution [n_workers]
#'   - B: Assignment matrix [n_workers x n_tasks]
#'   - A: Preference matrix exp(-cost/gamma)
#'   - converged: Logical indicating convergence
#'   - entropy: Entropy of the assignment (sum of B * log(B/(E*alpha)))
#'
#' @details
#' The equilibrium is characterized by:
#'   B[i,j] = A[i,j] * alpha[j] / sum_k(A[k,j] * E[k]) * E[i]
#' where A[i,j] = exp(-cost[i,j] / gamma).
#'
#' The fixed-point iteration updates E until row sums of B equal E.
#'
#' Numerical safeguards:
#'   1. Log-space computation of A to prevent overflow
#'   2. Clamping of extreme values
#'   3. Protected division to prevent NaN
#'
solve_equilibrium <- function(cost_matrix, alpha, gamma, innertol, config = CONFIG) {

  n_w <- nrow(cost_matrix)
  n_t <- ncol(cost_matrix)

  # -------------------------------------------------------------------------
  # Step 1: Compute A matrix with numerical safeguards
  # -------------------------------------------------------------------------
  # A[i,j] = exp(-cost[i,j] / gamma)
  #
  # For numerical stability, we compute in log space first:
  #   log(A[i,j]) = -cost[i,j] / gamma
  #
  # Then shift by the maximum to prevent overflow when exponentiating:
  #   A = exp(log_A - max(log_A)) * exp(max(log_A))
  # But since we only use A in ratios, the scaling factor cancels out.

  log_A <- -cost_matrix / gamma

  # Shift to prevent overflow (the shift cancels in ratios)
  log_A_max <- max(log_A)
  log_A_shifted <- log_A - log_A_max
  A <- exp(log_A_shifted)

  # Clamp extreme values to prevent numerical issues
  A <- pmax(A, config$numeric_floor)
  A <- pmin(A, config$numeric_ceiling)

  # Pre-compute transpose for efficiency (avoid repeated t() calls)
  A_t <- t(A)

  # -------------------------------------------------------------------------
  # Step 2: Fixed-point iteration for E
  # -------------------------------------------------------------------------
  # Initialize E with uniform distribution
  E <- get_initial_E(config)

  # Ensure E has correct length if config was modified
  if (length(E) != n_w) {
    E <- rep(1/n_w, n_w)
  }

  # Define fixed-point function
  # The update is: E_new[i] = E[i] * sum_j( A[i,j] * alpha[j] / sum_k(A[k,j] * E[k]) )
  fxpt <- function(p) {
    # Column sums: sum_k(A[k,j] * p[k]) for each task j
    col_sums <- as.vector(A_t %*% p)  # More efficient than colSums(A * p)

    # Protect against division by zero
    col_sums <- pmax(col_sums, config$numeric_floor)

    # Compute scaling factors for each task
    task_scales <- alpha / col_sums  # [n_tasks]

    # Update: E_new[i] = E[i] * sum_j(A[i,j] * task_scales[j])
    update_factors <- as.vector(A %*% task_scales)  # [n_workers]

    return(p * update_factors)
  }

  # Run SQUAREM accelerated fixed-point iteration
  result <- squarem(E,
                    fixptfn = fxpt,
                    control = list(maxiter = config$fixedpoint_max_iter,
                                   tol = innertol))

  E <- result$par
  converged <- (result$convergence == 0)

  # Ensure E is positive (clamp before normalization)
  E <- pmax(E, config$numeric_floor)

  # Normalize E to ensure it sums to 1 (correct for any numerical drift)
  E <- E / sum(E)

  # -------------------------------------------------------------------------
  # Step 3: Compute assignment matrix B
  # -------------------------------------------------------------------------
  # B[i,j] = A[i,j] * alpha[j] / sum_k(A[k,j] * E[k]) * E[i]

  col_sums <- as.vector(A_t %*% E)
  col_sums <- pmax(col_sums, config$numeric_floor)

  # Compute B column by column
  B <- A * E  # First multiply rows by E
  B <- t(t(B) * alpha / col_sums)  # Then scale columns by alpha/col_sums

  # Zero out tiny values to maintain sparsity
  B[abs(B) < config$B_zero_threshold] <- 0

  # -------------------------------------------------------------------------
  # Step 4: Compute entropy for later use
  # -------------------------------------------------------------------------
  # Entropy = sum_ij B[i,j] * log(B[i,j] / (E[i] * alpha[j]))
  #         = sum_ij B[i,j] * log(B_rel[i,j])
  # where B_rel[i,j] = B[i,j] / (E[i] * alpha[j])

  # Protect against division by zero in B_rel calculation
  E_safe <- pmax(E, config$numeric_floor)
  alpha_safe <- pmax(alpha, config$numeric_floor)

  B_rel <- B / E_safe  # Divide rows by E
  B_rel <- t(t(B_rel) / alpha_safe)  # Divide columns by alpha

  # Use spec_log to handle log(0) = 0 convention
  entropy <- sum(B * spec_log(B_rel))

  return(list(
    E = E,
    B = B,
    A = A,
    converged = converged,
    entropy = entropy
  ))
}

#' Compute corner solution (maximum specialization)
#'
#' When gamma -> 0, workers fully specialize: each task is done entirely by
#' the worker type with lowest cost for that task. This gives the maximum
#' achievable entropy bound.
#'
#' @param cost_matrix Cost matrix for a single county [n_workers x n_tasks]
#' @param alpha Task mix vector [n_tasks]
#' @param config Configuration list
#' @return List with E (worker shares), B (assignment), and entropy bound
#'
compute_corner_solution <- function(cost_matrix, alpha, config = CONFIG) {

  n_w <- nrow(cost_matrix)
  n_t <- ncol(cost_matrix)

  # Initialize B as zeros
  B <- matrix(0, nrow = n_w, ncol = n_t)

  # For each task, assign all work to the lowest-cost worker
  for (j in 1:n_t) {
    best_worker <- which.min(cost_matrix[, j])
    B[best_worker, j] <- alpha[j]
  }

  # Worker shares are row sums of B
  E <- rowSums(B)

  # Handle case where some workers have zero share
  E_safe <- pmax(E, config$numeric_floor)
  alpha_safe <- pmax(alpha, config$numeric_floor)

  # Compute B_rel = B / (E * alpha)
  B_rel <- B / E_safe
  B_rel <- t(t(B_rel) / alpha_safe)

  # Entropy bound (this is the max entropy achievable = max specialization)
  entropy_bound <- sum(B * spec_log(B_rel))

  return(list(E = E, B = B, entropy_bound = entropy_bound))
}

#' Find gamma that produces a target entropy (s_index)
#'
#' Uses bisection to find the regularization parameter gamma such that
#' the equilibrium entropy matches the target s_index.
#'
#' @param cost_matrix Cost matrix for a single county
#' @param alpha Task mix vector
#' @param s_index Target entropy (specialization index)
#' @param innertol Tolerance for inner fixed-point iteration
#' @param outertol Tolerance for outer bisection
#' @param config Configuration list
#' @return List with:
#'   - gamma: Found regularization parameter (0 if corner solution)
#'   - E: Worker type distribution
#'   - B: Assignment matrix
#'   - is_corner: TRUE if corner solution was used
#'   - converged: TRUE if bisection converged
#'
#' @details
#' The relationship between gamma and entropy:
#'   - gamma -> 0: Maximum specialization (corner solution), entropy -> entropy_bound
#'   - gamma -> Inf: Maximum mixing (uniform assignment), entropy -> 0
#'
#' If s_index > entropy_bound, the corner solution is returned since the
#' requested specialization level is unachievable.
#'
find_gamma_for_sindex <- function(cost_matrix, alpha, s_index, innertol, outertol,
                                   config = CONFIG) {

  # -------------------------------------------------------------------------
  # Step 1: Check if corner solution is needed
  # -------------------------------------------------------------------------
  corner <- compute_corner_solution(cost_matrix, alpha, config)

  # Handle NaN in entropy_bound (can happen with degenerate inputs)
  if (is.na(corner$entropy_bound) || is.nan(corner$entropy_bound)) {
    corner$entropy_bound <- Inf
  }

  # If requested entropy is beyond the achievable bound, return corner solution
  if (s_index > corner$entropy_bound) {
    return(list(
      gamma = 0,
      E = corner$E,
      B = corner$B,
      is_corner = TRUE,
      converged = TRUE,
      entropy = corner$entropy_bound
    ))
  }

  # -------------------------------------------------------------------------
  # Step 2: Define objective function for bisection
  # -------------------------------------------------------------------------
  # We want to find gamma such that entropy(gamma) = s_index
  # Objective: f(gamma) = s_index - entropy(gamma)
  # f > 0 when entropy < s_index (need to increase gamma to increase mixing)
  # f < 0 when entropy > s_index (need to decrease gamma)

  objective <- function(gamma) {
    eq <- solve_equilibrium(cost_matrix, alpha, gamma, innertol, config)
    return(s_index - eq$entropy)
  }

  # -------------------------------------------------------------------------
  # Step 3: Run bisection
  # -------------------------------------------------------------------------
  result <- bisection(
    f = objective,
    a = config$bisection_lower,
    b = config$bisection_upper,
    n = config$bisection_max_iter,
    xtol = outertol,
    ftol = outertol,
    config = config
  )

  # Check convergence
  if (!result$conv) {
    warning("Bisection did not converge for s_index = ", s_index,
            ". Final gamma = ", result$root, ", residual = ", result$val)
  }

  # -------------------------------------------------------------------------
  # Step 4: Compute final equilibrium at found gamma
  # -------------------------------------------------------------------------
  gamma_final <- result$root
  eq_final <- solve_equilibrium(cost_matrix, alpha, gamma_final, innertol, config)

  return(list(
    gamma = gamma_final,
    E = eq_final$E,
    B = eq_final$B,
    is_corner = FALSE,
    converged = result$conv,
    entropy = eq_final$entropy
  ))
}

#' Safe logarithm with log(0) = 0 convention
#'
#' Returns log(x) for positive x, and 0 for x=0 or NaN.
#' This is useful for entropy calculations where 0*log(0) should equal 0.
#'
#' @param x Numeric vector
#' @return Numeric vector with log(x), or 0 where x is 0 or NaN
#'
spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

#' Bisection root-finding algorithm with automatic bracket adjustment
#'
#' Finds root of f(x) = 0 using bisection method. Automatically adjusts
#' the initial bracket [a, b] to ensure f(a) and f(b) have opposite signs.
#'
#' @param f Function to find root of
#' @param a Initial lower bound
#' @param b Initial upper bound
#' @param n Maximum number of iterations
#' @param xtol Tolerance for bracket width
#' @param ftol Tolerance for function value
#' @param config Configuration list (default: CONFIG)
#' @return List with:
#'   - root: Found root value
#'   - val: Function value at root
#'   - conv: TRUE if converged
#'
bisection <- function(f, a, b, n, xtol, ftol, config = CONFIG) {

  a_prime <- a
  b_prime <- b


  # When function value at low test point is positive, start higher
  if (f(config$bisection_low_test_point) > 0) {
    a_prime <- config$bisection_high_start_if_positive
  }

  # Search for non-NaN region if needed
  if (is.nan(f(a_prime))) {
    while (is.nan(f(a_prime))) {
      a_prime <- a_prime + config$bisection_nan_step
      stopifnot(b_prime > a_prime)
    }
  }

  # Adjust lower bound until f(a_prime) < 0
  while (f(a_prime) > 0) {
    a_prime <- a_prime - config$bisection_lower_step
    stopifnot(a_prime > 0)
  }

  # Adjust upper bound until f(b_prime) > 0
  while (f(b_prime) < 0) {
    b_prime <- b_prime + config$bisection_upper_step
  }


  stopifnot(b_prime>a_prime)


  for (i in 1:n) {
    c <- (a_prime + b_prime) / 2 # Calculate midpoint

    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the
    # function and return the root.
    if (abs(f(c))<ftol || ((b_prime - a_prime) / 2) < xtol) {
      return(list("root"=c, "val"=f(c),"conv"=abs(f(c))<ftol || ((b_prime - a_prime) / 2) < xtol  ))
    }

    # If another iteration is required,
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    ifelse(sign(f(c)) == sign(f(a_prime)),
           a_prime <- c,
           b_prime <- c)
  }
  # If the max number of iterations is reached and no root has been found,
  # return message and end function.
  return(list("root"=c, "val"=f(c),"conv"=abs(f(c))<ftol || ((b_prime - a_prime) / 2) < xtol  ))
}


####### shared setup for estimation equations
## 04_estimation_sample.R owns the data assembly. 05_estimation.R and
## 06_iv_spec_comparison.R pass those saved objects into this helper so
## sourcing preamble.R no longer mutates ambient session state.
build_estimation_setup <- function(working_data, estim_matrix, config = CONFIG) {
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

  ## the instrument matrix swaps out the endogenous price terms for the
  ## county-specific cost shifters built in 04_estimation_sample.R.
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
  e_match <- data.frame(as.matrix(estim_matrix[, c(e_raw_cols_no_first, "s_index")]), factor(estim_matrix[, "county"]))
  e_col_names <- paste0("E_", 2:config$n_worker_types)
  colnames(e_match) <- c(e_col_names, "s_index", "county")
  e_mat <- model.matrix(build_E_formula(2:config$n_worker_types, include_s_index = TRUE), data = e_match)

  ztz_inv <- solve(t(z_mm_1) %*% z_mm_1)
  proj_z <- z_mm_1 %*% ztz_inv %*% t(z_mm_1)

  tryCatch({
    beta <- solve(t(mm_1) %*% proj_z %*% mm_1) %*%
      (t(mm_1) %*% proj_z %*% as.matrix(estim_matrix[, "log_rel_mkt"]))
    p_adj <- estim_matrix[, "cust_price"]
    for (cnty in config$counties) {
      p_adj <- p_adj + estim_matrix[, "mk_piece"] *
        (1 / beta[grep(paste0(cnty, ":cust_price"), rownames(beta))]) *
        (estim_matrix$county == cnty)
    }
    beta_2 <- solve(t(z_mm_2) %*% z_mm_2) %*% t(z_mm_2) %*% as.matrix(p_adj)
  }, error = function(e) {
    stop("Analytic estimator failed: ", e$message,
         "\nCheck for collinearity in instruments or singular matrices.")
  })

  return(list(
    beta = beta,
    beta_2 = beta_2,
    mm_1 = mm_1,
    mm_2 = mm_2,
    z_mm_1 = z_mm_1,
    z_mm_2 = z_mm_2,
    e_mat = e_mat
  ))
}




#' =============================================================================
#' GMM OBJECTIVE FUNCTIONS (REFACTORED)
#' =============================================================================
#' These functions use the core model functions defined above.
#' =============================================================================

#' Compute worker allocation for a single observation
#'
#' @param cost_matrix Pre-computed cost matrix for the county
#' @param alpha Task mix vector
#' @param s_index Target entropy (specialization index)
#' @param innertol Inner fixed-point tolerance
#' @param outertol Outer bisection tolerance
#' @param config Configuration list
#' @return Numeric vector E[2:N] (worker type shares, excluding type 1)
#'
get_worker_allocation <- function(cost_matrix, alpha, s_index, innertol, outertol,
                                   config = CONFIG) {
  result <- find_gamma_for_sindex(cost_matrix, alpha, s_index, innertol, outertol, config)
  return(result$E[2:config$n_worker_types])
}

#' GMM objective function for wage parameter estimation
#'
#' Computes moment conditions for GMM estimation by comparing model-predicted
#' worker type shares with observed shares.
#'
#' @param theta Numeric vector of wage parameters to estimate
#' @param x Data frame containing estimation sample
#' @param beta First-stage IV estimates matrix
#' @param beta_2_subset Named vector of parameter names
#' @param config Configuration list
#' @param clust Parallel cluster object (NULL if not using Windows parallel)
#' @return Matrix of moment conditions (one row per observation, columns by county-worker type)
#'
objective_gmm <- function(theta, x, beta, beta_2_subset, config = CONFIG, clust = NULL) {
  data <- x
  pre_parms <- theta
  names(pre_parms) <- names(beta_2_subset)

  # Build cost matrices for all counties using refactored function
  tild_theta <- build_cost_matrix(pre_parms, beta, config)

  task_mix_cols <- get_task_mix_cols(config)

  # Helper function: compute worker allocation for one observation
  get_demands <- function(idx) {
    alpha <- as.numeric(data[idx, task_mix_cols])
    get_worker_allocation(tild_theta[[data$county[idx]]], alpha, data$s_index[idx],
                          config$innertol, config$outertol, config)
  }

  # Run computation (parallel or serial)
  if (config$pl_on == TRUE) {
    if (get_os() == "windows") {
      temp_res <- data.table(do.call(rbind, parLapply(clust, 1:nrow(data), get_demands)))
    } else {
      temp_res <- data.table(do.call(rbind, mclapply(1:nrow(data), get_demands, mc.cores = get_core_count(config))))
    }
  } else {
    temp_res <- data.table(do.call(rbind, lapply(1:nrow(data), get_demands)))
  }

  # Build moment matrix: (model E - observed E) by county
  # Exclude E_1 to avoid collinearity
  E_raw_cols_no_first <- paste0("E_raw_", 2:config$n_worker_types)
  E_match <- data.frame(
    (as.matrix(temp_res) - as.matrix(data[, E_raw_cols_no_first])),
    factor(data[, "county"])
  )
  E_col_names <- paste0("E_", 2:config$n_worker_types)
  colnames(E_match) <- c(E_col_names, "county")
  E_mat <- model.matrix(build_E_formula(2:config$n_worker_types, include_s_index = FALSE), data = E_match)

  return(E_mat)
}


#' Evaluate moments: compare model-predicted E with observed E
#'
#' @param theta Numeric vector of wage parameters
#' @param x Data frame containing estimation sample
#' @param beta First-stage IV estimates matrix
#' @param beta_2_subset Named vector of parameter names
#' @param config Configuration list
#' @param clust Parallel cluster object (NULL if not using Windows parallel)
#' @return Matrix with two columns: colMeans(E_model), colMeans(E_raw)
#'
eval_moments <- function(theta, x, beta, beta_2_subset, config = CONFIG, clust = NULL) {
  data <- x
  pre_parms <- theta
  names(pre_parms) <- names(beta_2_subset)

  # Build cost matrices using refactored function
  tild_theta <- build_cost_matrix(pre_parms, beta, config)

  task_mix_cols <- get_task_mix_cols(config)

  # Helper: compute full E vector for one observation
  get_demands <- function(idx) {
    alpha <- as.numeric(data[idx, task_mix_cols])
    result <- find_gamma_for_sindex(tild_theta[[data$county[idx]]], alpha, data$s_index[idx],
                                     config$innertol, config$outertol, config)
    return(result$E)  # Return full E (all worker types)
  }

  # Run computation (parallel or serial)
  if (config$pl_on == TRUE) {
    if (get_os() == "windows") {
      temp_res <- data.table(do.call(rbind, parLapply(clust, 1:nrow(data), get_demands)))
    } else {
      temp_res <- data.table(do.call(rbind, mclapply(1:nrow(data), get_demands, mc.cores = get_core_count(config))))
    }
  } else {
    temp_res <- data.table(do.call(rbind, lapply(1:nrow(data), get_demands)))
  }

  # Build observed E matrix
  E_raw_all_cols <- get_E_raw_cols(config)
  E_all_names <- paste0("E_", 1:config$n_worker_types)
  E_match <- data.frame(
    as.matrix(data[, E_raw_all_cols]),
    factor(data[, "county"])
  )
  colnames(E_match) <- c(E_all_names, "county")
  E_raw <- model.matrix(build_E_formula(1:config$n_worker_types, include_s_index = FALSE), data = E_match)

  # Build model E matrix
  E_match <- data.frame(as.matrix(temp_res), factor(data[, "county"]))
  colnames(E_match) <- c(E_all_names, "county")
  E_model <- model.matrix(build_E_formula(1:config$n_worker_types, include_s_index = FALSE), data = E_match)

  return(cbind(colMeans(E_model), colMeans(E_raw)))
}

#' Get gamma values for each observation
#'
#' Returns the organizational cost parameter gamma for each observation.
#' Returns 0 for corner solutions (maximum specialization).
#'
#' @param theta Numeric vector of wage parameters
#' @param x Data frame containing estimation sample
#' @param beta First-stage IV estimates matrix
#' @param beta_2_subset Named vector of parameter names
#' @param config Configuration list
#' @param clust Parallel cluster object (NULL if not using Windows parallel)
#' @return Numeric vector of gamma values (one per observation)
#'
get_gammas <- function(theta, x, beta, beta_2_subset, config = CONFIG, clust = NULL) {
  data <- x
  pre_parms <- theta
  names(pre_parms) <- names(beta_2_subset)

  # Build cost matrices using refactored function
  tild_theta <- build_cost_matrix(pre_parms, beta, config)

  task_mix_cols <- get_task_mix_cols(config)

  # Helper: compute gamma for one observation
  get_gamma_single <- function(idx) {
    alpha <- as.numeric(data[idx, task_mix_cols])
    result <- find_gamma_for_sindex(tild_theta[[data$county[idx]]], alpha, data$s_index[idx],
                                     config$innertol, config$outertol, config)
    return(result$gamma)  # Returns 0 for corner solutions
  }

  # Run computation (parallel or serial)
  if (config$pl_on == TRUE) {
    if (get_os() == "windows") {
      temp_res <- data.table(do.call(rbind, parLapply(clust, 1:nrow(data), get_gamma_single)))
    } else {
      temp_res <- data.table(do.call(rbind, mclapply(1:nrow(data), get_gamma_single, mc.cores = get_core_count(config))))
    }
  } else {
    temp_res <- data.table(do.call(rbind, lapply(1:nrow(data), get_gamma_single)))
  }

  return(temp_res$V1)
}
