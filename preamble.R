library("lubridate")
library("stringr")
library("zoo")
library("lessR")
library("stats")
library("parallel")
library("xtable")
library("gmm")
library("readxl")
library("nloptr")
library("SQUAREM")
library("BB")

source("config.R")

#' Build cost matrix (tild_theta) for each county
#'
#' Constructs the effective cost matrix that combines wage premiums and skill
#' requirements. The cost matrix is normalized so the minimum in each column is 0.
#'
#' @param theta named numeric vector of wage-premium parameters
#' @param beta matrix of demand-side coefficients (rows named by parameter)
#' @param config Configuration list
#' @return A named list of cost matrices (worker x task) keyed by county
build_cost_matrix <- function(theta, beta, config = CONFIG) {
  n_counties <- length(config$counties)
  n_w <- config$n_worker_types
  n_t <- config$n_task_types

  tild_theta <- vector(mode = "list", length = n_counties)
  names(tild_theta) <- config$counties

  for (cnty in config$counties) {
    w_pattern <- paste0(cnty, ":avg_labor:E")
    w_params <- theta[grep(w_pattern, names(theta))]
    w_mat <- matrix(c(0, w_params), ncol = n_t, nrow = n_w, byrow = FALSE)

    skill_pattern <- paste0(cnty, ":avg_labor:B")
    skill_params <- beta[grep(skill_pattern, rownames(beta))]
    skills <- matrix(skill_params, ncol = n_t, nrow = n_w, byrow = FALSE)

    rho_pattern <- paste0(cnty, ":cust_price$")
    rho <- beta[grep(rho_pattern, rownames(beta))]

    tild_theta[[cnty]] <- w_mat + (1 / rho) * skills
    tild_theta[[cnty]] <- sweep(
      tild_theta[[cnty]],
      2,
      apply(tild_theta[[cnty]], 2, min),
      "-"
    )
  }

  tild_theta
}

#' Compute corner solution (maximum specialization)
#'
#' Finds the allocation where tasks are assigned entirely to the lowest-cost
#' worker type, ignoring entropy regularization.
#'
#' @param cost_matrix matrix of effective costs (worker x task)
#' @param alpha vector of task shares
#' @param config Configuration list
#' @return A list containing E (worker shares), B (allocation matrix), and entropy_bound
compute_corner_solution <- function(cost_matrix, alpha, config = CONFIG) {
  n_w <- nrow(cost_matrix)
  n_t <- ncol(cost_matrix)
  B <- matrix(0, nrow = n_w, ncol = n_t)

  for (j in seq_len(n_t)) {
    best_worker <- which.min(cost_matrix[, j])
    B[best_worker, j] <- alpha[j]
  }

  E <- rowSums(B)
  E_safe <- pmax(E, config$numeric_floor)
  alpha_safe <- pmax(alpha, config$numeric_floor)
  B_rel <- B / E_safe
  B_rel <- t(t(B_rel) / alpha_safe)

  list(E = E, B = B, entropy_bound = sum(B * spec_log(B_rel)))
}

#' Safe logarithm with log(0) = 0 convention
#'
#' @param x numeric vector or matrix
#' @return numeric vector or matrix of same dimensions as x
spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

## One production solver source of truth. This file owns solve_equilibrium(),
## bisection(), find_gamma_for_sindex(), objective_gmm(), eval_moments(), and
## get_gammas().
source(project_path("utils", "structural_solver.R"))
source(project_path("utils", "estimation_pipeline.R"))
source(project_path("utils", "constrained_demand_iv.R"))

####### shared setup for estimation equations
## 04_estimation_sample.R owns the data assembly. 05_iv_spec_comparison.R and
## 06_estimation.R pass those saved objects into this helper so sourcing
## preamble.R no longer mutates ambient session state. The actual linear solves
## use the rank-aware implementation in utils/estimation_pipeline.R.

#' Build shared estimation setup for linear components
#'
#' Prepares the model matrices and solves the demand-side and price-adjustment
#' linear parameters using rank-aware SVD solvers.
#'
#' @param working_data data.table containing the full sample
#' @param estim_matrix data.table containing the estimation-ready sample
#' @param config Configuration list
#' @return A list containing estimated coefficients (beta, beta_2) and model matrices
build_estimation_setup <- function(working_data, estim_matrix, config = CONFIG) {
  build_estimation_setup_rank_aware(working_data, estim_matrix, config = config)
}
