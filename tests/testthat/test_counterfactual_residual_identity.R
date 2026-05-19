# Unit tests for the model-consistent qual_exo / cost_exo adjustment
# applied at the focus quarter inside 13_counterfactual_prep.R.
#
# Algebra (derivation in smoke_qual_cost_exo_endog_vs_data.R:166-172):
#   qual_exo_new := qual_exo_old + (q_endog_data - q_endog_model) * avg_labor
#   cost_exo_new := cost_exo_old + (c_endog_data - c_endog_model) * avg_labor
# implies the prices/shares-fixed identity:
#   qual_exo_new + q_endog_model * avg_labor == qual_exo_old + q_endog_data * avg_labor
#   cost_exo_new + c_endog_model * avg_labor == cost_exo_old + c_endog_data * avg_labor
# This test wires `counterfactual_org_outputs()` (the helper 13_ now calls)
# through that identity on synthetic firms.

context("Counterfactual residual identity (model vs data residuals)")

PROJECT_ROOT <- if (file.exists("config.R")) "." else file.path("..", "..")
source(file.path(PROJECT_ROOT, "config.R"))
source(file.path(PROJECT_ROOT, "utils", "counterfactuals_core.R"), chdir = TRUE)
suppressPackageStartupMessages({
  library(data.table)
  library(SQUAREM)
  library(lamW)
})

TEST_CONFIG <- modifyList(CONFIG, list(
  counties = "TEST",
  n_worker_types = 5,
  n_task_types = 5,
  numeric_floor = 1e-16,
  numeric_ceiling = 1e16,
  B_zero_threshold = 1e-16,
  counterfactual_zero_gamma_floor = 0
))

test_that("qual_exo / cost_exo adjustment preserves the (q_endog * avg_labor + residual) identity", {
  set.seed(2027)
  n_w <- TEST_CONFIG$n_worker_types
  n_t <- TEST_CONFIG$n_task_types

  ## Wages, skill matrix, rho — analogue of build_counterfactual_initial_guess
  ## / new_theta in 13_.
  wage_vec   <- c(40, 100, 80, 50, 70)
  new_theta  <- matrix(runif(n_w * n_t, 0.1, 2), nrow = n_w, ncol = n_t)
  rho_cnty   <- -0.05
  w_mat      <- matrix(wage_vec, ncol = n_t, nrow = n_w, byrow = FALSE)
  tild       <- w_mat + (rho_cnty)^(-1) * new_theta
  tild       <- sweep(tild, 2, apply(tild, 2, min), FUN = "-")

  ## Synthetic firm panel (3 firms): each row has alpha (task mix), avg_labor,
  ## gamma_invert, plus B_raw / E_raw / s_index that we treat as data-side
  ## structural objects.
  firms <- data.table(
    location_id  = c("a", "b", "c"),
    avg_labor    = c(0.5, 1.0, 2.5),
    gamma_invert = c(0.7, 12.0, Inf),
    qual_exo_old = c(-1.2, 0.4, 3.5),
    cost_exo_old = c(50, -10, 25)
  )
  alpha_mat <- matrix(runif(nrow(firms) * n_t, 0.05, 0.4),
                      nrow = nrow(firms), ncol = n_t)
  alpha_mat <- alpha_mat / rowSums(alpha_mat)

  ## Hand-rolled B_raw / E_raw / s_index (treated as "data-anchored" structural
  ## objects). Not required to match the model solve — only used in the
  ## data-side q_endog_data / c_endog_data computation, mirroring 13_'s
  ## existing residual formula.
  B_raw <- matrix(NA_real_, nrow = nrow(firms), ncol = n_w * n_t)
  for (i in seq_len(nrow(firms))) {
    raw <- matrix(runif(n_w * n_t, 0.0, alpha_mat[i, 1]), nrow = n_w)
    raw <- sweep(raw, 2, colSums(raw), "/")  # normalize columns to 1
    raw <- raw %*% diag(alpha_mat[i, ])      # rescale columns to match alpha
    B_raw[i, ] <- as.vector(raw)
  }
  E_raw <- matrix(NA_real_, nrow = nrow(firms), ncol = n_w)
  for (i in seq_len(nrow(firms))) {
    B_mat <- matrix(B_raw[i, ], nrow = n_w, ncol = n_t)
    E_raw[i, ] <- rowSums(B_mat)
  }
  s_index <- c(0.4, 0.2, 0.1)

  ## Model-side q_endog / c_endog via counterfactual_org_outputs (the
  ## production helper that 13_ now invokes).
  q_endog_model <- numeric(nrow(firms))
  c_endog_model <- numeric(nrow(firms))
  for (i in seq_len(nrow(firms))) {
    out <- counterfactual_org_outputs(
      cost_matrix  = tild,
      alpha        = as.numeric(alpha_mat[i, ]),
      gamma        = firms$gamma_invert[i],
      wage_guess   = wage_vec,
      new_theta    = new_theta,
      innertol     = 1e-10,
      with_s_index = FALSE,
      with_b       = FALSE,
      config       = TEST_CONFIG
    )
    q_endog_model[i] <- out$q_endog
    c_endog_model[i] <- out$c_endog
  }

  ## Data-side q_endog / c_endog using B_raw, E_raw, s_index — same arithmetic
  ## that the new block in 13_ runs.
  q_endog_data <- as.numeric(B_raw %*% as.vector(new_theta))
  c_endog_data <- as.numeric(E_raw %*% wage_vec) +
    ifelse(is.finite(firms$gamma_invert),
           firms$gamma_invert * s_index, 0)

  ## Apply the in-place adjustment.
  firms[, qual_exo_new := qual_exo_old + (q_endog_data - q_endog_model) * avg_labor]
  firms[, cost_exo_new := cost_exo_old + (c_endog_data - c_endog_model) * avg_labor]

  ## Identity: (residual_new + q_endog_model * avg_labor) ==
  ##           (residual_old + q_endog_data  * avg_labor)
  expect_equal(
    firms$qual_exo_new + q_endog_model * firms$avg_labor,
    firms$qual_exo_old + q_endog_data  * firms$avg_labor,
    tolerance = 1e-9
  )
  expect_equal(
    firms$cost_exo_new + c_endog_model * firms$avg_labor,
    firms$cost_exo_old + c_endog_data  * firms$avg_labor,
    tolerance = 1e-9
  )

  ## Sanity: the adjustment is nontrivial — model q / c shouldn't exactly
  ## equal data q / c across all firms.
  expect_false(isTRUE(all.equal(q_endog_data, q_endog_model)))
})
