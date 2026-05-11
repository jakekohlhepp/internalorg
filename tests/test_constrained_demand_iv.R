## =============================================================================
## Unit tests for utils/constrained_demand_iv.R
## =============================================================================
## Verifies:
##   1. solve_constrained_2sls() with no inequality constraints reproduces
##      rank_aware_2sls() to machine precision (the QP machinery does not
##      bias the unconstrained solution).
##   2. On synthetic data whose true coefficient block is monotone under a
##      known worker-type permutation, search_workers_perm_per_county()
##      recovers the permutation and the coefficients.
##   3. Label-permutation invariance: relabeling worker-type indices in the
##      input data produces the corresponding label-permuted recovered
##      ordering, and the per-permutation QP objective values match exactly
##      under the relabeling. This is the explicit guarantee that the
##      estimator does NOT pre-specify which worker type is most productive.
##   4. End-to-end hook: build_estimation_setup_rank_aware() with
##      config$skill_monotone_orientation = "workers_rows" produces a beta
##      whose per-county skill matrix is monotone under the reported perm.
## =============================================================================

suppressPackageStartupMessages({
  library("data.table")
  library("stringr")
})

set.seed(42L)

source("config.R")
source(file.path("utils", "estimation_pipeline.R"))
source(file.path("utils", "constrained_demand_iv.R"))

n_t <- CONFIG$n_task_types
n_w <- CONFIG$n_worker_types
ALL_PERMS <- enumerate_permutations(seq_len(n_w))

cat("\n========== TEST 1: unconstrained QP path equals rank_aware_2sls ==========\n")

estimation_sample <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data <- as.data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix

config0 <- CONFIG
config0$skill_monotone_orientation <- "none"
setup0 <- build_estimation_setup_rank_aware(working_data, estim_matrix, config = config0)
beta_unc <- setup0$beta
mm_1 <- setup0$mm_1
z_mm_1 <- setup0$z_mm_1
y_vec <- estim_matrix[, "log_rel_mkt"]

fit_unc <- solve_constrained_2sls(mm_1, z_mm_1, y_vec, A_ineq = NULL)
delta1 <- max(abs(fit_unc$beta - beta_unc))
cat(sprintf("max |solve_constrained_2sls(no constraints) - rank_aware_2sls| = %.3e\n", delta1))
stopifnot(delta1 < 1e-10)
cat("PASS\n")

make_synthetic <- function(seed = 42L, n_obs = 600L, sigma = 1e-5) {
  set.seed(seed)
  syn_names <- character(0)
  for (t in seq_len(n_t)) for (w in seq_len(n_w)) {
    syn_names <- c(syn_names,
                   paste0("factor(county)17031:avg_labor:B_raw_", t, "_", w))
  }
  beta_true <- numeric(length(syn_names))
  names(beta_true) <- syn_names

  true_perm <- c(3L, 1L, 4L, 2L, 5L)
  for (t in seq_len(n_t)) {
    sorted_vals <- sort(runif(n_w, -1, 1))
    for (i in seq_len(n_w)) {
      w <- true_perm[i]
      beta_true[paste0("factor(county)17031:avg_labor:B_raw_", t, "_", w)] <-
        sorted_vals[i]
    }
  }

  X <- matrix(rnorm(n_obs * length(syn_names)), n_obs, length(syn_names))
  colnames(X) <- syn_names
  Z <- X
  y <- as.numeric(X %*% beta_true + rnorm(n_obs, sd = sigma))
  list(X = X, Z = Z, y = y, beta_true = beta_true,
       names = syn_names, true_perm = true_perm)
}

cat("\n========== TEST 2: synthetic monotone recovery ==========\n")
syn <- make_synthetic(seed = 42L, sigma = 1e-5)
res <- search_workers_perm_per_county(syn$X, syn$Z, syn$y,
                                       beta_names_c = syn$names,
                                       n_t = n_t, n_w = n_w)
cat(sprintf("recovered perm   : %s\n", paste(res$perm, collapse = ", ")))
cat(sprintf("expected perm    : %s\n", paste(syn$true_perm, collapse = ", ")))
delta_beta <- max(abs(as.numeric(res$beta) - syn$beta_true))
cat(sprintf("max |beta - beta_true| = %.3e\n", delta_beta))
stopifnot(identical(as.integer(res$perm), as.integer(syn$true_perm)))
stopifnot(delta_beta < 1e-2)
cat("PASS\n")

cat("\n========== TEST 3: label-permutation invariance ==========\n")
label_perm <- c(5L, 4L, 3L, 2L, 1L)
relab_names <- syn$names
for (i in seq_along(syn$names)) {
  m <- str_match(syn$names[i], "^(.*B_raw_[0-9]+_)([0-9]+)$")
  if (!is.na(m[1, 1])) {
    new_w <- label_perm[as.integer(m[1, 3])]
    relab_names[i] <- paste0(m[1, 2], new_w)
  }
}
X_relab <- syn$X
Z_relab <- syn$Z
colnames(X_relab) <- relab_names
colnames(Z_relab) <- relab_names

res2 <- search_workers_perm_per_county(X_relab, Z_relab, syn$y,
                                        beta_names_c = relab_names,
                                        n_t = n_t, n_w = n_w)
expected_pi2 <- label_perm[syn$true_perm]
cat(sprintf("recovered perm (relab) : %s\n", paste(res2$perm, collapse = ", ")))
cat(sprintf("expected perm  (relab) : %s\n", paste(expected_pi2, collapse = ", ")))
stopifnot(identical(as.integer(res2$perm), as.integer(expected_pi2)))

relab_perms <- t(apply(ALL_PERMS, 1, function(pi) label_perm[pi]))
match_idx <- vapply(seq_len(nrow(ALL_PERMS)), function(k) {
  for (j in seq_len(nrow(ALL_PERMS))) {
    if (all(ALL_PERMS[j, ] == relab_perms[k, ])) return(j)
  }
  NA_integer_
}, integer(1))
stopifnot(!any(is.na(match_idx)))

obj_diff <- max(abs(res$obj_by_perm - res2$obj_by_perm[match_idx]))
cat(sprintf("max |obj1[pi] - obj2[label_perm(pi)]| = %.3e\n", obj_diff))
stopifnot(obj_diff < 1e-6)
cat("PASS\n")

cat("\n========== TEST 4: end-to-end hook in build_estimation_setup ==========\n")
config1 <- CONFIG
config1$skill_monotone_orientation <- "workers_rows"
setup1 <- build_estimation_setup_rank_aware(working_data, estim_matrix, config = config1)
mfit <- setup1$skill_monotone_fit
stopifnot(!is.null(mfit))

for (cnty in CONFIG$counties) {
  pi_c <- as.integer(mfit$perms_by_county[[as.character(cnty)]])
  pat <- paste0("^factor\\(county\\)", cnty, ":avg_labor:B_raw_")
  b_idx <- grep(pat, rownames(setup1$beta))
  b_names <- rownames(setup1$beta)[b_idx]
  B_mat <- matrix(NA_real_, n_t, n_w)
  for (nm in b_names) {
    m <- str_match(nm, "B_raw_([0-9]+)_([0-9]+)$")
    t_idx <- as.integer(m[1, 2])
    w_idx <- as.integer(m[1, 3])
    B_mat[t_idx, w_idx] <- setup1$beta[nm, 1]
  }
  ord_violation <- max(unlist(lapply(seq_len(n_t), function(t) {
    row_under_perm <- B_mat[t, pi_c]
    -min(diff(row_under_perm))
  })))
  cat(sprintf("county %s: chosen perm = %s; max non-monotone violation = %.3e\n",
              cnty, paste(pi_c, collapse = ", "), ord_violation))
  stopifnot(ord_violation < 1e-6)
}
cat("PASS\n")

cat("\nAll constrained-demand-IV tests passed.\n")
