## Robustness smoke: does widening the PSO search box past +/-2000 reveal a
## lower NYC wage basin? Sweeps halfwidth in {2000, 5000, 10000}, with the
## same prior best as a warm-start particle, NM polish from each PSO best.
##
## NOT incorporated in the main pipeline; read-only on the data.

library(data.table)
set.seed(4459665)
source("config.R")

cat("=== smoke_pso_widebox  NYC robustness test ===\n")

est <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data <- data.table(est$working_data)
estim_matrix <- est$estim_matrix
source("preamble.R")
setup <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta <- setup$beta
beta_2 <- setup$beta_2

beta_2_subset <- readRDS(file.path(CONFIG$prep_output_dir, "seeit_bb.rds"))
wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
names(beta_2_subset) <- rownames(beta_2)[wage_idx]

cnty <- "36061"
data <- as.data.frame(estim_matrix)
full_par <- beta_2_subset
county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
par_idx <- grepl(county_pattern, names(full_par), fixed = TRUE)
row_idx <- as.character(data$county) == cnty
x_county <- data[row_idx, , drop = FALSE]

fn <- function(parms) {
  candidate <- full_par
  candidate[par_idx] <- parms
  moments <- tryCatch(
    weighted_col_means(objective_gmm(
      theta = candidate, x = x_county, beta = beta,
      beta_2_subset = beta_2_subset, config = CONFIG,
      clust = NULL, solver_state = NULL
    ), weights = NULL),
    error = function(e) NULL
  )
  if (is.null(moments)) return(1e6)
  sub <- grepl(paste0("county", cnty, ":E_"), names(moments), fixed = TRUE)
  v <- as.numeric(moments[sub])
  if (anyNA(v) || any(!is.finite(v))) return(1e6)
  sum(v^2)
}

PSO_PRIOR_BEST <- c(-22.4, 498.2, 150.94, 1826.32)
d <- length(PSO_PRIOR_BEST)
cat("Prior best (warm-start particle):", PSO_PRIOR_BEST, "\n")
cat("Prior best ssq:", signif(fn(PSO_PRIOR_BEST), 6), "\n")

pso <- function(fn, lower, upper, seed_particle = NULL,
                n_particles = 40, n_iter = 100,
                w = 0.7, c1 = 1.5, c2 = 1.5, label = "") {
  d <- length(lower)
  X <- matrix(runif(n_particles * d, rep(lower, each = n_particles),
                    rep(upper, each = n_particles)), n_particles, d)
  if (!is.null(seed_particle)) X[1, ] <- seed_particle
  V <- matrix(runif(n_particles * d, -abs(upper - lower), abs(upper - lower)),
              n_particles, d) * 0.1
  fX <- apply(X, 1, fn)
  pbest <- X; fpbest <- fX
  gi <- which.min(fpbest); gbest <- X[gi, ]; fgbest <- fpbest[gi]
  for (t in seq_len(n_iter)) {
    r1 <- matrix(runif(n_particles * d), n_particles, d)
    r2 <- matrix(runif(n_particles * d), n_particles, d)
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
    if (t %% 10 == 0 || t == 1) {
      cat(sprintf("  PSO[hw=%s] iter %3d: gbest ssq=%.6g\n", label, t, fgbest))
    }
  }
  list(par = gbest, value = fgbest)
}

halfwidths <- c(2000, 5000, 10000)
results <- list()

for (hw in halfwidths) {
  cat(sprintf("\n========== PSO halfwidth = %d ==========\n", hw))
  t0 <- Sys.time()
  res <- pso(fn, lower = rep(-hw, d), upper = rep(hw, d),
             seed_particle = PSO_PRIOR_BEST,
             n_particles = 40, n_iter = 100,
             label = as.character(hw))
  pso_time <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("PSO best ssq = %.6g  par = (%s)  time = %.0fs\n",
              res$value, paste(round(res$par, 3), collapse = ", "), pso_time))

  t0 <- Sys.time()
  polish <- tryCatch(
    optim(par = res$par, fn = fn, method = "Nelder-Mead",
          control = list(maxit = 5000, reltol = CONFIG$obj_tol,
                         parscale = rep(200, d), trace = 0)),
    error = function(e) list(par = res$par, value = res$value, convergence = -1)
  )
  polish_time <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("NM polish ssq = %.6g  par = (%s)  conv=%d  time = %.0fs\n",
              polish$value, paste(round(polish$par, 3), collapse = ", "),
              polish$convergence, polish_time))

  results[[as.character(hw)]] <- list(pso = res, polish = polish,
                                      pso_time = pso_time)
}

cat("\n========== SUMMARY ==========\n")
cat(sprintf("%-10s %-12s %-12s %s\n", "halfwidth", "PSO_ssq", "polish_ssq", "polished_par"))
for (hw_key in names(results)) {
  r <- results[[hw_key]]
  cat(sprintf("hw=%-7s %.6g    %.6g    %s\n",
              hw_key, r$pso$value, r$polish$value,
              paste(round(r$polish$par, 2), collapse = ", ")))
}
