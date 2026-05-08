## Smoke test: can a particle swarm optimizer find a NYC wage basin with
## ssq < 0.0248 (the local minimum that NM/BFGS dead-end at)? Compares:
##
##   (A) NM multi-start (5 random starts in [-1000, +1000]^4, parscale=200)
##   (B) Vanilla PSO (40 particles, 100 iterations, search box [-2000, 2000]^4)
##
## Read-only: does NOT touch seeit_bb.rds or 06_parameters.rds.

library(data.table)
set.seed(4459665)
source("config.R")

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

start_par <- full_par[par_idx]
d <- length(start_par)
cat("NYC dimension d =", d, "\n")
cat("Seed start ssq:", signif(fn(start_par), 6), "\n")

## ---------- (A) NM multi-start ----------
cat("\n========== (A) Nelder-Mead multi-start, 5 starts ==========\n")
ms_starts <- list(start_par)
for (i in 2:5) {
  rp <- runif(d, -1000, 1000); names(rp) <- names(start_par)
  ms_starts[[i]] <- rp
}
ms_results <- list()
for (i in seq_along(ms_starts)) {
  r <- optim(ms_starts[[i]], fn = fn, method = "Nelder-Mead",
             control = list(maxit = 5000, reltol = CONFIG$obj_tol,
                            parscale = rep(200, d), trace = 0))
  cat(sprintf("  start %d: ssq=%.6g (conv=%d, evals=%d)\n",
              i, r$value, r$convergence, r$counts[1]))
  ms_results[[i]] <- r
}
ms_best <- ms_results[[which.min(vapply(ms_results, function(x) x$value, 0))]]
cat(sprintf("NM multi-start best ssq: %.6g\n", ms_best$value))
cat("NM multi-start best par:", round(ms_best$par, 2), "\n")

## ---------- (B) Hand-rolled vanilla PSO ----------
##   x_i^{t+1}   = x_i^t + v_i^{t+1}
##   v_i^{t+1}   = w*v_i^t + c1*r1*(pbest_i - x_i) + c2*r2*(gbest - x_i)
pso <- function(fn, lower, upper, n_particles = 40, n_iter = 100,
                w = 0.7, c1 = 1.5, c2 = 1.5, verbose = TRUE) {
  d <- length(lower)
  X <- matrix(runif(n_particles * d, rep(lower, each = n_particles),
                    rep(upper, each = n_particles)), n_particles, d)
  V <- matrix(runif(n_particles * d, -abs(upper - lower), abs(upper - lower)),
              n_particles, d) * 0.1
  fX <- apply(X, 1, fn)
  pbest <- X; fpbest <- fX
  gi <- which.min(fpbest); gbest <- X[gi, ]; fgbest <- fpbest[gi]
  trace <- numeric(n_iter)
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
    if (fpbest[gi] < fgbest) {
      gbest <- pbest[gi, ]; fgbest <- fpbest[gi]
    }
    trace[t] <- fgbest
    if (verbose && (t %% 10 == 0 || t == 1)) {
      cat(sprintf("  iter %3d: gbest ssq=%.6g\n", t, fgbest))
    }
  }
  list(par = gbest, value = fgbest, trace = trace)
}

cat("\n========== (B) Vanilla PSO, 40 particles, 100 iters ==========\n")
lower <- rep(-2000, d); upper <- rep(2000, d)
pso_res <- pso(fn, lower, upper, n_particles = 40, n_iter = 100)
cat(sprintf("PSO best ssq: %.6g\n", pso_res$value))
cat("PSO best par:", round(pso_res$par, 2), "\n")

## Polish PSO best with NM
cat("\n--- (B') Polish PSO best with NM ---\n")
pso_polish <- optim(pso_res$par, fn = fn, method = "Nelder-Mead",
                    control = list(maxit = 5000, reltol = CONFIG$obj_tol,
                                   parscale = rep(200, d), trace = 0))
cat(sprintf("PSO+NM polish ssq: %.6g (conv=%d)\n",
            pso_polish$value, pso_polish$convergence))
cat("PSO+NM polish par:", round(pso_polish$par, 2), "\n")

cat("\n========== SUMMARY (NYC) ==========\n")
cat(sprintf("seed                  ssq = %.6g\n", fn(start_par)))
cat(sprintf("NM multi-start best   ssq = %.6g\n", ms_best$value))
cat(sprintf("PSO                   ssq = %.6g\n", pso_res$value))
cat(sprintf("PSO + NM polish       ssq = %.6g\n", pso_polish$value))
