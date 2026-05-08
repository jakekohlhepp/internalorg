## Warm-started PSO smoke test for NYC wage parameters.
##
## Env vars (defaults shown):
##   JMP_PSO_SEED    seed for set.seed (default 4459665)
##   JMP_PSO_LABEL   label printed in summary (default "default")
##   JMP_PSO_MODE    "polish_nm" (single NM polish, default) or
##                   "polish_all" (try NM/BFGS/L-BFGS-B/CG/nlminb/SANN/nleqslv)
##
## Reads seeit_bb.rds + 04_estimation_sample.rds; writes nothing to mkdata/data
## or results/data.

library(data.table)
seed_value <- as.integer(Sys.getenv("JMP_PSO_SEED", unset = "4459665"))
run_label  <- Sys.getenv("JMP_PSO_LABEL", unset = "default")
mode       <- Sys.getenv("JMP_PSO_MODE", unset = "polish_nm")
set.seed(seed_value)
source("config.R")

cat("=== smoke_pso_warmstart  label=", run_label,
    "  seed=", seed_value, "  mode=", mode, " ===\n", sep = "")

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

## fn_vec returns the moment vector itself (used by nleqslv as a root finder).
fn_vec <- function(parms) {
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
  if (is.null(moments)) return(rep(1e3, sum(par_idx)))
  sub <- grepl(paste0("county", cnty, ":E_"), names(moments), fixed = TRUE)
  v <- as.numeric(moments[sub])
  if (anyNA(v) || any(!is.finite(v))) return(rep(1e3, sum(par_idx)))
  v
}

## Prior best from job 48962785 (PSO + NM polish), used as one warm-start
## particle in the swarm.
PSO_PRIOR_BEST <- c(-22.4, 498.2, 150.94, 1826.32)
d <- length(PSO_PRIOR_BEST)
names(PSO_PRIOR_BEST) <- names(beta_2_subset)[par_idx]

cat("Prior best ssq (warm-start point):", signif(fn(PSO_PRIOR_BEST), 6), "\n")

## ---------- Vanilla PSO ----------
pso <- function(fn, lower, upper, seed_particle = NULL,
                n_particles = 40, n_iter = 100,
                w = 0.7, c1 = 1.5, c2 = 1.5, verbose = TRUE) {
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
    if (verbose && (t %% 10 == 0 || t == 1)) {
      cat(sprintf("  iter %3d: gbest ssq=%.6g\n", t, fgbest))
    }
  }
  list(par = gbest, value = fgbest)
}

cat("\n========== PSO 40p x 100i, seed=", seed_value,
    ", search box [-2000,+2000]^4, prior best as one particle ==========\n",
    sep = "")
pso_res <- pso(fn, lower = rep(-2000, d), upper = rep(2000, d),
               seed_particle = PSO_PRIOR_BEST,
               n_particles = 40, n_iter = 100)
cat(sprintf("PSO best ssq = %.6g\n", pso_res$value))
cat("PSO best par =", paste(round(pso_res$par, 3), collapse = ", "), "\n")

## ---------- Polishing ----------
polish_results <- list()
polish_results[["raw_pso"]] <- list(value = pso_res$value, par = pso_res$par,
                                    convergence = NA, time_sec = 0)

run_polish <- function(name, optim_call) {
  t0 <- Sys.time()
  res <- tryCatch(optim_call(), error = function(e) {
    list(par = pso_res$par, value = NA_real_, convergence = -99,
         message = conditionMessage(e))
  })
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("polish %-12s ssq = %-12.6g convergence = %s  time = %.1fs\n",
              name, res$value, format(res$convergence), dt))
  list(value = res$value, par = res$par,
       convergence = res$convergence, time_sec = dt,
       message = res$message %||% NA_character_)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

methods_to_run <- if (mode == "polish_all") {
  c("NM", "BFGS", "L-BFGS-B", "CG", "nlminb", "SANN", "nleqslv")
} else {
  "NM"
}

cat("\n========== Polishing (methods: ", paste(methods_to_run, collapse = ", "),
    ") ==========\n", sep = "")
parscale_w <- rep(200, d)

for (m in methods_to_run) {
  if (m %in% c("NM", "BFGS", "L-BFGS-B", "CG", "SANN")) {
    method_name <- if (m == "NM") "Nelder-Mead" else m
    ctrl <- list(parscale = parscale_w, maxit = 5000, trace = 0)
    if (m == "SANN") ctrl$maxit <- 2000
    polish_results[[m]] <- run_polish(m, function() {
      optim(par = pso_res$par, fn = fn, method = method_name, control = ctrl)
    })
  } else if (m == "nlminb") {
    polish_results[[m]] <- run_polish("nlminb", function() {
      r <- nlminb(start = pso_res$par, objective = fn,
                  scale = 1 / parscale_w,
                  control = list(iter.max = 5000, eval.max = 10000))
      list(par = r$par, value = r$objective, convergence = r$convergence,
           message = r$message)
    })
  } else if (m == "nleqslv") {
    polish_results[[m]] <- run_polish("nleqslv", function() {
      r <- nleqslv::nleqslv(x = pso_res$par, fn = fn_vec,
                            method = "Broyden", global = "dbldog",
                            control = list(maxit = 200))
      list(par = r$x, value = sum(r$fvec^2), convergence = r$termcd,
           message = r$message)
    })
  }
}

cat("\n========== SUMMARY (label=", run_label, ", seed=", seed_value, ") ==========\n",
    sep = "")
for (m in names(polish_results)) {
  r <- polish_results[[m]]
  cat(sprintf("  %-12s ssq = %-12.6g par = [%s]\n", m, r$value,
              paste(round(r$par, 2), collapse = ", ")))
}
