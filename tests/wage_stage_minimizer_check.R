## Confirm the minimizer-vs-root-finder hypothesis. Hold demand `beta`
## fixed at the sequential value. For the LA wage block (4 params), compare:
##   (a) nleqslv on g_wage_LA = 0  (current pipeline behavior)
##   (b) BBoptim on ||g_wage_LA||^2  (minimizer alternative)
##   (c) optim Nelder-Mead on ||g_wage_LA||^2 with parscale
## All starting from the cold-start v2 LA wages = (0,0,0,0). Other counties
## held at cold-start v2 fits.
suppressPackageStartupMessages({ library(data.table); library(BB); library(nleqslv); library(parallel) })
source("config.R"); source("preamble.R")

es <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
estim_matrix <- es$estim_matrix
eo <- build_estimation_setup(as.data.table(es$working_data), estim_matrix, config = CONFIG)
beta <- eo$beta; beta_2 <- eo$beta_2
wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
wage_names <- rownames(beta_2)[wage_idx]

clust <- make_windows_solver_cluster(CONFIG)
on.exit(if (!is.null(clust)) parallel::stopCluster(clust), add = TRUE)

cold <- readRDS("mkdata/data/seeit_bb.rds")
fixed_others <- cold; fixed_others[c(3, 6, 9, 12)] <- 0

mk <- function(la_w) {
  v <- fixed_others; v[c(3, 6, 9, 12)] <- la_w
  setNames(as.numeric(v), wage_names)
}
g_la <- function(la_w) {
  m <- weighted_col_means(objective_gmm(theta = mk(la_w), x = estim_matrix,
    beta = beta, beta_2_subset = mk(la_w), config = CONFIG, clust = clust))
  as.numeric(m[grepl("county6037:E_", names(m), fixed = TRUE)])
}
ssq_la <- function(la_w) {
  v <- tryCatch(g_la(la_w), error = function(e) rep(NA, 4))
  if (anyNA(v)) return(1e6); sum(v^2)
}

start <- c(0, 0, 0, 0)
parscale_la <- c(20, 20, 20, 20)

cat("=== Starting LA wage ssq from (0,0,0,0):", round(ssq_la(start), 6), "===\n")

cat("\n--- (a) nleqslv root-finder ---\n")
t1 <- system.time({
  r1 <- tryCatch(nleqslv::nleqslv(start, fn = g_la,
    control = list(xtol = 1e-8, ftol = 1e-8, maxit = 200, trace = 0)),
    error = function(e) list(x = start, fvec = g_la(start),
                              termcd = -1, message = conditionMessage(e)))
})
cat("  par:", round(r1$x, 4), "\n")
cat("  ssq:", sprintf("%.6f", sum(r1$fvec^2)), "  termcd:", r1$termcd,
    "  time:", round(t1[3], 1), "s\n")

cat("\n--- (b) BBoptim minimizer ---\n")
t2 <- system.time({
  r2 <- tryCatch(BB::BBoptim(start, fn = ssq_la,
    control = list(M = 50, ftol = 1e-8, maxit = 500, trace = FALSE)),
    error = function(e) list(par = start, value = ssq_la(start),
                              convergence = -1, message = conditionMessage(e)))
})
cat("  par:", round(r2$par, 4), "\n")
cat("  ssq:", sprintf("%.6f", r2$value), "  conv:", r2$convergence,
    "  time:", round(t2[3], 1), "s\n")

cat("\n--- (c) optim Nelder-Mead minimizer with parscale ---\n")
t3 <- system.time({
  r3 <- tryCatch(optim(start, fn = ssq_la, method = "Nelder-Mead",
    control = list(maxit = 500, reltol = 1e-8, parscale = parscale_la,
                   trace = 0)),
    error = function(e) list(par = start, value = ssq_la(start),
                              convergence = -1, message = conditionMessage(e)))
})
cat("  par:", round(r3$par, 4), "\n")
cat("  ssq:", sprintf("%.6f", r3$value), "  conv:", r3$convergence,
    "  time:", round(t3[3], 1), "s\n")

cat("\n=== Summary ===\n")
cat(sprintf("nleqslv (root):     ssq = %.5f, time = %.1fs\n", sum(r1$fvec^2), t1[3]))
cat(sprintf("BBoptim (min):      ssq = %.5f, time = %.1fs\n", r2$value, t2[3]))
cat(sprintf("optim NM (min):     ssq = %.5f, time = %.1fs\n", r3$value, t3[3]))
