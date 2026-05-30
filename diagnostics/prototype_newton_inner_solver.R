## PROTOTYPE: Newton inner assignment solver vs SQUAREM, on the slow LA firms.
##
## The inner assignment fixed point is E = fxpt(E), with
##   fxpt(p) = p * C(p),  C_i(p) = sum_j A[i,j] alpha[j] / sum_k A[k,j] p[k],
##   A = exp(-cost_matrix / gamma).
## At the fixed point C(E)=1. SQUAREM crawls when the fxpt Jacobian has an
## eigenvalue ~1 (near-corner / small-gamma firms), needing ~1e6 evals. Newton
## on h(E)=log(C(E))=0 should converge in tens of iters regardless.
##
## Compares, per firm, at the diffusion reorg anchor cost matrix:
##   - SQUAREM maxiter=1e6, tol=1e-12   (reference truth)
##   - SQUAREM maxiter=1e5, tol=1e-12   (cheap gate attempt)
##   - Newton (nleqslv) on log(C(E))
## Reports E agreement (max|dE| vs reference), fpevals/iters, wall time.
##
## Output: smoke_prototype_newton_inner.rds (gitignored)
suppressPackageStartupMessages({ library(data.table); library(SQUAREM); library(nleqslv) })

source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types
n_t <- CONFIG$n_task_types
task_mix_cols <- get_task_mix_cols(CONFIG)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()

ctx <- load_counterfactual_context()
wd  <- copy(ctx$working_data)
market_parms <- ctx$market_parms
rho <- ctx$rho

## diffusion shock on gamma so the gamma~0.898 firms appear (matches 14_/audit)
improve_it <- function(x){ r <- frank(x, ties.method="first"); sort(x)[pmax(r-1,1)] }
wd[county == LA & quarter_year == QY, gamma_invert := improve_it(gamma_invert)]
la <- wd[county == LA & quarter_year == QY]

## diffusion reorg anchor wages -> cost matrix
anchor <- as.numeric(unlist(as.data.table(readRDS(counterfactual_data_path("14_wages_diffusion.rds", CONFIG)))[
  county == LA & quarter_year == QY & sol_type == "reorg", paste0("w", seq_len(n_w)), with = FALSE]))
new_theta <- matrix(market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
                    ncol = n_t, nrow = n_w, byrow = FALSE)
w_mat <- matrix(anchor, ncol = n_t, nrow = n_w, byrow = FALSE)
cost_matrix <- sweep(w_mat + (rho[LA])^(-1) * new_theta, 2, apply(w_mat + (rho[LA])^(-1) * new_theta, 2, min))

make_A <- function(gamma) {
  A <- exp(-cost_matrix / gamma)
  A[A >= Inf] <- CONFIG$numeric_ceiling; A[A <= 0] <- CONFIG$numeric_floor; A
}
B_from <- function(A, alpha, E) {
  B <- t(t(A) * alpha / colSums(A * E)) * E
  B[abs(B) < CONFIG$B_zero_threshold] <- 0
  B
}

solve_squarem <- function(A, alpha, maxiter, tol) {
  fxpt <- function(p){ p * colSums(t(A) * alpha / colSums(A * p)) }
  t0 <- proc.time()[[3]]
  r <- squarem(rep(1/n_w, n_w), fixptfn = fxpt, control = list(maxiter = maxiter, tol = tol))
  el <- proc.time()[[3]] - t0
  list(E = rowSums(B_from(A, alpha, r$par)), fpevals = r$fpevals,
       conv = as.integer(r$convergence), sec = el)
}
solve_newton <- function(A, alpha, start = rep(1/n_w, n_w)) {
  Cfun <- function(p) colSums(t(A) * alpha / colSums(A * pmax(p, CONFIG$numeric_floor)))
  ## residual in log-C: zero iff C(E)=1 iff E is the fixed point. Solve in log-E
  ## to keep shares positive.
  h <- function(u) log(pmax(Cfun(exp(u)), CONFIG$numeric_floor))
  t0 <- proc.time()[[3]]
  nl <- tryCatch(nleqslv(log(start), h, method = "Newton", global = "dbldog",
                         control = list(xtol = 1e-12, ftol = 1e-12, maxit = 500,
                                        allowSingular = TRUE, cndtol = 1e-14)),
                 error = function(e) NULL)
  el <- proc.time()[[3]] - t0
  if (is.null(nl)) return(list(E = rep(NA_real_, n_w), iters = NA, fevals = NA, termcd = NA, sec = el))
  E <- exp(nl$x)
  list(E = rowSums(B_from(A, alpha, E)), iters = nl$iter, fevals = nl$nfcnt,
       termcd = nl$termcd, sec = el, maxabs_h = max(abs(nl$fvec)))
}

## pick firms: the 2 slowest-gamma + 3 well-behaved (median-ish gamma)
la_ord <- la[order(gamma_invert)]
slow_idx <- which(la$gamma_invert %in% head(sort(unique(la$gamma_invert)), 1))  # gamma ~0.898 (ties)
fast_pick <- la[, .I[order(abs(gamma_invert - median(gamma_invert)))][1:3]]
test_rows <- unique(c(slow_idx, fast_pick))
cat("Testing ", length(test_rows), " firms (slow gamma + median gamma).\n\n", sep = "")

res <- list()
for (i in test_rows) {
  alpha <- as.numeric(la[i, ..task_mix_cols])
  gamma <- la$gamma_invert[i]
  ge <- counterfactual_effective_gamma(gamma, CONFIG)
  if (!(is.finite(ge) && ge > 0)) next
  A <- make_A(ge)

  ref   <- solve_squarem(A, alpha, 1e6, 1e-12)
  cheap <- solve_squarem(A, alpha, 1e5, 1e-12)
  nt    <- solve_newton(A, alpha)
  nt_ws <- solve_newton(A, alpha, start = cheap$E)  # warm-started from cheap squarem

  res[[length(res)+1]] <- data.table(
    loc = la$location_id[i], gamma = gamma,
    sq1e6_fpevals = ref$fpevals, sq1e6_sec = round(ref$sec, 3),
    cheap_conv = cheap$conv, cheap_fpevals = cheap$fpevals,
    cheap_dE = max(abs(cheap$E - ref$E)),
    nt_iters = nt$iters, nt_fevals = nt$fevals, nt_termcd = nt$termcd,
    nt_sec = round(nt$sec, 3), nt_dE = max(abs(nt$E - ref$E)),
    ntws_iters = nt_ws$iters, ntws_sec = round(nt_ws$sec, 3),
    ntws_dE = max(abs(nt_ws$E - ref$E))
  )
}
out <- rbindlist(res, fill = TRUE)
setorder(out, gamma)

cat("=== Newton vs SQUAREM on LA inner assignment (diffusion reorg anchor) ===\n")
print(out[, .(loc = substr(loc,1,8), gamma = round(gamma,4),
              sq1e6_fpevals, sq1e6_sec,
              cheap_conv, cheap_dE = signif(cheap_dE,3),
              nt_iters, nt_termcd, nt_sec, nt_dE = signif(nt_dE,3),
              ntws_sec, ntws_dE = signif(ntws_dE,3))])

cat("\n=== summary ===\n")
slow <- out[gamma < 1.5]
cat("Slow firms (gamma<1.5): ", nrow(slow), "\n", sep = "")
cat("  SQUAREM 1e6: median fpevals=", median(slow$sq1e6_fpevals),
    ", median sec=", median(slow$sq1e6_sec), "\n", sep = "")
cat("  Newton:      median iters=", median(slow$nt_iters, na.rm=TRUE),
    ", median sec=", median(slow$nt_sec, na.rm=TRUE),
    ", max|dE vs 1e6|=", signif(max(slow$nt_dE, na.rm=TRUE), 3), "\n", sep = "")
cat("  cheap SQUAREM 1e5 max|dE vs 1e6| on slow firms=", signif(max(slow$cheap_dE, na.rm=TRUE),3),
    " (this is the false-basin error the cheap solve makes)\n", sep = "")
spd <- median(slow$sq1e6_sec, na.rm=TRUE) / pmax(median(slow$nt_sec, na.rm=TRUE), 1e-6)
cat("  Newton speedup vs SQUAREM-1e6 on slow firms: ~", round(spd, 1), "x\n", sep = "")

verdict <- if (all(out$nt_dE < 1e-8, na.rm=TRUE) && all(!is.na(out$nt_termcd) & out$nt_termcd == 1))
  "NEWTON REPRODUCES SQUAREM-1e6 (dE<1e-8) AND CONVERGES" else
  "CHECK: Newton did not match/converge on some firms"
cat("\nVERDICT: ", verdict, "\n", sep = "")

saveRDS(list(out = out, anchor = anchor, verdict = verdict),
        "smoke_prototype_newton_inner.rds")
cat("\nSaved: smoke_prototype_newton_inner.rds\n")
