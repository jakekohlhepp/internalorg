## PROTOTYPE v2: fast+correct inner assignment via cheap-SQUAREM warm start +
## quasi-Newton polish, vs the slow SQUAREM-1e6 reference.
##
## v1 found: (a) cold Newton on log(C) fails; (b) at innertol=1e-12, maxiter=1e5
## SQUAREM is already within ~1.5e-5 of maxiter=1e6 -> the false basin is an
## innertol=1e-8 artifact, not a maxiter problem.
##
## v2 tests the practical fast gate: run a CHEAP short SQUAREM (a few hundred
## iters), then polish with nleqslv on g(E)=fxpt(E)-E (Broyden and damped
## Newton). Goal: reach <1e-8 of the SQUAREM-1e6 reference in a tiny fraction of
## the time. Reported per firm.
##
## Output: smoke_prototype_newton_inner_v2.rds (gitignored)
suppressPackageStartupMessages({ library(data.table); library(SQUAREM); library(nleqslv) })

source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types
n_t <- CONFIG$n_task_types
task_mix_cols <- get_task_mix_cols(CONFIG)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()

ctx <- load_counterfactual_context()
wd  <- copy(ctx$working_data); market_parms <- ctx$market_parms; rho <- ctx$rho
improve_it <- function(x){ r <- frank(x, ties.method="first"); sort(x)[pmax(r-1,1)] }
wd[county == LA & quarter_year == QY, gamma_invert := improve_it(gamma_invert)]
la <- wd[county == LA & quarter_year == QY]

anchor <- as.numeric(unlist(as.data.table(readRDS(counterfactual_data_path("14_wages_diffusion.rds", CONFIG)))[
  county == LA & quarter_year == QY & sol_type == "reorg", paste0("w", seq_len(n_w)), with = FALSE]))
new_theta <- matrix(market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
                    ncol = n_t, nrow = n_w, byrow = FALSE)
w_mat <- matrix(anchor, ncol = n_t, nrow = n_w, byrow = FALSE)
tmp <- w_mat + (rho[LA])^(-1) * new_theta
cost_matrix <- sweep(tmp, 2, apply(tmp, 2, min))

make_A <- function(gamma){ A <- exp(-cost_matrix / gamma); A[A>=Inf] <- CONFIG$numeric_ceiling; A[A<=0] <- CONFIG$numeric_floor; A }
B_from <- function(A, alpha, E){ B <- t(t(A) * alpha / colSums(A * E)) * E; B[abs(B)<CONFIG$B_zero_threshold] <- 0; B }

sq <- function(A, alpha, maxiter, tol=1e-12){
  fxpt <- function(p) p * colSums(t(A) * alpha / colSums(A * p))
  t0 <- proc.time()[[3]]
  r <- squarem(rep(1/n_w, n_w), fixptfn = fxpt, control = list(maxiter=maxiter, tol=tol))
  list(par = r$par, E = rowSums(B_from(A, alpha, r$par)), fpevals = r$fpevals,
       conv = as.integer(r$convergence), sec = proc.time()[[3]]-t0)
}

## polish: nleqslv on g(E)=fxpt(E)-E, in log-E space (keeps shares positive),
## from a warm start. Try Broyden then damped Newton; keep whichever reaches the
## smaller ||g||.
polish <- function(A, alpha, start, method){
  fxpt <- function(p){ d <- colSums(A * pmax(p, CONFIG$numeric_floor)); p * colSums(t(A) * alpha / pmax(d, CONFIG$numeric_floor)) }
  g <- function(u){ E <- exp(u); fxpt(E) - E }
  t0 <- proc.time()[[3]]
  nl <- tryCatch(nleqslv(log(pmax(start, CONFIG$numeric_floor)), g, method = method, global = "dbldog",
                         control = list(xtol=1e-13, ftol=1e-13, maxit=200, allowSingular=TRUE, cndtol=1e-16)),
                 error = function(e) NULL)
  sec <- proc.time()[[3]]-t0
  if (is.null(nl)) return(list(E=rep(NA_real_,n_w), iters=NA, termcd=NA, sec=sec, gnorm=NA))
  E <- exp(nl$x)
  list(E = rowSums(B_from(A, alpha, E)), iters = nl$iter, termcd = nl$termcd,
       sec = sec, gnorm = max(abs(nl$fvec)))
}

la_min_g <- head(sort(unique(la$gamma_invert)), 1)
test_rows <- unique(c(which(la$gamma_invert == la_min_g),
                      la[, .I[order(abs(gamma_invert - median(gamma_invert)))][1:2]]))

res <- list()
for (i in test_rows) {
  alpha <- as.numeric(la[i, ..task_mix_cols]); gamma <- la$gamma_invert[i]
  ge <- counterfactual_effective_gamma(gamma, CONFIG); if (!(is.finite(ge)&&ge>0)) next
  A <- make_A(ge)
  ref <- sq(A, alpha, 1e6, 1e-12)
  warm500  <- sq(A, alpha, 500,  1e-12)
  warm2000 <- sq(A, alpha, 2000, 1e-12)
  pol_br  <- polish(A, alpha, warm500$E,  "Broyden")
  pol_nt  <- polish(A, alpha, warm500$E,  "Newton")
  pol_br2 <- polish(A, alpha, warm2000$E, "Broyden")

  res[[length(res)+1]] <- data.table(
    loc=substr(la$location_id[i],1,8), gamma=round(gamma,4),
    ref_fpevals=ref$fpevals, ref_sec=round(ref$sec,2),
    w500_dE=signif(max(abs(warm500$E-ref$E)),3), w500_sec=round(warm500$sec,3),
    br_from500_dE=signif(max(abs(pol_br$E-ref$E)),3), br_from500_iters=pol_br$iters, br_from500_sec=round(pol_br$sec,3), br_termcd=pol_br$termcd,
    nt_from500_dE=signif(max(abs(pol_nt$E-ref$E)),3), nt_from500_iters=pol_nt$iters, nt_termcd=pol_nt$termcd,
    br_from2000_dE=signif(max(abs(pol_br2$E-ref$E)),3), br_from2000_sec=round(pol_br2$sec,3))
}
out <- rbindlist(res, fill=TRUE); setorder(out, gamma)

cat("=== v2: cheap-warm + quasi-Newton polish vs SQUAREM-1e6 ===\n")
print(out)

cat("\n=== interpretation ===\n")
slow <- out[gamma < 1.5]
best_total_sec <- slow$w500_sec + slow$br_from500_sec
cat("Slow firms: SQUAREM-1e6 sec=", paste(slow$ref_sec, collapse=","),
    " | warm500+Broyden sec=", paste(round(best_total_sec,3), collapse=","), "\n", sep="")
cat("warm500+Broyden dE vs 1e6 (slow firms): ", paste(slow$br_from500_dE, collapse=", "), "\n", sep="")
ok <- all(out$br_from500_dE < 1e-8, na.rm=TRUE) || all(out$br_from2000_dE < 1e-8, na.rm=TRUE)
cat("\nVERDICT: ", if (ok) "WARM+POLISH REACHES <1e-8 (viable fast gate)" else
    "still not <1e-8; report best achieved", "\n", sep="")

saveRDS(list(out=out), "smoke_prototype_newton_inner_v2.rds")
cat("\nSaved: smoke_prototype_newton_inner_v2.rds\n")
