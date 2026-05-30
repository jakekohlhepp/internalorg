## PROTOTYPE: hand-rolled Anderson Acceleration (type-II) for the inner
## assignment fixed point, vs SQUAREM, on the stiff LA firms. Pure R, no new
## package. If AA converges the gamma~0.898 firms in far fewer fxpt evals than
## SQUAREM's ~100k-145k, we wire AA into counterfactual_assignment.
##
## Fixed point: E = fxpt(E), fxpt(p) = p * colSums(t(A) alpha / colSums(A p)).
## Stiff because the fxpt Jacobian has an eigenvalue ~0.9998. AA extrapolates
## exactly that slow mode.
##
## Output: smoke_prototype_anderson_inner.rds (gitignored)
suppressPackageStartupMessages({ library(data.table); library(SQUAREM) })

source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types; n_t <- CONFIG$n_task_types
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

make_A <- function(gamma){ A <- exp(-cost_matrix/gamma); A[A>=Inf] <- CONFIG$numeric_ceiling; A[A<=0] <- CONFIG$numeric_floor; A }
B_from <- function(A, alpha, E){ B <- t(t(A) * alpha / colSums(A * E)) * E; B[abs(B)<CONFIG$B_zero_threshold] <- 0; B }

make_fxpt <- function(A, alpha) function(p){
  pp <- pmax(p, CONFIG$numeric_floor)
  d <- pmax(colSums(A * pp), CONFIG$numeric_floor)
  pp * colSums(t(A) * alpha / d)
}

## type-II Anderson acceleration (Walker-Ni), with positivity guard.
anderson <- function(par, g, m = 6L, maxiter = 100000L, tol = 1e-12, beta = 1) {
  x <- par
  gx <- g(x); f <- gx - x
  evals <- 1L
  if (sqrt(sum(f*f)) < tol) return(list(par=x, evals=evals, resid=sqrt(sum(f*f))))
  x_prev <- x; f_prev <- f
  x <- pmax(gx, CONFIG$numeric_floor)            # first step: plain
  Xh <- list(); Fh <- list()
  rn <- Inf
  for (k in seq_len(maxiter)) {
    gx <- g(x); f <- gx - x; evals <- evals + 1L
    rn <- sqrt(sum(f*f))
    if (rn < tol) break
    Xh[[length(Xh)+1L]] <- x - x_prev
    Fh[[length(Fh)+1L]] <- f - f_prev
    if (length(Fh) > m) { Xh <- Xh[-1L]; Fh <- Fh[-1L] }
    dF <- do.call(cbind, Fh); dX <- do.call(cbind, Xh)
    gamma <- tryCatch(qr.solve(dF, f), error = function(e) rep(0, ncol(dF)))
    x_new <- x + beta*f - (dX + beta*dF) %*% gamma
    x_prev <- x; f_prev <- f
    x <- pmax(as.numeric(x_new), CONFIG$numeric_floor)
  }
  list(par = x, evals = evals, resid = rn)
}

sq_ref <- function(A, alpha){
  fx <- make_fxpt(A, alpha); t0 <- proc.time()[[3]]
  r <- squarem(rep(1/n_w,n_w), fixptfn=fx, control=list(maxiter=1e6, tol=1e-12))
  list(E=rowSums(B_from(A,alpha,r$par)), fpevals=r$fpevals, sec=proc.time()[[3]]-t0)
}

la_min_g <- head(sort(unique(la$gamma_invert)),1)
test_rows <- unique(c(which(la$gamma_invert==la_min_g),
                      la[, .I[order(abs(gamma_invert-median(gamma_invert)))][1:2]]))

res <- list()
for (i in test_rows){
  alpha <- as.numeric(la[i, ..task_mix_cols]); gamma <- la$gamma_invert[i]
  ge <- counterfactual_effective_gamma(gamma, CONFIG); if(!(is.finite(ge)&&ge>0)) next
  A <- make_A(ge); fx <- make_fxpt(A, alpha)
  ref <- sq_ref(A, alpha)
  row <- data.table(loc=substr(la$location_id[i],1,8), gamma=round(gamma,4),
                    sq_fpevals=ref$fpevals, sq_sec=round(ref$sec,2))
  for (m in c(3L,6L,10L)){
    t0 <- proc.time()[[3]]
    aa <- anderson(rep(1/n_w,n_w), fx, m=m, maxiter=100000L, tol=1e-12)
    sec <- proc.time()[[3]]-t0
    E_aa <- rowSums(B_from(A, alpha, aa$par))
    row[[paste0("aa_m",m,"_evals")]] <- aa$evals
    row[[paste0("aa_m",m,"_sec")]]   <- round(sec,3)
    row[[paste0("aa_m",m,"_dE")]]    <- signif(max(abs(E_aa-ref$E)),3)
    row[[paste0("aa_m",m,"_resid")]] <- signif(aa$resid,3)
  }
  res[[length(res)+1]] <- row
}
out <- rbindlist(res, fill=TRUE); setorder(out, gamma)

cat("=== Anderson acceleration vs SQUAREM-1e6 (LA inner assignment) ===\n")
print(out)

slow <- out[gamma < 1.5]
cat("\n=== slow firms (gamma<1.5) ===\n")
cat("SQUAREM fpevals: ", paste(slow$sq_fpevals, collapse=","), " (sec ", paste(slow$sq_sec, collapse=","), ")\n", sep="")
for (m in c(3L,6L,10L)){
  ev <- slow[[paste0("aa_m",m,"_evals")]]; de <- slow[[paste0("aa_m",m,"_dE")]]; sc <- slow[[paste0("aa_m",m,"_sec")]]
  cat(sprintf("AA m=%-2d evals: %s  sec: %s  dE vs 1e6: %s\n",
              m, paste(ev,collapse=","), paste(sc,collapse=","), paste(de,collapse=",")))
}
best_dE <- min(sapply(c(3,6,10), function(m) max(slow[[paste0("aa_m",m,"_dE")]], na.rm=TRUE)))
best_speed <- max(sapply(c(3,6,10), function(m) median(slow$sq_fpevals)/median(slow[[paste0("aa_m",m,"_evals")]])))
cat(sprintf("\nBest AA: dE<=%.2g vs SQUAREM-1e6; up to ~%.0fx fewer fxpt evals\n", best_dE, best_speed))
verdict <- if (best_dE < 1e-8) "ANDERSON CONVERGES STIFF FIRMS TO <1e-8, FASTER (viable)" else
  paste0("AA best dE=", signif(best_dE,3), " (not <1e-8; report)")
cat("VERDICT: ", verdict, "\n", sep="")

saveRDS(list(out=out, verdict=verdict), "smoke_prototype_anderson_inner.rds")
cat("\nSaved: smoke_prototype_anderson_inner.rds\n")
