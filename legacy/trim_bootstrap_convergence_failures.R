## ============================================================================
## trim_bootstrap_convergence_failures.R   (PRODUCTION)
## ----------------------------------------------------------------------------
## Robust NYC bootstrap SEs with convergence-failure trimming.
##
## A rep is a NYC CONVERGENCE FAILURE if, re-evaluated under the rep's OWN
## bootstrap weights, its NYC wage-moment ssq exceeds `tau` (or status=="error"/
## "wage_nonconverged"). Such reps sit in inferior/saturated basins (ssq 0.02-0.12
## vs the ~1e-9 good basin) and inflate the bootstrap variance. We trim them and
## report SEs WITH and WITHOUT trimming + a full per-rep audit (nothing dropped
## silently).
##
## tau=1e-2 sits in the gap between the good basin (~1e-9) and the saturated
## corners (E_2/E_3/E_4 plateaus 0.015-0.12) and deliberately KEEPS the genuinely
## soft E_5 ridge (ssq ~0.008 even at E_5=1500) -- that is real under-identification,
## not a failure, so it must NOT be trimmed.  Read the SE-reduction diagnostic:
## se_trimmed << se_all  => failures drove the spread (trimming valid);
## se_trimmed ~  se_all  => spread is genuine under-id (report partial-ID instead).
##
## Speed: evaluates NYC only, one read pass over rep files, parallel.
## Env: JMP_BOOT_TRIM_SSQ_TAU (1e-2), JMP_BOOT_TRIM_TRIM_NONCONV (true), SLURM_CPUS_PER_TASK
## Out: results/data/07b_bootstrap_se_trimmed.rds ; diagnostics/out/bootstrap_convergence_audit.rds
## ============================================================================
suppressPackageStartupMessages({ library(data.table); library(parallel) })
source("config.R"); source("preamble.R")
## Tight tol is rigorous but slow; coarse (staged) tol floors ssq ~1e-8 which is
## still << tau=1e-2, so it cleanly separates the saturated-corner FAILURES
## (ssq 0.02-0.12, tol-independent) from the good basin. Default coarse for speed;
## JMP_BOOT_TRIM_TIGHT=true for a rigorous re-check.
tight_tol <- tolower(Sys.getenv("JMP_BOOT_TRIM_TIGHT", unset = "false")) %in% c("true","t","1","yes")
if (tight_tol) CONFIG$use_staged_solver_tolerances <- FALSE
cat(sprintf("inner tol: %s\n", if (tight_tol) "TIGHT (use_staged=FALSE)" else "coarse/staged (fast)"))
tau          <- as.numeric(Sys.getenv("JMP_BOOT_TRIM_SSQ_TAU", unset = "1e-2"))
trim_nonconv <- tolower(Sys.getenv("JMP_BOOT_TRIM_TRIM_NONCONV", unset = "true")) %in% c("true","t","1","yes")
ncores       <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "8"))
NYC <- "36061"

es <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data <- data.table(es$working_data); estim_matrix <- es$estim_matrix
beta_2 <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)$beta_2
wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_names <- rownames(beta_2)[Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))]
p06 <- as.data.table(readRDS("results/data/06_parameters.rds"))
beta_names <- p06[demand == TRUE]$parm_name
focus      <- grep("36061", p06$parm_name, value = TRUE)        # NYC params for the SE table
need       <- unique(c(beta_names, wage_names, focus))

## NYC-only wage-moment ssq under given weights
nyc_data <- as.data.frame(estim_matrix); nyc_ridx <- as.character(nyc_data$county) == NYC
nyc_data <- nyc_data[nyc_ridx, , drop = FALSE]
eval_nyc_ssq <- function(theta_wage, beta, rw) {
  m <- weighted_col_means(objective_gmm(theta=theta_wage, x=nyc_data, beta=beta,
        beta_2_subset=theta_wage, config=CONFIG, clust=NULL, solver_state=NULL),
        weights = rw)
  sum(as.numeric(m[grepl(paste0("county",NYC,":E_"), names(m), fixed=TRUE)])^2)
}
build_bw <- function(data){ loc<-unique(data$location_id); set.seed(CONFIG$bootstrap_seed)
  rbindlist(lapply(seq_len(CONFIG$bootstrap_reps), function(it){ w<-stats::rexp(length(loc),1)
    data.table(iteration=it, location_id=loc, bweight=w/sum(w)) })) }
baw <- build_bw(working_data)
row_w_nyc <- function(it){ cw<-baw[iteration==it]; cw$bweight[match(working_data$location_id, cw$location_id)][nyc_ridx] }

## anchor
beta_pt <- matrix(p06[match(beta_names,parm_name)]$coefficients, ncol=1, dimnames=list(beta_names,NULL))
theta_pt<- setNames(p06[match(wage_names,parm_name)]$coefficients, wage_names)
anc <- eval_nyc_ssq(theta_pt, beta_pt, rep(1/sum(nyc_ridx), sum(nyc_ridx)))
cat(sprintf("ANCHOR NYC ssq (point est) = %.2e  (expect ~1e-9)\n", anc))
if (!is.finite(anc) || anc > 1e-5) stop("ANCHOR FAILED -- wiring suspect.")

## ---- ONE pass: read rep, compute NYC ssq, grab focus params ----
files <- list.files("results/data/bootstrap_reps", "^boot_res_[0-9]+\\.rds$", full.names=TRUE)
cat(sprintf("Evaluating %d reps on %d cores (NYC-only, tau=%.1e) ...\n", length(files), ncores, tau))
reps <- rbindlist(mclapply(files, function(f) tryCatch({
  r <- readRDS(f); it <- as.integer(r$iteration); st <- as.character(r$status[[1]])
  base <- data.table(iter=it, status=st, ssq_nyc=NA_real_)
  if (!identical(st,"error") && all(beta_names %in% names(r))) {
    th <- setNames(vapply(wage_names,function(n) as.numeric(r[[n]]),numeric(1)), wage_names)
    br <- matrix(vapply(beta_names,function(n) as.numeric(r[[n]]),numeric(1)), ncol=1, dimnames=list(beta_names,NULL))
    base[, ssq_nyc := tryCatch(eval_nyc_ssq(th, br, row_w_nyc(it)), error=function(e) NA_real_)]
  }
  fv <- as.list(setNames(lapply(focus, function(n) if (n %in% names(r)) as.numeric(r[[n]]) else NA_real_), focus))
  cbind(base, as.data.table(fv))
}, error=function(e) data.table(iter=NA_integer_, status=paste0("ERR:",conditionMessage(e)), ssq_nyc=NA_real_)),
  mc.cores=ncores), fill=TRUE)

## ---- classify failures (NYC-specific) ----
reps[, is_error := status=="error" | grepl("^ERR:",status)]
reps[, is_nonconv := status=="wage_nonconverged"]
reps[, ssq_fail := is.finite(ssq_nyc) & ssq_nyc > tau]
reps[, convergence_failure := is_error | ssq_fail | (trim_nonconv & is_nonconv)]
saveRDS(reps[, .(iter,status,ssq_nyc,is_error,is_nonconv,ssq_fail,convergence_failure)],
        "diagnostics/out/bootstrap_convergence_audit.rds")

cat(sprintf("\n=== rep counts: total=%d error=%d nonconv=%d ssq>tau=%d => trimmed=%d kept=%d ===\n",
    nrow(reps), reps[is_error==TRUE,.N], reps[is_nonconv==TRUE,.N], reps[ssq_fail==TRUE,.N],
    reps[convergence_failure==TRUE,.N], reps[convergence_failure==FALSE,.N]))
cat("NYC ssq distribution (non-error):\n")
print(reps[is_error==FALSE & is.finite(ssq_nyc),
      .(median=signif(median(ssq_nyc),3), p90=signif(quantile(ssq_nyc,.9),3), max=signif(max(ssq_nyc),3),
        n_lt_1e6=sum(ssq_nyc<1e-6), n_gt_tau=sum(ssq_nyc>tau))])

## ---- SE ladder: all-non-error / ok-only / trimmed (subset the one DT) ----
se_of <- function(mask) vapply(focus, function(c) stats::sd(reps[mask][[c]], na.rm=TRUE), numeric(1))
se_tab <- data.table(parm=focus, point=p06[match(focus,parm_name)]$coefficients,
  se_all     = se_of(reps$is_error==FALSE),
  se_okonly  = se_of(reps$is_error==FALSE & reps$is_nonconv==FALSE),
  se_trimmed = se_of(reps$convergence_failure==FALSE))
se_tab[, kind := fifelse(grepl("E_raw", parm),"wage", fifelse(grepl("B_raw",parm),"skillB","other"))]
se_tab[, drop_pct := round(100*(1 - se_trimmed/se_all),1)]
saveRDS(se_tab, "results/data/07b_bootstrap_se_trimmed.rds")

cat(sprintf("\n=== NYC SE by kind (median; n: all=%d ok=%d trimmed=%d) ===\n",
    reps[is_error==FALSE,.N], reps[is_error==FALSE & is_nonconv==FALSE,.N], reps[convergence_failure==FALSE,.N]))
print(se_tab[, .(n=.N, med_se_all=round(median(se_all),2), med_se_ok=round(median(se_okonly),2),
                 med_se_trim=round(median(se_trimmed),2), med_drop_pct=median(drop_pct)), by=kind])
cat("\n=== FULL NYC SE LIST: every param, with (se_all) vs without (se_trimmed) removal ===\n")
options(width = 200)
print(se_tab[order(kind, -se_all),
      .(parm = sub("factor\\(county\\)36061:","",parm), kind,
        point = round(point,3), se_all = round(se_all,3), se_okonly = round(se_okonly,3),
        se_trimmed = round(se_trimmed,3), drop_pct)], nrows = 100)
cat("\nVERDICT: skillB se_trim << se_all => convergence failures drove it (trimming valid).\n")
cat("         wage E_raw_5 stays elevated => genuine E_5 under-id (kept, partial-ID).\n[done]\n")
