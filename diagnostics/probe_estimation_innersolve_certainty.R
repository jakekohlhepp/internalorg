## CERTAINTY PROBE: does the estimation-side inner SQUAREM assignment solve
## hit the fixedpoint_max_iter cap or pseudo-converge at the gammas the gamma
## inversion / GMM objective actually evaluates?
##
## Context: 09_invert_gammas.R inverts gamma per firm by bisection on [1, 10000]
## with a low test point of gamma = 0.1 (config$bisection_low_test_point), inner
## SQUAREM at innertol = CONFIG$innertol (1e-10), maxiter = 100000. The same
## bisection runs inside find_gamma_for_sindex during the 06 GMM objective.
## So the inner solve is evaluated at gamma as low as 0.1.
##
## Part A: gamma_invert distribution (09 estimation output) + counterfactual
##         working_data, overall and for LA 2021.2.
## Part B: per-firm inner-solve stress test for LA (6037) interior firms at
##         gamma in {0.1, 1.0, estimated}, production (innertol=1e-10,
##         maxiter=100000) vs reference (innertol=1e-12, maxiter=1e6). Reports
##         convergence flags, fpevals, max|dE|, and |d s_index| (the quantity
##         the bisection matches, so divergence => contaminated gamma).
##
## Output: smoke_estimation_innersolve_certainty.rds (gitignored)
suppressPackageStartupMessages({
  library("data.table")
  library("parallel")
  library("SQUAREM")
})

source("config.R")

n_w <- CONFIG$n_worker_types
n_t <- CONFIG$n_task_types

spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))

## ---------------------------------------------------------------- tild_theta
all_results  <- readRDS(file.path("results", "data", "06_parameters.rds"))
market_parms <- all_results$coefficients
names(market_parms) <- all_results$parm_name

tild_theta <- vector(mode = "list", length = length(CONFIG$counties))
names(tild_theta) <- CONFIG$counties
for (cnty in names(tild_theta)) {
  w_mat <- matrix(c(0, market_parms[grep(paste0(cnty, ":avg_labor:E"), names(market_parms))]),
                  ncol = n_t, nrow = n_w, byrow = FALSE)
  skills <- matrix(market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
                   ncol = n_t, nrow = n_w, byrow = FALSE)
  rho <- market_parms[grep(paste0(cnty, ":cust_price$"), names(market_parms))]
  tt <- w_mat + (rho)^(-1) * skills
  tild_theta[[cnty]] <- sweep(tt, 2, apply(tt, 2, min))
}

## ---------------------------------------------------------------- Part A
fu <- as.data.table(readRDS(file.path("results", "data", "09_withgammas.rds")))
fu[!(county %in% names(tild_theta)) & county == "06037", county := "6037"]

interior <- fu[is.finite(gamma_invert) & gamma_invert > 0]
cat("==================== Part A: gamma_invert distribution ====================\n")
cat("09_withgammas total firm-quarters: ", nrow(fu), "\n", sep = "")
cat("  gamma == 0   (s_index >= s_bound corner): ", sum(fu$gamma_invert == 0, na.rm = TRUE), "\n", sep = "")
cat("  gamma == Inf (s_index == 0 corner):       ", sum(is.infinite(fu$gamma_invert)), "\n", sep = "")
cat("  interior (0 < gamma < Inf):               ", nrow(interior), "\n", sep = "")
qs <- c(0, .001, .01, .05, .25, .5)
cat("  interior gamma quantiles (min/.1%/1%/5%/25%/50%): ",
    paste(sprintf("%.4f", quantile(interior$gamma_invert, qs)), collapse = " / "), "\n", sep = "")
cat("  interior gamma below 1.0:   ", sum(interior$gamma_invert < 1.0), "\n", sep = "")
cat("  interior gamma below 1.5:   ", sum(interior$gamma_invert < 1.5), "\n", sep = "")
cat("  interior gamma below 2.0:   ", sum(interior$gamma_invert < 2.0), "\n", sep = "")

la_int <- interior[county == "6037" & quarter_year == 2021.2]
cat("\nLA (6037) 2021.2 interior firm-quarters: ", nrow(la_int), "\n", sep = "")
if (nrow(la_int) > 0) {
  cat("  LA 2021.2 gamma quantiles (min/.1%/1%/5%/25%/50%): ",
      paste(sprintf("%.4f", quantile(la_int$gamma_invert, qs)), collapse = " / "), "\n", sep = "")
}

## counterfactual working_data gamma (what 14 actually shocks)
cf_path <- file.path("results", "data", "counterfactuals", "13_working_data.rds")
if (file.exists(cf_path)) {
  cf_wd <- as.data.table(readRDS(cf_path))
  cf_la <- cf_wd[county == "6037" & quarter_year == 2021.2 &
                 is.finite(gamma_invert) & gamma_invert > 0]
  cat("\nCounterfactual 13_working_data LA 2021.2 interior firms: ", nrow(cf_la), "\n", sep = "")
  if (nrow(cf_la) > 0) {
    cat("  gamma quantiles (min/.1%/1%/5%/25%/50%): ",
        paste(sprintf("%.4f", quantile(cf_la$gamma_invert, qs)), collapse = " / "), "\n", sep = "")
    cat("  min gamma firm: ", sprintf("%.6f", min(cf_la$gamma_invert)), "\n", sep = "")
  }
}

## ---------------------------------------------------------------- Part B
task_mix_cols <- paste0("task_mix_", seq_len(n_t))
la_firms <- fu[county == "6037" & quarter_year == 2021.2 &
               is.finite(gamma_invert) & gamma_invert > 0]
cat("\n==================== Part B: LA inner-solve stress test ====================\n")
cat("LA interior firms to stress: ", nrow(la_firms), "\n", sep = "")

inner_solve <- function(alpha, gamma, county, tol, maxiter) {
  A <- exp(-1 / gamma * tild_theta[[county]])
  A[A >= Inf] <- CONFIG$numeric_ceiling
  A[A <= 0]   <- CONFIG$numeric_floor
  fxpt <- function(p) {
    C <- colSums(t(A) * alpha / colSums(A * p))
    p * C
  }
  res <- squarem(rep(0.2, n_w), fixptfn = fxpt,
                 control = list(maxiter = maxiter, tol = tol))
  E <- res$par
  B <- t(t(A) * alpha / colSums(A * E)) * E
  B[abs(B) < CONFIG$B_zero_threshold] <- 0
  Brel <- t(t(B / rowSums(B)) / alpha)
  s_index <- sum(B * spec_log(Brel))
  list(E = rowSums(B), s_index = s_index,
       fpevals = res$fpevals, convergence = as.integer(res$convergence))
}

PROD_TOL <- CONFIG$innertol      # 1e-10
PROD_MAX <- CONFIG$fixedpoint_max_iter  # 100000
REF_TOL  <- 1e-12
REF_MAX  <- 1000000L             # 10x cap

stress_gammas <- c(g0.1 = 0.1, g1.0 = 1.0)

n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "16"))
cat("Cores: ", n_cores, "\n", sep = "")

run_firm <- function(i) {
  alpha <- as.numeric(la_firms[i, ..task_mix_cols])
  county <- la_firms$county[i]
  est_gamma <- la_firms$gamma_invert[i]
  gammas <- c(stress_gammas, est = est_gamma)
  rows <- lapply(names(gammas), function(gname) {
    g <- gammas[[gname]]
    prod_ <- tryCatch(inner_solve(alpha, g, county, PROD_TOL, PROD_MAX),
                      error = function(e) NULL)
    ref_  <- tryCatch(inner_solve(alpha, g, county, REF_TOL, REF_MAX),
                      error = function(e) NULL)
    if (is.null(prod_) || is.null(ref_)) {
      return(data.table(idx = i, gname = gname, gamma = g,
                        prod_fpevals = NA, prod_conv = NA,
                        ref_fpevals = NA, ref_conv = NA,
                        maxabs_dE = NA, ds_index = NA))
    }
    data.table(idx = i, gname = gname, gamma = g,
               prod_fpevals = prod_$fpevals, prod_conv = prod_$convergence,
               ref_fpevals = ref_$fpevals, ref_conv = ref_$convergence,
               maxabs_dE = max(abs(prod_$E - ref_$E)),
               ds_index = prod_$s_index - ref_$s_index)
  })
  rbindlist(rows)
}

t0 <- Sys.time()
res <- rbindlist(mclapply(seq_len(nrow(la_firms)), run_firm,
                          mc.cores = n_cores, mc.preschedule = FALSE))
cat("Stress test done in ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n", sep = "")

cap <- PROD_MAX
for (gn in c("g0.1", "g1.0", "est")) {
  sub <- res[res$gname == gn]
  cat("\n--- gamma stress = ", gn, " ---\n", sep = "")
  cat("  firms: ", nrow(sub), "\n", sep = "")
  cat("  production conv != 0 (FAILED):        ", sum(sub$prod_conv != 0, na.rm = TRUE), "\n", sep = "")
  cat("  production fpevals >= cap (",cap,"):   ", sum(sub$prod_fpevals >= cap, na.rm = TRUE), "\n", sep = "")
  cat("  pseudo-converged (prod_conv==0 & maxabs_dE > 1e-3): ",
      sum(sub$prod_conv == 0 & sub$maxabs_dE > 1e-3, na.rm = TRUE), "\n", sep = "")
  cat("  max(maxabs_dE) across firms:          ", sprintf("%.4g", max(sub$maxabs_dE, na.rm = TRUE)), "\n", sep = "")
  cat("  firms with |d s_index| > outertol(1e-8): ",
      sum(abs(sub$ds_index) > 1e-8, na.rm = TRUE), "\n", sep = "")
  cat("  max |d s_index|:                      ", sprintf("%.4g", max(abs(sub$ds_index), na.rm = TRUE)), "\n", sep = "")
}

cat("\nWorst 10 firms by maxabs_dE (any gamma):\n")
print(res[order(-maxabs_dE)][1:10,
          .(idx, gname, gamma, prod_fpevals, prod_conv, ref_fpevals, ref_conv,
            maxabs_dE, ds_index)])

saveRDS(list(part_a_interior = interior[, .(county, quarter_year, gamma_invert)],
             stress = res,
             prod_tol = PROD_TOL, prod_max = PROD_MAX,
             ref_tol = REF_TOL, ref_max = REF_MAX),
        "smoke_estimation_innersolve_certainty.rds")
cat("\nSaved: smoke_estimation_innersolve_certainty.rds\n")
