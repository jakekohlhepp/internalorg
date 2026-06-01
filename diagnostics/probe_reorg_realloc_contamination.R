## Read-only diagnostic. Answers two questions about 14_counterfactual_diffusion.R:
##   (Q1) At the SAME LA-reorg warm-start wages, does the production reorg
##        residual differ from the smoke's make_solve_wages residual?
##   (Q2) Does running the realloc loop first change the reorg residual at the
##        warm start (i.e. does realloc contaminate reorg via shared state)?
## Strategy: eval 14's actual setup lines verbatim (no transcription drift),
## then instrument. Saves nothing canonical.
suppressPackageStartupMessages(library(data.table))

code <- readLines("14_counterfactual_diffusion.R")
## Lines 1..183 = full setup: load context, helper closures, diffusion shock
## (line 139), solve_reloc, solve_wages, solve_wage_abs. Line 185 starts the
## realloc loop. Verify the boundary so we don't accidentally run a solve.
stopifnot(grepl("solve_wage_abs", code[183]))
stopifnot(grepl("for \\(cnty in CONFIG\\$counties\\)", code[185]))
eval(parse(text = paste(code[1:183], collapse = "\n")), envir = globalenv())

LA  <- "6037"
QY  <- get_counterfactual_focus_quarter()[1]
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)

mxr  <- function(g) max(abs(g))
sumr <- function(g) sum(abs(g))

## ---- The exact LA-reorg warm-start wages production uses -------------------
warm_reorg_la <- counterfactual_warm_start_by_sol_type(warm_table, LA, QY, "reorg", n_worker_types)
cat("\n==== WARM START (LA reorg) ====\n")
cat("prod warm_reorg wages :", paste(signif(as.numeric(warm_reorg_la), 7), collapse = ", "), "\n")

## smoke extraction of the same row (independent path)
.tbl <- read_counterfactual_warm_start_table("14_warm_start_wages_diffusion.rds")
.row <- .tbl[county == LA & quarter_year == QY & sol_type == "reorg"]
smoke_warm <- as.numeric(.row[, paste0("w", seq_len(n_worker_types)), with = FALSE])
cat("smoke warm wages      :", paste(signif(smoke_warm, 7), collapse = ", "), "\n")
cat("warm wages identical? :", isTRUE(all.equal(as.numeric(warm_reorg_la), smoke_warm)), "\n")

## ---- Q1: production reorg residual BEFORE any realloc solve ----------------
cnty <- LA; qy <- QY
wd_pre  <- copy(working_data)
gl_pre  <- working_data[county == LA & quarter_year == QY, gamma_invert]
tl_pre  <- as.numeric(total_labor[county == LA & quarter_year == QY, ..tot_field_names])
gap0 <- solve_wages(warm_reorg_la)
cat("\n==== Q1: PROD reorg residual at warm start, BEFORE realloc loop ====\n")
cat("components:", paste(signif(gap0, 4), collapse = ", "), "\n")
cat(sprintf("max|r|=%.6g   sum|r|=%.6g\n", mxr(gap0), sumr(gap0)))

## ---- Build the smoke's make_solve_wages with the SAME context --------------
n_t <- n_task_types; n_w <- n_worker_types
make_solve_wages <- function(working_data, market_parms, total_labor, rho) {
  function(wage_guess) {
    counter_res <- copy(working_data[county == LA & quarter_year == QY, ..market_input_cols])
    new_theta <- matrix(market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
                        ncol = n_t, nrow = n_w, byrow = FALSE)
    w_mat <- matrix(wage_guess, ncol = n_t, nrow = n_w, byrow = FALSE)
    new_tild_theta <- w_mat + (rho[LA])^(-1) * new_theta
    new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))
    output_fields <- c("c_endog", "q_endog", e_field_names)
    counter_res[, (output_fields) := counterfactual_org_outputs(
        cost_matrix = new_tild_theta, alpha = as.numeric(.SD), gamma = gamma_invert,
        wage_guess = wage_guess, new_theta = new_theta, innertol = innertol, config = CONFIG
      )[c("c_endog", "q_endog", e_field_names)],
      by = c("location_id"), .SDcols = task_mix_cols]
    counter_res[, Q := q_endog * avg_labor + qual_exo]
    counter_res[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
    counter_res[, newprice := counterfactual_best_response_prices(cust_price, Q, C, weight, rho[LA], outertol, paste(LA, QY))]
    counter_res[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[LA])]
    new_total_labor <- counter_res[, setNames(
      lapply(seq_len(n_w), function(idx) sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)),
      tot_field_names)]
    as.numeric(counterfactual_labor_gap(new_total_labor, total_labor, LA, QY))
  }
}
smoke_solve <- make_solve_wages(working_data, market_parms, total_labor, rho)
gapS <- smoke_solve(warm_reorg_la)
cat("\n==== Q1: SMOKE make_solve_wages residual at warm start (same context) ====\n")
cat("components:", paste(signif(gapS, 4), collapse = ", "), "\n")
cat(sprintf("max|r|=%.6g   sum|r|=%.6g\n", mxr(gapS), sumr(gapS)))
cat(sprintf("PROD vs SMOKE identical at warm? %s  (diff max|r|=%.3g)\n",
            isTRUE(all.equal(as.numeric(gap0), as.numeric(gapS))), mxr(gap0 - gapS)))

## ---- Q2: run the realloc loop, then re-evaluate reorg residual -------------
cat("\n==== Q2: running realloc loop (lines 185-209) ... ====\n")
eval(parse(text = paste(code[185:209], collapse = "\n")), envir = globalenv())

cnty <- LA; qy <- QY
gap1 <- solve_wages(warm_reorg_la)
gl_post <- working_data[county == LA & quarter_year == QY, gamma_invert]
tl_post <- as.numeric(total_labor[county == LA & quarter_year == QY, ..tot_field_names])
cat("\n==== Q2: PROD reorg residual at warm start, AFTER realloc loop ====\n")
cat("components:", paste(signif(gap1, 4), collapse = ", "), "\n")
cat(sprintf("max|r|=%.6g   sum|r|=%.6g\n", mxr(gap1), sumr(gap1)))
cat(sprintf("BEFORE vs AFTER realloc identical? %s  (diff max|r|=%.3g)\n",
            isTRUE(all.equal(as.numeric(gap0), as.numeric(gap1))), mxr(gap0 - gap1)))

cat("\n==== shared-state mutation checks (realloc loop) ====\n")
cat("working_data unchanged by realloc loop? :", isTRUE(all.equal(wd_pre, working_data)), "\n")
cat("LA gamma_invert unchanged?              :", isTRUE(all.equal(gl_pre, gl_post)), "\n")
cat("LA total_labor target unchanged?        :", isTRUE(all.equal(tl_pre, tl_post)), "\n")
cat("LA total_labor target:", paste(signif(tl_pre, 6), collapse = ", "), "\n")

## ---- Q3: run the ACTUAL production reorg multistart solve call for LA ------
cat("\n==== Q3: residual of EACH start under solve_wages (the reorg closure) ====\n")
realloc_start <- as.numeric(unlist(res_wages[county == LA & quarter_year == QY & sol_type == "realloc",
                            paste0("w", seq_len(n_worker_types)), with = FALSE], use.names = FALSE))
baseline_start <- as.numeric(unlist(initial_wages[county == LA & quarter_year == QY,
                            paste0("w", seq_len(n_worker_types)), with = FALSE], use.names = FALSE))
cnty <- LA; qy <- QY
cat(sprintf("warm    : %s  -> max|r|=%.6g\n", paste(signif(warm_reorg_la,6),collapse=", "), mxr(solve_wages(warm_reorg_la))))
cat(sprintf("realloc : %s  -> max|r|=%.6g\n", paste(signif(realloc_start,6),collapse=", "), mxr(solve_wages(realloc_start))))
cat(sprintf("baseline: %s  -> max|r|=%.6g\n", paste(signif(baseline_start,6),collapse=", "), mxr(solve_wages(baseline_start))))
## the extra perturbed multistarts counterfactual_multistarts() injects:
for (m in list(c("x0.75",0.75), c("x1.25",1.25))) {
  s <- as.numeric(warm_reorg_la) * as.numeric(m[2])
  cat(sprintf("warm %s : -> max|r|=%.6g\n", m[1], mxr(solve_wages(s))))
}
s_shift <- pmax(as.numeric(warm_reorg_la) + c(-5,-2,0,2,5), CONFIG$numeric_floor)
cat(sprintf("warm +(-5,-2,0,2,5): -> max|r|=%.6g\n", mxr(solve_wages(s_shift))))

cat("\n[skipping the slow real solve call; per-start residuals above are the diagnostic]\n")

cat("\nDONE.\n")
