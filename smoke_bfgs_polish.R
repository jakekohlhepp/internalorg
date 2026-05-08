## Smoke test: after Nelder-Mead stalls (degenerate simplex), does a
## quasi-Newton polish (BFGS / L-BFGS-B with numerical gradients) reduce the
## per-county wage moment ssq?  Compares three solves on each county:
##
##   (a) starting ssq at the seeit_bb.rds vector
##   (b) Nelder-Mead with the current min_optim_parscale_wage_by_county settings
##   (c) BFGS polish from wherever NM exited
##
## Read-only: does NOT overwrite seeit_bb.rds or results/data/06_parameters.rds.

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

data <- as.data.frame(estim_matrix)
full_par <- beta_2_subset

build_county_objective <- function(cnty) {
  county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
  par_idx <- grepl(county_pattern, names(full_par), fixed = TRUE)
  row_idx <- as.character(data$county) == cnty
  x_county <- data[row_idx, , drop = FALSE]
  function(parms) {
    candidate <- full_par
    candidate[par_idx] <- parms
    moments <- weighted_col_means(objective_gmm(
      theta = candidate,
      x = x_county,
      beta = beta,
      beta_2_subset = beta_2_subset,
      config = CONFIG,
      clust = NULL,
      solver_state = NULL
    ), weights = NULL)
    sub <- grepl(paste0("county", cnty, ":E_"), names(moments), fixed = TRUE)
    sum(as.numeric(moments[sub])^2)
  }
}

results <- list()
for (cnty in CONFIG$counties) {
  cnty_key <- as.character(cnty)
  parscale_w <- if (!is.null(CONFIG$min_optim_parscale_wage_by_county[[cnty_key]])) {
    CONFIG$min_optim_parscale_wage_by_county[[cnty_key]]
  } else {
    CONFIG$min_optim_parscale_wage
  }
  county_pattern <- paste0("factor(county)", cnty, ":avg_labor:E_raw_")
  par_idx <- grepl(county_pattern, names(full_par), fixed = TRUE)
  start_par <- full_par[par_idx]
  fn <- build_county_objective(cnty)

  cat("\n========== county ", cnty, " (parscale=", parscale_w, ") ==========\n", sep = "")
  ssq_start <- fn(start_par)
  cat("(a) starting ssq:        ", signif(ssq_start, 6), "\n", sep = "")

  nm <- optim(
    par = start_par, fn = fn, method = "Nelder-Mead",
    control = list(maxit = CONFIG$min_optim_maxit, reltol = CONFIG$min_optim_reltol,
                   parscale = rep(parscale_w, length(start_par)), trace = 0)
  )
  cat("(b) NM final ssq:        ", signif(nm$value, 6),
      "  (convergence=", nm$convergence, ", evals=", nm$counts[1], ")\n", sep = "")

  bfgs <- tryCatch(
    optim(par = nm$par, fn = fn, method = "BFGS",
          control = list(maxit = 200, reltol = CONFIG$obj_tol,
                         parscale = rep(parscale_w, length(start_par)), trace = 0)),
    error = function(e) list(value = NA, convergence = -1, message = conditionMessage(e))
  )
  cat("(c) BFGS polish ssq:     ", signif(bfgs$value, 6),
      "  (convergence=", bfgs$convergence, ")\n", sep = "")

  lbfgs <- tryCatch(
    optim(par = nm$par, fn = fn, method = "L-BFGS-B",
          control = list(maxit = 200, factr = 1e7,
                         parscale = rep(parscale_w, length(start_par)), trace = 0)),
    error = function(e) list(value = NA, convergence = -1, message = conditionMessage(e))
  )
  cat("(d) L-BFGS-B polish ssq: ", signif(lbfgs$value, 6),
      "  (convergence=", lbfgs$convergence, ")\n", sep = "")

  cat("\nNM   par: ", paste(round(nm$par, 2), collapse = ", "), "\n")
  if (!is.null(bfgs$par)) cat("BFGS par: ", paste(round(bfgs$par, 2), collapse = ", "), "\n")
  if (!is.null(lbfgs$par)) cat("LB   par: ", paste(round(lbfgs$par, 2), collapse = ", "), "\n")

  results[[cnty_key]] <- list(start = ssq_start, nm = nm$value,
                              bfgs = bfgs$value, lbfgs = lbfgs$value)
}

cat("\n========== SUMMARY ==========\n")
for (cnty_key in names(results)) {
  r <- results[[cnty_key]]
  cat(sprintf("county %-6s start=%-10.4g NM=%-10.4g BFGS=%-10.4g L-BFGS-B=%-10.4g\n",
              cnty_key, r$start, r$nm, r$bfgs, r$lbfgs))
}
