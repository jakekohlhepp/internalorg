#' =============================================================================
#' STEP 06c: Wage-stage local identification diagnostic
#' =============================================================================
#' Computes the per-county Hessian of the moment ssq at the current wage
#' solution, eigendecomposes, and -- if the smallest eigenvalue falls below
#' a threshold -- runs a perturbation test along that direction to determine
#' whether the solution is a strict local minimum (gradient-near-zero
#' polishers cannot improve) or a shallow saddle (perturbation + polish does).
#'
#' Inputs:
#'   - mkdata/data/04_estimation_sample.rds
#'   - mkdata/data/seeit_bb.rds  (current wage block)
#'
#' Outputs:
#'   - results/data/06c_wage_identification.rds (raw eigen + perturbation data)
#'   - results/out/tables/06c_wage_eigenvalues.tex (spectrum + verdict)
#'   - results/out/tables/06c_wage_perturbation.tex (saddle-confirmation tests)
#' =============================================================================

library("data.table")
library("xtable")
library("numDeriv")
set.seed(4459665)

source("config.R")

estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
assert_required_files(estimation_sample_path)

estimation_sample <- readRDS(estimation_sample_path)
working_data <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix

source("preamble.R")
estimation_objects <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta <- estimation_objects$beta
beta_2 <- estimation_objects$beta_2
rm(estimation_objects)

beta_2_subset <- readRDS(file.path(CONFIG$prep_output_dir, "seeit_bb.rds"))
wage_terms <- paste0(":avg_labor:E_raw_", 2:CONFIG$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
names(beta_2_subset) <- rownames(beta_2)[wage_idx]
stopifnot(length(beta_2_subset) == sum(wage_idx))

data <- as.data.frame(estim_matrix)
full_par <- beta_2_subset

## ---------------------------------------------------------------------------
## Per-county objective (matches estimate_wage_parameters_min_optim/pso)
## ---------------------------------------------------------------------------
build_objective <- function(cnty) {
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
  list(fn = fn, par_idx = par_idx, x_star = as.numeric(full_par[par_idx]))
}

## Tunables
NEG_EIG_TOLERANCE <- 1e-9   # below this is treated as "not strictly positive"
PERTURB_STEPS <- c(-100, -50, -10, -1, 1, 10, 50, 100)

## ---------------------------------------------------------------------------
## Per-county loop
## ---------------------------------------------------------------------------
results <- list()
for (cnty in CONFIG$counties) {
  message("\n=== county ", cnty, " ===")
  ob <- build_objective(cnty)
  parscale_w <- if (!is.null(CONFIG$min_optim_parscale_wage_by_county[[as.character(cnty)]])) {
    CONFIG$min_optim_parscale_wage_by_county[[as.character(cnty)]]
  } else {
    CONFIG$min_optim_parscale_wage
  }
  ## numDeriv's `d` is a RELATIVE finite-difference fraction: genD forms the base
  ## step as h = |d*x| + eps*(|x| < zero.tol). Passing the intended ABSOLUTE step
  ## max(parscale_w*0.05, 1) (>= 1) as `d` perturbed each coordinate by 100%-1000%
  ## of its own magnitude, sampling curvature far from x* (NYC's ~1826 coord out to
  ## ~+/-18000, into the 1e6 penalty region). Use a small relative fraction so the
  ## Hessian is local; at wages O(10-1000) a 1% step is ~0.1-10 absolute units,
  ## matching the originally intended magnitude while staying properly scale-free.
  fd_rel <- 1e-2

  f_at_x_star <- ob$fn(ob$x_star)
  message(sprintf("  x* = (%s)", paste(round(ob$x_star, 3), collapse = ", ")))
  message(sprintf("  f(x*) = %.6g, rel FD step (numDeriv d) = %g", f_at_x_star, fd_rel))

  H <- numDeriv::hessian(ob$fn, ob$x_star,
                         method.args = list(d = fd_rel, eps = 1e-4,
                                            zero.tol = 1e-12, r = 4, v = 2))
  eig <- eigen(H, symmetric = TRUE)
  npos <- sum(eig$values >  NEG_EIG_TOLERANCE)
  nneg <- sum(eig$values < -NEG_EIG_TOLERANCE)
  nzer <- sum(abs(eig$values) <= NEG_EIG_TOLERANCE)

  verdict <- if (nneg > 0) {
    "saddle"
  } else if (nzer > 0 || min(eig$values) < 1e-9) {
    "ridge"
  } else {
    "local_min"
  }
  message(sprintf("  eigenvalues: %s",
                  paste(signif(eig$values, 4), collapse = ", ")))
  message(sprintf("  verdict: %s", verdict))

  county_record <- list(
    cnty = cnty, x_star = ob$x_star, f_at_x_star = f_at_x_star,
    parscale = parscale_w, step = fd_rel,
    hessian = H, eigenvalues = eig$values, eigenvectors = eig$vectors,
    npos = npos, nneg = nneg, nzero = nzer, verdict = verdict,
    perturbation = NULL
  )

  ## Perturbation test only when the smallest eigenvalue is suspicious.
  if (verdict %in% c("saddle", "ridge")) {
    smallest_idx <- which.min(eig$values)
    direction <- eig$vectors[, smallest_idx]
    message(sprintf("  perturbing along eigenvector %d (lambda = %.4g): %s",
                    smallest_idx, eig$values[smallest_idx],
                    paste(round(direction, 3), collapse = ", ")))
    pert <- data.table(step = numeric(), f_perturbed = numeric(),
                       polish_value = numeric(), polish_convergence = integer(),
                       polish_par = character())
    for (s in PERTURB_STEPS) {
      x_pert <- ob$x_star + s * direction
      f_p <- ob$fn(x_pert)
      polish <- tryCatch(
        optim(par = x_pert, fn = ob$fn, method = "Nelder-Mead",
              control = list(maxit = 5000, reltol = CONFIG$obj_tol,
                             parscale = rep(parscale_w, length(x_pert)),
                             trace = 0)),
        error = function(e) list(par = x_pert, value = NA_real_, convergence = -1L)
      )
      pert <- rbind(pert, data.table(
        step = s, f_perturbed = f_p,
        polish_value = polish$value,
        polish_convergence = polish$convergence,
        polish_par = paste(round(polish$par, 2), collapse = ", ")
      ))
      message(sprintf("    step=%+5g  f(x*+s*v)=%.6g  polish=%.6g (conv=%d)",
                      s, f_p, polish$value, polish$convergence))
    }
    best_polish <- min(pert$polish_value, na.rm = TRUE)
    saddle_confirmed <-
      is.finite(best_polish) &&
      best_polish < f_at_x_star * (1 - 1e-3)
    county_record$perturbation <- pert
    county_record$best_perturb_polish <- best_polish
    county_record$saddle_confirmed <- saddle_confirmed
    if (saddle_confirmed) {
      message(sprintf("  -> SADDLE CONFIRMED: polish from perturbation found ssq=%.6g (< x* %.6g)",
                      best_polish, f_at_x_star))
    } else {
      message(sprintf("  -> NOT CONFIRMED: best perturbed polish ssq=%.6g not materially better than x* %.6g",
                      best_polish, f_at_x_star))
    }
  }

  results[[as.character(cnty)]] <- county_record
}

ensure_directory(file.path("results", "data"))
saveRDS(results, file.path("results", "data", "06c_wage_identification.rds"))

## ---------------------------------------------------------------------------
## TeX outputs
## ---------------------------------------------------------------------------
ensure_directory(file.path("results", "out", "tables"))

format_eig <- function(v) {
  formatC(v, digits = 3, format = "g")
}
format_ssq <- function(v) {
  if (!is.finite(v)) return("--")
  if (abs(v) < 1e-10) return(formatC(v, digits = 3, format = "e"))
  formatC(v, digits = 4, format = "g")
}

verdict_label <- function(v, saddle_confirmed = NULL) {
  base <- switch(v,
                 local_min = "strict local min",
                 ridge     = "ridge / weak id",
                 saddle    = "saddle (eigenvalue $<0$)")
  if (!is.null(saddle_confirmed)) {
    if (isTRUE(saddle_confirmed)) {
      base <- paste0(base, " (perturbation confirms)")
    } else if (v != "local_min") {
      base <- paste0(base, " (perturbation does not confirm)")
    }
  }
  base
}

eig_rows <- list()
for (cnty in names(results)) {
  r <- results[[cnty]]
  eig_rows[[length(eig_rows) + 1L]] <- data.table(
    County = cnty,
    "$f(x^*)$" = format_ssq(r$f_at_x_star),
    "$\\lambda_1$ (max)" = format_eig(r$eigenvalues[1]),
    "$\\lambda_2$" = format_eig(r$eigenvalues[2]),
    "$\\lambda_3$" = format_eig(r$eigenvalues[3]),
    "$\\lambda_4$ (min)" = format_eig(r$eigenvalues[4]),
    "Cond. \\#" = if (r$npos == length(r$eigenvalues)) {
      formatC(max(r$eigenvalues) / min(r$eigenvalues), digits = 0, format = "f")
    } else {
      "--"
    },
    "Verdict" = verdict_label(r$verdict, r$saddle_confirmed)
  )
}
eig_tab <- rbindlist(eig_rows)
eig_xt <- xtable(eig_tab,
                 caption = "Per-county Hessian spectrum at the wage solution. The condition number is reported only when all eigenvalues are strictly positive. ``Verdict'' classifies the solution as a strict local minimum (all $\\lambda > 10^{-9}$), a flat ridge (smallest eigenvalue near zero), or a saddle (some $\\lambda < -10^{-9}$). A saddle that the perturbation test does not confirm should be read as a numerically flat local minimum: the negative eigenvalue is finite-difference noise, since no perturbed-and-polished point improves on $x^*$.",
                 label = "tab:wage_hessian_spectrum",
                 align = c("l", "l", rep("r", 6), "l"))
print(eig_xt, file = file.path("results", "out", "tables", "06c_wage_eigenvalues.tex"),
      include.rownames = FALSE, sanitize.text.function = identity,
      booktabs = TRUE, floating = TRUE,
      caption.placement = "top", comment = FALSE)

pert_rows <- list()
for (cnty in names(results)) {
  r <- results[[cnty]]
  if (is.null(r$perturbation)) next
  for (i in seq_len(nrow(r$perturbation))) {
    pert_rows[[length(pert_rows) + 1L]] <- data.table(
      County = cnty,
      "Step" = r$perturbation$step[i],
      "$f(x^* + s v)$" = format_ssq(r$perturbation$f_perturbed[i]),
      "Polish ssq" = format_ssq(r$perturbation$polish_value[i]),
      "Polish conv." = r$perturbation$polish_convergence[i]
    )
  }
}
if (length(pert_rows) > 0) {
  pert_tab <- rbindlist(pert_rows)
  pert_xt <- xtable(pert_tab,
                    caption = "Perturbation tests along the smallest-eigenvalue direction $v$ at each county whose Hessian is not strictly positive definite. If polishing from $x^* + s v$ for any $s$ produces a materially lower ssq than at $x^*$, the original point is a saddle and a downhill direction was missed by local solvers; otherwise the negative or near-zero eigenvalue reflects a flat ridge below numerical detection.",
                    label = "tab:wage_perturbation",
                    align = c("l", "l", "r", "r", "r", "r"))
  print(pert_xt, file = file.path("results", "out", "tables", "06c_wage_perturbation.tex"),
        include.rownames = FALSE, sanitize.text.function = identity,
        booktabs = TRUE, floating = TRUE,
        caption.placement = "top", comment = FALSE)
} else {
  ## Write a stub so the output file always exists for downstream tooling.
  cat("% No counties triggered the perturbation test (all strict local minima).\n",
      file = file.path("results", "out", "tables", "06c_wage_perturbation.tex"))
}

message("\nWrote results/data/06c_wage_identification.rds")
message("Wrote results/out/tables/06c_wage_eigenvalues.tex")
message("Wrote results/out/tables/06c_wage_perturbation.tex")
