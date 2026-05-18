## Driver to run scripts 09 through 19 sequentially against the current
## results/data/06_parameters.rds, capture any errors, and summarise
## counterfactual-solver convergence at the end. Single R session so
## inner-solver caches stay warm.

scripts <- c(
  "09_invert_gammas.R",
  "10_substitution.R",
  "11_substitution_prod.R",
  "12_validate.R",
  "13_counterfactual_prep.R",
  "14_counterfactual_diffusion.R",
  "15_counterfactual_sales_tax.R",
  "16_counterfactual_immigration.R",
  "17_counterfactual_merger.R",
  "18_counterfactual_summary.R",
  "19_counterfactual_figures.R"
)

results <- list()
overall_t0 <- Sys.time()
for (s in scripts) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("RUNNING ", s, "  start = ", format(Sys.time(), "%H:%M:%S"), "\n", sep = "")
  cat(strrep("=", 70), "\n")
  t0 <- Sys.time()
  ok <- TRUE
  err_msg <- NA_character_
  warning_msgs <- character(0)

  withCallingHandlers(
    tryCatch(
      source(s, local = new.env(parent = globalenv())),
      error = function(e) {
        ok <<- FALSE
        err_msg <<- conditionMessage(e)
        message("ERROR in ", s, ": ", err_msg)
      }
    ),
    warning = function(w) {
      warning_msgs <<- c(warning_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  dt <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(sprintf("\n[%s] %s   elapsed = %.2f min\n",
              if (ok) "OK   " else "ERROR",
              s, dt))
  if (length(warning_msgs) > 0) {
    cat("  warnings (", length(warning_msgs), "):\n", sep = "")
    for (w in head(unique(warning_msgs), 10)) {
      cat("    - ", substr(w, 1, 200), "\n", sep = "")
    }
    if (length(unique(warning_msgs)) > 10) {
      cat("    ... and ", length(unique(warning_msgs)) - 10, " more unique warning(s)\n", sep = "")
    }
  }
  results[[s]] <- list(ok = ok, error = err_msg, n_warn = length(warning_msgs),
                       seconds = dt * 60, warnings = warning_msgs)
  if (!ok) {
    cat("Pipeline halted at ", s, "; remaining scripts not attempted.\n", sep = "")
    break
  }
}
overall_dt <- as.numeric(difftime(Sys.time(), overall_t0, units = "mins"))

## ---------- summary ----------
cat("\n\n", strrep("#", 70), "\n", sep = "")
cat("PIPELINE SUMMARY  (total = ", round(overall_dt, 2), " min)\n", sep = "")
cat(strrep("#", 70), "\n")
cat(sprintf("%-35s  %-6s  %-8s  %s\n", "script", "ok", "minutes", "warns"))
for (s in scripts) {
  if (is.null(results[[s]])) {
    cat(sprintf("%-35s  %-6s  %-8s  %s\n", s, "SKIP", "-", "-"))
  } else {
    cat(sprintf("%-35s  %-6s  %-8.2f  %d\n",
                s,
                if (results[[s]]$ok) "OK" else "FAIL",
                results[[s]]$seconds / 60,
                results[[s]]$n_warn))
  }
}

## ---------- counterfactual convergence diagnostics ----------
cat("\n", strrep("-", 70), "\n", sep = "")
cat("COUNTERFACTUAL WAGE SOLVER CONVERGENCE DIAGNOSTICS\n")
cat(strrep("-", 70), "\n")

scan_wage_table <- function(rds_path, label) {
  if (!file.exists(rds_path)) {
    cat(label, ": [file not found: ", rds_path, "]\n", sep = "")
    return(invisible(NULL))
  }
  dt <- tryCatch(readRDS(rds_path), error = function(e) NULL)
  if (is.null(dt)) {
    cat(label, ": [read failed]\n", sep = "")
    return(invisible(NULL))
  }
  if (!is.data.frame(dt)) {
    cat(label, ": [not a data.frame, skipping]\n", sep = "")
    return(invisible(NULL))
  }
  required <- c("county", "quarter_year")
  if (!all(required %in% names(dt))) {
    cat(label, ": [missing county/quarter columns]\n", sep = "")
    return(invisible(NULL))
  }
  has_conv  <- "converged" %in% names(dt)
  has_resid <- "fval" %in% names(dt)
  cat("\n", label, "  (rows = ", nrow(dt), ")\n", sep = "")
  cat(sprintf("%-8s %-9s %-10s %-12s %-9s %s\n",
              "county", "quarter", "sol_type",
              "fval (resid)", "converged", "method"))
  cols_present <- c("county", "quarter_year",
                    if ("sol_type" %in% names(dt)) "sol_type" else NULL,
                    if (has_resid) "fval" else NULL,
                    if (has_conv) "converged" else NULL,
                    if ("method" %in% names(dt)) "method" else NULL,
                    if ("target_tol" %in% names(dt)) "target_tol" else NULL)
  for (i in seq_len(nrow(dt))) {
    row <- dt[i, ]
    cat(sprintf("%-8s %-9s %-10s %-12s %-9s %s\n",
                as.character(row$county),
                as.character(row$quarter_year),
                if ("sol_type" %in% names(dt)) as.character(row$sol_type) else "-",
                if (has_resid) format(row$fval, digits = 4) else "-",
                if (has_conv) as.character(row$converged) else "-",
                if ("method" %in% names(dt)) as.character(row$method) else "-"))
  }
  if (has_conv) {
    n_total <- nrow(dt)
    n_ok    <- sum(dt$converged, na.rm = TRUE)
    cat(sprintf("=> %d / %d rows converged (target_tol = %s)\n",
                n_ok, n_total,
                if ("target_tol" %in% names(dt)) format(unique(dt$target_tol), digits = 4) else "n/a"))
  }
}

cf_data_dir <- if (!is.null(get0("CONFIG"))) CONFIG$counterfactual_data_dir else file.path("results", "data", "counterfactuals")
cat("counterfactual data dir =", cf_data_dir, "\n")

candidates <- c(
  baseline           = file.path(cf_data_dir, "13_initial_wages.rds"),
  diffusion          = file.path(cf_data_dir, "14_wages_diffusion.rds"),
  sales_tax          = file.path(cf_data_dir, "15_wages_salestax.rds"),
  immigration        = file.path(cf_data_dir, "16_wages_immigration.rds"),
  merger             = file.path(cf_data_dir, "17_wages_merger.rds")
)
for (nm in names(candidates)) {
  scan_wage_table(candidates[[nm]], nm)
}
