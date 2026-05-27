## Probe each cf script's "λ=0 = unshocked" baseline.
## For each of the four cfs, parse-eval the script up to the first for-loop,
## apply the cf-specific apply_shock(0) (reverting the shock), then call
## solve_wages(initial_wages_LA). Compare against a clean unshocked
## reference (13_initial_wages residual).
##
## Hypothesis: salestax/immigration give residual = 0 at apply_shock(0) but
## diffusion/merger do not. Find out why.

suppressPackageStartupMessages(library(data.table))
source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types

probe_cf <- function(CF) {
  cat(sprintf("\n========== probe_cf: %s ==========\n", CF))

  cf_script <- switch(CF,
    diffusion   = "14_counterfactual_diffusion.R",
    salestax    = "15_counterfactual_sales_tax.R",
    immigration = "16_counterfactual_immigration.R",
    merger      = "17_counterfactual_merger.R"
  )

  pristine_total_labor <- NULL
  if (CF == "immigration") {
    ctx_pristine <- load_counterfactual_context()
    pristine_total_labor <- copy(ctx_pristine$total_labor)
    rm(ctx_pristine); gc()
  }

  ## Use a private envir to avoid cross-contamination between cf probes.
  env <- new.env(parent = globalenv())

  exprs <- parse(cf_script)
  .is_for <- function(e) is.call(e) && length(e) > 0 && as.character(e[[1]]) == "for"
  .first_for <- which(vapply(seq_along(exprs),
                             function(i) .is_for(exprs[[i]]), logical(1)))[1]
  stopifnot(is.finite(.first_for))
  for (i in seq_len(.first_for - 1L)) eval(exprs[[i]], envir = env)

  env$cnty <- "6037"; env$qy <- 2021.2

  solve_wages_la <- env$solve_wages
  init_wages_dt  <- env$initial_wages
  baseline_wages <- as.numeric(unlist(
    init_wages_dt[county == "6037" & quarter_year == 2021.2,
                  paste0("w", seq_len(n_w)), with = FALSE],
    use.names = FALSE))

  cat(sprintf("  baseline_wages = [%s]\n",
              paste(round(baseline_wages, 4), collapse = ", ")))

  ## Check working_data$gamma_invert and weight at LA pre-apply_shock.
  wd_la <- env$working_data[county == "6037" & quarter_year == 2021.2,
                            .(gamma_invert, weight)]
  cat(sprintf("  POST-EVAL working_data gamma_invert range LA = [%.4g, %.4g]  mean=%.4g\n",
              min(wd_la$gamma_invert), max(wd_la$gamma_invert), mean(wd_la$gamma_invert)))
  cat(sprintf("  POST-EVAL working_data weight        range LA = [%.4g, %.4g]  mean=%.4g\n",
              min(wd_la$weight), max(wd_la$weight), mean(wd_la$weight)))

  if (!is.null(env$orig_data)) {
    od_la <- env$orig_data[county == "6037" & quarter_year == 2021.2,
                           .(gamma_invert, weight)]
    cat(sprintf("  orig_data        gamma_invert range LA = [%.4g, %.4g]  mean=%.4g\n",
                min(od_la$gamma_invert), max(od_la$gamma_invert), mean(od_la$gamma_invert)))
    cat(sprintf("  orig_data        weight        range LA = [%.4g, %.4g]  mean=%.4g\n",
                min(od_la$weight), max(od_la$weight), mean(od_la$weight)))
  }

  ## Residual at baseline AS-IS (post-eval, fully shocked).
  r_shocked <- solve_wages_la(baseline_wages)
  cat(sprintf("  AS-IS (fully shocked)   residual = [%s]  ||r||_inf=%.4g\n",
              paste(signif(r_shocked, 4), collapse = ", "), max(abs(r_shocked))))

  ## ----- Define and call apply_shock(0). -----
  if (CF == "diffusion") {
    wd <- env$working_data; od <- env$orig_data; impr <- env$improve_it
    tg <- copy(od); tg[, target_gamma := impr(gamma_invert),
                       by = c("county", "quarter_year")]
    wd[, orig_gamma := od$gamma_invert]
    wd[, target_gamma := tg$target_gamma]
    apply_shock <- function(lam) {
      wd[, gamma_invert := (1 - lam) * orig_gamma + lam * target_gamma]
    }
  } else if (CF == "salestax") {
    rho_orig_g <- env$rho_orig
    apply_shock <- function(lam) {
      r_local <- env$rho
      r_local['6037'] <- rho_orig_g['6037'] * (1 + 0.04 * lam)
      env$rho <- r_local
    }
  } else if (CF == "immigration") {
    tot_field_names_g <- env$tot_field_names
    apply_shock <- function(lam) {
      tl <- env$total_labor
      for (col in tot_field_names_g) {
        val <- pristine_total_labor[county == "6037" & quarter_year == 2021.2,
                                    get(col)]
        tl[county == "6037" & quarter_year == 2021.2, (col) := val]
      }
      base <- as.numeric(as.matrix(pristine_total_labor[
        county == "6037" & quarter_year == 2021.2,
        .SD, .SDcols = tot_field_names_g
      ]))
      delta <- 0.05 * lam * sum(base)
      target_col <- tot_field_names_g[1]
      tl[county == "6037" & quarter_year == 2021.2,
         (target_col) := get(target_col) + delta]
    }
  } else if (CF == "merger") {
    wd <- env$working_data; od <- env$orig_data
    wd[, orig_weight := od$weight]
    apply_shock <- function(lam) {
      wd[, weight := orig_weight * (1 - lam / 2)]
    }
  }

  apply_shock(0)

  wd_la2 <- env$working_data[county == "6037" & quarter_year == 2021.2,
                             .(gamma_invert, weight)]
  cat(sprintf("  AFTER apply_shock(0) gamma_invert range LA = [%.4g, %.4g]  mean=%.4g\n",
              min(wd_la2$gamma_invert), max(wd_la2$gamma_invert), mean(wd_la2$gamma_invert)))
  cat(sprintf("  AFTER apply_shock(0) weight        range LA = [%.4g, %.4g]  mean=%.4g\n",
              min(wd_la2$weight), max(wd_la2$weight), mean(wd_la2$weight)))

  r_unshocked <- solve_wages_la(baseline_wages)
  cat(sprintf("  apply_shock(0) residual = [%s]  ||r||_inf=%.4g\n",
              paste(signif(r_unshocked, 4), collapse = ", "),
              max(abs(r_unshocked))))

  ## Check total_labor (the target labor) — does it match what would be
  ## computed at initial_wages under apply_shock(0)?
  if (CF == "immigration") {
    tl_la <- env$total_labor[county == "6037" & quarter_year == 2021.2]
    pt_la <- pristine_total_labor[county == "6037" & quarter_year == 2021.2]
    cat("  total_labor LA after apply_shock(0):\n")
    print(tl_la[, .SD, .SDcols = grep("^tot_", names(tl_la), value = TRUE)])
    cat("  pristine_total_labor LA:\n")
    print(pt_la[, .SD, .SDcols = grep("^tot_", names(pt_la), value = TRUE)])
  }
}

for (cf in c("diffusion", "salestax", "immigration", "merger")) {
  probe_cf(cf)
}
