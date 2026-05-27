## Verify apply_shock(0) gives residual=0 under the fixed approach
## (plain vectors + data.table::set on existing columns, no column-add).
suppressPackageStartupMessages(library(data.table))
source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types

probe_cf <- function(CF) {
  cat(sprintf("\n========== probe_cf_fixed: %s ==========\n", CF))
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

  env <- new.env(parent = globalenv())
  exprs <- parse(cf_script)
  .is_for <- function(e) is.call(e) && length(e) > 0 && as.character(e[[1]]) == "for"
  .first_for <- which(vapply(seq_along(exprs),
                             function(i) .is_for(exprs[[i]]), logical(1)))[1]
  for (i in seq_len(.first_for - 1L)) eval(exprs[[i]], envir = env)
  env$cnty <- "6037"; env$qy <- 2021.2

  baseline_wages <- as.numeric(unlist(
    env$initial_wages[county == "6037" & quarter_year == 2021.2,
                      paste0("w", seq_len(n_w)), with = FALSE],
    use.names = FALSE))

  if (CF == "diffusion") {
    wd_ref <- env$working_data
    orig_gamma_vec <- env$orig_data$gamma_invert
    tg_dt <- copy(env$orig_data)
    tg_dt[, target_gamma := env$improve_it(gamma_invert),
          by = c("county", "quarter_year")]
    target_gamma_vec <- tg_dt$target_gamma
    apply_shock <- function(lam) {
      val <- if (lam == 0) orig_gamma_vec
             else if (lam == 1) target_gamma_vec
             else (1 - lam) * orig_gamma_vec + lam * target_gamma_vec
      set(wd_ref, j = "gamma_invert", value = val)
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
    tl_ref <- env$total_labor
    apply_shock <- function(lam) {
      for (col in tot_field_names_g) {
        val <- pristine_total_labor[county == "6037" & quarter_year == 2021.2,
                                    get(col)]
        tl_ref[county == "6037" & quarter_year == 2021.2, (col) := val]
      }
      base <- as.numeric(as.matrix(pristine_total_labor[
        county == "6037" & quarter_year == 2021.2,
        .SD, .SDcols = tot_field_names_g
      ]))
      delta <- 0.05 * lam * sum(base)
      target_col <- tot_field_names_g[1]
      tl_ref[county == "6037" & quarter_year == 2021.2,
             (target_col) := get(target_col) + delta]
    }
  } else if (CF == "merger") {
    wd_ref <- env$working_data
    orig_weight_vec <- env$orig_data$weight
    apply_shock <- function(lam) {
      set(wd_ref, j = "weight",
          value = orig_weight_vec * (1 - lam / 2))
    }
  }

  apply_shock(0)
  r0 <- env$solve_wages(baseline_wages)
  cat(sprintf("  apply_shock(0) residual = [%s]  ||r||_inf=%.4g\n",
              paste(signif(r0, 4), collapse = ", "), max(abs(r0))))

  apply_shock(1)
  r1 <- env$solve_wages(baseline_wages)
  cat(sprintf("  apply_shock(1) residual = [%s]  ||r||_inf=%.4g\n",
              paste(signif(r1, 4), collapse = ", "), max(abs(r1))))
}

for (cf in c("diffusion", "salestax", "immigration", "merger")) {
  probe_cf(cf)
}
