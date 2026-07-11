## Verify that every output of the 06+ pipeline was refreshed after a given
## timestamp, so a re-run can prove it actually regenerated everything (a
## silently skipped or crashed step leaves a stale mtime behind).
##
## Usage (typically the last job of a dependency chain, with afterany so it
## still reports when an upstream job failed):
##   JMP_REFRESH_SINCE=logs/pipeline_start.stamp Rscript diagnostics/check_outputs_refreshed.R
## JMP_REFRESH_SINCE is either a path (threshold = that file's mtime) or an
## epoch-seconds number. Exits 0 iff every expected output exists, is newer
## than the threshold, and every counterfactual wage cell converged at the
## focus quarter.

if (file.exists("renv/activate.R")) source("renv/activate.R")
suppressMessages(library(data.table))
source("config.R")

since_raw <- Sys.getenv("JMP_REFRESH_SINCE", unset = "")
if (!nzchar(since_raw)) {
  stop("Set JMP_REFRESH_SINCE to a stamp-file path or epoch seconds.")
}
threshold <- if (file.exists(since_raw)) {
  file.mtime(since_raw)
} else {
  as.POSIXct(as.numeric(since_raw), origin = "1970-01-01")
}
if (is.na(threshold)) stop("Could not parse JMP_REFRESH_SINCE: ", since_raw)
cat("Refresh threshold:", format(threshold), "\n\n")

tab <- function(...) file.path("results", "out", "tables", ...)
fig <- function(...) file.path("results", "out", "figures", ...)
dat <- function(...) file.path("results", "data", ...)
cfd <- function(...) file.path("results", "data", "counterfactuals", ...)

manifest <- list(
  "06"  = c(dat("06_parameters.rds"),
            file.path(CONFIG$prep_output_dir, "06_seeit_bb_next.rds")),
  "06b" = c(dat("06b_parameters_monotone.rds"), dat("06b_perms.rds"),
            dat("06b_qp_diagnostics.rds"),
            file.path(CONFIG$prep_output_dir, "06b_seeit_bb.rds")),
  "06c" = c(dat("06c_wage_identification.rds"),
            tab("06c_wage_eigenvalues.tex"), tab("06c_wage_perturbation.tex")),
  "07"  = c(dat("07_first_stage_vcov.rds"), dat("07_murphy_topel_vcov.rds"),
            dat("07_murphy_topel_jacobian_checkpoint.rds")),
  "08"  = tab(c("08_org_price.tex", "08_time_effects.tex", "08_model_fit.tex",
                "08_wages_skills_Cook.tex", "08_wages_skills_LosAngeles.tex",
                "08_wages_skills_NewYork.tex")),
  "08b" = c(tab(c("08b_model_fit_monotone.tex", "08b_qp_objective_monotone.tex")),
            dat("08b_model_fit_monotone.rds")),
  "09"  = c(dat("09_withgammas.rds"), fig("09_gamma_dist.png")),
  "12"  = c(dat("12_data_for_counterfactuals.rds"), tab("12_validate_corr.tex"),
            fig(c("12_marginal_cut.png", "12_marginal_color.png",
                  "12_marginal_other3.png", "12_bivariate_cut_color.png",
                  "12_bivariate_cut_other3.png", "12_bivariate_color_other3.png"))),
  "13"  = cfd(c("13_initial_wages.rds", "13_working_data.rds",
                "13_total_labor.rds", "13_prod_initial.rds")),
  "14"  = cfd(c("14_wages_diffusion.rds", "14_prod_diffusion.rds")),
  "15"  = cfd(c("15_wages_salestax.rds", "15_prod_salestax.rds")),
  "16"  = cfd(c("16_wages_immigration.rds", "16_prod_immigration.rds")),
  "17"  = cfd(c("17_wages_merger.rds", "17_prod_merger.rds")),
  "18"  = tab(c("18_tot_counterfactuals.tex", "18_bytype_counterfactuals.tex")),
  "19"  = fig(c(paste0("19_immigration_realloc_",
                       c("scatter", "price", "marketshare", "sindex"), ".png"),
                paste0("19_immigration_reorg_",
                       c("scatter", "price", "marketshare", "sindex"), ".png"),
                paste0("19_merger_realloc_",
                       c("price", "marketshare", "sindex"), ".png"),
                paste0("19_merger_reorg_",
                       c("price", "marketshare", "sindex"), ".png"),
                "19_prod_reversal_dumbbell.png",
                paste0("19_immigration_",
                       c("between_contrib", "within_firm", "wage_response"), ".png"),
                paste0("19_merger_",
                       c("between_contrib", "within_firm", "wage_response"), ".png"))),
  "20"  = c(tab("20_substitute.tex"),
            fig(c("20_middle_decreasing_count.png",
                  "20_middle_decreasing_count_blank.png",
                  "20_middle_non_mon1_count.png", "20_middle_non_mon2_count.png",
                  "20_middle_non_mon3_count.png", "20_increasing_count.png"))),
  "21"  = tab("21_substitute_prod.tex"),
  "22"  = c(tab("22_skill_units.tex"), dat("22_skill_units.csv"))
)

rows <- rbindlist(lapply(names(manifest), function(step) {
  data.table(step = step, path = manifest[[step]])
}))
rows[, exists := file.exists(path)]
rows[, mtime := file.mtime(path)]
rows[, fresh := exists & mtime >= threshold]

stale <- rows[exists & !fresh]
missing <- rows[!(exists)]

cat(sprintf("Outputs expected: %d | fresh: %d | STALE: %d | MISSING: %d\n\n",
            nrow(rows), sum(rows$fresh), nrow(stale), nrow(missing)))
if (nrow(missing) > 0) {
  cat("MISSING:\n")
  for (i in seq_len(nrow(missing))) {
    cat(sprintf("  [%s] %s\n", missing$step[i], missing$path[i]))
  }
}
if (nrow(stale) > 0) {
  cat("STALE (older than threshold):\n")
  for (i in seq_len(nrow(stale))) {
    cat(sprintf("  [%s] %s (mtime %s)\n", stale$step[i], stale$path[i],
                format(stale$mtime[i])))
  }
}

## Convergence audit: every counterfactual wage cell at the focus quarter
## must report converged=TRUE (13's grid has no sol_type; 14-17 have two).
conv_fail <- character(0)
fq <- CONFIG$counterfactual_focus_quarters[[1]]
wage_files <- c("13_initial_wages.rds", "14_wages_diffusion.rds",
                "15_wages_salestax.rds", "16_wages_immigration.rds",
                "17_wages_merger.rds")
for (wf in wage_files) {
  p <- cfd(wf)
  if (!file.exists(p)) next
  w <- as.data.table(readRDS(p))[quarter_year == fq]
  bad <- w[!(converged %in% TRUE)]
  if (nrow(bad) > 0) {
    conv_fail <- c(conv_fail, sprintf(
      "%s: %d non-converged cell(s): %s", wf, nrow(bad),
      paste(bad$county, if ("sol_type" %in% names(bad)) bad$sol_type else "",
            signif(bad$fval, 3), collapse = "; ")))
  } else {
    cat(sprintf("CONVERGED: %s (%d cells at %s, max fval %.3g)\n",
                wf, nrow(w), as.character(fq), max(w$fval)))
  }
}
if (length(conv_fail) > 0) {
  cat("\nNON-CONVERGED WAGE CELLS:\n")
  for (m in conv_fail) cat("  ", m, "\n")
}

ok <- nrow(stale) == 0 && nrow(missing) == 0 && length(conv_fail) == 0
cat(if (ok) "\nALL OUTPUTS REFRESHED AND CONVERGED\n" else "\nREFRESH CHECK FAILED\n")
quit(save = "no", status = if (ok) 0L else 1L)
