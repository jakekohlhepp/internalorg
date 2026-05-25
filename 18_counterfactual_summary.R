## Counterfactual summary tables.
## Reads the wage and productivity panels written by 13-17 and aggregates
## them into the LaTeX comparison tables consumed by the manuscript.
source("config.R")
source("utils/counterfactuals_core.R")

load_counterfactual_packages(c("kableExtra"))
ensure_counterfactual_dirs()

n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
e_field_names  <- counterfactual_e_field_names(CONFIG)

## County FIPS -> display name. Lookup table for both table outputs.
county_display_name <- function(cnty) {
  ifelse(cnty == "17031", "Cook",
         ifelse(cnty == "36061", "New York", "Los Angeles"))
}

## Reshape a per-firm productivity panel into (i) a firm-level summary
## (mean s-index and labor productivity per county x sol_type) and (ii) a
## per-worker-type productivity summary (one column per worker type after
## dcast). Used identically for the baseline and each counterfactual.
summarize_prod_panel <- function(prod_dt, version_label) {
  panel <- melt(
    prod_dt,
    id.vars = c("location_id", "avg_labor", "new_share", "CSPOP", "county",
                "s_index", "sol_type", "weight", e_field_names),
    measure.vars = patterns("^B_[0-9]")
  )
  panel[, multiplier := avg_labor * CSPOP * new_share * weight]
  panel[, worker_type := str_replace(variable, "^B_[0-9]_", "")]
  panel[, type_E := NA_real_]
  for (idx in seq_len(n_worker_types)) {
    panel[worker_type == as.character(idx), type_E := get(paste0("E_", idx))]
  }

  firm_summary <- panel[, .(tot_prod = sum(value)),
                        by = c("s_index", "multiplier", "location_id", "county", "sol_type")]
  firm_summary <- firm_summary[, .(
    s_avg      = weighted.mean(s_index, multiplier),
    labor_prod = sum(tot_prod * multiplier) / sum(multiplier)
  ), by = c("county", "sol_type")]
  firm_summary[, version := version_label]

  type_summary <- panel[, .(
    tot_prod = sum(multiplier * value) * n_task_types / sum(multiplier * type_E)
  ), by = c("county", "worker_type", "sol_type")]
  type_summary <- dcast(type_summary, county + sol_type ~ worker_type,
                        value.var = "tot_prod")
  type_summary[, version := version_label]

  list(firm = firm_summary, type = type_summary)
}

## Scenario specifications. `initial` is the baseline panel produced by
## 13_; the four counterfactuals are the panels written by 14-17. Order of
## this list is the rbind order used to build the cross-counterfactual
## tables below.
panel_specs <- list(
  initial     = list(version = "Initial",              wages_rds = "13_initial_wages.rds",     prod_rds = "13_prod_initial.rds",     prod_legacy = "15_prod_initial.rds"),
  salestax    = list(version = "Sales Tax",            wages_rds = "15_wages_salestax.rds",    prod_rds = "15_prod_salestax.rds"),
  diffusion   = list(version = "Management Diffusion", wages_rds = "14_wages_diffusion.rds",   prod_rds = "14_prod_diffusion.rds"),
  immigration = list(version = "Immigration",          wages_rds = "16_wages_immigration.rds", prod_rds = "16_prod_immigration.rds"),
  merger      = list(version = "Incr. Concentration",  wages_rds = "17_wages_merger.rds",      prod_rds = "17_prod_merger.rds")
)
cf_names <- c("salestax", "diffusion", "immigration", "merger")

panels <- lapply(panel_specs, function(spec) {
  prod_legacy <- if (is.null(spec$prod_legacy)) spec$prod_rds else spec$prod_legacy
  wages <- read_counterfactual_rds(
    spec$wages_rds, legacy_filenames = spec$wages_rds,
    description = paste(spec$version, "counterfactual wages")
  )
  prod_panel <- read_counterfactual_rds(
    spec$prod_rds, legacy_filenames = prod_legacy,
    description = paste(spec$version, "counterfactual productivity panel")
  )
  c(list(version = spec$version, wages = wages),
    summarize_prod_panel(prod_panel, spec$version))
})

wage_vect_initial <- panels$initial$wages
prod_data_initial <- panels$initial$type
## firm_initial only contributes the baseline (s, prod) per county. Drop the
## version tag so it doesn't collide on the merge below.
firm_initial      <- copy(panels$initial$firm)
firm_initial[, version := NULL]


## ===== firm-level summary table (Reallocation/Reorganization x s-index/Prod.) =====
tot_table <- rbindlist(lapply(panels[cf_names], `[[`, "firm"))
colnames(firm_initial) <- c("county", "sol_type", "initial_s", "initial_prod")

tot_table <- merge(tot_table, firm_initial[, -"sol_type"], by = "county")
tot_table[, pct_sindex := (s_avg - initial_s) / initial_s]
tot_table[, pct_prod   := (labor_prod - initial_prod) / initial_prod]

tot_table <- dcast(tot_table, version + county ~ sol_type,
                   value.var = c("pct_sindex", "pct_prod"))
setcolorder(tot_table, c("version", "county", "pct_sindex_realloc", "pct_prod_realloc"))
cols <- colnames(tot_table)[-c(1, 2)]
tot_table[, (cols) := lapply(.SD, function(x) as.character(format(round(x, 3), nsmall = 3))),
          .SDcols = cols]

tot_table[, county_name := county_display_name(county)]
setcolorder(tot_table, "county_name")
colnames(tot_table)[-c(1, 2, 3)] <- c("S-Index Change", "Prod. Change",
                                      "S-Index Change", "Prod. Change")
setnames(tot_table, "county_name", "County")
setnames(tot_table, "version", "Counterfactual")

output <- kable(tot_table[, -"county"], "latex", align = "c", booktabs = TRUE,
                linesep = c(""), escape = F, caption = NA, label = NA)
output <- add_header_above(output, c(" ", " ", "Reallocation" = 2, "Reorganization" = 2))
write_counterfactual_text(
  output,
  "18_tot_counterfactuals.tex",
  type = "tables",
  legacy_filename = "18_tot_counterfactuals.tex"
)


## ===== per-worker-type (Prod. / Wage) change table =====
wage_changes <- rbindlist(lapply(panels[cf_names], function(p) {
  w <- copy(p$wages)
  w[, version := p$version]
  w
}))
wage_changes <- merge(wage_changes, wage_vect_initial, by = c("county", "quarter_year"))
for (idx in seq_len(n_worker_types)) {
  new_col <- paste0("wage", idx)
  x_col   <- paste0("w", idx, ".x")
  y_col   <- paste0("w", idx, ".y")
  wage_changes[, (new_col) := (get(x_col) - get(y_col)) / get(y_col)]
}

prod_changes <- rbindlist(lapply(panels[cf_names], `[[`, "type"))
prod_changes <- merge(prod_changes, prod_data_initial[, -c("sol_type", "version")],
                      by = c("county"))
for (idx in seq_len(n_worker_types)) {
  new_col <- paste0("prod", idx)
  x_col   <- paste0(idx, ".x")
  y_col   <- paste0(idx, ".y")
  prod_changes[, (new_col) := (get(x_col) - get(y_col)) / get(y_col)]
}

het_prod <- merge(prod_changes, wage_changes[is.finite(w1.x), ],
                  by = c("county", "sol_type", "version"))
prod_wage_pairs <- as.vector(rbind(paste0("prod", seq_len(n_worker_types)),
                                   paste0("wage", seq_len(n_worker_types))))
cols <- prod_wage_pairs
het_prod[, (cols) := lapply(.SD, function(x) as.character(format(round(x, 3), nsmall = 3))),
         .SDcols = cols]
het_prod[, county_name := county_display_name(county)]
setnames(het_prod, "county_name", "County")
setnames(het_prod, "version", "Counterfactual")
setorder(het_prod, "Counterfactual", "County")
het_prod <- het_prod[sol_type == "reorg",
                     c("Counterfactual", "County", prod_wage_pairs),
                     with = FALSE]
setnames(het_prod, old = prod_wage_pairs,
         new = rep(c("Prod.", "Wage"), n_worker_types))
output <- kable(het_prod, "latex", align = "c", booktabs = TRUE,
                linesep = c(""), escape = F, caption = NA, label = NA)
header_skill_sets <- setNames(rep(2, n_worker_types),
                              paste0("Skill Set ", seq_len(n_worker_types)))
output <- add_header_above(output, c(" ", " ", header_skill_sets))
write_counterfactual_text(
  output,
  "18_bytype_counterfactuals.tex",
  type = "tables",
  legacy_filename = "18_bytype_counterfactuals.tex"
)
