## Figure companion to compare_06_vs_06b.R. Renders the unconstrained (06) vs
## workers-as-rows-monotone (06b) skill matrices B[task, worker] as paired
## heatmaps and writes them to the standard figure directory
## (results/out/figures) via save_counterfactual_plot(). Part of the 06b
## monotone-diagnostic track; runs standalone (outside run_all.R), like its
## siblings compare_06_vs_06b.R / display_06b_skills.R.
##
## Inputs:
##   - results/data/06_parameters.rds            (unconstrained point estimates)
##   - results/data/06b_parameters_monotone.rds  (monotone point estimates)
##   - results/data/06b_perms.rds                (QP-recovered worker rank order)
## Outputs (results/out/figures/, .png + .pdf):
##   - 06b_skillmatrix_percounty   paired heatmaps, per-county color scale
##   - 06b_skillmatrix_sharedlog   paired heatmaps, one shared signed-log scale
##   - 06b_skillmatrix_diff        constrained - unconstrained, per county
suppressPackageStartupMessages({
  library("data.table"); library("stringr"); library("ggplot2")
})
source("config.R")
source("utils/counterfactuals_core.R")

con_path <- file.path("results", "data", "06b_parameters_monotone.rds")
if (!file.exists(con_path)) {
  message("compare_06_vs_06b_figures: ", con_path,
          " not found; run 06b_estimation_monotone.R first. Skipping.")
  quit(save = "no", status = 0)
}

n_t <- CONFIG$n_task_types
n_w <- CONFIG$n_worker_types

unc   <- as.data.table(readRDS(file.path("results", "data", "06_parameters.rds")))
con   <- as.data.table(readRDS(con_path))
perms <- readRDS(file.path("results", "data", "06b_perms.rds"))$perms_by_county
stopifnot(identical(sort(unc$parm_name), sort(con$parm_name)))

merged <- merge(unc[, .(parm_name, unc = coefficients)],
                con[, .(parm_name, con = coefficients)], by = "parm_name", sort = FALSE)
merged[, county := str_extract(parm_name, "(?<=factor\\(county\\))[0-9]+")]
merged[, other  := str_replace_all(str_replace(parm_name, "factor\\(county\\)[0-9]+", ""), "^:|:$", "")]

county_label <- c(`17031` = "Cook (Chicago)", `36061` = "New York (Manhattan)", `6037` = "Los Angeles")
county_order <- as.character(CONFIG$counties)
county_order <- county_order[county_order %in% names(county_label)]

extract_B <- function(cnty, col) {
  out <- matrix(NA_real_, n_t, n_w)
  rows <- merged[county == cnty & grepl("avg_labor:B_raw_", other)]
  for (i in seq_len(nrow(rows))) {
    m <- str_match(rows$other[i], "B_raw_([0-9]+)_([0-9]+)$")
    out[as.integer(m[1, 2]), as.integer(m[1, 3])] <- rows[[col]][i]
  }
  out
}
froben <- function(M) sqrt(sum(M^2, na.rm = TRUE))
slog   <- function(x) sign(x) * log1p(abs(x))   # signed log: symmetric, 0 -> 0

## ---- wide table: one row per (county, task, rank) with unc, con, diff ----
wide <- rbindlist(lapply(county_order, function(cnty) {
  pi_c  <- as.integer(perms[[cnty]])
  M_unc <- extract_B(cnty, "unc")[, pi_c, drop = FALSE]
  M_con <- extract_B(cnty, "con")[, pi_c, drop = FALSE]
  data.table(county = cnty,
             task = rep(seq_len(n_t), times = n_w),
             rank = rep(seq_len(n_w), each = n_t),
             worker = rep(pi_c, each = n_t),
             unc = as.numeric(M_unc), con = as.numeric(M_con),
             fro_unc = froben(M_unc), fro_con = froben(M_con),
             fro_diff = froben(M_con - M_unc))
}))
wide[, diff := con - unc]
wide[, rank_lab := factor(rank, levels = seq_len(n_w),
      labels = paste0(seq_len(n_w), ifelse(seq_len(n_w) == 1, "\n(worst)",
                                    ifelse(seq_len(n_w) == n_w, "\n(best)", ""))))]
wide[, task_lab := factor(task, levels = rev(seq_len(n_t)), labels = paste0("task ", rev(seq_len(n_t))))]

raw_breaks <- c(-190, -50, -10, 0, 10, 50, 190)   # signed-log legend ticks (real B units)
heat_theme <- theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold", size = 9.5),
        strip.placement = "outside", plot.subtitle = element_text(size = 8.2, colour = "grey35"),
        axis.text = element_text(size = 8), legend.position = "right",
        legend.key.height = unit(1.15, "cm"))
write_fig <- function(plot, stem, w, h) {
  save_counterfactual_plot(paste0(stem, ".png"), plot = plot,
                           width = w, height = h, units = "in", dpi = 150)
  save_counterfactual_plot(paste0(stem, ".pdf"), plot = plot,
                           width = w, height = h, units = "in")
  message("wrote results/out/figures/", stem, ".{png,pdf}")
}

## ===== FIGURE 1: per-county scale (robust-capped), values printed =====
long <- melt(wide, id.vars = setdiff(names(wide), c("unc", "con", "diff")),
             measure.vars = c("unc", "con"), variable.name = "version", value.name = "value")
long[, version_f := factor(version, levels = c("unc", "con"),
                           labels = c("Unconstrained", "Constrained"))]
long[, cap := as.numeric(quantile(abs(value), 0.90, na.rm = TRUE)), by = county]
long[, fill_scaled := pmax(pmin(value, cap), -cap) / cap]
long[, txt := ifelse(abs(value) >= 100, sprintf("%.0f", value), sprintf("%.1f", value))]
## column-strip labels, ordered by county_order (one label per county)
long[, lab_b := sprintf("%s\n||B||: %.0f -> %.0f", county_label[county], fro_unc, fro_con)]
lev_b <- unique(long[match(county_order, county)]$lab_b)
long[, col_lab := factor(lab_b, levels = lev_b)]

p_pc <- ggplot(long, aes(rank_lab, task_lab, fill = fill_scaled)) +
  geom_tile(colour = "grey92", linewidth = 0.4) +
  geom_text(aes(label = txt, colour = abs(fill_scaled) > 0.6), size = 2.7, show.legend = FALSE) +
  facet_grid(version_f ~ col_lab, switch = "y") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-1, 1), breaks = c(-1, 0, 1),
                       labels = c("-1 (county max)", "0", "+1 (county max)"),
                       name = "B, scaled\nwithin county") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
  labs(title = "Skill matrix B[task, worker]: unconstrained (06) vs workers-as-rows monotone (06b)",
       subtitle = "Columns ordered by QP worker rank (worst -> best). Constrained rows ramp left-to-right = monotone ladder. Cell = true B; colour capped at each county's 90th pctile |B|.",
       x = "worker rank (within county)", y = NULL) + heat_theme
write_fig(p_pc, "06b_skillmatrix_percounty", 12, 6.2)

## ===== FIGURE 2: shared signed-log scale (cross-county comparable) =====
long[, sval := slog(value)]
lim1 <- slog(max(abs(long$value), na.rm = TRUE))
p_sl <- ggplot(long, aes(rank_lab, task_lab, fill = sval)) +
  geom_tile(colour = "grey92", linewidth = 0.4) +
  geom_text(aes(label = txt, colour = abs(sval) > 0.6 * lim1), size = 2.7, show.legend = FALSE) +
  facet_grid(version_f ~ col_lab, switch = "y") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-lim1, lim1), breaks = slog(raw_breaks), labels = raw_breaks,
                       name = "B\n(signed-log,\nshared scale)") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
  labs(title = "Skill matrix B: unconstrained (06) vs monotone (06b) -- shared signed-log scale",
       subtitle = "One colour scale across all six panels: magnitudes comparable across counties. NYC's unconstrained spike (task 4 = -190) dominates, as it should.",
       x = "worker rank (within county)", y = NULL) + heat_theme
write_fig(p_sl, "06b_skillmatrix_sharedlog", 12, 6.2)

## ===== FIGURE 3: constrained - unconstrained per county =====
d <- copy(wide)
d[, sdiff := slog(diff)]
d[, txt := ifelse(abs(diff) >= 100, sprintf("%+.0f", diff), sprintf("%+.1f", diff))]
d[, lab_d := sprintf("%s\n||diff||: %.0f  (rel %.2f)", county_label[county], fro_diff, fro_diff / fro_unc)]
lev_d <- unique(d[match(county_order, county)]$lab_d)
d[, col_lab := factor(lab_d, levels = lev_d)]
lim2 <- slog(max(abs(d$diff), na.rm = TRUE))
p_df <- ggplot(d, aes(rank_lab, task_lab, fill = sdiff)) +
  geom_tile(colour = "grey92", linewidth = 0.4) +
  geom_text(aes(label = txt, colour = abs(sdiff) > 0.6 * lim2), size = 2.9, show.legend = FALSE) +
  facet_wrap(~col_lab, nrow = 1) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-lim2, lim2), breaks = slog(raw_breaks), labels = raw_breaks,
                       name = "con - unc\n(signed-log)") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
  labs(title = "Where monotonicity moves the skill matrix: constrained - unconstrained",
       subtitle = "Red = constraint raised B for that (task, worker rank); blue = lowered it. NYC task 4 (+197) is the noisy spike being flattened; LA barely moves (rel 0.27).",
       x = "worker rank (within county)", y = NULL) + heat_theme
write_fig(p_df, "06b_skillmatrix_diff", 12, 3.9)

message("compare_06_vs_06b_figures: done.")
