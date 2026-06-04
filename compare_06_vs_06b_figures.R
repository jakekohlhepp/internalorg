## Figure companion to compare_06_vs_06b.R. Renders the unconstrained (06) vs
## workers-as-rows-monotone (06b) skill matrices B as paired heatmaps and writes
## them to the standard figure directory (results/out/figures) via
## save_counterfactual_plot(). Part of the 06b monotone-diagnostic track; runs
## standalone (outside run_all.R), like compare_06_vs_06b.R / display_06b_skills.R.
##
## Layout convention: WORKERS AS ROWS, TASKS AS COLUMNS, with counties stacked
## vertically so the figures fit a portrait page. Worker rows are in QP rank
## order (1 = least productive at top -> 5 = most productive at bottom); the
## workers-as-rows monotonicity shows up as each task column shading
## monotonically top -> bottom in the constrained panels.
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

## ---- wide table: one row per (county, task, worker rank) with unc, con, diff ----
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

## WORKERS AS ROWS (y): rank 1 (worst) at top -> rank n_w (best) at bottom.
## ggplot draws the first factor level at the bottom, so reverse the levels.
wide[, rank_lab := factor(rank, levels = rev(seq_len(n_w)), labels = as.character(rev(seq_len(n_w))))]
## TASKS AS COLUMNS (x): task 1 .. n_t left -> right.
wide[, task_lab := factor(task, levels = seq_len(n_t), labels = paste0("task ", seq_len(n_t)))]

raw_breaks <- c(-190, -50, -10, 0, 10, 50, 190)   # signed-log legend ticks (real B units)
y_title    <- "worker rank  (1 = least .. 5 = most productive)"
heat_theme <- theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(),
        strip.text.x = element_text(face = "bold", size = 9.5),
        strip.text.y.left = element_text(face = "bold", size = 8.5, angle = 0),
        strip.placement = "outside",
        plot.subtitle = element_text(size = 7.8, colour = "grey35"),
        axis.text = element_text(size = 7.5), axis.title = element_text(size = 9),
        legend.position = "right", legend.key.height = unit(1.0, "cm"))
write_fig <- function(plot, stem, w, h) {
  save_counterfactual_plot(paste0(stem, ".png"), plot = plot,
                           width = w, height = h, units = "in", dpi = 150)
  save_counterfactual_plot(paste0(stem, ".pdf"), plot = plot,
                           width = w, height = h, units = "in")
  message("wrote results/out/figures/", stem, ".{png,pdf}")
}

## ===== FIGURE 1: per-county scale (robust-capped), values printed =====
## counties stacked as rows, versions as the two columns -> portrait.
long <- melt(wide, id.vars = setdiff(names(wide), c("unc", "con", "diff")),
             measure.vars = c("unc", "con"), variable.name = "version", value.name = "value")
long[, version_f := factor(version, levels = c("unc", "con"),
                           labels = c("Unconstrained (06)", "Monotone (06b)"))]
long[, cap := as.numeric(quantile(abs(value), 0.90, na.rm = TRUE)), by = county]
long[, fill_scaled := pmax(pmin(value, cap), -cap) / cap]
long[, txt := ifelse(abs(value) >= 100, sprintf("%.0f", value), sprintf("%.1f", value))]
long[, lab_b := sprintf("%s\n||B||: %.0f -> %.0f", county_label[county], fro_unc, fro_con)]
long[, col_lab := factor(lab_b, levels = unique(long[match(county_order, county)]$lab_b))]

p_pc <- ggplot(long, aes(task_lab, rank_lab, fill = fill_scaled)) +
  geom_tile(colour = "grey92", linewidth = 0.4) +
  geom_text(aes(label = txt, colour = abs(fill_scaled) > 0.6), size = 2.5, show.legend = FALSE) +
  facet_grid(col_lab ~ version_f, switch = "y") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-1, 1), breaks = c(-1, 0, 1),
                       labels = c("-1 (cty max)", "0", "+1 (cty max)"),
                       name = "B, scaled\nwithin county") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
  labs(title = "Skill matrix B: unconstrained (06) vs workers-as-rows monotone (06b)",
       subtitle = paste0("Workers as rows (QP rank, worst top -> best bottom), tasks as columns. In the monotone panels each\n",
                         "task column shades monotonically top -> bottom. Cell = true B; colour capped at county 90th pctile |B|."),
       x = "task", y = y_title) + heat_theme
write_fig(p_pc, "06b_skillmatrix_percounty", 7.5, 9.2)

## ===== FIGURE 2: shared signed-log scale (cross-county comparable) =====
long[, sval := slog(value)]
lim1 <- slog(max(abs(long$value), na.rm = TRUE))
p_sl <- ggplot(long, aes(task_lab, rank_lab, fill = sval)) +
  geom_tile(colour = "grey92", linewidth = 0.4) +
  geom_text(aes(label = txt, colour = abs(sval) > 0.6 * lim1), size = 2.5, show.legend = FALSE) +
  facet_grid(col_lab ~ version_f, switch = "y") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-lim1, lim1), breaks = slog(raw_breaks), labels = raw_breaks,
                       name = "B\n(signed-log,\nshared scale)") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
  labs(title = "Skill matrix B: shared signed-log scale (magnitudes comparable across counties)",
       subtitle = paste0("Workers as rows, tasks as columns. One colour scale across all panels: NYC's unconstrained spike\n",
                         "(task 4, best-rank worker = -190) dominates, as it should."),
       x = "task", y = y_title) + heat_theme
write_fig(p_sl, "06b_skillmatrix_sharedlog", 7.5, 9.2)

## ===== FIGURE 3: constrained - unconstrained, counties stacked =====
d <- copy(wide)
d[, sdiff := slog(diff)]
d[, txt := ifelse(abs(diff) >= 100, sprintf("%+.0f", diff), sprintf("%+.1f", diff))]
d[, lab_d := sprintf("%s\n||diff||: %.0f  (rel %.2f)", county_label[county], fro_diff, fro_diff / fro_unc)]
d[, col_lab := factor(lab_d, levels = unique(d[match(county_order, county)]$lab_d))]
lim2 <- slog(max(abs(d$diff), na.rm = TRUE))
p_df <- ggplot(d, aes(task_lab, rank_lab, fill = sdiff)) +
  geom_tile(colour = "grey92", linewidth = 0.4) +
  geom_text(aes(label = txt, colour = abs(sdiff) > 0.6 * lim2), size = 2.6, show.legend = FALSE) +
  facet_wrap(~col_lab, ncol = 1) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-lim2, lim2), breaks = slog(raw_breaks), labels = raw_breaks,
                       name = "con - unc\n(signed-log)") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
  labs(title = "Where monotonicity moves B: constrained - unconstrained",
       subtitle = paste0("Workers as rows, tasks as columns. Red = constraint raised B; blue = lowered it.\n",
                         "NYC task-4 best-worker spike (+197) is flattened; LA barely moves (rel 0.27)."),
       x = "task", y = y_title) + heat_theme
write_fig(p_df, "06b_skillmatrix_diff", 5.6, 9.2)

message("compare_06_vs_06b_figures: done.")
