## Merger (Incr. Concentration) prototypes for the 19_ redesign, LA cell.
## Mirrors diagnostics/proto_19_prod_figures.R (immigration) with the merger
## mechanics: between-firm contribution plot, within-firm re-specialization
## panel (two x-axis candidates, cor test printed), and wage-response bars.
## Reads existing rds only; writes diagnostics/out/proto19_m*.png.
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(scales)
})
source("config.R"); source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types
efn <- counterfactual_e_field_names(CONFIG)
bfn <- counterfactual_b_field_names(CONFIG)
dir.create("diagnostics/out", showWarnings = FALSE)

COL_REALLOC <- "#377EB8"; COL_REORG <- "#E69F00"
scen_fill <- scale_fill_manual(values = c(Reallocation = COL_REALLOC,
                                          Reorganization = COL_REORG))
proto_theme <- theme_bw(base_size = 22) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

LA <- "6037"; fq <- get_counterfactual_focus_quarter()
dedup <- function(dt) dt[, !duplicated(names(dt)), with = FALSE]
rd <- function(f) dedup(as.data.table(readRDS(counterfactual_data_path(f))))

mk <- function(dt, lab) {
  d <- copy(dt[as.character(county) == LA])
  if ("quarter_year" %in% names(d)) d <- d[quarter_year == fq]
  d[, tot_prod := Reduce(`+`, .SD), .SDcols = bfn]
  d[, mult := avg_labor * CSPOP * new_share * weight]
  cols <- c("location_id", "tot_prod", "s_index", "mult", "new_share",
            "newprice", "weight", "E_1")
  o <- d[, ..cols]
  setnames(o, setdiff(cols, "location_id"),
           paste0(setdiff(cols, "location_id"), "_", lab))
  o
}
mrg <- rd("17_prod_merger.rds")
f <- Reduce(function(x, y) merge(x, y, by = "location_id"), list(
  mk(rd("13_prod_initial.rds"), "b"),
  mk(mrg[sol_type == "realloc"], "rl"),
  mk(mrg[sol_type == "reorg"],   "rg")))
om <- function(w) w / sum(w)
f[, w0 := om(mult_b)]; f[, wrl := om(mult_rl)]; f[, wrg := om(mult_rg)]
P0 <- f[, sum(tot_prod_b * w0)]

p6 <- as.data.table(readRDS("results/data/06_parameters.rds"))
rho_la <- abs(p6[parm_name == "factor(county)6037:cust_price", coefficients][1])

## =================== FIGURE M2: between-firm contribution ===================
cb <- rbind(
  f[, .(scenario = "Reallocation", p_dollar = tot_prod_b / rho_la, w0,
        contrib = 100 * (wrl - w0) * (tot_prod_b - P0) / P0)],
  f[, .(scenario = "Reorganization", p_dollar = tot_prod_b / rho_la, w0,
        contrib = 100 * (wrg - w0) * (tot_prod_b - P0) / P0)])
sums <- cb[, .(tot = sum(contrib)), by = scenario]
sums[, lab := sprintf("between-firm total: %+.1f pp", tot)]
star <- cb[scenario == "Reallocation"][which.max(contrib)]
loo <- f[tot_prod_b < max(tot_prod_b)]
loo_rl <- loo[, sum(tot_prod_rl * wrl) / sum(wrl)] /
          loo[, sum(tot_prod_b * w0) / sum(w0)] - 1
note_df <- data.table(scenario = "Reallocation", x = star$p_dollar * 0.93,
                      y = star$contrib,
                      lab = sprintf("one salon — drop it and reallocation is %+.1f%%",
                                    100 * loo_rl))
p2 <- ggplot(cb, aes(x = p_dollar, y = contrib)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
  geom_point(aes(size = w0, fill = scenario), shape = 21,
             color = "white", stroke = 0.6, alpha = 0.95, show.legend = FALSE) +
  geom_text(data = sums, aes(label = lab), x = -Inf, y = Inf,
            hjust = -0.05, vjust = 1.6, size = 6.5, fontface = "bold") +
  geom_text(data = note_df, aes(x = x, y = y, label = lab),
            hjust = 1, size = 5.5, inherit.aes = FALSE) +
  facet_wrap(~scenario) +
  scale_x_log10(breaks = c(100, 200, 300, 500, 750),
                labels = dollar_format(accuracy = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
  scale_size_area(max_size = 15, guide = "none") +
  scen_fill + proto_theme +
  labs(x = "Baseline labor productivity ($ quality premium per hour, log scale)",
       y = "Contribution to aggregate\nproductivity change (pp)",
       title = "Merger: the reallocation gain is one salon",
       subtitle = "Point area = baseline market weight; contributions sum to the between-firm effect")
ggsave("diagnostics/out/proto19_m2_between.png", p2, width = 14, height = 7.5, dpi = 220)

## =============== FIGURE M3: within-firm re-specialization ==================
wn <- f[, .(dE1 = 100 * (E_1_rg - E_1_rl),
            dP  = 100 * (tot_prod_rg - tot_prod_rl) / P0,
            dS  = s_index_rg - s_index_rl,
            wbar = (wrl + wrg) / 2)]
within_tot <- wn[, sum(wbar * dP)]
cat(sprintf("x-axis candidates: cor(dP, dE1) = %+.3f | cor(dP, dS) = %+.3f\n",
            wn[, cor(dP, dE1)], wn[, cor(dP, dS)]))

p3a <- ggplot(wn, aes(x = dE1, y = dP)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey70", linetype = "dashed") +
  geom_point(aes(size = wbar, fill = dS), shape = 21, color = "grey35", stroke = 0.5) +
  scale_fill_gradient2(low = "#762A83", mid = "grey92", high = "#1B7837",
                       midpoint = 0, name = "Δ s-index") +
  scale_size_area(max_size = 15, guide = "none") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.6, size = 6.5,
           fontface = "bold",
           label = sprintf("labor-weighted within-firm total: %+.1f pp", within_tot)) +
  proto_theme + theme(legend.position = "right",
                      legend.title = element_text(size = 18)) +
  labs(x = "Change in type-1 (generalist) time share within the firm (pp)",
       y = "Within-firm productivity change\n(reorg − realloc, % of baseline)",
       title = "Merger: shed the generalist type, re-specialize",
       subtitle = "Point area = market weight; green = deeper specialization")
ggsave("diagnostics/out/proto19_m3_within.png", p3a, width = 11, height = 7.5, dpi = 220)

p3b <- p3a + aes(x = dS) +
  labs(x = "Change in s-index within the firm (reorg − realloc)")
ggsave("diagnostics/out/proto19_m3b_within_sindex.png", p3b, width = 11, height = 7.5, dpi = 220)

## ======================= FIGURE M4: wage response ===========================
iw <- as.data.table(readRDS(counterfactual_data_path("13_initial_wages.rds")))
w17 <- as.data.table(readRDS(counterfactual_data_path("17_wages_merger.rds")))
wcols <- paste0("w", seq_len(n_w))
wb <- as.numeric(iw[county == LA & quarter_year == fq, ..wcols])
wr <- as.numeric(w17[county == LA & quarter_year == fq & sol_type == "realloc", ..wcols])
wg <- as.numeric(w17[county == LA & quarter_year == fq & sol_type == "reorg", ..wcols])
wd <- rbind(
  data.table(type = 1:5, scenario = "Reallocation",   pct = (wr - wb) / wb),
  data.table(type = 1:5, scenario = "Reorganization", pct = (wg - wb) / wb))
wd[, type_lab := sprintf("Type %d\n($%.0f)", type, wb[type])]

dp_rl <- f[, weighted.mean(newprice_rl / newprice_b - 1, w0)]
dp_rg <- f[, weighted.mean(newprice_rg / newprice_b - 1, w0)]
cov0 <- f[, sum(weight_b * new_share_b)]
cov_rl <- f[, sum(weight_rl * new_share_rl)]
cov_rg <- f[, sum(weight_rg * new_share_rg)]

p4 <- ggplot(wd, aes(x = type_lab, y = pct, fill = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(aes(label = percent(pct, accuracy = 0.1),
                vjust = ifelse(pct < 0, 1.35, -0.5)),
            position = position_dodge(width = 0.72), size = 5.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.14, 0.10))) +
  scen_fill + proto_theme +
  labs(x = NULL, y = "Equilibrium wage change",
       title = "Frozen organizations crash the specialist wages",
       subtitle = sprintf(
         "LA merger: half the salons absorb the same labor force\nPrice change: %+.1f%% (realloc), %+.1f%% (reorg); coverage %.0f%% → %.0f%%/%.0f%%",
         100 * dp_rl, 100 * dp_rg, 100 * cov0, 100 * cov_rl, 100 * cov_rg))
ggsave("diagnostics/out/proto19_m4_wages.png", p4, width = 12, height = 7.5, dpi = 220)

cat(sprintf("wrote proto19_m{2,3,3b,4}; checks: realloc between %+0.2fpp (expect +7.6), reorg between %+0.2fpp, within %+0.2fpp (expect +2.4)\n",
            sums[scenario == "Reallocation"]$tot,
            sums[scenario == "Reorganization"]$tot, within_tot))
