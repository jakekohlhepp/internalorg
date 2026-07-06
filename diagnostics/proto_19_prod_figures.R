## Prototypes for the 19_ productivity-figure redesign.
## Four candidate figures replacing the s-index-organized panels:
##   1. Dumbbell: realloc -> reorg productivity change, all county x cf cells
##      (+ the LA type-1 immigration variant from the target probe).
##   2. Between-firm contribution plot (LA immigration): each firm's
##      contribution to the aggregate productivity change; points sum to the
##      between-firm component.
##   3. Within-firm absorption plot (LA immigration, reorg): d(target-type
##      share) vs within-firm productivity change, colored by d(s-index).
##   4. Wage response by worker type (LA immigration), realloc vs reorg.
## Prototypes only: reads existing rds outputs, writes diagnostics/out/proto19_*.
## Scenario colors fixed everywhere: Reallocation = blue, Reorganization = orange.
suppressPackageStartupMessages({
  library(data.table); library(stringr); library(ggplot2); library(scales)
})
source("config.R"); source("utils/counterfactuals_core.R")

n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
e_field_names  <- counterfactual_e_field_names(CONFIG)
b_field_names  <- counterfactual_b_field_names(CONFIG)
dir.create("diagnostics/out", showWarnings = FALSE)

COL_REALLOC <- "#377EB8"
COL_REORG   <- "#E69F00"
scen_fill <- scale_fill_manual(values = c(Reallocation = COL_REALLOC,
                                          Reorganization = COL_REORG))
scen_col  <- scale_color_manual(values = c(Reallocation = COL_REALLOC,
                                           Reorganization = COL_REORG))
proto_theme <- theme_bw(base_size = 22) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

## ---- reuse 18's aggregation verbatim (native basis for immigration) --------
ex <- parse("18_counterfactual_summary.R")
want <- c("summarize_prod_panel", "build_immigration_native_fraction")
for (e in ex) if (is.call(e) && length(e) >= 3 &&
    identical(as.character(e[[1]]), "<-") && is.symbol(e[[2]]) &&
    as.character(e[[2]]) %in% want) eval(e, globalenv())
stopifnot(exists("summarize_prod_panel"), exists("build_immigration_native_fraction"))
nf_imm <- build_immigration_native_fraction()

county_disp <- c(`17031` = "Cook", `36061` = "New York", `6037` = "Los Angeles")
dedup <- function(dt) dt[, !duplicated(names(dt)), with = FALSE]
rd <- function(f) dedup(as.data.table(readRDS(counterfactual_data_path(f))))

## ============================ FIGURE 1: dumbbell ============================
specs <- list(
  list("Sales Tax",            "15_prod_salestax.rds",    NULL),
  list("Management Diffusion", "14_prod_diffusion.rds",   NULL),
  list("Immigration",          "16_prod_immigration.rds", nf_imm),
  list("Incr. Concentration",  "17_prod_merger.rds",      NULL))
base_firm <- summarize_prod_panel(rd("13_prod_initial.rds"), "Initial")$firm
base_firm <- base_firm[, .(initial_prod = labor_prod[1]), by = county]

dumb <- rbindlist(lapply(specs, function(s) {
  f <- summarize_prod_panel(rd(s[[2]]), s[[1]], native_fraction_by_county = s[[3]])$firm
  f <- merge(f, base_firm, by = "county")
  f[, .(cf = s[[1]], county = as.character(county), sol_type,
        dprod = (labor_prod - initial_prod) / initial_prod)]
}))
dumb <- dcast(dumb, cf + county ~ sol_type, value.var = "dprod")
dumb[, county_name := county_disp[county]]
dumb[, variant := "main"]

probe <- as.data.table(readRDS("diagnostics/imm_la_target_probe_results.rds"))
b_row <- probe[scenario == "B_t1_5pctTotal"]
dumb <- rbind(dumb, data.table(
  cf = "Immigration", county = "6037", realloc = b_row$dprod_nat_rl,
  reorg = b_row$dprod_nat_rg, county_name = "Los Angeles (type-1 target)",
  variant = "alt"))
dumb[, county_name := factor(county_name, levels = rev(c(
  "Cook", "New York", "Los Angeles", "Los Angeles (type-1 target)")))]
dumb[, cf := factor(cf, levels = c("Immigration", "Incr. Concentration",
                                   "Management Diffusion", "Sales Tax"))]
dlong <- melt(dumb, id.vars = c("cf", "county_name", "variant"),
              measure.vars = c("realloc", "reorg"),
              variable.name = "scenario", value.name = "dprod")
dlong[, scenario := fifelse(scenario == "realloc", "Reallocation", "Reorganization")]

p1 <- ggplot(dumb, aes(y = county_name)) +
  geom_vline(xintercept = 0, linewidth = 0.6, color = "grey30") +
  geom_segment(aes(x = realloc, xend = reorg, yend = county_name, alpha = variant),
               linewidth = 1.1, color = "grey45",
               arrow = arrow(length = unit(9, "pt"), type = "closed")) +
  geom_point(data = dlong, aes(x = dprod, fill = scenario, alpha = variant),
             size = 5.5, shape = 21, color = "white", stroke = 0.8) +
  facet_grid(cf ~ ., scales = "free_y", space = "free_y",
             labeller = label_wrap_gen(14)) +
  scale_alpha_manual(values = c(main = 1, alt = 0.45), guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(-0.02, 0.08, 0.02)) +
  scen_fill + proto_theme +
  theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
  labs(x = "Labor productivity change vs. baseline", y = NULL,
       title = "Reorganization reverses the productivity impact",
       subtitle = "Arrow: reallocation-only → full reorganization")
ggsave("diagnostics/out/proto19_1_dumbbell.png", p1, width = 12, height = 10, dpi = 220)

## ============= firm frame for LA immigration (figs 2 and 3) ================
LA <- "6037"
fq <- get_counterfactual_focus_quarter()
nfv <- nf_imm[county == LA][order(worker_type)]$native_fraction
mkf <- function(dt, lab) {
  d <- copy(dt[as.character(county) == LA])
  if ("quarter_year" %in% names(d)) d <- d[quarter_year == fq]
  d[, tot_prod := Reduce(`+`, .SD), .SDcols = b_field_names]
  d[, ns := Reduce(`+`, lapply(seq_len(n_worker_types),
                               function(k) get(e_field_names[k]) * nfv[k]))]
  d[, mult := avg_labor * CSPOP * new_share * weight]
  cols <- c("location_id", "tot_prod", "s_index", "mult", "ns", "E_4")
  o <- d[, ..cols]
  setnames(o, setdiff(cols, "location_id"),
           paste0(setdiff(cols, "location_id"), "_", lab))
  o
}
imm <- rd("16_prod_immigration.rds")
f <- Reduce(function(x, y) merge(x, y, by = "location_id"), list(
  mkf(rd("13_prod_initial.rds"), "b"),
  mkf(imm[sol_type == "realloc"], "rl"),
  mkf(imm[sol_type == "reorg"],   "rg")))
f[, w0    := mult_b / sum(mult_b)]
f[, w_rl  := (mult_rl * ns_rl) / sum(mult_rl * ns_rl)]
f[, w_rg  := (mult_rg * ns_rg) / sum(mult_rg * ns_rg)]
P0 <- f[, sum(tot_prod_b * w0)]

p6 <- as.data.table(readRDS("results/data/06_parameters.rds"))
rho_la <- abs(p6[parm_name == "factor(county)6037:cust_price", coefficients][1])

## ==================== FIGURE 2: between-firm contribution ===================
cb <- rbind(
  f[, .(location_id, scenario = "Reallocation", p_dollar = tot_prod_b / rho_la,
        w0, contrib = 100 * (w_rl - w0) * (tot_prod_b - P0) / P0)],
  f[, .(location_id, scenario = "Reorganization", p_dollar = tot_prod_b / rho_la,
        w0, contrib = 100 * (w_rg - w0) * (tot_prod_b - P0) / P0)])
sums <- cb[, .(tot = sum(contrib)), by = scenario]
sums[, lab := sprintf("between-firm total: %+.1f pp", tot)]
star <- cb[scenario == "Reallocation"][which.max(contrib)]

note_df <- data.table(scenario = "Reallocation", x = star$p_dollar * 0.93,
                      y = star$contrib,
                      lab = "one salon — drop it and the total is +0.2 pp")
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
       title = "Who gains market weight, and how productive are they?",
       subtitle = "Point area = baseline market weight; contributions sum to the between-firm effect")
ggsave("diagnostics/out/proto19_2_between.png", p2, width = 14, height = 7.5, dpi = 220)

## ===================== FIGURE 3: within-firm absorption =====================
wn <- f[, .(dE4 = 100 * (E_4_rg - E_4_rl),
            dP  = 100 * (tot_prod_rg - tot_prod_rl) / P0,
            dS  = s_index_rg - s_index_rl,
            wbar = (w_rl + w_rg) / 2)]
within_tot <- wn[, sum(wbar * dP)]

p3 <- ggplot(wn, aes(x = dE4, y = dP)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey70", linetype = "dashed") +
  geom_point(aes(size = wbar, fill = dS), shape = 21, color = "grey35", stroke = 0.5) +
  scale_fill_gradient2(low = "#762A83", mid = "grey92", high = "#1B7837",
                       midpoint = 0, name = "Δ s-index") +
  scale_size_area(max_size = 15, guide = "none") +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.6, size = 6.5,
           fontface = "bold",
           label = sprintf("labor-weighted within-firm total: %+.1f pp", within_tot)) +
  proto_theme + theme(legend.position = "right", legend.title = element_text(size = 18)) +
  labs(x = "Change in target-type (nail/spa) time share within the firm (pp)",
       y = "Within-firm productivity change\n(reorg − realloc, % of baseline)",
       title = "Reorganization: absorb the low-wage type, de-specialize",
       subtitle = "Point area = market weight; purple = de-specialization")
ggsave("diagnostics/out/proto19_3_within.png", p3, width = 11, height = 7.5, dpi = 220)

## ======================= FIGURE 4: wage response ============================
iw <- as.data.table(readRDS(counterfactual_data_path("13_initial_wages.rds")))
w16 <- as.data.table(readRDS(counterfactual_data_path("16_wages_immigration.rds")))
wcols <- paste0("w", seq_len(n_worker_types))
wb <- as.numeric(iw[county == LA & quarter_year == fq, ..wcols])
wr <- as.numeric(w16[county == LA & quarter_year == fq & sol_type == "realloc", ..wcols])
wg <- as.numeric(w16[county == LA & quarter_year == fq & sol_type == "reorg", ..wcols])
wd <- rbind(
  data.table(type = 1:5, scenario = "Reallocation",   pct = (wr - wb) / wb),
  data.table(type = 1:5, scenario = "Reorganization", pct = (wg - wb) / wb))
wd[, type_lab := sprintf("Type %d\n($%.0f)", type, wb[type])]

## customer-weighted price changes for the subtitle
pr <- merge(
  dedup(as.data.table(readRDS(counterfactual_data_path("13_prod_initial.rds"))))[
    as.character(county) == LA, .(location_id, p0 = newprice,
                                  m0 = avg_labor * CSPOP * new_share * weight)],
  dcast(imm[as.character(county) == LA, .(location_id, sol_type, newprice)],
        location_id ~ sol_type, value.var = "newprice"), by = "location_id")
dp_rl <- pr[, weighted.mean(realloc / p0 - 1, m0)]
dp_rg <- pr[, weighted.mean(reorg   / p0 - 1, m0)]

p4 <- ggplot(wd, aes(x = type_lab, y = pct, fill = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(aes(label = percent(pct, accuracy = 0.1),
                vjust = ifelse(pct < 0, 1.35, -0.5)),
            position = position_dodge(width = 0.72), size = 5.2) +
  annotate("text", x = 4, y = min(wd$pct) - 0.022, label = "↑ shocked type",
           size = 6.5, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.18, 0.12))) +
  scen_fill + proto_theme +
  labs(x = NULL, y = "Equilibrium wage change",
       title = "Frozen organizations force the wage adjustment",
       subtitle = sprintf(
         "LA immigration, +5%% county labor to type 4\nCustomer-weighted price change: %+.1f%% (realloc), %+.1f%% (reorg)",
         100 * dp_rl, 100 * dp_rg))
ggsave("diagnostics/out/proto19_4_wages.png", p4, width = 12, height = 7.5, dpi = 220)

cat("wrote 4 prototypes to diagnostics/out/proto19_{1,2,3,4}_*.png\n")
cat(sprintf("check sums: fig2 realloc %+0.2fpp (expect ~+1.8), reorg %+0.2fpp; fig3 within %+0.2fpp (expect ~-0.4)\n",
            sums[scenario == "Reallocation"]$tot, sums[scenario == "Reorganization"]$tot, within_tot))
