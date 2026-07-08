## 06b_compare.R
## -----------------------------------------------------------------------------
## Unified comparison of the unconstrained (06) vs monotone (06b)
## structural estimates. Supersedes the three former diagnostic scripts
##   - compare_06_vs_06b.R          (parameter comparison tables, console)
##   - compare_06_vs_06b_figures.R  (paired skill-matrix heatmaps, figures)
##   - display_06b_skills.R         (per-county dominance / monotone / QP-gap)
## by reading the SAME saved point estimates as a single source of truth instead
## of recomputing the demand-IV QP from the estimation sample.
##
## Inputs (results/data/):
##   - 06_parameters.rds            unconstrained point estimates
##   - 06b_parameters_monotone.rds  monotone point estimates
##   - 06b_perms.rds                QP-recovered worker rank order (perms_by_county)
##   - 06b_qp_diagnostics.rds       per-perm QP objective + chosen index (optional;
##                                  the QP-gap readout is skipped if absent)
##
## Outputs:
##   - console: parameter comparison tables + per-county dominance / monotone /
##     QP-gap readout
##   - results/out/figures/ (.png + .pdf):
##       06b_skillmatrix_percounty   paired heatmaps, per-county color scale
##       06b_skillmatrix_sharedlog   paired heatmaps, shared signed-log scale
##       06b_skillmatrix_diff        constrained - unconstrained, per county
##
## Runs standalone (outside run_all.R); requires 06_estimation.R and
## 06b_estimation_monotone.R to have produced their parameter files.
## -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library("data.table")
  library("stringr")
  library("ggplot2")
})

source("config.R")
source("utils/counterfactuals_core.R")   # save_counterfactual_plot()

main <- function() {
  unc_path   <- file.path("results", "data", "06_parameters.rds")
  con_path   <- file.path("results", "data", "06b_parameters_monotone.rds")
  perms_path <- file.path("results", "data", "06b_perms.rds")
  qp_path    <- file.path("results", "data", "06b_qp_diagnostics.rds")

  required <- c(unc_path, con_path, perms_path)
  missing <- required[!file.exists(required)]
  if (length(missing) > 0) {
    message("06b_compare: missing required inputs (run 06_estimation.R and ",
            "06b_estimation_monotone.R first):\n  ",
            paste(missing, collapse = "\n  "), "\nSkipping.")
    return(invisible(NULL))
  }

  n_t <- CONFIG$n_task_types
  n_w <- CONFIG$n_worker_types

  county_label <- c(`17031` = "Cook (Chicago)",
                    `36061` = "New York (Manhattan)",
                    `6037`  = "Los Angeles")

  froben <- function(M) sqrt(sum(M^2, na.rm = TRUE))
  slog   <- function(x) sign(x) * log1p(abs(x))   # signed log: symmetric, 0 -> 0

  ## ---- load saved estimates and build the merged long table once ----
  unc   <- as.data.table(readRDS(unc_path))
  con   <- as.data.table(readRDS(con_path))
  perms <- readRDS(perms_path)$perms_by_county
  qp    <- if (file.exists(qp_path)) readRDS(qp_path) else NULL

  stopifnot(identical(sort(unc$parm_name), sort(con$parm_name)))

  merged <- merge(
    unc[, .(parm_name, demand, unc = coefficients)],
    con[, .(parm_name, con = coefficients)],
    by = "parm_name", sort = FALSE
  )
  merged[, county := str_extract(parm_name, "(?<=factor\\(county\\))[0-9]+")]
  merged[, quarter_year := str_extract(parm_name, "(?<=factor\\(quarter_year\\))[0-9]+\\.?[0-9]*")]
  merged[, other := {
    o <- parm_name
    o <- str_replace(o, "factor\\(county\\)[0-9]+", "")
    o <- str_replace(o, "factor\\(quarter_year\\)[0-9]+\\.?[0-9]*", "")
    o <- str_replace_all(o, "^:|:$", "")
    o
  }]
  merged[, diff := con - unc]
  merged[, abs_diff := abs(diff)]
  merged[, rel_diff := ifelse(abs(unc) > 1e-10, diff / unc, NA_real_)]

  ## B[task, worker] for one county, from a chosen value column ("unc"/"con").
  extract_B <- function(dt, cnty, value_col) {
    out <- matrix(NA_real_, n_t, n_w)
    rows <- dt[county == cnty & grepl("avg_labor:B_raw_", other)]
    for (i in seq_len(nrow(rows))) {
      m <- str_match(rows$other[i], "B_raw_([0-9]+)_([0-9]+)$")
      out[as.integer(m[1, 2]), as.integer(m[1, 3])] <- rows[[value_col]][i]
    }
    out
  }

  ## ===========================================================================
  ## SECTION A: comparison tables (console)
  ## ===========================================================================

  cat("\n========================================================================\n")
  cat("  TABLE 1: Price sensitivity and reference org cost (per county)\n")
  cat("           rho display follows 08's convention: sign-flipped on price.\n")
  cat("========================================================================\n")
  tab1 <- merged[other %in% c("cust_price", "org_cost") & is.na(quarter_year)]
  tab1[, c("unc_disp", "con_disp") := list(
    ifelse(other == "cust_price", -unc, unc),
    ifelse(other == "cust_price", -con, con)
  )]
  tab1[, name := ifelse(other == "cust_price", "Price Sensitivity",
                        "Reference Org. Cost")]
  print(tab1[, .(County = county_label[county], Parameter = name,
                 unconstrained = round(unc_disp, 4),
                 constrained   = round(con_disp, 4),
                 diff = round(con_disp - unc_disp, 4))][order(County, Parameter)])

  cat("\n========================================================================\n")
  cat("  TABLE 2: Wage premiums per worker type (E_raw_<i>:avg_labor)\n")
  cat("           Worker type 1 is normalized to 0 (preamble.R:36).\n")
  cat("========================================================================\n")
  tab2 <- merged[grepl("avg_labor:E_raw_", other) & is.na(quarter_year)]
  tab2[, worker := as.integer(str_extract(other, "(?<=E_raw_)[0-9]+"))]
  print(tab2[, .(County = county_label[county], `Worker type` = worker,
                 unconstrained = round(unc, 3),
                 constrained   = round(con, 3),
                 diff = round(diff, 3))][order(County, `Worker type`)])

  cat("\n========================================================================\n")
  cat("  TABLE 3: Skill matrices B[task, worker] in QP rank order (per county)\n")
  cat("           Columns reordered: rank 1 = least productive, rank n = most.\n")
  cat("           Per county: unc/con cells, constrained monotone check, QP gap.\n")
  cat("========================================================================\n")
  for (cnty in CONFIG$counties) {
    cnty_str <- as.character(cnty)
    pi_c <- as.integer(perms[[cnty_str]])
    M_unc <- extract_B(merged, cnty_str, "unc")
    M_con <- extract_B(merged, cnty_str, "con")
    M_unc_re <- M_unc[, pi_c, drop = FALSE]
    M_con_re <- M_con[, pi_c, drop = FALSE]

    cat("\n--- ", cnty_str, " — ", county_label[cnty_str],
        "  (rank order: ", paste(pi_c, collapse = " -> "), ") ---\n", sep = "")
    combined <- matrix(sprintf("%8.2f / %8.2f", M_unc_re, M_con_re),
                       nrow = n_t, ncol = n_w)
    rownames(combined) <- paste0("task ", seq_len(n_t))
    colnames(combined) <- paste0("rank ", seq_len(n_w), " (w", pi_c, ")")
    cat("Each cell: unconstrained / constrained\n")
    print(noquote(combined))

    ## constrained B should be non-decreasing across ranks (each entry >= 0).
    diff_mat <- t(apply(M_con_re, 1, diff))
    rownames(diff_mat) <- rownames(combined)
    colnames(diff_mat) <- paste0("rank ", 2:n_w, "-", 1:(n_w - 1))
    cat("\nConstrained monotone diff per task (each entry should be >= 0):\n")
    print(round(diff_mat, 3))

    fro_unc <- froben(M_unc)
    fro_con <- froben(M_con)
    fro_diff <- froben(M_unc - M_con)
    linf_diff <- max(abs(M_unc - M_con))
    cat(sprintf(
      "  ||B_unc||_F = %.3f, ||B_con||_F = %.3f, ||diff||_F = %.3f (relative %.3f), L_inf diff = %.3f\n",
      fro_unc, fro_con, fro_diff, fro_diff / fro_unc, linf_diff
    ))

    if (!is.null(qp)) {
      obj_vec  <- qp$obj_by_perm_by_county[[cnty_str]]
      best_idx <- qp$best_idx_by_county[[cnty_str]]
      if (!is.null(obj_vec) && !is.null(best_idx)) {
        best_obj <- obj_vec[best_idx]
        next_obj <- min(obj_vec[-best_idx], na.rm = TRUE)
        rel_gap <- if (best_obj > 0) (next_obj - best_obj) / best_obj else next_obj - best_obj
        cat(sprintf(
          "  QP obj at chosen perm = %.6g; next-best perm obj = %.6g; relative gap = %.4f\n",
          best_obj, next_obj, rel_gap
        ))
      }
    }
  }

  cat("\n========================================================================\n")
  cat("  TABLE 4: Time-varying parameters (quarter-year level shifts)\n")
  cat("           Demand level (FE), Cost level (FE), Material cost (task mix)\n")
  cat("========================================================================\n")
  tab4 <- merged[!is.na(quarter_year)]
  tab4[, group := fcase(
    other == "" & demand == TRUE, "Demand Level",
    other == "" & demand == FALSE, "Cost Level",
    other == "avg_labor", "Wage Level",
    grepl("task_mix", other), "Material Cost"
  )]
  tab4 <- tab4[!is.na(group)]
  movement <- tab4[, .(
    n = .N,
    mean_unc = mean(unc), mean_con = mean(con),
    mean_diff = mean(diff),
    rmse_diff = sqrt(mean(diff^2)),
    max_abs_diff = max(abs(diff))
  ), keyby = .(County = county_label[county], group)]
  print(movement[, .(County, group, n,
                     mean_unc = round(mean_unc, 3),
                     mean_con = round(mean_con, 3),
                     mean_diff = round(mean_diff, 3),
                     rmse_diff = round(rmse_diff, 3),
                     max_abs_diff = round(max_abs_diff, 3))])

  cat("\n========================================================================\n")
  cat("  TABLE 5: Headline summary (Frobenius moves per parameter block)\n")
  cat("========================================================================\n")
  classify_block <- function(other, demand, qy) {
    fcase(
      !is.na(qy),                                    "time FE / material",
      other %in% c("cust_price", "org_cost"),        "price / org cost",
      grepl("avg_labor:E_raw_", other),              "wage premiums",
      grepl("avg_labor:B_raw_", other),              "skill matrix",
      default = "other"
    )
  }
  merged[, block := classify_block(other, demand, quarter_year)]
  overall <- merged[, .(
    n         = .N,
    Frob_unc  = round(sqrt(sum(unc^2)),  3),
    Frob_diff = round(sqrt(sum(diff^2)), 3)
  ), keyby = .(County = county_label[county], Block = block)]
  overall[, Relative := round(ifelse(Frob_unc > 0, Frob_diff / Frob_unc, NA_real_), 3)]
  print(overall[Block != "other"])

  ## ===========================================================================
  ## SECTION B: skill-matrix heatmaps (figures)
  ## ===========================================================================

  county_order <- as.character(CONFIG$counties)
  county_order <- county_order[county_order %in% names(county_label)]

  ## task display labels ("representative text" from the keytask lookup, the same
  ## source 08_display_estimates.R / 22_skill_parameter_units.R use). Falls back
  ## to a generic label if the lookup is unavailable so the figures still render.
  kt_path <- file.path(CONFIG$prep_output_dir, "01_keytask.rds")
  task_labels <- paste0("Task ", seq_len(n_t))
  if (file.exists(kt_path)) {
    kt <- as.data.table(readRDS(kt_path))
    task_labels <- vapply(seq_len(n_t), function(t) {
      lbl <- kt[task == t, rep_text_cluster]
      if (length(lbl) == 0L) paste0("Task ", t) else lbl[[1]]
    }, character(1))
  }

  ## wide table: one row per (county, task, rank) with unc, con, diff
  wide <- rbindlist(lapply(county_order, function(cnty) {
    pi_c  <- as.integer(perms[[cnty]])
    M_unc <- extract_B(merged, cnty, "unc")[, pi_c, drop = FALSE]
    M_con <- extract_B(merged, cnty, "con")[, pi_c, drop = FALSE]
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
  wide[, task_lab := factor(task, levels = rev(seq_len(n_t)), labels = task_labels[rev(seq_len(n_t))])]

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

  ## ----- FIGURE 1: per-county scale (robust-capped), values printed -----
  long <- melt(wide, id.vars = setdiff(names(wide), c("unc", "con", "diff")),
               measure.vars = c("unc", "con"), variable.name = "version", value.name = "value")
  long[, version_f := factor(version, levels = c("unc", "con"),
                             labels = c("Unconstrained", "Constrained"))]
  long[, cap := as.numeric(quantile(abs(value), 0.90, na.rm = TRUE)), by = county]
  long[, fill_scaled := pmax(pmin(value, cap), -cap) / cap]
  long[, txt := ifelse(abs(value) >= 100, sprintf("%.0f", value), sprintf("%.1f", value))]
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
    labs(subtitle = "Columns ordered by QP worker rank (worst -> best). Constrained rows ramp left-to-right = monotone ladder. Cell = true B; colour capped at each county's 90th pctile |B|.",
         x = "worker rank (within county)", y = NULL) + heat_theme
  write_fig(p_pc, "06b_skillmatrix_percounty", 12, 6.2)

  ## ----- FIGURE 2: shared signed-log scale (cross-county comparable) -----
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
    labs(subtitle = "One colour scale across all six panels: magnitudes comparable across counties. NYC's unconstrained spike (Administrative = -190) dominates, as it should.",
         x = "worker rank (within county)", y = NULL) + heat_theme
  write_fig(p_sl, "06b_skillmatrix_sharedlog", 12, 6.2)

  ## ----- FIGURE 3: constrained - unconstrained per county -----
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
    labs(subtitle = "Red = constraint raised B for that (task, worker rank); blue = lowered it. NYC Administrative is the noisy spike being flattened; LA barely moves.",
         x = "worker rank (within county)", y = NULL) + heat_theme
  write_fig(p_df, "06b_skillmatrix_diff", 12, 3.9)

  message("06b_compare: done.")
  invisible(NULL)
}

main()
