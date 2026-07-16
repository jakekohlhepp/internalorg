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
##     QP-gap readout (console readouts keep the repo's internal names)
##   - results/out/figures/ (.png + .pdf) -- these are manuscript-facing, so
##     they use the paper's notation (skill matrix = Theta, workers = skill
##     sets) and carry a self-contained Note; see the notation comment above
##     SECTION B:
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

  ## Vintage guard. This script compares point estimates from two separate
  ## runs and silently produces a figure whichever files happen to be on disk,
  ## so a partial re-run yields a plausible-looking but meaningless comparison.
  ## 06b writes perms/qp_diagnostics early ("before any solver risk") and its
  ## parameters at the end, so a 06b that died mid-solve leaves a NEW perms file
  ## beside an OLD parameters file -- the figure then reorders one run's matrix
  ## by another run's ranking, which breaks the monotone pattern the figure
  ## exists to show. Warn rather than abort: a deliberate cross-vintage
  ## comparison is still someone's prerogative.
  input_mtime <- function(p) if (file.exists(p)) file.mtime(p) else NA
  vintages <- c(unc = unc_path, con = con_path, perms = perms_path)
  message("06b_compare input vintages:")
  for (nm in names(vintages)) {
    message(sprintf("  %-6s %s  (%s)", nm,
                    format(input_mtime(vintages[[nm]]), "%Y-%m-%d %H:%M"),
                    basename(vintages[[nm]])))
  }
  perms_gap <- abs(as.numeric(difftime(input_mtime(perms_path),
                                       input_mtime(con_path), units = "mins")))
  if (isTRUE(perms_gap > 10)) {
    warning("06b_compare: results/data/06b_perms.rds and ",
            basename(con_path), " were written ",
            round(perms_gap), " minutes apart, but 06b writes both in one run. ",
            "The worker ordering and the constrained estimates are therefore ",
            "from different 06b runs, and the constrained panel will not be ",
            "monotone. Re-run 06b_estimation_monotone.R to completion before ",
            "trusting these figures.", call. = FALSE)
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

  ## Figure text follows the manuscript's notation and house style, not the
  ## repo's internal parameter names (docs/manuscript_terminology.md):
  ##   - the skill matrix is Theta (the repo stores it as `B_raw_<task>_<worker>`
  ##     because of the model-matrix naming; the paper reserves B_j for a firm's
  ##     task ASSIGNMENTS, so calling these cells "B" in a figure clashes with
  ##     the text). Cell (k, i) is theta_i(k), skill set i's skill at task k.
  ##   - workers are "Skill Set 1-5", numbered arbitrarily (p. 33).
  ##   - Greek is spelled out in labels, per the paper's "Log Organization Cost
  ##     (Gamma)" convention -- also avoids missing-glyph risk on the pdf device.
  ##   - explanations go in a "Note:" block under the figure, not in jargon
  ##     subtitles.
  ## The constraint 06b imposes is the paper's "ranked by absolute advantage"
  ## (p. 22): one order of skill sets in which, at EVERY task, each skill set is
  ## at least as skilled as the previous one.

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
  wide[, task_lab := factor(task, levels = rev(seq_len(n_t)), labels = task_labels[rev(seq_len(n_t))])]

  ## Columns are the actual skill sets (paper numbering) laid out least- to
  ## most-skilled. That order is county-specific, so the x key carries its
  ## county ("<label>___<county>") and the facets use free x scales; the suffix
  ## is stripped at render. Without this the axis could only show a rank
  ## position, which is what forced the old "worker rank" (solver) labelling and
  ## left the columns impossible to match against the paper's skill-set tables.
  wide[, x_disp := fifelse(
    rank == 1L,  paste0(worker, "\n(least\nskilled)"),
    fifelse(rank == n_w, paste0(worker, "\n(most\nskilled)"),
            as.character(worker))
  )]
  wide[, x_key := paste0(x_disp, "___", county)]
  x_levels <- unique(wide[order(match(county, county_order), rank), x_key])
  wide[, x_key := factor(x_key, levels = x_levels)]
  strip_key <- function(x) sub("___.*$", "", x)

  ## Panel heading: county + the Frobenius distance between the unconstrained
  ## and constrained matrices -- the quantity the text compares across counties.
  ## (The old heading printed each matrix's own norm, ||Theta_unc|| -> ||Theta_con||,
  ## which is a different quantity and left the text's claim uncheckable here.)
  ## Three lines: a single line does not fit the strip at this panel width and
  ## gets clipped ("robenius distance = ...").
  head_lab <- function(cnty, fdiff, func) sprintf(
    "%s\nFrobenius distance = %.1f\n(%.0f%% of unconstrained)",
    county_label[cnty], fdiff, 100 * fdiff / func
  )

  raw_breaks <- c(-190, -50, -10, 0, 10, 50, 190)   # signed-log legend ticks (real Theta units)
  heat_theme <- theme_bw(base_size = 11) +
    theme(panel.grid = element_blank(), strip.text = element_text(face = "bold", size = 8.6),
          strip.placement = "outside", plot.subtitle = element_text(size = 8.2, colour = "grey35"),
          plot.caption = element_text(size = 7.6, colour = "grey25", hjust = 0),
          axis.text = element_text(size = 8), legend.position = "right",
          legend.key.height = unit(1.15, "cm"))
  write_fig <- function(plot, stem, w, h) {
    save_counterfactual_plot(paste0(stem, ".png"), plot = plot,
                             width = w, height = h, units = "in", dpi = 150)
    save_counterfactual_plot(paste0(stem, ".pdf"), plot = plot,
                             width = w, height = h, units = "in")
    message("wrote results/out/figures/", stem, ".{png,pdf}")
  }

  ## Shared sentences so the three notes stay consistent with each other.
  note_cells <- paste(
    "Each cell is an estimated skill parameter Theta: the skill of a worker",
    "skill set (column) at a task (row), in the same units as the paper's",
    "skill-parameter tables."
  )
  note_constraint <- paste(
    "Unconstrained estimates are the baseline; constrained estimates add the",
    "requirement that skill sets can be ranked by absolute advantage, i.e. that",
    "one ordering exists in which each skill set is at least as skilled as the",
    "previous one at every task. Columns are ordered least- to most-skilled",
    "under that ranking, so under the constraint values never fall from left to",
    "right; skill sets are numbered as in the paper and the ordering differs by",
    "county."
  )
  note_distance <- paste(
    "Each heading reports the Frobenius distance between that county's",
    "unconstrained and constrained matrices (the square root of the summed",
    "squared cell-by-cell changes), with that distance as a percentage of the",
    "unconstrained matrix's own Frobenius norm."
  )
  note_fmt <- function(...) str_wrap(paste("Note:", paste(...)), width = 178)

  ## ----- FIGURE 1: per-county scale (robust-capped), values printed -----
  long <- melt(wide, id.vars = setdiff(names(wide), c("unc", "con", "diff")),
               measure.vars = c("unc", "con"), variable.name = "version", value.name = "value")
  long[, version_f := factor(version, levels = c("unc", "con"),
                             labels = c("Unconstrained", "Constrained"))]
  long[, cap := as.numeric(quantile(abs(value), 0.90, na.rm = TRUE)), by = county]
  long[, fill_scaled := pmax(pmin(value, cap), -cap) / cap]
  long[, txt := ifelse(abs(value) >= 100, sprintf("%.0f", value), sprintf("%.1f", value))]
  long[, lab_b := head_lab(county, fro_diff, fro_unc)]
  lev_b <- unique(long[order(match(county, county_order))]$lab_b)
  long[, col_lab := factor(lab_b, levels = lev_b)]

  p_pc <- ggplot(long, aes(x_key, task_lab, fill = fill_scaled)) +
    geom_tile(colour = "grey92", linewidth = 0.4) +
    geom_text(aes(label = txt, colour = abs(fill_scaled) > 0.6), size = 2.7, show.legend = FALSE) +
    facet_grid(version_f ~ col_lab, switch = "y", scales = "free_x") +
    scale_x_discrete(labels = strip_key) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                         limits = c(-1, 1), breaks = c(-1, 0, 1),
                         labels = c("-1 (county max)", "0", "+1 (county max)"),
                         name = "Skill parameter\n(Theta), scaled\nwithin county") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    labs(x = "Worker skill set, ordered least- to most-skilled", y = NULL,
         caption = note_fmt(
           note_cells, note_constraint,
           "Printed numbers are the estimates themselves; colour is rescaled within each",
           "county and saturates at that county's 90th percentile of |Theta|, so that a",
           "single extreme cell does not wash out the rest. Colours are therefore",
           "comparable within a county but not across counties.",
           note_distance
         )) + heat_theme
  write_fig(p_pc, "06b_skillmatrix_percounty", 12, 6.6)

  ## ----- FIGURE 2: shared signed-log scale (cross-county comparable) -----
  long[, sval := slog(value)]
  lim1 <- slog(max(abs(long$value), na.rm = TRUE))
  p_sl <- ggplot(long, aes(x_key, task_lab, fill = sval)) +
    geom_tile(colour = "grey92", linewidth = 0.4) +
    geom_text(aes(label = txt, colour = abs(sval) > 0.6 * lim1), size = 2.7, show.legend = FALSE) +
    facet_grid(version_f ~ col_lab, switch = "y", scales = "free_x") +
    scale_x_discrete(labels = strip_key) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                         limits = c(-lim1, lim1), breaks = slog(raw_breaks), labels = raw_breaks,
                         name = "Skill parameter\n(Theta)") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    labs(x = "Worker skill set, ordered least- to most-skilled", y = NULL,
         caption = note_fmt(
           note_cells, note_constraint,
           "All six panels share one colour scale, so magnitudes are comparable across",
           "counties; the scale is compressed logarithmically (preserving sign) because a",
           "few cells are an order of magnitude larger than the rest, and its tick labels",
           "are in skill-parameter units.",
           note_distance
         )) + heat_theme
  write_fig(p_sl, "06b_skillmatrix_sharedlog", 12, 6.6)

  ## ----- FIGURE 3: constrained - unconstrained per county -----
  d <- copy(wide)
  d[, sdiff := slog(diff)]
  d[, txt := ifelse(abs(diff) >= 100, sprintf("%+.0f", diff), sprintf("%+.1f", diff))]
  d[, lab_d := head_lab(county, fro_diff, fro_unc)]
  lev_d <- unique(d[order(match(county, county_order))]$lab_d)
  d[, col_lab := factor(lab_d, levels = lev_d)]
  lim2 <- slog(max(abs(d$diff), na.rm = TRUE))
  p_df <- ggplot(d, aes(x_key, task_lab, fill = sdiff)) +
    geom_tile(colour = "grey92", linewidth = 0.4) +
    geom_text(aes(label = txt, colour = abs(sdiff) > 0.6 * lim2), size = 2.9, show.legend = FALSE) +
    facet_wrap(~col_lab, nrow = 1, scales = "free_x") +
    scale_x_discrete(labels = strip_key) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                         limits = c(-lim2, lim2), breaks = slog(raw_breaks), labels = raw_breaks,
                         name = "Change in skill\nparameter (Theta)") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    labs(x = "Worker skill set, ordered least- to most-skilled", y = NULL,
         caption = note_fmt(
           "Each cell is the change in the estimated skill parameter Theta from imposing",
           "the constraint (constrained minus unconstrained) for one worker skill set",
           "(column) at one task (row): red means the constraint raised the estimate, blue",
           "that it lowered it.", note_constraint,
           "The colour scale is compressed logarithmically (preserving sign) and shared",
           "across counties.", note_distance
         )) + heat_theme
  write_fig(p_df, "06b_skillmatrix_diff", 12, 4.6)

  message("06b_compare: done.")
  invisible(NULL)
}

main()
