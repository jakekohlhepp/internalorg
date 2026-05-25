#' =============================================================================
#' STEP 22: Skill Parameters in Interpretable Units (Worst-Worker Normalized)
#' =============================================================================
#' Re-presents the estimated skill matrix B (worker x task; entered into the
#' model as part of Q_j = avg_labor_j * sum_{w,t} B[w,t] * theta[w,t] + qual_exo)
#' in three additional unit systems so the magnitudes are easier to interpret.
#' 06 estimates the parameters; 08 reports them in raw utility units; this
#' script transforms them into more economically meaningful scales.
#'
#' Each column (task) is normalized so the entry for the *worst worker at that
#' task* equals zero, and every other worker's value is the gain over that
#' worst baseline. The worst worker is the row index with the minimum theta in
#' the column; because every unit transformation is a positive linear scaling
#' of theta within a column, the worst-worker identity is the same in all four
#' unit blocks.
#'
#' Unit blocks emitted (one column block per task, side-by-side):
#'   1. Raw (utility).            theta[w,t] - min_w theta[w,t]
#'   2. Dollars (consumer WTP).   (theta[w,t] / |rho_c|) recentered. Reads as
#'                                $ per (hour of labor per customer) per unit
#'                                of B[w,t] above the worst worker. Multiply
#'                                by avg_labor (hours/customer) and by the
#'                                fraction of B[w,t] you move to convert into
#'                                dollars per customer of consumer WTP.
#'   3. % market share / 1% task  theta[w,t] * (1 - sbar_c) * Lbar_c *
#'      reassignment.             alphabar_{c,t}, recentered. Reads as the
#'                                percentage-point change in market share at
#'                                the mean firm of moving 1% of task t mass
#'                                from the worst worker to worker w. Reported
#'                                for two samples: (a) all county-quarters in
#'                                the estimation sample (pooled), and (b) only
#'                                the counterfactual focus quarter (2021.2).
#'
#' The data variable avg_labor is constructed in cluster.R as
#'   avg_labor := tot_duration / cust_count / 60
#' where tot_duration is summed appointment minutes and the / 60 converts to
#' hours. So Lbar above is in hours of labor per customer.
#'
#' Inputs:
#'   - results/data/06_parameters.rds (point estimates)
#'   - mkdata/data/04_estimation_sample.rds (firm-level avg_labor, task_mix,
#'                                            salon_share_subdiv)
#'   - mkdata/data/01_keytask.rds (task display labels)
#'
#' Outputs:
#'   - results/out/tables/22_skill_units_<county>.tex (per-county wide table
#'     with a threeparttable note block; requires \usepackage{threeparttable})
#'   - results/data/22_skill_units.csv (long-format with all unit blocks,
#'     recentered)
#' =============================================================================

library('data.table')
library('stringr')
library('knitr')
library('kableExtra')

source('config.R')
source('utils/counterfactuals_core.R')  # for get_counterfactual_focus_quarter()

ensure_directory('results/out/tables')
ensure_directory('results/data')

n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
counties       <- CONFIG$counties

county_label <- function(c) {
  switch(c, "17031" = "Cook", "36061" = "New York", "6037" = "Los Angeles", c)
}


## ---- inputs ----------------------------------------------------------------
keytask_path        <- file.path(CONFIG$prep_output_dir, "01_keytask.rds")
parameters_path     <- file.path("results", "data", "06_parameters.rds")
estimation_path     <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
assert_required_files(c(keytask_path, parameters_path, estimation_path))

key_task <- data.table(readRDS(keytask_path))
## Match 08's task-label shortening so this script's tables read the same as
## the wages-skills table the manuscript already uses.
key_task[clust == 5, rep_text_cluster := "Nail/Misc."]
key_task[clust == 3, rep_text_cluster := "Blowdry/Style/Etc."]
task_labels <- vapply(seq_len(n_task_types), function(t) {
  lbl <- key_task[task == t, rep_text_cluster]
  if (length(lbl) == 0L) paste0("Task", t) else lbl[[1]]
}, character(1))

all_results <- readRDS(parameters_path)
parms <- all_results$coefficients
names(parms) <- all_results$parm_name

estimation_sample <- readRDS(estimation_path)
wd <- data.table::as.data.table(estimation_sample$working_data)


## ---- per-county skill matrix theta[w,t] from B_raw_<task>_<worker> ---------
extract_theta <- function(cnty) {
  m <- matrix(NA_real_, nrow = n_worker_types, ncol = n_task_types)
  for (t in seq_len(n_task_types)) {
    for (w in seq_len(n_worker_types)) {
      m[w, t] <- parms[paste0(
        "factor(county)", cnty, ":avg_labor:B_raw_", t, "_", w
      )]
    }
  }
  m
}
theta_by_county <- setNames(lapply(counties, extract_theta), counties)

rho <- vapply(counties, function(c) {
  parms[paste0("factor(county)", c, ":cust_price")]
}, numeric(1))
names(rho) <- counties
stopifnot(all(rho < 0))


## ---- mean-firm scales for the share-elasticity transformation --------------
focus_qy <- get_counterfactual_focus_quarter()

mean_firm_scales <- function(filt) {
  if (nrow(filt) == 0L) return(NULL)
  list(
    L           = mean(filt$avg_labor, na.rm = TRUE),
    alpha       = vapply(seq_len(n_task_types),
                         function(t) mean(filt[[paste0("task_mix_", t)]], na.rm = TRUE),
                         numeric(1)),
    one_minus_s = 1 - mean(filt$salon_share_subdiv, na.rm = TRUE),
    n_firms     = nrow(filt)
  )
}

share_scale_matrix <- function(theta_mat, scale) {
  if (is.null(scale)) return(NULL)
  col_factor <- scale$L * scale$one_minus_s * scale$alpha
  sweep(theta_mat, 2, col_factor, FUN = "*")
}

## Subtract the column minimum (the worst worker's value) from every entry,
## so each cell reads as the gain over the worst worker at that task.
recenter_to_worst_worker <- function(mat) {
  if (is.null(mat)) return(NULL)
  sweep(mat, 2, apply(mat, 2, min, na.rm = TRUE), FUN = "-")
}


unit_blocks_for <- function(cnty) {
  theta        <- theta_by_county[[cnty]]
  pooled_scale <- mean_firm_scales(wd[county == cnty])
  focus_scale  <- mean_firm_scales(wd[county == cnty & quarter_year == focus_qy])

  ## Identify the worst worker per task on the raw theta. Every unit transform
  ## is a positive linear scaling of theta column-by-column, so the same row
  ## index minimizes each block.
  worst_worker <- apply(theta, 2, which.min)

  list(
    raw          = recenter_to_worst_worker(theta),
    dollars      = recenter_to_worst_worker(theta / abs(rho[[cnty]])),
    share_pooled = recenter_to_worst_worker(share_scale_matrix(theta, pooled_scale)),
    share_focus  = recenter_to_worst_worker(share_scale_matrix(theta, focus_scale)),
    pooled_n     = if (is.null(pooled_scale)) NA_integer_ else pooled_scale$n_firms,
    focus_n      = if (is.null(focus_scale))  NA_integer_ else focus_scale$n_firms,
    pooled_scale = pooled_scale,
    focus_scale  = focus_scale,
    worst_worker = worst_worker
  )
}
units_by_county <- setNames(lapply(counties, unit_blocks_for), counties)


## ---- console summary of the scaling factors and worst-worker map ----------
message("\n=== Skill-unit scaling factors ===")
for (cnty in counties) {
  blocks <- units_by_county[[cnty]]
  message(sprintf("\n%s (FIPS %s):", county_label(cnty), cnty))
  message(sprintf("  rho      = %.5f  (|rho| = %.5f, dollars divisor)",
                  rho[[cnty]], abs(rho[[cnty]])))
  if (!is.null(blocks$pooled_scale)) {
    s <- blocks$pooled_scale
    message(sprintf(
      "  pooled (n=%d): Lbar=%.3f hrs/cust, (1-sbar)=%.4f, alphabar=[%s]",
      s$n_firms, s$L, s$one_minus_s,
      paste(sprintf("%.3f", s$alpha), collapse = ", ")
    ))
  }
  if (!is.null(blocks$focus_scale)) {
    s <- blocks$focus_scale
    message(sprintf(
      "  focus  (n=%d): Lbar=%.3f hrs/cust, (1-sbar)=%.4f, alphabar=[%s]",
      s$n_firms, s$L, s$one_minus_s,
      paste(sprintf("%.3f", s$alpha), collapse = ", ")
    ))
  }
  worst_lab <- vapply(seq_len(n_task_types),
                      function(t) paste0(task_labels[t], "=W", blocks$worst_worker[t]),
                      character(1))
  message(sprintf("  worst worker per task: %s",
                  paste(worst_lab, collapse = ", ")))
}


## ---- one wide LaTeX table per county ---------------------------------------
unit_header_for <- function(blocks) {
  hdr <- list(
    "Raw (utility)" = blocks$raw,
    "Dollars (USD)" = blocks$dollars
  )
  if (!is.null(blocks$share_pooled)) {
    hdr[[sprintf("Pct.\\ Share (pooled, n=%d)", blocks$pooled_n)]] <- blocks$share_pooled
  }
  if (!is.null(blocks$share_focus)) {
    hdr[[sprintf("Pct.\\ Share (%s, n=%d)",
                 format(focus_qy), blocks$focus_n)]] <- blocks$share_focus
  }
  hdr
}

format_block <- function(mat, digits = 3L) {
  data.table(formatC(round(mat, digits), digits = digits, format = "f"))
}

## Notes block: defines each unit, the avg_labor convention, |rho|, the mean-
## firm scaling used, and the worst-worker reference per task.
build_notes <- function(cnty, blocks) {
  worst_str <- paste(
    vapply(seq_len(n_task_types), function(t) {
      sprintf("%s = W%d", task_labels[t], blocks$worst_worker[t])
    }, character(1)),
    collapse = "; "
  )
  rho_c <- abs(rho[[cnty]])

  parts <- c(
    sprintf(
      "Each cell is the parameter for worker $w$ at task $t$ minus the parameter for the worst worker at that same task, so the worst worker reads 0 and all others are gains relative to that baseline. Worst worker per task: %s.",
      worst_str
    ),
    sprintf(
      "\\textit{Raw} is utility units. \\textit{Dollars (USD)} divides by $|\\rho_c| = %.4f$; the result is dollars per (hour of labor per customer) per unit of $B[w,t]$. Multiply by \\texttt{avg\\_labor} (\\texttt{tot\\_duration / cust\\_count / 60}, in hours per customer) and by the fraction of $B[w,t]$ moved to convert into per-customer consumer WTP.",
      rho_c
    )
  )

  if (!is.null(blocks$pooled_scale)) {
    s <- blocks$pooled_scale
    parts <- c(parts, sprintf(
      "\\textit{Pct.\\ Share (pooled)} scales the raw parameter by $(1-\\bar s)\\,\\bar L\\,\\bar\\alpha_t$ over $n=%d$ county-quarters: $\\bar L = %.3f$ hours/customer, $1-\\bar s = %.4f$, $\\bar\\alpha = (%s)$. Row difference $\\approx$ percentage-point change in market share at the mean firm from moving 1\\%% of task $t$ mass from the worst worker to worker $w$.",
      s$n_firms, s$L, s$one_minus_s,
      paste(sprintf("%.3f", s$alpha), collapse = ", ")
    ))
  }
  if (!is.null(blocks$focus_scale)) {
    s <- blocks$focus_scale
    parts <- c(parts, sprintf(
      "\\textit{Pct.\\ Share (%s)} uses the same scaling but restricted to the $n=%d$ firms in the counterfactual focus quarter: $\\bar L = %.3f$ hours/customer, $1-\\bar s = %.4f$, $\\bar\\alpha = (%s)$.",
      format(focus_qy), s$n_firms, s$L, s$one_minus_s,
      paste(sprintf("%.3f", s$alpha), collapse = ", ")
    ))
  }

  paste(parts, collapse = " ")
}

## Manually wrap kable output in a threeparttable + tablenotes block.
## kableExtra::footnote(..., escape = FALSE) silently strips lone backslashes
## from the notes string (mangles \textit, \bar, \rho, etc.), so the wrapper
## is built by hand to keep the LaTeX intact.
n_total_cols <- 1L + n_task_types * 4L  # worker col + 4 unit blocks
for (cnty in counties) {
  blocks <- units_by_county[[cnty]]
  header_blocks <- unit_header_for(blocks)

  body <- data.table(`Worker Skill Set` = seq_len(n_worker_types))
  block_widths <- integer(0)
  for (nm in names(header_blocks)) {
    blk <- format_block(header_blocks[[nm]])
    body <- cbind(body, blk)
    block_widths <- c(block_widths, n_task_types)
  }
  setnames(body, c("worker", paste0("c", seq_len(ncol(body) - 1L))))
  display_cols <- c("Worker Skill Set", rep(task_labels, length(header_blocks)))

  output <- kable(body, "latex", align = "c", booktabs = TRUE, linesep = "",
                  escape = FALSE, caption = NA, label = NA,
                  col.names = display_cols)
  header_row <- c(" " = 1L, setNames(block_widths, names(header_blocks)))
  output <- add_header_above(output, header_row, escape = FALSE)

  tabular_tex  <- as.character(output)
  notes_tex    <- build_notes(cnty, blocks)
  wrapped <- paste0(
    "\\begin{threeparttable}\n",
    tabular_tex,
    "\n\\begin{tablenotes}[para,flushleft]\n",
    "\\footnotesize\n",
    "\\item[] \\textit{Notes:} ", notes_tex, "\n",
    "\\end{tablenotes}\n",
    "\\end{threeparttable}\n"
  )

  out_path <- file.path("results", "out", "tables",
                        paste0("22_skill_units_",
                               gsub(" ", "", county_label(cnty)), ".tex"))
  cat(wrapped, file = out_path)
  message("Wrote ", out_path)
}


## ---- long-format CSV dump (recentered values + worst-worker map) ----------
unit_long_rows <- function() {
  rows <- list()
  for (cnty in counties) {
    blocks <- units_by_county[[cnty]]
    for (unit_nm in c("raw", "dollars", "share_pooled", "share_focus")) {
      mat <- blocks[[unit_nm]]
      if (is.null(mat)) next
      for (t in seq_len(n_task_types)) for (w in seq_len(n_worker_types)) {
        rows[[length(rows) + 1L]] <- list(
          county         = cnty,
          county_name    = county_label(cnty),
          unit           = unit_nm,
          task           = t,
          task_label     = task_labels[t],
          worker         = w,
          worst_worker_t = blocks$worst_worker[t],
          value          = mat[w, t]
        )
      }
    }
  }
  rbindlist(rows)
}

long_dt <- unit_long_rows()
csv_path <- file.path("results", "data", "22_skill_units.csv")
fwrite(long_dt, csv_path)
message("Wrote ", csv_path, "  (", nrow(long_dt), " rows)")
