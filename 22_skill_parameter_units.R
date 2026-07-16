#' =============================================================================
#' STEP 22: Skill Parameters in Interpretable Units (Worst-Worker Normalized)
#' =============================================================================
#' Re-presents the estimated skill matrix B (worker x task; entered into the
#' model as part of Q_j = avg_labor_j * sum_{w,t} B[w,t] * theta[w,t] + qual_exo)
#' in two economically interpretable unit systems. 06 estimates the parameters;
#' 08 reports them in raw utility units; this script transforms them into
#' dollars and percentage-point market-share terms.
#'
#' Each column (task) is normalized so the entry for the *worst worker at that
#' task* equals zero (within the county), and every other worker's value is the
#' gain over that worst baseline. The worst worker is the row index with the
#' minimum theta in the column; because every unit transformation is a positive
#' linear scaling of theta within a column, the worst-worker identity is the
#' same in every unit block.
#'
#' Both blocks price the SAME experiment -- reassigning 1% of task t's own mass
#' (i.e. 0.01 * alphabar_{c,t} in absolute task-mass units) from the worst
#' worker to worker w -- and differ only in the units they report it in.
#'
#' Unit blocks emitted (one column block per task, side-by-side):
#'   1. Willingness-to-pay (USD). (theta[w,t] - theta_worst,t) / |rho_c| *
#'                                Lbar_c * 0.01 * alphabar_{c,t}, where |rho_c|
#'                                is the county disutility from price, Lbar_c is
#'                                the unweighted mean of avg_labor (hours per
#'                                customer) over firm-quarters in county c, and
#'                                alphabar_{c,t} is the mean share of task t in
#'                                the firm's appointment mix. Reads as consumer
#'                                WTP per customer (USD) at a mean-labor firm in
#'                                county c from reassigning 1% of task t's own
#'                                mass from the worst worker to worker w.
#'   2. % market share (pooled).  theta[w,t] * (1 - sbar_c) * Lbar_c *
#'                                alphabar_{c,t}, recentered. Reads as the
#'                                percent change (Delta s / s, not Delta s)
#'                                in market share at the mean firm of moving
#'                                1% of task t mass from the worst worker to
#'                                worker w. Pooled over firm-quarters in
#'                                county c. (The 0.01 of the reassignment
#'                                cancels against the x100 that turns the
#'                                share response into percent, which is why
#'                                no 0.01 appears in this block's factor.)
#'
#' Because the two blocks now share one experiment, within a county they are
#' proportional: share_cell / wtp_cell = |rho_c| * (1 - sbar_c) * 100 for every
#' worker and task.
#'
#' The data variable avg_labor is constructed in cluster.R as
#'   avg_labor := tot_duration / cust_count / 60
#' where tot_duration is summed appointment minutes and the / 60 converts to
#' hours. So Lbar above is in hours of labor per customer.
#'
#' All counties are stacked into one LaTeX table grouped by pack_rows.
#'
#' Inputs:
#'   - results/data/06_parameters.rds (point estimates)
#'   - mkdata/data/04_estimation_sample.rds (firm-level avg_labor, task_mix,
#'                                            salon_share_subdiv)
#'   - mkdata/data/01_keytask.rds (task display labels)
#'
#' Outputs:
#'   - results/out/tables/22_skill_units.tex (one stacked table, all counties;
#'     requires \usepackage{threeparttable} and \usepackage{booktabs})
#'   - results/data/22_skill_units.csv (long-format with both unit blocks,
#'     recentered)
#' =============================================================================

library('data.table')
library('stringr')
library('knitr')
library('kableExtra')

source('config.R')

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
## Use shorter task labels than 08's wages-skills table so the stacked-county
## table fits page width. The unit blocks (WTP + Pct.\ Share) repeat the five
## task columns side-by-side, doubling header width compared to 08.
key_task[clust == 1, rep_text_cluster := "Haircut"]
key_task[clust == 2, rep_text_cluster := "Color"]
key_task[clust == 3, rep_text_cluster := "Style"]
key_task[clust == 4, rep_text_cluster := "Admin"]
key_task[clust == 5, rep_text_cluster := "Nail"]
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

## Per-county column-scaling factors for the Pct. Share block:
## theta[w,t] * (1 - sbar_c) * Lbar_c * alphabar_{c,t}. Linear in theta, so
## the recentered value is the gain in market share at the mean firm from a
## fractional reassignment of task mass.
share_scale_matrix <- function(theta_mat, scale) {
  if (is.null(scale)) return(NULL)
  col_factor <- scale$L * scale$one_minus_s * scale$alpha
  sweep(theta_mat, 2, col_factor, FUN = "*")
}

## Per-county column-scaling factors for the WTP block:
## (theta[w,t] / |rho_c|) * Lbar_c * 0.01 * alphabar_{c,t}. The alphabar_{c,t}
## factor sizes the reassignment at 1% of task t's OWN mass (0.01 *
## alphabar_{c,t} of total task mass), which is the experiment the Pct. Share
## block prices; without it the cell priced a flat 1 percentage point of total
## task mass instead, so the two blocks described different reassignments and
## the dollar figures overstated thin tasks (a task with alphabar = 0.05 does
## not have a percentage point of mass to move in the first place). Linear in
## theta, so recentering then scaling matches scaling then recentering.
wtp_scale_matrix <- function(theta_mat, scale, rho_c) {
  if (is.null(scale)) return(NULL)
  col_factor <- scale$L * 0.01 * scale$alpha
  sweep(theta_mat / abs(rho_c), 2, col_factor, FUN = "*")
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

  ## Identify the worst worker per task on the raw theta. Every unit transform
  ## is a positive linear scaling of theta column-by-column, so the same row
  ## index minimizes each block.
  worst_worker <- apply(theta, 2, which.min)

  list(
    dollars      = recenter_to_worst_worker(
      wtp_scale_matrix(theta, pooled_scale, rho[[cnty]])
    ),
    share_pooled = recenter_to_worst_worker(share_scale_matrix(theta, pooled_scale)),
    pooled_scale = pooled_scale,
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
  worst_lab <- vapply(seq_len(n_task_types),
                      function(t) paste0(task_labels[t], "=W", blocks$worst_worker[t]),
                      character(1))
  message(sprintf("  worst worker per task: %s",
                  paste(worst_lab, collapse = ", ")))
}


## ---- one stacked LaTeX table across all counties ---------------------------
format_block <- function(mat, digits = 2L) {
  data.table(formatC(round(mat, digits), digits = digits, format = "f"))
}

header_block_names <- c("Willingness-to-pay (USD)", "Pct.\\ Share (pooled)")

build_body_for_county <- function(cnty) {
  blocks <- units_by_county[[cnty]]
  body <- data.table(`Worker Skill Set` = seq_len(n_worker_types))
  body <- cbind(body,
                format_block(blocks$dollars),
                format_block(blocks$share_pooled))
  setnames(body, c("worker", paste0("c", seq_len(ncol(body) - 1L))))
  body
}

body_full <- rbindlist(lapply(counties, build_body_for_county), use.names = FALSE)
display_cols <- c("Worker Skill Set", rep(task_labels, length(header_block_names)))

output <- kable(body_full, "latex", align = "c", booktabs = TRUE, linesep = "",
                escape = FALSE, caption = NA, label = NA,
                col.names = display_cols)
block_widths <- rep(n_task_types, length(header_block_names))
header_row   <- c(" " = 1L, setNames(block_widths, header_block_names))
output <- add_header_above(output, header_row, escape = FALSE)

## Group rows by county using pack_rows.
row_cursor <- 1L
for (cnty in counties) {
  end_row <- row_cursor + n_worker_types - 1L
  output <- pack_rows(output, county_label(cnty), row_cursor, end_row,
                      latex_align = "l", escape = FALSE)
  row_cursor <- end_row + 1L
}

## Notes block: general unit definitions only. Per-county scaling constants
## (|rho_c|, Lbar, sbar, alphabar) and the worst-worker map are still emitted
## to the console summary and the CSV for reference; they're omitted here to
## keep the printed caption short. Worst workers are identifiable from the
## table itself as the zero entry in each task column within each county.
build_stacked_notes <- function() {
  intro <- paste0(
    "Each cell is the parameter for worker $w$ at task $t$ minus the parameter ",
    "for the worst worker at that same task \\emph{within the county}, so the ",
    "worst worker reads 0 and all others are gains relative to that ",
    "county-specific baseline."
  )
  units_explain <- paste0(
    "Both blocks price the same experiment --- reassigning 1\\% of task $t$'s ",
    "own mass from the worst worker to worker $w$ at the mean firm --- and ",
    "differ only in units. \\textit{Willingness-to-pay (USD)}: cell $= ",
    "(\\theta[w,t] - \\theta_{\\min,t})\\,/\\,|\\rho_c| \\cdot \\bar L_c \\cdot ",
    "0.01\\,\\bar\\alpha_{c,t}$, where $\\theta[w,t]$ is the estimated skill ",
    "parameter, $\\theta_{\\min,t}$ is the worst worker's value in column $t$, ",
    "$|\\rho_c|$ is the negative-utility coefficient on price, $\\bar L_c$ is ",
    "the unweighted mean of hours of labor per customer over firm-quarters in ",
    "county $c$, and $\\bar\\alpha_{c,t}$ is the mean share of task $t$ in the ",
    "firm's appointment mix. Reads as the consumer WTP per customer (USD) at a ",
    "mean-labor firm in county $c$. ",
    "\\textit{Pct.\\ Share (pooled)}: the same recentered $\\theta$ gain ",
    "multiplied by $\\bar L_c\\,(1-\\bar s_c)\\,\\bar\\alpha_{c,t}$, where each ",
    "$\\bar{\\cdot}$ is the unweighted mean over firm-quarters in county $c$ ",
    "($\\bar L_c$ in hours/customer, $\\bar s_c$ the within-subdivision market ",
    "share, $\\bar\\alpha_{c,t}$ the share of task $t$ in the firm's ",
    "appointment mix). A cell value $\\approx$ the percent change in the ",
    "mean firm's market share (i.e., $\\Delta s_j / s_j \\cdot 100$, not ",
    "$\\Delta s_j \\cdot 100$); the $0.01$ of the reassignment cancels against ",
    "the $\\times 100$ that converts to percent."
  )

  paste(c(intro, units_explain), collapse = " ")
}

## Manually wrap kable output in a threeparttable + tablenotes block.
## kableExtra::footnote(..., escape = FALSE) silently strips lone backslashes
## from the notes string (mangles \textit, \bar, \rho, etc.), so the wrapper
## is built by hand to keep the LaTeX intact.
tabular_tex <- as.character(output)
notes_tex   <- build_stacked_notes()
wrapped <- paste0(
  "\\begin{threeparttable}\n",
  tabular_tex,
  "\n\\begin{tablenotes}[para,flushleft]\n",
  "\\footnotesize\n",
  "\\item[] \\textit{Notes:} ", notes_tex, "\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n"
)

out_path <- file.path("results", "out", "tables", "22_skill_units.tex")
cat(wrapped, file = out_path)
message("Wrote ", out_path)


## ---- long-format CSV dump (recentered values + worst-worker map) ----------
unit_long_rows <- function() {
  rows <- list()
  for (cnty in counties) {
    blocks <- units_by_county[[cnty]]
    for (unit_nm in c("dollars", "share_pooled")) {
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
