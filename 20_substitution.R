#' =============================================================================
#' STEP 20: Wage-Shock Substitution Patterns at Equilibrium Wages
#' =============================================================================
#' Computes 1% wage-shock substitution elasticities at the cleared baseline
#' equilibrium produced by 13_counterfactual_prep.R, plus expansion-path
#' figures for representative salons.
#'
#' Why this lives after 13_ (was previously a pre-counterfactual step):
#'   - The baseline wage state used to be the supply-regression parm
#'     decomposition `avg_labor:factor(county):factor(quarter_year)` +
#'     `factor(county):avg_labor:E_raw_k`. That decomposition is *not* a
#'     labor-market-clearing wage; at it, many firms ended up at a corner
#'     allocation (degenerate B), so the substitution matrices showed
#'     mostly-zero ediff entries.
#'   - 13_ now solves for cleared equilibrium wages at the focus quarter
#'     (2021.2), and `compile_warm_start_wages.R` seeds it with smoke-derived
#'     warm-starts so all three counties clear or near-clear. Substitution
#'     elasticities computed at those wages are more meaningful (more
#'     non-degenerate B), which is what makes the resulting table denser.
#'
#' Scope:
#'   - The substitution *table* is restricted to the focus quarter, because
#'     equilibrium wages are only solved at the focus quarter.
#'   - The expansion-path *figures* sweep gamma at firm-specific wages for
#'     example salons that happen to live in 2018.1 / 2020.1 / 2020.4. For
#'     those firms we keep the supply-regression parm-decomposition wages
#'     (no equilibrium available outside the focus quarter).
#'
#' Inputs:
#'   - results/data/counterfactuals/13_initial_wages.rds (cleared wages)
#'   - results/data/counterfactuals/13_working_data.rds  (firm panel)
#'   - results/data/06_parameters.rds                    (skill matrix B,
#'                                                        market_parms, rho)
#'
#' Outputs:
#'   - results/out/tables/20_substitute.tex
#'   - results/out/figures/20_middle_decreasing_count.png
#'   - results/out/figures/20_middle_decreasing_count_blank.png
#'   - results/out/figures/20_middle_non_mon{1,2,3}_count.png
#'   - results/out/figures/20_increasing_count.png
#' =============================================================================

library('data.table')
library('lubridate')
library('stringr')
library('ggplot2')
library('gridExtra')
library('kableExtra')

source('config.R')
source('utils/counterfactuals_core.R')

ensure_directory('results/out/tables')
ensure_directory('results/out/figures')

ctx <- load_counterfactual_context()
full_unsmoothed_orig <- copy(ctx$working_data)
initial_wages        <- ctx$initial_wages
market_parms         <- ctx$market_parms
rho                  <- ctx$rho

n_worker_types <- CONFIG$n_worker_types
n_task_types   <- CONFIG$n_task_types
focus_qy       <- get_counterfactual_focus_quarter()

## ---------------------------------------------------------------------------
## tild_theta for the expansion-path figures (gamma-sweep on example salons).
## Built across all county x quarter cells at the supply-regression
## parm-decomposition wages, matching pre-equilibrium behavior. Figures
## reference firms in 2018.1 / 2020.1 / 2020.4 where no cleared equilibrium
## is available.
## ---------------------------------------------------------------------------
tild_theta_parm <- vector(mode = 'list', length = length(CONFIG$counties))
names(tild_theta_parm) <- CONFIG$counties
for (cnty in CONFIG$counties) {
  qy_vec <- as.character(unique(full_unsmoothed_orig[county == cnty, quarter_year]))
  tild_theta_parm[[cnty]] <- vector(mode = 'list', length = length(qy_vec))
  names(tild_theta_parm[[cnty]]) <- qy_vec
  skills <- matrix(market_parms[grep(paste0(cnty, ':avg_labor:B'), names(market_parms))],
                   ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  for (qy in qy_vec) {
    w_mat <- matrix(c(0, market_parms[grep(paste0(cnty, ':avg_labor:E'), names(market_parms))]),
                    ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
    w_mat <- w_mat + market_parms[paste0('avg_labor:factor(county)', cnty, ':factor(quarter_year)', qy)]
    tild_theta_parm[[cnty]][[qy]] <- sweep(
      w_mat + (rho[cnty])^(-1) * skills, 2,
      apply(w_mat + (rho[cnty])^(-1) * skills, 2, min)
    )
  }
}

## ---------------------------------------------------------------------------
## tild_theta for the substitution table: cleared equilibrium wages at the
## focus quarter only (one entry per county).
## ---------------------------------------------------------------------------
tild_theta_cleared <- vector(mode = 'list', length = length(CONFIG$counties))
names(tild_theta_cleared) <- CONFIG$counties
for (cnty in CONFIG$counties) {
  tild_theta_cleared[[cnty]] <- vector(mode = 'list', length = 1L)
  names(tild_theta_cleared[[cnty]]) <- as.character(focus_qy)
  cleared_w <- as.numeric(unlist(
    initial_wages[county == cnty & quarter_year == focus_qy,
                  paste0('w', seq_len(n_worker_types)), with = FALSE],
    use.names = FALSE
  ))
  stopifnot(length(cleared_w) == n_worker_types,
            all(is.finite(cleared_w)))
  w_mat  <- matrix(cleared_w, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  skills <- matrix(market_parms[grep(paste0(cnty, ':avg_labor:B'), names(market_parms))],
                   ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  tild_theta_cleared[[cnty]][[as.character(focus_qy)]] <- sweep(
    w_mat + (rho[cnty])^(-1) * skills, 2,
    apply(w_mat + (rho[cnty])^(-1) * skills, 2, min)
  )
}

innertol <- 1e-08
outertol <- 1e-04

## Shared headcount-from-(alpha, gamma, tild_theta) helper. Indexes into the
## tild_theta dictionary passed in via `which_tt` ("parm" or "cleared").
get_demands <- function(a1, a2, a3, a4, a5, county, quarter_year, gamma,
                        which_tt = "cleared") {
  alpha <- c(a1, a2, a3, a4, a5)
  tt <- if (identical(which_tt, "cleared")) tild_theta_cleared else tild_theta_parm
  tild_mat <- tt[[county]][[as.character(quarter_year)]]
  if (is.null(tild_mat)) return(rep(NA_real_, n_worker_types))
  if (is.finite(gamma) & gamma > 0) {
    A <- exp(-1 / gamma * tild_mat)
    E <- rep(1 / n_worker_types, n_worker_types)
    A[A >= Inf] <- 1e16
    A[A <= 0]   <- 1e-16
    fxpt <- function(p) {
      C <- colSums(t(A) * alpha / colSums(A * p))
      p * C
    }
    for (i in 1:1000000) {
      E_old <- E
      E <- fxpt(E_old)
      if (all(abs(E - E_old) < innertol)) break
    }
    B <- t(t(A) * alpha / colSums(A * E)) * E
  } else if (!is.na(gamma) && gamma == 0) {
    ## no frictions: cheapest worker takes each task
    B <- matrix(0, ncol = n_task_types, nrow = n_worker_types)
    for (col in seq_len(n_task_types)) {
      B[which.min(tild_mat[, col]), col] <- alpha[col]
    }
  } else {
    ## gamma Inf or NA: max frictions (matches 13_counterfactual_prep's
    ## get_demands; NA previously fell into the no-frictions corner here)
    B <- matrix(0, ncol = n_task_types, nrow = n_worker_types)
    B[which.min(rowSums(t(t(tild_mat) * alpha))), ] <- alpha
  }
  B[abs(B) < 1e-16] <- 0
  rowSums(B)
}

## ---------------------------------------------------------------------------
## Substitution table: 1% wage shock on each worker type, on focus-quarter
## firms only, evaluated against cleared equilibrium wages.
## ---------------------------------------------------------------------------
sub_panel <- copy(full_unsmoothed_orig[quarter_year == focus_qy])

## baseline E at cleared wages.
for (i in seq_len(nrow(sub_panel))) {
  sub_panel[i, c("E_1", "E_2", "E_3", "E_4", "E_5") :=
              as.list(get_demands(task_mix_1, task_mix_2, task_mix_3,
                                  task_mix_4, task_mix_5,
                                  county, quarter_year, gamma_invert,
                                  which_tt = "cleared"))]
}
for (idx in seq_len(n_worker_types)) {
  raw_col <- paste0("E_raw_", idx)
  pred_col <- paste0("E_", idx)
  sub_panel[is.na(get(raw_col)), (raw_col) := get(pred_col)]
}
stopifnot(all(!is.na(sub_panel[, c("E_raw_1", "E_raw_2", "E_raw_3", "E_raw_4", "E_raw_5")])))

## Shocked E: rebuild tild_theta_cleared with the w-th row * 1.01 and re-solve.
shock_factor <- 1.01
for (w in seq_len(n_worker_types)) {
  for (cnty in CONFIG$counties) {
    cleared_w <- as.numeric(unlist(
      initial_wages[county == cnty & quarter_year == focus_qy,
                    paste0('w', seq_len(n_worker_types)), with = FALSE],
      use.names = FALSE
    ))
    cleared_w_shocked <- cleared_w
    cleared_w_shocked[w] <- cleared_w_shocked[w] * shock_factor
    w_mat  <- matrix(cleared_w_shocked, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
    skills <- matrix(market_parms[grep(paste0(cnty, ':avg_labor:B'), names(market_parms))],
                     ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
    tild_theta_cleared[[cnty]][[as.character(focus_qy)]] <- sweep(
      w_mat + (rho[cnty])^(-1) * skills, 2,
      apply(w_mat + (rho[cnty])^(-1) * skills, 2, min)
    )
  }
  for (i in seq_len(nrow(sub_panel))) {
    sub_panel[i, paste0(c("E_1", "E_2", "E_3", "E_4", "E_5"), "_", w) :=
                as.list(get_demands(task_mix_1, task_mix_2, task_mix_3,
                                    task_mix_4, task_mix_5,
                                    county, quarter_year, gamma_invert,
                                    which_tt = "cleared"))]
  }
}

## Restore the unshocked tild_theta_cleared.
for (cnty in CONFIG$counties) {
  cleared_w <- as.numeric(unlist(
    initial_wages[county == cnty & quarter_year == focus_qy,
                  paste0('w', seq_len(n_worker_types)), with = FALSE],
    use.names = FALSE
  ))
  w_mat  <- matrix(cleared_w, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  skills <- matrix(market_parms[grep(paste0(cnty, ':avg_labor:B'), names(market_parms))],
                   ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
  tild_theta_cleared[[cnty]][[as.character(focus_qy)]] <- sweep(
    w_mat + (rho[cnty])^(-1) * skills, 2,
    apply(w_mat + (rho[cnty])^(-1) * skills, 2, min)
  )
}

for (enum in seq_len(n_worker_types)) {
  for (eshock in seq_len(n_worker_types)) {
    sub_panel[, (paste0("ediff_", enum, "_", eshock)) :=
                get(paste0("E_", enum, "_", eshock)) - get(paste0("E_", enum))]
  }
}
sub_panel[is.nan(gamma_invert), gamma_invert := Inf]
sub_panel[, gamma_quant := ecdf(gamma_invert)(gamma_invert), by = 'county']

quantfunc <- function(x, q) {
  format(round(quantile(x, q), digits = 3), nsmall = 3)
}
sub_patterns <- sub_panel[, lapply(.SD, quantfunc, q = 0), by = c("county"),
                          .SDcols = names(sub_panel)[grep("ediff", names(sub_panel))]]
sub_patterns[, stat_name := "Min."]
sub_patterns <- rbind(sub_patterns,
                     sub_panel[, lapply(.SD, quantfunc, q = 1), by = c("county"),
                               .SDcols = names(sub_panel)[grep("ediff", names(sub_panel))]],
                     fill = TRUE)
sub_patterns[is.na(stat_name), stat_name := "Max."]
sub_patterns <- rbind(sub_patterns,
                     sub_panel[, lapply(.SD, quantfunc, q = 0.5), by = c("county"),
                               .SDcols = names(sub_panel)[grep("ediff", names(sub_panel))]],
                     fill = TRUE)
sub_patterns[is.na(stat_name), stat_name := "Med."]

sub_patterns <- melt(sub_patterns, id.vars = c("county", "stat_name"),
                     measure = patterns("^ediff"))
sub_patterns[, enum   := str_replace_all(str_extract(variable, "_[0-9]_"), "_", "")]
sub_patterns[, eshock := str_replace_all(str_extract(variable, "_[0-9]$"), "_", "")]
sub_patterns <- dcast(sub_patterns, county + enum ~ eshock + stat_name)

colnames(sub_patterns) <- c("County", "Skill Set", rep(c("Max.", "Med.", "Min."), 5))
setkey(sub_patterns, "County", "Skill Set")
sub_patterns[County == '17031', County := "Cook"]
sub_patterns[County == '36061', County := "New York"]
sub_patterns[County == '6037',  County := "Los Angeles"]

output <- kable(sub_patterns, "latex", align = "c", booktabs = TRUE,
                linesep = c(""), escape = FALSE, caption = NA, label = NA)
output <- add_header_above(output, c(" ", " ",
                                     "Skill Set 1" = 3, "Skill Set 2" = 3,
                                     "Skill Set 3" = 3, "Skill Set 4" = 3,
                                     "Skill Set 5" = 3))
cat(output, file = "results/out/tables/20_substitute.tex")


## ---------------------------------------------------------------------------
## Expansion-path figures: gamma sweep for representative salons. These live
## in non-focus quarters; the figures intentionally use parm-decomposition
## wages (existing pre-equilibrium behavior) because no cleared wage is
## available outside 2021.2.
## ---------------------------------------------------------------------------
full_unsmoothed <- full_unsmoothed_orig

list_todo <- c(
  which(full_unsmoothed$location_id == '03d9b605-d697-461a-abc4-b9fbc502fecc' & full_unsmoothed$quarter_year == 2020.1),
  which(full_unsmoothed$location_id == '8310d870-fb13-414b-ba6c-2902eaf0276f' & full_unsmoothed$quarter_year == 2020.4),
  which(full_unsmoothed$location_id == '6240c56b-a6fd-4c9a-bb4e-3d9aac3254e9' & full_unsmoothed$quarter_year == 2020.4),
  which(full_unsmoothed$location_id == 'c7ea80fe-c02f-4534-ab57-f7188317ea64' & full_unsmoothed$quarter_year == 2018.1),
  which(full_unsmoothed$location_id == '5e616d9e-259e-4ace-a25a-e5037683c78f' & full_unsmoothed$quarter_year == 2020.4)
)

total_res <- data.table()
for (i in list_todo) {
  for (g in seq(from = 0, to = 1000, by = 1)) {
    res <- as.vector(get_demands(full_unsmoothed$task_mix_1[i], full_unsmoothed$task_mix_2[i],
                                 full_unsmoothed$task_mix_3[i], full_unsmoothed$task_mix_4[i],
                                 full_unsmoothed$task_mix_5[i],
                                 full_unsmoothed$county[i], full_unsmoothed$quarter_year[i], g,
                                 which_tt = "parm"))
    res <- data.table(location_id = full_unsmoothed$location_id[i],
                      quarter_year = full_unsmoothed$quarter_year[i],
                      county = full_unsmoothed$county[i], gamma = g,
                      E_1 = res[1], E_2 = res[2], E_3 = res[3], E_4 = res[4], E_5 = res[5])
    total_res <- rbind(total_res, res)
  }
}

total_res[, count_emp := (E_1 > 1e-02) + (E_2 > 1e-02) + (E_3 > 1e-02) +
                        (E_4 > 1e-02) + (E_5 > 1e-02)]
setkey(total_res, "location_id", "quarter_year", "gamma")

## fully decreasing
ggplot(data = total_res[location_id == '03d9b605-d697-461a-abc4-b9fbc502fecc' & quarter_year == 2020.1 & gamma <= 800]) +
  geom_line(aes(x = gamma, y = count_emp)) +
  ylab("# Skill Sets Employed") + xlab("Coordination Cost Parameter") + theme_bw() +
  theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_vline(xintercept = as.numeric(full_unsmoothed[location_id == '03d9b605-d697-461a-abc4-b9fbc502fecc' & quarter_year == 2020.1, "gamma_invert"][1]),
             linetype = "dashed", color = "blue", size = 1)
ggsave("results/out/figures/20_middle_decreasing_count.png", width = 12, height = 8, units = "in")

ggplot(data = total_res[location_id == '03d9b605-d697-461a-abc4-b9fbc502fecc' & quarter_year == 2020.1 & gamma <= 800]) +
  geom_line(aes(x = gamma, y = count_emp), color = "white") +
  ylab("# Skill Sets Employed") + xlab("Coordination Cost Parameter") + theme_bw() +
  theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("results/out/figures/20_middle_decreasing_count_blank.png", width = 12, height = 8, units = "in")

## middle non-mons
ggplot(data = total_res[location_id == '8310d870-fb13-414b-ba6c-2902eaf0276f' & quarter_year == 2020.4 & gamma <= 800]) +
  geom_line(aes(x = gamma, y = count_emp)) +
  ylab("# Skill Sets Employed") + xlab("Coordination Cost Parameter") + theme_bw() +
  theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_vline(xintercept = as.numeric(full_unsmoothed[location_id == '8310d870-fb13-414b-ba6c-2902eaf0276f' & quarter_year == 2020.4, "gamma_invert"][1]),
             linetype = "dashed", color = "blue", size = 1)
ggsave("results/out/figures/20_middle_non_mon1_count.png", width = 6, height = 4, units = "in")

ggplot(data = total_res[location_id == '6240c56b-a6fd-4c9a-bb4e-3d9aac3254e9' & quarter_year == 2020.4 & gamma <= 800]) +
  geom_line(aes(x = gamma, y = count_emp)) +
  ylab("# Skill Sets Employed") + xlab("Coordination Cost Parameter") + theme_bw() +
  theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_vline(xintercept = as.numeric(full_unsmoothed[location_id == '6240c56b-a6fd-4c9a-bb4e-3d9aac3254e9' & quarter_year == 2020.4, "gamma_invert"][1]),
             linetype = "dashed", color = "blue", size = 1)
ggsave("results/out/figures/20_middle_non_mon2_count.png", width = 6, height = 4, units = "in")

ggplot(data = total_res[location_id == 'c7ea80fe-c02f-4534-ab57-f7188317ea64' & quarter_year == 2018.1 & gamma <= 800]) +
  geom_line(aes(x = gamma, y = count_emp)) +
  ylab("# Skill Sets Employed") + xlab("Coordination Cost Parameter") + theme_bw() +
  theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_vline(xintercept = as.numeric(full_unsmoothed[location_id == 'c7ea80fe-c02f-4534-ab57-f7188317ea64' & quarter_year == 2018.1, "gamma_invert"][1]),
             linetype = "dashed", color = "blue", size = 1)
ggsave("results/out/figures/20_middle_non_mon3_count.png", width = 6, height = 4, units = "in")

## increasing
ggplot(data = total_res[location_id == '5e616d9e-259e-4ace-a25a-e5037683c78f' & quarter_year == 2020.4 & gamma <= 800]) +
  geom_line(aes(x = gamma, y = count_emp)) +
  ylab("# Skill Sets Employed") + xlab("Coordination Cost Parameter") + theme_bw() +
  theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_vline(xintercept = as.numeric(full_unsmoothed[location_id == '5e616d9e-259e-4ace-a25a-e5037683c78f' & quarter_year == 2020.4, "gamma_invert"][1]),
             linetype = "dashed", color = "blue", size = 1)
ggsave("results/out/figures/20_increasing_count.png", width = 6, height = 4, units = "in")
