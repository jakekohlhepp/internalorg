#' =============================================================================
#' STEP 12b: Model Validation for the Monotone-Restricted Estimates (06b)
#' =============================================================================
#' Repeats, in one program, the two steps that turn structural estimates into
#' the observed-vs-model validation artifacts, but using the workers-as-rows
#' monotone estimates from 06b_estimation_monotone.R in place of the main
#' estimates:
#'   (1) the 09_invert_gammas.R gamma inversion for the full firm-quarter panel
#'       (the wage-adjusted cost matrix tild_theta, the no-friction s-index
#'       bound, and the bisection inversion all depend on the parameter
#'       estimates, so they are recomputed here under the monotone estimates);
#'   (2) the 12_validate.R comparison of model-implied employee time
#'       allocations B against the raw observed staff-task shares
#'       (correlation table, marginal and bivariate density plots).
#' Tolerances mirror the main pipeline: CONFIG$innertol/CONFIG$outertol for the
#' gamma inversion (as in 09) and 1e-8 for the internals fixed point (as in 12).
#'
#' Inputs:
#'   - results/data/06b_parameters_monotone.rds
#'   - mkdata/data/01_staff_task.rds
#'   - mkdata/data/01_staff_task_full.rds
#'   - mkdata/data/01_worker_type_lookup.rds
#'   - mkdata/data/04_estimation_sample.rds
#'
#' Outputs:
#'   - results/data/12b_withgammas_monotone.rds
#'   - results/data/12b_data_for_counterfactuals_monotone.rds
#'   - results/out/tables/12b_validate_corr_monotone.tex
#'   - results/out/figures/12b_gamma_dist_monotone.png
#'   - results/out/figures/12b_marginal_cut_monotone.png
#'   - results/out/figures/12b_marginal_color_monotone.png
#'   - results/out/figures/12b_marginal_other3_monotone.png
#'   - results/out/figures/12b_bivariate_cut_color_monotone.png
#'   - results/out/figures/12b_bivariate_cut_other3_monotone.png
#'   - results/out/figures/12b_bivariate_color_other3_monotone.png
#' =============================================================================

library('data.table')
library('lubridate')
library('stringr')
library('SQUAREM')
library('ggplot2')
library('gridExtra')
library('kableExtra')
library('matrixStats')

source('config.R')

ensure_directory(file.path('results', 'data'))
ensure_directory(file.path('results', 'out', 'tables'))
ensure_directory(file.path('results', 'out', 'figures'))

staff_task_path         <- file.path(CONFIG$prep_output_dir, "01_staff_task.rds")
staff_task_full_path    <- file.path(CONFIG$prep_output_dir, "01_staff_task_full.rds")
estimation_sample_path  <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
worker_type_lookup_path <- file.path(CONFIG$prep_output_dir, "01_worker_type_lookup.rds")
parameters_path         <- file.path("results", "data", "06b_parameters_monotone.rds")
assert_required_files(c(staff_task_path, staff_task_full_path,
                        estimation_sample_path, worker_type_lookup_path,
                        parameters_path))

#' -----------------------------------------------------------------------------
#' PART 1: gamma inversion under the monotone estimates (mirrors 09)
#' -----------------------------------------------------------------------------

##### reuse the baseline within-firm assignment from 01_build_data.R so that
##### the cutlevel and firm-type counts stay in lockstep with the upstream
##### worker-type lookup instead of being silently re-derived here.
staff_task <- data.table(readRDS(staff_task_path))
worker_type_lookup_artifact <- readRDS(worker_type_lookup_path)
effective_cutlevels <- data.table(worker_type_lookup_artifact$effective_cutlevels)
stopifnot(all(c("location_id", "quarter_year", "types_observed_firm") %in%
              names(effective_cutlevels)))

staff_task <- merge(
  staff_task,
  effective_cutlevels[, .(location_id, quarter_year, types_observed_firm)],
  by = c("location_id", "quarter_year"),
  all.x = TRUE
)
stopifnot(nrow(staff_task[is.na(types_observed_firm)]) == 0L)

firm_groups <- unique(staff_task[, c("types_observed_firm", "service_mix_id", "quarter_year", "location_id")])
stopifnot(uniqueN(firm_groups[, c("quarter_year", "location_id")]) == nrow(firm_groups))

### restore the unsmoothed data just to get task_mix and s_index, and set group numbers.
full_unsmoothed <- readRDS(staff_task_full_path)
cust_counts <- unique(full_unsmoothed[, .(location_id, quarter_year, cust_count)])
stopifnot(nrow(cust_counts) == uniqueN(cust_counts[, .(location_id, quarter_year)]))
emps_counts <- unique(full_unsmoothed[, .(location_id, quarter_year, emps)])
stopifnot(nrow(emps_counts) == uniqueN(emps_counts[, .(location_id, quarter_year)]))
full_unsmoothed <- unique(full_unsmoothed[, .SD, .SDcols = c(colnames(full_unsmoothed)[grep("^task_mix", colnames(full_unsmoothed))],
                                                              "s_index", "location_id", "quarter_year", "county", "tot_duration")])
full_unsmoothed <- merge(full_unsmoothed, firm_groups[, c("location_id", "quarter_year", "types_observed_firm", "service_mix_id")],
                         by = c("location_id", "quarter_year"), all.x = TRUE)

## restrict to the three counties and quarters
full_unsmoothed <- full_unsmoothed[county %in% CONFIG$counties_padded &
                                     quarter_year %in% CONFIG$estimation_quarters, ]

## Outlier screen, ported from 09_invert_gammas.R (see the comment there).
## One 2021.2 salon-quarter (a single-staffer whose only bookings are
## multi-day "Administrative" calendar blocks) carries an avg_labor of 61.3
## hours per customer against a sample maximum of 4.4 otherwise. Assert the
## known shape of the outlier before dropping, so a change in the underlying
## data surfaces here instead of silently shifting the counterfactuals.
avg_labor_screen <- merge(
  full_unsmoothed[, .(location_id, quarter_year, county, tot_duration, s_index)],
  cust_counts, by = c("location_id", "quarter_year"), all.x = TRUE)
avg_labor_screen <- merge(avg_labor_screen, emps_counts,
                          by = c("location_id", "quarter_year"), all.x = TRUE)
stopifnot(nrow(avg_labor_screen[is.na(cust_count) | is.na(emps)]) == 0)
avg_labor_screen[, avg_labor := tot_duration / cust_count / 60]
outlier_2021q2 <- avg_labor_screen[quarter_year == 2021.2 & avg_labor > 5]
stopifnot(nrow(outlier_2021q2) == 1)
stopifnot(outlier_2021q2$avg_labor > 60)
stopifnot(outlier_2021q2$s_index == 0)
stopifnot(outlier_2021q2$emps == 1)
excluded_salon_quarters <- avg_labor_screen[avg_labor > 60,
                                            .(location_id, quarter_year)]
print(paste0("12b: excluding ", nrow(excluded_salon_quarters),
             " salon-quarter(s) with avg_labor > 60"))
full_unsmoothed <- full_unsmoothed[!excluded_salon_quarters,
                                   on = c("location_id", "quarter_year")]

### first construct wage-adjusted skill matrix from the MONOTONE estimates
all_results <- readRDS(parameters_path)
market_parms <- all_results$coefficients
names(market_parms) <- all_results$parm_name

tild_theta <- vector(mode = 'list', length = length(CONFIG$counties))
names(tild_theta) <- CONFIG$counties

for (cnty in names(tild_theta)) {
  w_mat <- matrix(c(0, market_parms[grep(paste0(cnty, ":avg_labor:E"), names(market_parms))]),
                  ncol = CONFIG$n_task_types, nrow = CONFIG$n_worker_types, byrow = FALSE)
  skills <- matrix(market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
                   ncol = CONFIG$n_task_types, nrow = CONFIG$n_worker_types, byrow = FALSE)
  rho <- market_parms[grep(paste0(cnty, ":cust_price$"), names(market_parms))]
  tild_theta[[cnty]] <- w_mat + (rho)^(-1) * skills
  tild_theta[[cnty]] <- sweep(tild_theta[[cnty]], 2, apply(tild_theta[[cnty]], 2, min))

}

#### Get all estimated gammas from the estimation sample (for sanity/merge).
estimation_sample <- readRDS(estimation_sample_path)
working_data <- data.table(estimation_sample$working_data)

source('preamble.R')

## set innertolerance, and choose if we want to accelerate the fixedpoint
innertol <- CONFIG$innertol
outertol <- CONFIG$outertol
pl_on <- CONFIG$pl_on

full_unsmoothed <- merge(full_unsmoothed,
                         working_data[, .SD, .SDcols = c("location_id", "quarter_year",
                                                          "E_raw_1", "E_raw_2", "E_raw_3", "E_raw_4", "E_raw_5",
                                                          colnames(working_data)[grep("^B_raw_", colnames(working_data))])],
                         by = c("location_id", "quarter_year"), all.x = TRUE)

## match county coding used by the cost matrix (drop leading zero for 06037 -> 6037 if CONFIG uses that form)
full_unsmoothed[!(county %in% names(tild_theta)) & county == "06037", county := "6037"]


## how many firm quarters were in the estimation sample?
print(paste0("Salon-quarters in estimation sample: ", nrow(full_unsmoothed[!is.na(E_raw_1), ])))

print(paste0("Salon-quarters NOT in estimation sample: ", nrow(full_unsmoothed[is.na(E_raw_1), ])))


### for firms that have a s-index greater than the maximum, set gamma to 0.
# maximum s_index if no frictions.
sbound <- Vectorize(function(i) {
  a1 <- full_unsmoothed[i, ]$task_mix_1[1]
  a2 <- full_unsmoothed[i, ]$task_mix_2[1]
  a3 <- full_unsmoothed[i, ]$task_mix_3[1]
  a4 <- full_unsmoothed[i, ]$task_mix_4[1]
  a5 <- full_unsmoothed[i, ]$task_mix_5[1]
  county <- full_unsmoothed[i, ]$county[1]
  alpha <- c(a1, a2, a3, a4, a5)
  B <- matrix(0, ncol = 5, nrow = 5)
  for (col in 1:5) {
    B[which.min(tild_theta[[county]][, col]), col] <- alpha[col]
  }
  E <- rowSums(B)
  Brel <- t(t(B / E) / alpha)
  return(sum(B * spec_log(Brel)))
})

full_unsmoothed$s_bound <- sbound(1:nrow(full_unsmoothed))

# how many salons have an s-index beyond the bound?
print(paste0("Salon-quarters with s-index at or beyond the no-friction bound: ",
             nrow(full_unsmoothed[s_index >= s_bound])))

# mutual information has infinite marginal cost near maximum, so these have org cost of 0.
full_unsmoothed[s_index >= s_bound, gamma_invert := 0]


### invert gamma for everyone except those with 0 s-index or gamma above the bound
full_unsmoothed[, to_invert := s_index > 0 & s_index < s_bound]
start_gamma_found <- 1
max_gamma_found <- 10000

findgamma <- function(i) {
  a1 <- full_unsmoothed[i, ]$task_mix_1[1]
  a2 <- full_unsmoothed[i, ]$task_mix_2[1]
  a3 <- full_unsmoothed[i, ]$task_mix_3[1]
  a4 <- full_unsmoothed[i, ]$task_mix_4[1]
  a5 <- full_unsmoothed[i, ]$task_mix_5[1]
  county <- full_unsmoothed[i, ]$county[1]
  s_actual <- full_unsmoothed[i]$s_index[1]
  get_struct <- function(gamma) {
    alpha <- c(a1, a2, a3, a4, a5)
    A <- exp(-1 / gamma * (tild_theta[[county]]))
    E <- rep(0.2, 5)
    A[A >= Inf] <- 1e16
    A[A <= 0] <- 1e-16
    fxpt <- function(p) {
      C <- colSums(t(A) * alpha / colSums(A * p))
      return(p * C)
    }
    E <- squarem(E, fixptfn = fxpt, control = list(maxiter = 100000, tol = innertol))$par
    B <- t(t(A) * alpha / colSums(A * E)) * E
    B[abs(B) < 1e-16] <- 0
    Brel <- t(t(B / E) / alpha)
    return(s_actual - sum(B * spec_log(Brel)))
  }
  gamma <- bisection(get_struct, a = start_gamma_found, b = max_gamma_found, ftol = outertol, xtol = outertol, n = 10000)
  stopifnot(gamma$conv)
  return(gamma$root)

}


if (pl_on) {
  if (get_os() == "windows") {
    clust <- makeCluster(get_core_count(CONFIG))
    clusterExport(
      clust,
      c("CONFIG", "full_unsmoothed", "tild_theta", "start_gamma_found",
        "max_gamma_found", "innertol", "outertol", "findgamma",
        "bisection", "spec_log"),
      envir = environment()
    )
    clusterEvalQ(clust, library("data.table"))
    clusterEvalQ(clust, library("SQUAREM"))

    temp <- data.table(do.call(rbind, parLapply(clust, which(full_unsmoothed$to_invert), findgamma)))
    stopCluster(clust)
  } else {
    temp <- data.table(do.call(rbind, mclapply(which(full_unsmoothed$to_invert), findgamma, mc.cores = get_core_count(CONFIG))))
  }
  full_unsmoothed[which(full_unsmoothed$to_invert), gamma_invert := temp$V1]

} else {
  for (i in which(full_unsmoothed$to_invert)) {
    full_unsmoothed[i, gamma_invert := findgamma(i)]
    print(i)
  }

}


# the salons without a gamma invert have an sindex of 0
stopifnot(all(full_unsmoothed[is.na(gamma_invert)]$s_index == 0))

# firms with an s-index of 0 should have gamma invert set to infinite
# for counterfactuals we assume they choose a 0 complexity structure
full_unsmoothed[s_index == 0, gamma_invert := Inf]

stopifnot(nrow(full_unsmoothed[is.na(gamma_invert)]) == 0)

saveRDS(full_unsmoothed, file.path("results", "data", "12b_withgammas_monotone.rds"))

### plot the distribution of gamma
ggplot(full_unsmoothed, aes(x = gamma_invert)) +
  geom_histogram(color = "black", fill = "lightblue", linewidth = 0.5, bins = 40) +
  ylab("Salon-Quarter Count") + xlab("Org. Cost Parameter") +
  theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(file.path("results", "out", "figures", "12b_gamma_dist_monotone.png"), width = 8, height = 4, units = "in")

#' -----------------------------------------------------------------------------
#' PART 2: model-vs-data validation (mirrors 12)
#' -----------------------------------------------------------------------------

get_internals <- function(a1, a2, a3, a4, a5, county, gamma) {
  alpha <- c(a1, a2, a3, a4, a5)
  if (is.finite(gamma) & gamma > 0) {

    alpha <- c(a1, a2, a3, a4, a5)
    ## this function will return matrix given gamma
    A <- exp(-1 / gamma * (tild_theta[[county]]))
    E <- rep(0.2, 5)
    A[A >= Inf] <- 1e16
    A[A <= 0] <- 1e-16
    fxpt <- function(p) {
      C <- colSums(t(A) * alpha / colSums(A * p))
      return(p * C)
    }
    for (i in 1:1000000) {
      E_old <- E
      E <- fxpt(E_old)
      if (all(abs(E - E_old) < innertol)) break
    }
    B <- t(t(A) * alpha / colSums(A * E)) * E
  } else if (gamma == 0) {
    # no frictions
    B <- matrix(0, ncol = 5, nrow = 5)
    for (col in 1:5) {
      B[which.min(tild_theta[[county]][, col]), col] <- alpha[col]
    }
  } else {
    # max frictions
    B <- matrix(0, ncol = 5, nrow = 5)
    B[which.min(rowSums(t(t(tild_theta[[county]]) * alpha))), ] <- alpha
  }

  return(as.vector(B))
}

innertol <- 1e-08
for (i in 1:nrow(full_unsmoothed)) {
  full_unsmoothed[i, c("B_1_1", "B_1_2", "B_1_3", "B_1_4", "B_1_5",
                       "B_2_1", "B_2_2", "B_2_3", "B_2_4", "B_2_5",
                       "B_3_1", "B_3_2", "B_3_3", "B_3_4", "B_3_5",
                       "B_4_1", "B_4_2", "B_4_3", "B_4_4", "B_4_5",
                       "B_5_1", "B_5_2", "B_5_3", "B_5_4", "B_5_5") := as.list(get_internals(task_mix_1, task_mix_2, task_mix_3, task_mix_4, task_mix_5, county, gamma_invert))]
}

## save out with internals
saveRDS(full_unsmoothed, file.path('results', 'data', '12b_data_for_counterfactuals_monotone.rds'))

model_people <- melt(full_unsmoothed, measure.vars = colnames(full_unsmoothed)[grep("^B_[0-9]_[0-9]", colnames(full_unsmoothed))])
model_people[, type_num := str_extract(variable, "[0-9]$")]
model_people[, task_num := paste0("job_", str_extract(str_replace(variable, "_[0-9]$", ""), "[0-9]$"))]
model_people[, emp_duration := sum(value), by = c("location_id", "quarter_year", "type_num")]
model_people <- model_people[emp_duration > innertol * 10, ]
model_people[, value := value / emp_duration]
model_people[, emp_duration := emp_duration * tot_duration]

attach_model_people <- dcast(model_people, location_id + quarter_year + emp_duration + type_num + county ~ task_num)

model_people[, task_num := str_extract(task_num, "[0-9]$")]
model_people <- merge(model_people, attach_model_people, by = c("location_id", "quarter_year", "emp_duration", "type_num", "county"))


data_people <- readRDS(file.path(CONFIG$prep_output_dir, "01_staff_task_full.rds"))
colnames(data_people)[grep("^E_", colnames(data_people))] <- str_replace(colnames(data_people)[grep("^E_", colnames(data_people))], "E_", "job_")
data_people <- data_people[, .SD, .SDcols = c(colnames(data_people)[grep("^job_", colnames(data_people))],
                                              "location_id", "quarter_year", "county", "staff_num", "emp_duration")]
data_people[county == '06037', county := "6037"]

attach_data_people <- copy(data_people[, .SD, .SDcols = c("location_id", "quarter_year", "county", "staff_num", colnames(data_people)[grep("^job", colnames(data_people))])])
data_people <- melt(data_people, id.vars = c("location_id", "quarter_year", "county", "staff_num", "emp_duration"), pattern = "^job")
data_people[, task_num := str_extract(variable, "[0-9]$")]
data_people[, group := "Data"]
model_people[, group := "Model"]
data_people <- merge(data_people, attach_data_people, by = c("location_id", "quarter_year", "county", "staff_num"))
setnames(data_people, "staff_num", "type_num")
quarter_list <- CONFIG$estimation_quarters

all_data <- rbind(data_people[county %in% CONFIG$counties &
                                quarter_year %in% quarter_list,
                              .SD, .SDcols = c("group", "location_id", "quarter_year", "county", "emp_duration", "value", "type_num", "task_num",
                                               colnames(model_people)[grep("^job", colnames(model_people))])]
                  , model_people[, .SD, .SDcols = c("group", "location_id", "quarter_year", "county", "emp_duration", "value", "type_num", "task_num",
                                                    colnames(model_people)[grep("^job", colnames(model_people))])])

get_cor <- function(x, y, wt) {
  dat <- cbind(x, y)
  res <- cov.wt(cbind(x, y), wt = wt, cor = TRUE)$cor[2, 1]
  return(as.character(format(round(res, 3), nsmall = 3)))
}


out_sample_moments <- all_data[, .(Variance = weightedVar(value, w = emp_duration),
                                   `Cor. Task 1` = get_cor(job_1, value, emp_duration),
                                   `Cor. Task 2` = get_cor(job_2, value, emp_duration),
                                   `Cor. Task 3` = get_cor(job_3, value, emp_duration),
                                   `Cor. Task 4` = get_cor(job_4, value, emp_duration),
                                   `Cor. Task 5` = get_cor(job_5, value, emp_duration)
                                   ), by = c("group", "task_num")]
setorder(out_sample_moments, "task_num", -"group")
setnames(out_sample_moments, "group", "")
setnames(out_sample_moments, "task_num", "Task")

# white out bottom triangular
out_sample_moments[3:.N, `Cor. Task 1` := ""]
out_sample_moments[5:.N, `Cor. Task 2` := ""]
out_sample_moments[7:.N, `Cor. Task 3` := ""]
out_sample_moments[9:.N, `Cor. Task 4` := ""]

out_sample_moments[, Variance := format(round(Variance, digits = 3), nsmall = 3)]

output <- kable(out_sample_moments, "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA)
cat(output, file = file.path("results", "out", "tables", "12b_validate_corr_monotone.tex"))


toplot <- unique(all_data[, c("county", "group", "emp_duration",
                              "job_1", "job_2", "job_3", "job_4", "job_5", "type_num")])
toplot[, comb_3 := job_3 + job_4 + job_5]
ggplot(toplot) + geom_density(aes(x = job_1, color = group, fill = group, weight = emp_duration), position = 'identity', alpha = 0.5) +
  xlab("Time on Haircut/Shave") + ylab("Density") +
  theme_bw() + theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave(file.path("results", "out", "figures", "12b_marginal_cut_monotone.png"), width = 6, height = 4, units = "in")


ggplot(toplot) + geom_density(aes(x = job_2, color = group, fill = group, weight = emp_duration), position = 'identity', alpha = 0.5) +
  xlab("Time on Color/Highlight/Wash") + ylab("Density") +
  theme_bw() + theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave(file.path("results", "out", "figures", "12b_marginal_color_monotone.png"), width = 6, height = 4, units = "in")

ggplot(toplot) + geom_density(aes(x = comb_3, color = group, fill = group, weight = emp_duration), position = 'identity', alpha = 0.5) +
  xlab("Time on Other 3 Tasks") + ylab("Density (weighted by hours)") +
  theme_bw() + theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave(file.path("results", "out", "figures", "12b_marginal_other3_monotone.png"), width = 6, height = 4, units = "in")


toplot <- toplot[, list(job_1 = rep(job_1, round(emp_duration / 60)), job_2 = rep(job_2, round(emp_duration / 60)),
                        job_3 = rep(job_3, round(emp_duration / 60)), job_4 = rep(job_4, round(emp_duration / 60)),
                        job_5 = rep(job_5, round(emp_duration / 60)),
                        comb_3 = rep(job_3 + job_4 + job_5, round(emp_duration / 60)),
                        group = rep(group, round(emp_duration / 60)), county = rep(county, round(emp_duration / 60)))]

ggplot(toplot) + geom_density_2d(aes(x = job_1, y = job_2), bins = 50, color = "black") +
  xlab("Time on Haircut/Shave") + ylab("Time on Color/Highlight/Wash") +
  theme_bw() + theme(axis.text = element_text(size = 14), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~group)
ggsave(file.path("results", "out", "figures", "12b_bivariate_cut_color_monotone.png"), width = 10, height = 6, units = "in")


ggplot(toplot) + geom_density_2d(aes(x = job_1, y = comb_3), bins = 50, color = "black") +
  xlab("Time on Haircut/Shave") + ylab("Time on Other Three Tasks") +
  theme_bw() + theme(axis.text = element_text(size = 10), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~group)

ggsave(file.path("results", "out", "figures", "12b_bivariate_cut_other3_monotone.png"), width = 10, height = 6, units = "in")


ggplot(toplot) + geom_density_2d(aes(x = job_2, y = comb_3), bins = 50, color = "black") +
  ylab("Time on Other Three Tasks") + xlab("Time Spent on Color/Highlight/Wash") +
  theme_bw() + theme(axis.text = element_text(size = 10), text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~group)
ggsave(file.path("results", "out", "figures", "12b_bivariate_color_other3_monotone.png"), width = 10, height = 6, units = "in")
