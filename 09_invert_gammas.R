#' =============================================================================
#' STEP 09: Invert Gammas for Full Firm-Quarter Panel
#' =============================================================================
#' Retrieves the organizational-cost parameter gamma for every firm-quarter in
#' the three focal counties (including firms that were not in the structural
#' estimation sample). Used as input to the substitution-pattern and
#' counterfactual prep scripts (10-13).
#'
#' Inputs:
#'   - mkdata/data/01_staff_task.rds
#'   - mkdata/data/01_staff_task_full.rds
#'   - mkdata/data/04_estimation_sample.rds
#'   - results/data/06_parameters.rds
#'   - preamble.R
#'
#' Outputs:
#'   - results/data/09_withgammas.rds
#'   - results/out/figures/09_gamma_dist.png
#'   - results/out/figures/09_total_orgcost_dist.png
#' =============================================================================

library('data.table')
library('lubridate')
library('stringr')
library('SQUAREM')
library('ggplot2')

source('config.R')

ensure_directory('results/data')
ensure_directory('results/out/figures')

staff_task_path      <- file.path(CONFIG$prep_output_dir, "01_staff_task.rds")
staff_task_full_path <- file.path(CONFIG$prep_output_dir, "01_staff_task_full.rds")
estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
worker_type_lookup_path <- file.path(CONFIG$prep_output_dir, "01_worker_type_lookup.rds")
parameters_path      <- file.path("results", "data", "06_parameters.rds")
assert_required_files(c(staff_task_path, staff_task_full_path,
                        estimation_sample_path, worker_type_lookup_path,
                        parameters_path))

##### reuse the baseline within-firm assignment from 01_build_data.R so that
##### the cutlevel and firm-type counts stay in lockstep with the upstream
##### worker-type lookup instead of being silently re-derived here.
staff_task <- data.table(readRDS(staff_task_path))
worker_type_lookup_artifact <- readRDS(worker_type_lookup_path)
effective_cutlevels <- data.table(worker_type_lookup_artifact$effective_cutlevels)
stopifnot(all(c("location_id", "quarter_year", "types_observed_firm") %in%
              names(effective_cutlevels)))

if (isTRUE(CONFIG$verbose_logging)) {
  for (cnty in CONFIG$counties_padded) {
    print(max(staff_task[county == cnty]$min_cutlevel))
  }
}

staff_task <- merge(
  staff_task,
  effective_cutlevels[, .(location_id, quarter_year, types_observed_firm)],
  by = c("location_id", "quarter_year"),
  all.x = TRUE
)
stopifnot(nrow(staff_task[is.na(types_observed_firm)]) == 0L)


all_tasks <- ""
for (col in gsub("^B_", "", names(staff_task)[grep("^Btilde_", names(staff_task))])) all_tasks <- paste0(all_tasks, "1")
firm_groups <- unique(staff_task[, c("types_observed_firm", "service_mix_id", "quarter_year", "location_id")])
stopifnot(uniqueN(firm_groups[, c("quarter_year", "location_id")]) == nrow(firm_groups))

### restore the unsmoothed data just to get task_mix and s_index, and set group numbers.
full_unsmoothed <- readRDS(staff_task_full_path)
## keep customer counts aside for the total-org-cost figure below; they stay out of
## full_unsmoothed so the saved 09_withgammas.rds schema is unchanged for step 13.
cust_counts <- unique(full_unsmoothed[, .(location_id, quarter_year, cust_count)])
stopifnot(nrow(cust_counts) == uniqueN(cust_counts[, .(location_id, quarter_year)]))
full_unsmoothed <- unique(full_unsmoothed[, .SD, .SDcols = c(colnames(full_unsmoothed)[grep("^task_mix", colnames(full_unsmoothed))],
                                                              "s_index", "location_id", "quarter_year", "county", "tot_duration")])
full_unsmoothed <- merge(full_unsmoothed, firm_groups[, c("location_id", "quarter_year", "types_observed_firm", "service_mix_id")],
                         by = c("location_id", "quarter_year"), all.x = TRUE)

## restrict to the three counties and quarters
full_unsmoothed <- full_unsmoothed[county %in% CONFIG$counties_padded &
                                     quarter_year %in% CONFIG$estimation_quarters, ]

### first construct wage-adjusted skill matrix
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


## how many
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
print(nrow(full_unsmoothed[s_index >= s_bound]))

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

saveRDS(full_unsmoothed, file.path("results", "data", "09_withgammas.rds"))

### plot the distribution of gamma
ggplot(full_unsmoothed, aes(x = gamma_invert)) +
  geom_histogram(color = "black", fill = "lightblue", linewidth = 0.5, bins = 40) +
  ylab("Salon-Quarter Count") + xlab("Org. Cost Parameter") +
  theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("results/out/figures/09_gamma_dist.png", width = 8, height = 4, units = "in")

### plot estimated total organization cost: gamma x observed s-index x observed
### average required labor. avg_labor is hours per customer, and firms with an
### infinite gamma have s_index = 0 and hence zero organization cost — both
### conventions match 13_counterfactual_prep.R.
orgcost_data <- merge(full_unsmoothed, cust_counts, by = c("location_id", "quarter_year"), all.x = TRUE)
stopifnot(nrow(orgcost_data[is.na(cust_count)]) == 0)
orgcost_data[, avg_labor := tot_duration / cust_count / 60]
orgcost_data[, total_org_cost := ifelse(is.finite(gamma_invert), gamma_invert * s_index * avg_labor, 0)]

ggplot(orgcost_data, aes(x = total_org_cost)) +
  geom_histogram(color = "black", fill = "lightblue", linewidth = 0.5, bins = 40) +
  ylab("Salon-Quarter Count") + xlab("Total Org. Cost") +
  theme(legend.position = "none") +
  theme_bw() +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("results/out/figures/09_total_orgcost_dist.png", width = 8, height = 4, units = "in")

### interquartile ranges of the two plotted objects. The gamma IQR is over
### finite gammas only (the histogram above drops the non-finite values);
### total org cost includes the zero-cost firms.
gamma_q <- quantile(orgcost_data[is.finite(gamma_invert)]$gamma_invert, c(0.25, 0.75))
cost_q <- quantile(orgcost_data$total_org_cost, c(0.25, 0.75))
print(paste0("Org. cost parameter (finite): p25 = ", round(gamma_q[1], 4),
             ", p75 = ", round(gamma_q[2], 4), ", IQR = ", round(gamma_q[2] - gamma_q[1], 4)))
print(paste0("Total org. cost: p25 = ", round(cost_q[1], 4),
             ", p75 = ", round(cost_q[2], 4), ", IQR = ", round(cost_q[2] - cost_q[1], 4)))
