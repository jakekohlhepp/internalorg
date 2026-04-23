## purpose: classify workers into types using their task assignments within the firm
## approach starting 12/26/2023
## step 1: merge within using firm-size specific weights
## step 2: merge across using clustGeo, so that we respect the fact that we cannot merge within firm.
## use complete linkage because it is the only method that is proven not to generate cycles.
## Update 20240213: now assume that wage follows parallel trends. this means we can group across quarters.
## this does not change the initial merging process. it only means that when we merge across firms
## we can treat firm-quarters as observations.
## update 20240424: add weights vector. this changes the clustering procedure but allows the entire process to be boostrapped.
## update 20240507: combine 01_01 and 01_02 into one program
## also the program now starts with a 5 type firm

library('data.table')
library('lubridate')
library('stringr')
library('zoo')
library('binsreg')
library('ggplot2')
library('lessR')
library('stats')
library('dendextend')
library('qgraph')
library('adespatial')

source('config.R')
source('cluster.R')

set.seed(588621)
working <- data.table(readRDS("mkdata/data/00_tasks_cosmo.rds"))

## verify upstream cleaning was applied
stopifnot("county" %in% colnames(working))
stopifnot("quarter_year" %in% colnames(working))
stopifnot(nrow(working[duration == 0, ]) == 0)

setkey(working, location_id, customer_id, date, app_id)
working[, first_visit := min(date), by = c("location_id", "customer_id")]
working[, last_visit := max(date), by = c("location_id", "customer_id")]

feature_results <- build_staff_task_features(working, CONFIG)

## save the full-sample worker-task panels before the structural sample filter.
## descriptive scripts such as 01_01_stylized_facts.R should read these files
## rather than the estimation-base artifact saved below.
stopifnot(nrow(feature_results$staff_task_full) >= nrow(feature_results$staff_task))
saveRDS(feature_results$staffnum_xwalk, 'mkdata/data/01_xwalk.rds')
saveRDS(feature_results$staff_task_full, "mkdata/data/01_staff_task_full.rds")
saveRDS(feature_results$staff_task_full_smoothed, "mkdata/data/01_staff_task_full_smoothed.rds")
saveRDS(feature_results$staff_task, 'mkdata/data/01_staff_task.rds')

baseline_assignment <- assign_worker_types_baseline(
  feature_results$staff_task,
  feature_results$staffnum_xwalk,
  CONFIG
)

worker_type_lookup_artifact <- list(
  worker_type_lookup = baseline_assignment$worker_type_lookup,
  supported_staff_keys = baseline_assignment$supported_staff_keys,
  reference_firms = baseline_assignment$reference_firms,
  county_cutlevels = baseline_assignment$county_cutlevels
)
saveRDS(worker_type_lookup_artifact, 'mkdata/data/01_worker_type_lookup.rds')

aux_data <- list(
  cex = readRDS('mkdata/data/cex_outside.rds'),
  county_msa_xwalk = data.table(readRDS('mkdata/data/county_msa_xwalk.rds')),
  qcew = readRDS('mkdata/data/qcew_county.rds')
)

working_results <- build_working_from_labels(
  feature_results$staff_task,
  worker_type_lookup_artifact,
  aux_data,
  CONFIG,
  mode = 'baseline'
)

## 01_working.rds is the estimation-base branch. It is already restricted to
## CONFIG$counties / CONFIG$estimation_quarters, but it does not yet contain
## estimation-only enrichments such as PPI, minimum wages, or instruments.
stopifnot(all(as.character(working_results$verywide_expanded$county) %in% CONFIG$counties))
stopifnot(all(working_results$verywide_expanded$quarter_year %in% CONFIG$estimation_quarters))
saveRDS(working_results$verywide_expanded, 'mkdata/data/01_working.rds')
