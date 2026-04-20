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

set.seed(588621)
rowMax <- function(data) apply(data,1, max, na.rm = TRUE)
rowMin <- function(data) apply(data,1, min, na.rm = TRUE)
working <- data.table(readRDS("mkdata/data/00_tasks_cosmo.rds"))

## verify upstream cleaning was applied
stopifnot("county" %in% colnames(working))
stopifnot("quarter_year" %in% colnames(working))
stopifnot(nrow(working[duration == 0, ]) == 0)

setkey(working, location_id, customer_id, date, app_id)
working[, first_visit := min(date), by = c("location_id", "customer_id")]
working[, last_visit := max(date), by = c("location_id", "customer_id")]

#### Step 0: Functions we need

#' Safe logarithm with log(0) = 0 convention
#' Returns log(x) for positive x, and 0 for x=0, -Inf, or NaN.
#' @param x Numeric vector
#' @return Numeric vector with log(x), or 0 where x is 0, -Inf, or NaN
spec_log <- function(x) ifelse(x == 0 | x == -Inf | is.nan(x), 0, log(x))

#' Replace NaN values with 0
#' @param x Numeric vector
#' @return Numeric vector with NaN replaced by 0
purge_nan <- function(x) ifelse(is.nan(x), 0, x)

#' Compute squared distances from each element to all others
#' @param y Numeric vector
#' @return Numeric vector of squared distances
distfun <- function(y) sapply(y, function(x) sum((x - y)^2))

#' Apply a column-wise transformation using a prefix-to-prefix mapping
#'
#' For every column in `dt` whose name starts with `in_prefix`, create (or
#' overwrite) a column named `paste0(out_prefix, suffix)` by calling
#' `fn(d, suffix)`, where `suffix` is everything after `in_prefix` in the
#' source column name. If `by` is supplied, the transformation is evaluated
#' per group and `fn` receives the per-group `.SD`; otherwise `fn` receives
#' the whole data.table.
#'
#' This replaces the many repeated
#'   for (col in gsub("^duration_", "", names(dt)[grep("^duration_", names(dt))])) ...
#' loops in this pipeline.
#'
#' @param dt data.table to modify in place
#' @param in_prefix prefix identifying source columns (e.g. "duration_")
#' @param out_prefix prefix for destination columns (e.g. "B_")
#' @param fn function(d, suffix) -> vector to assign
#' @param by optional character vector of grouping columns
#' @return `dt`, invisibly
transform_cols <- function(dt, in_prefix, out_prefix, fn, by = NULL) {
  cols <- grep(paste0("^", in_prefix), names(dt), value = TRUE)
  suffixes <- gsub(paste0("^", in_prefix), "", cols)
  for (s in suffixes) {
    local_s <- s
    if (is.null(by)) {
      dt[, (paste0(out_prefix, local_s)) := fn(dt, local_s)]
    } else {
      dt[, (paste0(out_prefix, local_s)) := fn(.SD, local_s), by = by]
    }
  }
  invisible(dt)
}

#### Step. 1: create task variables.

setkey(working, location_id, customer_id, date, app_id)

## important variables
working[,cust_count:=uniqueN(customer_id), by=c("location_id", "quarter_year") ]

## get task proportion of each worker.
stopifnot(nrow(working[is.na(price)])==0)

######## this is cut point for creating additional variables not needed for main analysis.


# save the mean duration of all tasks for smoothing, multiplied by parameter
smooth_parm<- mean(working$duration)
staff_task<-working[, .(count=.N, duration=as.double(sum(duration)),
                        revenue=sum(price)),
                    by=c("staff_id", "location_id","CSPOP", "county",
                         "rep_text_cluster", "quarter_year", "business_id", "clust", "location_state","location_city","location_zip", "cust_count")]
staff_task[,staff_num:= frank(staff_id, ties.method="dense"), by=c("location_id", "quarter_year")]
staffnum_xwalk<-unique(staff_task[, c("location_id", "quarter_year", "staff_num", "staff_id")])
saveRDS(staffnum_xwalk,'mkdata/data/01_xwalk.rds')

staff_task<-staff_task[, c("staff_revenue", "emp_duration"):=  list(sum(revenue), sum(duration)) ,by=c("location_id", "quarter_year", "staff_num")]
stopifnot(uniqueN(staff_task[,c("staff_id", "location_id", "clust", "quarter_year")])==nrow(staff_task))
staff_task<-staff_task[, c("service_types","mean_duration_firm","tot_duration","emps", "revenue"):=  list(uniqueN(clust),mean(duration), sum(duration), uniqueN(staff_id), sum(revenue)) ,by=c("location_id", "quarter_year")]

# cast so that each observation is one emp-quarter, and we have duration spent on each task.
# when we cast, we fill in 0 for tasks the worker spent no time on.
staff_task<-dcast(staff_task, location_state+location_city+location_zip+CSPOP+county+location_id + quarter_year+business_id+tot_duration+emp_duration+staff_num+staff_revenue+emps+service_types+revenue+mean_duration_firm+cust_count~clust, value.var=c("rep_text_cluster", "duration"),fill=0)
transform_cols(staff_task, "duration_", "firm_duration_",
               function(d, s) sum(d[[paste0("duration_", s)]]),
               by = c("location_id", "quarter_year"))
transform_cols(staff_task, "firm_duration_", "task_mix_",
               function(d, s) d[[paste0("firm_duration_", s)]] / d$tot_duration)
staff_task[, e_frac:= emp_duration/tot_duration]

# how many firms have only certain types of tasks
## example: 00001 means only task 5 is performed
staff_task[,service_mix_id:="" ]
for (col in gsub("^task_mix_","",names(staff_task)[grep("^task_mix_", names(staff_task))])) staff_task[,service_mix_id:=paste0(service_mix_id,ifelse(round(get(paste0("task_mix_",col)),8)==0,0,1)) ]
# worker_spec is the fraction of worker time spent on 
staff_task[, worker_spec:=duration_1/emp_duration]
staff_task[, staff_rev_perhour:=(staff_revenue/emp_duration/60)]
staff_task[, std_worker_spec:=worker_spec/sd(worker_spec)]
staff_task[, std_staff_rev_perhour:=staff_rev_perhour/sd(staff_rev_perhour)]
transform_cols(staff_task, "duration_", "E_",
               function(d, s) d[[paste0("duration_", s)]] / d$emp_duration)
## save out s_index (pre-smoothing)
transform_cols(staff_task, "duration_", "B_raw_",
               function(d, s) d[[paste0("duration_", s)]] / d$tot_duration)
transform_cols(staff_task, "B_raw_", "mipart_",
               function(d, s) d[[paste0("B_raw_", s)]] *
                 spec_log(d[[paste0("B_raw_", s)]] /
                          d[[paste0("task_mix_", s)]] / d$e_frac))
B<-as.matrix(staff_task[,.SD, .SDcols=grep("^mipart", names(staff_task))])
stopifnot(all(!is.na(B)))
stopifnot(all(!is.nan(B)))
stopifnot(all(is.finite(B)))
staff_task[, s_index:=rowSums(B)]
staff_task[, s_index:=sum(s_index), by=c("quarter_year", "location_id")]
staff_task<-staff_task[,.SD, .SDcols=-grep("^mipart", names(staff_task))]
#staff_task<-staff_task[,.SD, .SDcols=-grep("^B", names(staff_task))] 428
rm(B)
## save out raw matrix without merging or smoothing.
saveRDS(staff_task, "mkdata/data/01_staff_task_full.rds")

staff_task <- staff_task[quarter_year %in% CONFIG$estimation_quarters &
                         county %in% CONFIG$counties_padded]

#### Step 3: create B matrix.
### lidstone smooth: given a duration x for a worker-task pair i,k transform to x+tuning parm*emp_share*task_share*total task-worker combinations. Set tuning parm to be the average duration of a service at the salon that quarter. when computing fractions, divide by total duration plus the total number of active worker-task pairs.
### when a task is not performed at a firm at all during a quarter I take this as true and smooth as if that category did not exist for that firm.
# thus this smoothing is across employees for all tasks that are performed at that salon.
transform_cols(staff_task, "duration_", "smooth_duration_",
               function(d, s) d[[paste0("duration_", s)]] + smooth_parm)
staff_task[, smooth_tot_duration:=0]
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,smooth_tot_duration  := smooth_tot_duration+sum(get(paste0("smooth_duration_",col))), by=c("location_id", "quarter_year")]
saveRDS(staff_task, "mkdata/data/01_staff_task_full_smoothed.rds")
transform_cols(staff_task, "smooth_duration_", "B_",
               function(d, s) d[[paste0("smooth_duration_", s)]] / d$smooth_tot_duration)


# this is the worker's "job" that is time spent on task k divided by total time.
staff_task[, smooth_e_frac:=0]


transform_cols(staff_task, "B_raw_", "Btilde_raw_",
               function(d, s) d[[paste0("B_raw_", s)]] / d$e_frac)

# by this point, for all tasks performed at a firm, the smoothed data should have all employees doing a positive fraction of that task.

#### Step 4: Cluster workers within firm
# we will cluster within firm using Btild, and decide the number of clusters based on cluster stability, where we re-sample. we need to build a function to do this.

#mat<-as.matrix(staff_task[location_id=="0fc4d438-a00c-4076-9ae9-e84547cef4ab",colnames(staff_task) %like% "^Btilde", with=FALSE])

# smallest cutlevel which achieves k groups.
within_firm_min<-function(mat,k){
  # if only one employee, return one cluster
  if (nrow(mat)<=k){
    return(c(0))
  }else{
    clust_res<-hclust(dist(mat, method="euclidean"),method='complete')
    clust_res<-clust_res$height[nrow(mat)-k]
    return(clust_res)
  }
}

## create cut level
staff_task[,min_cutlevel:=within_firm_min(as.matrix(.SD),CONFIG$n_worker_types), by=c("location_id", "quarter_year"), .SDcols=colnames(staff_task)[colnames(staff_task) %like% "^Btilde_raw_"]]


saveRDS(staff_task,'mkdata/data/01_staff_task.rds')



### run cluster program
rm(list=setdiff(ls(), c("staff_task", "staffnum_xwalk", "CONFIG", "transform_cols", "get_core_count", "get_initial_E", "get_task_mix_cols", "get_E_raw_cols", "build_E_formula", "build_task_mix_sum")))
source('cluster.R', local = TRUE)


saveRDS(verywide_expanded,'mkdata/data/01_working.rds')


