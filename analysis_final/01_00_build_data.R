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
## also the progrma now starts with a 5 type firm

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

set.seed(588621)
rowMax <- function(data) apply(data,1, max, na.rm = TRUE)
rowMin <- function(data) apply(data,1, min, na.rm = TRUE)
working<-data.table(readRDS("mkdata/data/tasks_cosmo.rds"))

## merge extension task with blowdry task.
working[, clust:=ifelse(clust==3,4 ,clust)]
working[, rep_text_cluster:=ifelse(clust==4,"Blowdry/Style/Treatment/Extension" ,rep_text_cluster)]
working[, clust:=frank(clust, ties.method="dense")]
keytask<-unique(working[, c("clust", "rep_text_cluster")])
keytask[, task:=clust]
keytask[, type:=clust]
saveRDS(keytask, "analysis_final/data/01_00_keytask.rds")

#### data issue: drop 5 observations with negative time
stopifnot(nrow(working[duration<0])==5)
working<-working[duration>=0,]

## impute 0 time as average among quarter cluster (2% are imputed.)
nrow(working[duration==0,])/nrow(working)
working[, temp_dur:=ifelse(duration==0,NA,duration)]
working[, quarter_year:=year(date)+quarter(date)/10]
working[, year:=year(date)]
working[, temp_mean_dur:=mean(temp_dur, na.rm=TRUE), by=c("quarter_year", "clust")]
working[,duration:=ifelse(duration==0,temp_mean_dur,duration) ]
working[, c("temp_dur", "temp_mean_dur"):=list(NULL, NULL)]
setkey(working, location_id, customer_id, date, app_id)
working[, first_visit:=min(date), by=c("location_id", "customer_id")]
working[, last_visit:=max(date), by=c("location_id", "customer_id")]


### map zip codes to county
countypop<-fread('mkdata/raw/20220727_countypop/geocorr2022_2220806816.csv')[-1]
countypop[,CSPOP:=pop20]
stopifnot(uniqueN(countypop$county)==nrow(countypop))
data<-fread('mkdata/raw/20220727_countypop/geocorr2022_2220801561.csv')[-1]
data[, count:=uniqueN(county),by=zcta]
data<-data[afact>0.50 | count==1] # mapping only if more than 50 percent of zip is within county.
stopifnot(uniqueN(data$zcta)==nrow(data))
data<-merge(data, countypop[,c("county")], by="county")
stopifnot(uniqueN(data[zcta %in% unique(working$location_zip) ]$zcta)==nrow(data[zcta %in% unique(working$location_zip) ]))
data[, location_zip:=as.numeric(zcta)]
working<-merge(working,data[,c("location_zip", "county")], by="location_zip", all.x=TRUE)
stopifnot(uniqueN(working[is.na(county), location_id])==2)# one NA and one zip code not matched.

### get populations based on estimate for each year (except for census number in 2020)
working<-merge(working, readRDS("mkdata/data/county_census_pop.rds"), by=c("county", "year"),all.x=TRUE)



### drop firm-quarters that have 0 price
temp<-working[,.(rev=sum(price)),by=c("quarter_year", "location_id")]
temp[, is_zero:=rev<=0]
print(nrow(temp[is_zero==1]))
temp[,rev:=NULL]
working<-merge(working, temp, by=c("quarter_year", "location_id"), all.x=TRUE)
working<-working[is_zero==0,]
rm(temp)



#### Step 0: Functions we need
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
purge_nan<-function(x)  ifelse( is.nan(x),0,x)
distfun<-function(y) sapply(y,function(x)  sum((x - y)^2))

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
saveRDS(staffnum_xwalk,'analysis_final/data/01_00_xwalk.rds')

staff_task<-staff_task[, c("staff_revenue", "emp_duration"):=  list(sum(revenue), sum(duration)) ,by=c("location_id", "quarter_year", "staff_num")]
stopifnot(uniqueN(staff_task[,c("staff_id", "location_id", "clust", "quarter_year")])==nrow(staff_task))
staff_task<-staff_task[, c("service_types","mean_duration_firm","tot_duration","emps", "revenue"):=  list(uniqueN(clust),mean(duration), sum(duration), uniqueN(staff_id), sum(revenue)) ,by=c("location_id", "quarter_year")]

# cast so that each observation is one emp-quarter, and we have duration spent on each task.
# when we cast, we fill in 0 for tasks the worker spent no time on.
staff_task<-dcast(staff_task, location_state+location_city+location_zip+CSPOP+county+location_id + quarter_year+business_id+tot_duration+emp_duration+staff_num+staff_revenue+emps+service_types+revenue+mean_duration_firm+cust_count~clust, value.var=c("rep_text_cluster", "duration"),fill=0)
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,(paste0("firm_duration_",col))  := sum(get(paste0("duration_",col))), by=c("location_id", "quarter_year")]
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,(paste0("task_mix_",col))  := get(paste0("firm_duration_",col))/tot_duration, by=c("location_id", "quarter_year")]
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
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,(paste0("E_",col))  := get(paste0("duration_",col))/emp_duration ]
## save out s_index (pre-smoothing)
spec_log<-function(x)  ifelse(x==0 | is.nan(x),0,log(x))
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,(paste0("B_raw_",col))  := get(paste0("duration_",col))/tot_duration]
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,(paste0("mipart_",col))  := get(paste0("B_raw_",col))*spec_log(get(paste0("B_raw_",col))/get(paste0("task_mix_",col))/(e_frac))]
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
saveRDS(staff_task, "analysis_final/data/01_00_staff_task_full.rds")

quarter_list<-c(2018.1, 2018.2, 2018.3, 2018.4, 2019.1, 2019.2, 2019.3, 2019.4, 2020.1, 2020.4, 2021.1, 2021.2)
staff_task<-staff_task[quarter_year %in% quarter_list & county %in% c("36061", "06037", "17031")]

#### Step 3: create B matrix.
### lidstone smooth: given a duration x for a worker-task pair i,k transform to x+tuning parm*emp_share*task_share*total task-worker combinations. Set tuning parm to be the average duration of a service at the salon that quarter. when computing fractions, divide by total duration plus the total number of active worker-task pairs.
### when a task is not performed at a firm at all during a quarter I take this as true and smooth as if that category did not exist for that firm.
# thus this smoothing is across employees for all tasks that are performed at that salon.
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,(paste0("smooth_duration_",col))  := get(paste0("duration_",col))+smooth_parm ]
staff_task[, smooth_tot_duration:=0]
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,smooth_tot_duration  := smooth_tot_duration+sum(get(paste0("smooth_duration_",col))), by=c("location_id", "quarter_year")]
saveRDS(staff_task, "analysis_final/data/01_00_staff_task_full_smoothed.rds")
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,(paste0("B_",col))  := get(paste0("smooth_duration_",col))/smooth_tot_duration]


# this is the worker's "job" that is time spent on task k divided by total time.
staff_task[, smooth_e_frac:=0]


for (col in gsub("^B_raw_","",names(staff_task)[grep("^B_raw_", names(staff_task))])) staff_task[,(paste0("Btilde_raw_",col)):=get(paste0("B_raw_",col))/e_frac ]

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
staff_task[,min_cutlevel:=within_firm_min(as.matrix(.SD),5), by=c("location_id", "quarter_year"), .SDcols=colnames(staff_task)[colnames(staff_task) %like% "^Btilde_raw_"]]


saveRDS(staff_task,'analysis_final/data/01_00_staff_task.rds')



### run cluster program
rm(list=setdiff(ls(), c("staff_task", "staffnum_xwalk")))
source('analysis_final/cluster.R')


saveRDS(verywide_expanded,'analysis_final/data/01_01_working.rds')


