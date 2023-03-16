## create working data set for all analyses.
## should have:
###  Salon-Quarter 2019-2021
#### a. Salon Vars: Price, task mix, s-index, zip,entropy, emp count, task count
###   b. Zip code vars: Population, median income, fraction get haircut
### c. sum of squared distances between task mix of other firms.
## remake data by half quarters

### explore hypotheses on specialization
library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('lubridate')
library('stringr')
library('zoo')
library('binsreg')
library('ggplot2')
library('lessR')
rowMax <- function(data) apply(data,1, max, na.rm = TRUE)
rowMin <- function(data) apply(data,1, min, na.rm = TRUE)
working<-data.table(readRDS("../mkdata/data/tasks_cosmo.rds"))

## merge extension task with blowdry task.
working[, clust:=ifelse(clust==3,4 ,clust)]
working[, rep_text_cluster:=ifelse(clust==4,"Blowdry/Style/Treatment/Extension" ,rep_text_cluster)]
working[, clust:=frank(clust, ties.method="dense")]
keytask<-unique(working[, c("clust", "rep_text_cluster")])
keytask[, task:=clust]
keytask[, type:=clust]

### map zip codes to county, get county pop as of 2020
countypop<-fread('../mkdata/raw/20220727_countypop/geocorr2022_2220806816.csv')[-1]
countypop[,CSPOP:=pop20]
stopifnot(uniqueN(countypop$county)==nrow(countypop))
data<-fread('../mkdata/raw/20220727_countypop/geocorr2022_2220801561.csv')[-1]
data[, count:=uniqueN(county),by=zcta]
data<-data[afact>0.50 | count==1] # mapping only if more than 50 percent of zip is within county.
stopifnot(uniqueN(data$zcta)==nrow(data))
data<-merge(data, countypop[,c("county", "CSPOP")], by="county")
stopifnot(uniqueN(data[zcta %in% unique(working$location_zip) ]$zcta)==nrow(data[zcta %in% unique(working$location_zip) ]))
data[, location_zip:=as.numeric(zcta)]
working<-merge(working,data[,c("CSPOP", "location_zip", "county")], by="location_zip", all.x=TRUE)
stopifnot(uniqueN(working[is.na(county), location_id])==2)# one NA and oen zip code not matched.

#### data issue: negative time coded. drop observations
stopifnot(nrow(working[duration<0])==5)
working<-working[duration>=0,]
#### Step 0: Functions we need
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))

purge_nan<-function(x)  ifelse( is.nan(x),0,x)

distfun<-function(y) sapply(y,function(x)  sum((x - y)^2))

#### Step. 1: create task variables.

## create time variables.
working[, week_start_date:=floor_date(date, "weeks")]
working[, month_year:=year(date)*100+month(date)]

setkey(working, location_id, customer_id, date, app_id)
working[, app_ord:=frank(.SD[,c("location_id", "customer_id","date", "app_id")] , ties.method="dense")]
working[, app_ord:=app_ord-app_ord[1], by=c("location_id", "customer_id")]
working[, first_visit:=min(date), by=c("location_id", "customer_id")]
working[, last_visit:=max(date), by=c("location_id", "customer_id")]
working[,cust_count:=uniqueN(customer_id), by=c("location_id", "month_year") ]
working[,cust_visits:=uniqueN(.SD[,c("customer_id", "date")]), by=c("location_id", "month_year") ]
working[,cust_return:=c(rep(0, .N-1),first(date<last_visit)) , by=c("location_id","customer_id", "date") ]
working[,return_count:=sum(cust_return), by=c("location_id", "month_year") ]
working[, female_flag:=max(female_flag,na.rm=TRUE),by=c("location_id", "month_year") ]
working[, child_flag:=max(child_flag,na.rm=TRUE),by=c("location_id", "month_year") ]
working[, male_flag:=max(male_flag,na.rm=TRUE),by=c("location_id", "month_year") ]

## get task proportion of each worker.
stopifnot(nrow(working[is.na(price)])==0)
staff_task<-working[, .(count=.N, duration=as.double(sum(duration)),
                      revenue=sum(price)),
                    by=c("staff_id", "location_id","CSPOP", "county",
                         "rep_text_cluster", "month_year", "business_id", "clust", "location_state","location_city","location_zip","cust_count", "return_count", "cust_visits","female_flag", "male_flag","child_flag")]
staff_task[,c("tot_duration", "emps", "service_types", "revenue") := list(sum(duration), uniqueN(staff_id), uniqueN(clust), sum(revenue)), by=c("location_id", "month_year")]
staff_task[,c("emp_tot") := list(sum(duration)), by=c("location_id", "month_year", "staff_id")]
staff_task[,staff_num:= frank(staff_id, ties.method="dense"), by=c("location_id", "month_year")]
tm<-staff_task[, .(task_mix=sum(duration/tot_duration), rev_per_time=sum(revenue)/sum(tot_duration)), by=c("location_id", "clust", "month_year")]
tm<-dcast(tm,location_id+month_year~clust, value.var=c("task_mix", "rev_per_time" ))
staff_task[, jobvect:=duration/emp_tot]

# cast first by cluster
staff_task<-dcast(staff_task, location_state+location_city+location_zip+CSPOP+county+location_id + month_year+business_id+cust_count+return_count+cust_visits+tot_duration+staff_num+emps+emp_tot+service_types+revenue+female_flag+male_flag+child_flag~clust, value.var=c("rep_text_cluster", "jobvect"))
staff_task[,efrac:=emp_tot/tot_duration]

staff_task<-merge(staff_task,tm, by=c("location_id", "month_year") )
names(staff_task)[str_detect(names(staff_task), "task_mix")]<-to("task_mix",length(names(staff_task)[str_detect(names(staff_task), "task_mix")]))
# zero out all.
setnafill(staff_task,fill=0, type='cons',cols=grep("^jobvect", names(staff_task)))
setnafill(staff_task,fill=0, type='cons',cols=grep("^task_mix", names(staff_task)))
setnafill(staff_task,fill=0, type='cons',cols="efrac")


## create maximum variables: want maximum of jobvect for each task.
for (col in names(staff_task)[grep("^jobvect", names(staff_task))]) staff_task[,(paste0("max_",col))  := max(get(col),na.rm=TRUE) , by=c("location_id","month_year")]
for (col in names(staff_task)[grep("^jobvect", names(staff_task))]) staff_task[,(paste0("min_",col))  := min(get(col),na.rm=TRUE) , by=c("location_id","month_year")]


B<-as.matrix(staff_task[,.SD, .SDcols=grep("^jobvect", names(staff_task))])
A<-as.matrix(staff_task[,.SD, .SDcols=grep("^task_mix", names(staff_task))])
E<-as.matrix(staff_task[,.SD, .SDcols=grep("^efrac", names(staff_task))])
staff_task[,mi_part:=E*rowSums(purge_nan(B)*spec_log(B/A))]
firm_quarter<-staff_task[tot_duration>0, .(s_index=sum(mi_part)), by=c("location_state","location_id","business_id","location_city","location_zip", "month_year","CSPOP", "county",
                                                   "emps", "service_types","tot_duration","revenue","cust_count","return_count","cust_visits","female_flag", "child_flag", "male_flag",
                                                   names(staff_task)[grep("^task_mix", names(staff_task))],
                                                   names(staff_task)[grep("^max_jobvect", names(staff_task))],
                                                   names(staff_task)[grep("^min_jobvect", names(staff_task))],
                                                   names(staff_task)[grep("^rev_per_time", names(staff_task))]) ]
## compute entropy (maximum potential s_index)
firm_quarter[,s_max:=-task_mix1*spec_log(task_mix1)-task_mix2*spec_log(task_mix2)-task_mix3*spec_log(task_mix3)-task_mix4*spec_log(task_mix4)-task_mix5*spec_log(task_mix5)]
firm_quarter[,s_norm:=s_index/s_max]
firm_quarter[, CSPOP:=as.numeric(CSPOP)]
firm_quarter[, county:=as.numeric(county)]
firm_quarter[, salon_share_subdiv:=cust_count/CSPOP]## outside share.


stopifnot(nrow(firm_quarter)==uniqueN(firm_quarter[,c("location_id", "month_year")]))
saveRDS(firm_quarter,file="data/00_00b_firm_month.rds")



