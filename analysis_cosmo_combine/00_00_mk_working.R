## create working data set for all analyses.
## should have:
###  Salon-Quarter 2019-2021
#### a. Salon Vars: Price, task mix, s-index, zip,entropy, emp count, task count
###   b. Zip code vars: Population, median income, fraction get haircut
### c. sum of squared distances between task mix of other firms.

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
saveRDS(keytask, "data/00_00_keytask.rds")

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
stopifnot(uniqueN(working[is.na(county), location_id])==2)# one NA and one zip code not matched.

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
working[, quarter_year:=year(date)+quarter(date)/10]
setkey(working, location_id, customer_id, date, app_id)
working[, app_ord:=frank(.SD[,c("location_id", "customer_id","date", "app_id")] , ties.method="dense")]
working[, app_ord:=app_ord-app_ord[1], by=c("location_id", "customer_id")]
working[, first_visit:=min(date), by=c("location_id", "customer_id")]
working[, last_visit:=max(date), by=c("location_id", "customer_id")]
working[,cust_count:=uniqueN(customer_id), by=c("location_id", "quarter_year") ]
working[,cust_visits:=uniqueN(.SD[,c("customer_id", "date")]), by=c("location_id", "quarter_year") ]
working[,cust_return:=c(rep(0, .N-1),first(date<last_visit)) , by=c("location_id","customer_id", "date") ]
working[,return_count:=sum(cust_return), by=c("location_id", "quarter_year") ]
working[, female_flag:=max(female_flag,na.rm=TRUE),by=c("location_id", "quarter_year") ]
working[, child_flag:=max(child_flag,na.rm=TRUE),by=c("location_id", "quarter_year") ]
working[, male_flag:=max(male_flag,na.rm=TRUE),by=c("location_id", "quarter_year") ]

## measures of within visit specialization and whether staff was requested.
visit_data<-working[, .(services_invisit=.N,staff_invisit=uniqueN(staff_id), requested=sum(was_staff_requested),task_count=.N),by=c("location_id","customer_id", "date", "quarter_year")]
visit_data[,multi_service:= services_invisit>1 ]
visit_data[,multi_staff:= staff_invisit>1]
visit_data<-visit_data[,.(multi_staff=sum(multi_staff), multi_service=sum(multi_service),requested=sum(requested)), by=c("location_id", "quarter_year")]



## get task proportion of each worker.
stopifnot(nrow(working[is.na(price)])==0)
staff_task<-working[, .(count=.N, duration=as.double(sum(duration)),
                      revenue=sum(price)),
                    by=c("staff_id", "location_id","CSPOP", "county",
                         "rep_text_cluster", "quarter_year", "business_id", "clust", "location_state","location_city","location_zip","cust_count", "return_count", "cust_visits","female_flag", "male_flag","child_flag")]
staff_task[,c("tot_duration", "emps", "service_types", "revenue") := list(sum(duration), uniqueN(staff_id), uniqueN(clust), sum(revenue)), by=c("location_id", "quarter_year")]
staff_task[,c("emp_tot") := list(sum(duration)), by=c("location_id", "quarter_year", "staff_id")]
staff_task[,staff_num:= frank(staff_id, ties.method="dense"), by=c("location_id", "quarter_year")]
tm<-staff_task[, .(task_mix=sum(duration/tot_duration), rev_per_time=sum(revenue)/sum(tot_duration)), by=c("location_id", "clust", "quarter_year")]
tm<-dcast(tm,location_id+quarter_year~clust, value.var=c("task_mix", "rev_per_time" ))
staff_task[, jobvect:=duration/emp_tot]

# cast first by cluster
staff_task<-dcast(staff_task, location_state+location_city+location_zip+CSPOP+county+location_id + quarter_year+business_id+cust_count+return_count+cust_visits+tot_duration+staff_num+emps+emp_tot+service_types+revenue+female_flag+male_flag+child_flag~clust, value.var=c("rep_text_cluster", "jobvect"))
staff_task[,efrac:=emp_tot/tot_duration]

staff_task<-merge(staff_task,tm, by=c("location_id", "quarter_year") )
names(staff_task)[str_detect(names(staff_task), "task_mix")]<-to("task_mix",length(names(staff_task)[str_detect(names(staff_task), "task_mix")]))
# zero out all.
setnafill(staff_task,fill=0, type='cons',cols=grep("^jobvect", names(staff_task)))
setnafill(staff_task,fill=0, type='cons',cols=grep("^task_mix", names(staff_task)))
setnafill(staff_task,fill=0, type='cons',cols="efrac")

## save this out to visualize space of jobs.
saveRDS(staff_task,file="data/00_00_job_quarter.rds")

## create maximum variables: want maximum of jobvect for each task.
for (col in names(staff_task)[grep("^jobvect", names(staff_task))]) staff_task[,(paste0("max_",col))  := max(get(col),na.rm=TRUE) , by=c("location_id","quarter_year")]
for (col in names(staff_task)[grep("^jobvect", names(staff_task))]) staff_task[,(paste0("min_",col))  := min(get(col),na.rm=TRUE) , by=c("location_id","quarter_year")]


B<-as.matrix(staff_task[,.SD, .SDcols=grep("^jobvect", names(staff_task))])
A<-as.matrix(staff_task[,.SD, .SDcols=grep("^task_mix", names(staff_task))])
E<-as.matrix(staff_task[,.SD, .SDcols=grep("^efrac", names(staff_task))])
staff_task[,mi_part:=E*rowSums(purge_nan(B)*spec_log(B/A))]
firm_quarter<-staff_task[tot_duration>0, .(s_index=sum(mi_part)), by=c("location_state","location_id","business_id","location_city","location_zip", "quarter_year","CSPOP", "county",
                                                   "emps", "service_types","tot_duration","revenue","cust_count","return_count","cust_visits","female_flag", "child_flag", "male_flag",
                                                   names(staff_task)[grep("^task_mix", names(staff_task))],
                                                   names(staff_task)[grep("^max_jobvect", names(staff_task))],
                                                   names(staff_task)[grep("^min_jobvect", names(staff_task))],
                                                   names(staff_task)[grep("^rev_per_time", names(staff_task))]) ]
## compute entropy (maximum potential s_index)
firm_quarter[,s_max:=-task_mix1*spec_log(task_mix1)-task_mix2*spec_log(task_mix2)-task_mix3*spec_log(task_mix3)-task_mix4*spec_log(task_mix4)-task_mix5*spec_log(task_mix5)]
firm_quarter[,s_norm:=s_index/s_max]



### 2. Salon share variables
# first attach county.
#library('noncensus')
#data(zip_codes)
#zips<-data.table(zip_codes)
#setnames(zips, "fips", "county")
#zips<-zips[state %in% c("NY", "CA"),c("zip", "county")]
#zips[,location_zip:=as.integer(zip)]
#firm_quarter<-merge(firm_quarter, zips,by="location_zip", all.x=TRUE)
#
#demand_stats<-readRDS("../mkdata/data/demand.rds")
#firm_quarter<-merge(firm_quarter, demand_stats[,c("location_zip", "quarter_year", "county",
#                                                  "nohc_count", "count_sample")], by=c("location_zip", "quarter_year", "county"), all.x=TRUE)
## should have demand shares for all la and new york county salons between 2019.2 and 2020.1
#stopifnot(nrow(aug_firm_quarter[is.na(zip_demand) & quarter_year %between% c(2019.2, 2020.1) & county %in% c(6037 ,36061)])==0)
## fill in county pop when missing within year
#aug_firm_quarter[,helper:=max(county_pop,na.rm=TRUE), by=c("county", "quarter_year")]
#aug_firm_quarter[,county_pop:=ifelse(is.na(county_pop), helper, county_pop)]
#stopifnot(nrow(aug_firm_quarter[is.na(county_pop) & quarter_year %between% c(2019.2, 2020.1) & county %in% c(6037 ,36061)])==0)
#aug_firm_quarter[, salon_share:=cust_count/county_pop]
#aug_firm_quarter<-aug_firm_quarter[,!c("helper")]


firm_quarter[, CSPOP:=as.numeric(CSPOP)]
firm_quarter[, county:=as.numeric(county)]
firm_quarter[, salon_share_subdiv:=cust_count/CSPOP]## outside share.
cex<-readRDS('../mkdata/data/cex_outside.rds')
# psu is based on county.
keepers<-data.table(PSU=c('S12A', 'S49A','S49B'), county=c(36061,06037 ,06075))
cex<-merge(cex, keepers, by='PSU')
cex[, outside_share:=nohc_count /count_sample ]
firm_quarter<-merge(firm_quarter,cex[,c("county", "quarter_year", "outside_share") ], by=c("county", "quarter_year"),all.x=TRUE )


### 3. Price Variable
## we want average price per customer.
firm_quarter[, cust_price:=revenue/cust_count]


### 3. Make instruments.

## product space instruments
firm_quarter[ , d_2:=distfun(task_mix2), by=c("quarter_year","county")]
firm_quarter[ , d_3:=distfun(task_mix3), by=c("quarter_year","county")]
firm_quarter[ , d_4:=distfun(task_mix4), by=c("quarter_year","county")]
firm_quarter[ , d_5:=distfun(task_mix5), by=c("quarter_year","county")]

firm_quarter[ , s_2:=sum(task_mix2)-task_mix2, by=c("quarter_year", "county")]
firm_quarter[ , s_3:=sum(task_mix3)-task_mix3, by=c("quarter_year", "county")]
firm_quarter[ , s_4:=sum(task_mix4)-task_mix4, by=c("quarter_year", "county")]
firm_quarter[ , s_5:=sum(task_mix5)-task_mix5, by=c("quarter_year", "county")]


### more product space instruments
firm_quarter[ , m_serv:=sum(male_flag)-male_flag, by=c("quarter_year", "county")]
firm_quarter[ , child_serv:=sum(child_flag)-child_flag, by=c("quarter_year", "county")]


## hausman instruments - price in outside markets.
firm_quarter[, firm_count_county:=.N, by=c("quarter_year", "county")]
firm_quarter[, firm_count_all:=.N, by=c("quarter_year")]

firm_quarter[!is.na(rev_per_time_2), county_helper:=sum(rev_per_time_2, na.rm=TRUE), by=c("quarter_year", "county")]
firm_quarter[, county_helper:=ifelse(is.na(county_helper), 0, county_helper)]
firm_quarter[is.na(county_helper), county_helper:=0, by=c("quarter_year", "county")]
firm_quarter[!is.na(rev_per_time_2), hausman_2:=task_mix2*(sum(rev_per_time_2, na.rm=TRUE)-county_helper)/(firm_count_all-firm_count_county), by=c("quarter_year")]
firm_quarter[,county_helper:=NULL]
firm_quarter[!is.na(rev_per_time_3), county_helper:=sum(rev_per_time_3, na.rm=TRUE), by=c("quarter_year", "county")]
firm_quarter[, county_helper:=ifelse(is.na(county_helper), 0, county_helper)]
firm_quarter[!is.na(rev_per_time_3), hausman_3:=task_mix3*(sum(rev_per_time_3)-county_helper)/(firm_count_all-firm_count_county), by=c("quarter_year")]
firm_quarter[,county_helper:=NULL]
firm_quarter[!is.na(rev_per_time_4), county_helper:=sum(rev_per_time_4, na.rm=TRUE), by=c("quarter_year", "county")]
firm_quarter[, county_helper:=ifelse(is.na(county_helper), 0, county_helper)]
firm_quarter[!is.na(rev_per_time_4), hausman_4:=task_mix4*(sum(rev_per_time_4)-county_helper)/(firm_count_all-firm_count_county), by=c("quarter_year")]
firm_quarter[,county_helper:=NULL]
firm_quarter[!is.na(rev_per_time_5), county_helper:=sum(rev_per_time_5, na.rm=TRUE), by=c("quarter_year", "county")]
firm_quarter[, county_helper:=ifelse(is.na(county_helper), 0, county_helper)]
firm_quarter[!is.na(rev_per_time_5), hausman_5:=task_mix5*(sum(rev_per_time_5)-county_helper)/(firm_count_all-firm_count_county), by=c("quarter_year")]
firm_quarter[,county_helper:=NULL]
firm_quarter[!is.na(cust_price), county_helper:=sum(cust_price, na.rm=TRUE), by=c("quarter_year", "county")]
firm_quarter[is.na(county_helper), county_helper:=0, by=c("quarter_year", "county")]
firm_quarter[!is.na(cust_price), hausman_all:=(sum(cust_price, na.rm=TRUE)-county_helper)/(firm_count_all-firm_count_county), by=c("quarter_year")]


# make avg labor variable:
firm_quarter[, avg_labor:=tot_duration/cust_count/60]

# attach quarterly wage
qcew<-readRDS('../mkdata/data/qcew_county.rds')
# unique after we keep only private establishments
qcew<-qcew[own_code==5 & !is.na(county),]
stopifnot(nrow(qcew)==uniqueN(qcew[,c("county", "quarter_year")]))
qcew[,avg_wage_qtr:=total_qtrly_wages/qtrly_estabs]
firm_quarter<-merge(firm_quarter, qcew[,c("county", "quarter_year", "avg_wage_qtr")], by=c("county", "quarter_year"),
                    all.x=TRUE)
stopifnot(nrow(firm_quarter[(quarter_year %in% c(2019.2, 2019.3, 2019.4, 2020.1)) & (county %in% c(36061, 6037)) & is.na(avg_wage_qtr)])==0)

### read in and generate job-moments
#jobs<-readRDS("data/00_00_job_quarter.rds")
#helper<-readRDS("data/00_00_firm_quarter.rds")
#helper<-unique(helper[,c("bizsub_id", "county")])
#jobs<-merge(jobs,helper,by="bizsub_id", all.x=TRUE)
#jobs[, mean_vect1:=weighted.mean(jobvect_1, weight=emp_tot/sum(emp_tot)), by=c("county", "quarter_year")]
#jobs[, mean_vect2:=weighted.mean(jobvect_2, weight=emp_tot/sum(emp_tot)), by=c("county", "quarter_year")]
#jobs[, mean_vect3:=weighted.mean(jobvect_3, weight=emp_tot/sum(emp_tot)), by=c("county", "quarter_year")]
#jobs[, mean_vect4:=weighted.mean(jobvect_4, weight=emp_tot/sum(emp_tot)), by=c("county", "quarter_year")]
#jobs[, mean_vect5:=weighted.mean(jobvect_5, weight=emp_tot/sum(emp_tot)), by=c("county", "quarter_year")]
#jobs[, mean_vect6:=weighted.mean(jobvect_6, weight=emp_tot/sum(emp_tot)), by=c("county", "quarter_year")]
#tokeep<-c(colnames(jobs)[colnames(jobs) %like% "mean_vect"], "county", "quarter_year")
#jobs<-unique(jobs[,..tokeep])
#firm_quarter<-merge(firm_quarter, jobs, by=c("county", "quarter_year"),
#                    all.x=TRUE)
stopifnot(nrow(firm_quarter)==uniqueN(firm_quarter[,c("location_id", "quarter_year")]))

### attach multi service and multi staff visit counts.
firm_quarter<-merge(firm_quarter, visit_data, by=c("location_id", "quarter_year"),all.x=TRUE)

saveRDS(firm_quarter,file="data/00_00_firm_quarter.rds")
