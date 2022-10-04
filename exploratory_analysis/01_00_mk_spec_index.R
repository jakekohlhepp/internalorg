### explore hypotheses on specialization
library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('ggplot2')
library('lubridate')
library('stringr')
library('zoo')

setwd('C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/exploratory_analysis/')
theme_set(theme_bw())
theme_update(axis.text=element_text(size=12),
             axis.title=element_text(size=14))

working<-data.table(readRDS("../mkdata/data/tasks.rds"))
# outsheet services
services<-working[,.(trxns_count=.N, biz_count=uniqueN(business_id), location_count=uniqueN(location_id)), by=clean_task]
write.csv(services,"out/service_descriptions.csv", row.names = FALSE)

## create weekstart date.
working[, week_start_date:=floor_date(date, "weeks")]

setkey(working, location_id, customer_id, date, app_id)
working[, app_ord:=frank(.SD[,c("location_id", "customer_id","date", "app_id")] , ties.method="dense")]
working[, app_ord:=app_ord-app_ord[1], by=c("location_id", "customer_id")]
working[, first_visit:=min(date), by=c("location_id", "customer_id")]


## distribution of work across workers.
staff_time<-working[,.(time=as.double(sum(duration))), by=c("location_id", "week_start_date", "staff_id")]
staff_time[,tot_time:=sum(time), by=c("location_id", "week_start_date")]
staff_time[,staff_num:= frank(staff_id, ties.method="dense"), by=c("location_id", "week_start_date")]
staff_time[,staff_num_lab:=paste0("time_", staff_num)]
staff_time[,num_staff:=uniqueN(staff_id), by=c("location_id", "week_start_date")]
staff_time<-dcast(staff_time,location_id+week_start_date+tot_time+num_staff~staff_num_lab, value.var=c("time") )
# create fraction
for (col in names(staff_time)[grep("^time", names(staff_time))]) staff_time[,(paste0("frac_",col))  := get(col)/tot_time , by=c("location_id","week_start_date")]

# create equal distribution
for (col in names(staff_time)[grep("^time", names(staff_time))]) staff_time[,(paste0("even_",col))  := tot_time/num_staff, by=c("location_id","week_start_date")]

# distance from equal
P<-as.matrix(staff_time[,.SD, .SDcols=grep("^frac_time", names(staff_time))])
Q<-as.matrix(staff_time[,.SD, .SDcols=grep("^even_", names(staff_time))])
staff_time[,dist_staff_equal:=1/sqrt(2)*sqrt(rowSums((sqrt(P)-sqrt(Q))^2, na.rm=T))]


## staff - task
staff_task<-working[, .(count=.N,mean_price=mean(price), max_price=max(price), 
                        min_price=min(price), rev=sum(price), duration=as.double(sum(duration))),
                    by=c("staff_id", "location_id","location_zip",
                         "rep_text_cluster", "week_start_date", "business_id", "clust")]
staff_task[,c("tot_duration", "emps", "service_types") := list(sum(duration), uniqueN(staff_id), uniqueN(clust)), by=c("location_id", "week_start_date")]
staff_task[,c("emp_tot") := list(sum(duration)), by=c("location_id", "week_start_date", "staff_id")]
staff_task[,staff_num:= frank(staff_id, ties.method="dense"), by=c("location_id", "week_start_date")]
# cast first by cluster
staff_task<-dcast(staff_task, location_id + week_start_date+business_id+tot_duration+staff_num+emps+emp_tot+service_types~clust, value.var=c("rep_text_cluster", "duration"))
# ideal is equal proportions across all tasks, holding fixed emp time and task time.
# this can be had from multiplying the fraction of time the emp works times the fraction of time dedicated to that task.
for (col in names(staff_task)[grep("^duration", names(staff_task))]) staff_task[,(paste0("loc_frac_",col))  := sum(get(col), na.rm=TRUE)/tot_duration*emp_tot/tot_duration , by=c("location_id","week_start_date")]
# set to 0 any cluster which has non-NA elsewhere
for (col in names(staff_task)[grep("^duration", names(staff_task))]) staff_task[,(col)  :=  ifelse(is.na(get(paste0("loc_frac_",col))),get(col),ifelse(is.na(get(col)),0,get(col)/tot_duration )) , by=c("location_id","week_start_date")]

# now cast by staff.
staff_task<-dcast(staff_task, location_id + week_start_date+business_id+tot_duration+service_types~staff_num, value.var=c(names(staff_task)[grep("^duration", names(staff_task))], names(staff_task)[grep("^loc_frac_", names(staff_task))]))
# na should be only valid NAs: where staff not present or the activity not udnertaken


P<-as.matrix(staff_task[,.SD, .SDcols=grep("^duration", names(staff_task))])
Q<-as.matrix(staff_task[,.SD, .SDcols=grep("^loc_frac_", names(staff_task))])
## hellinger distance.
#staff_task[,dist_equal:=1/sqrt(2)*sqrt(rowSums((sqrt(P)-sqrt(Q))^2, na.rm=T))]
staff_task[,dist_equal:=rowSums(P*log(P/Q), na.rm=T)]
working[, is_haircut:=str_detect(str_to_lower(service_performed), "cut") ]
supp_price_index<-working[, .(mean_haircut=sum(price*(is_haircut==1))/sum(is_haircut==1), emps = uniqueN(staff_id)), by=c("location_id", "week_start_date", "location_state", "business_id", "location_city", "location_zip")]
extracols<-names(staff_task)[grep("^loc_frac_", names(staff_task))]
firm_week<-merge(staff_task[, c("dist_equal", "location_id","week_start_date", "service_types",..extracols) ],supp_price_index, all.x=TRUE, by=c("location_id", "week_start_date") )
firm_week<-merge(firm_week,staff_time[,c("location_id", "week_start_date","tot_time", "num_staff","dist_staff_equal"  )], by=c("location_id", "week_start_date") )

## give equal distribution nice name
names(firm_week)[grep("^loc_", names(firm_week))]<-str_replace(names(firm_week)[grep("^loc_", names(firm_week))], "loc_", "equality_")

setkey(firm_week, location_id, week_start_date)
firm_week[,roll_dist_equal:=rollmean(dist_equal,12, fill=list(NA, NULL,NA)) , by=location_id]
firm_week[,roll_price:=rollmean(mean_haircut,12, fill=list(NA, NULL,NA)) , by=location_id]
firm_week[,roll_dist_staff:=rollmean(dist_staff_equal,12, fill=list(NA, NULL,NA)) , by=location_id]
saveRDS(firm_week,file="data/01_01_spec_metric.rds")

