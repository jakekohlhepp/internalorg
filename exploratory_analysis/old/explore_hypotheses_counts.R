### explore hypotheses on specialization
## modify to do based on service count

library('data.table')
library('ggplot2')
library('lubridate')
library('stringr')
library('fixest')
library('DescTools')
library('zoo')

setwd('C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/exploratory_analysis/')
theme_set(theme_bw())
theme_update(axis.text=element_text(size=12),
             axis.title=element_text(size=14))

working<-data.table(readRDS("data/tasks.rds"))
working[,week_start_date:=min(date), by=weeks_since_2000]
setkey(working, location_id, customer_id, date, app_id)
working[, app_ord:=frank(.SD[,c("location_id", "customer_id","date", "app_id")] , ties.method="dense")]
working[, app_ord:=app_ord-app_ord[1], by=c("location_id", "customer_id")]
working[, first_visit:=min(date), by=c("location_id", "customer_id")]





## staff - task
staff_task<-working[, .(count=.N,mean_price=mean(price), max_price=max(price), min_price=min(price), rev=sum(price), duration=as.double(sum(duration))) , by=c("staff_id", "location_id", "rep_text_cluster", "week_start_date", "business_id", "clust")]
staff_task[,c("tot_count","tot_duration", "emps", "service_types") := list(sum(count),sum(duration), uniqueN(staff_id), uniqueN(clust)), by=c("location_id", "week_start_date")]
staff_task[,c("emp_tot") := list(sum(count)), by=c("location_id", "week_start_date", "staff_id")]
staff_task[,staff_num:= frank(staff_id, ties.method="dense"), by=c("location_id", "week_start_date")]
# need double precision
staff_task[, count:=as.double(count)]

# cast first by cluster
staff_task<-dcast(staff_task, location_id + week_start_date+business_id+tot_duration+tot_count+staff_num+emps+emp_tot+service_types~clust, value.var=c("rep_text_cluster", "count"))
# ideal is equal proportions across all tasks, holding fixed emp time and task time.
# this can be had from multiplying the fraction of time the emp works times the fraction of time dedicated to that task.


for (col in names(staff_task)[grep("^count", names(staff_task))]) staff_task[,(paste0("loc_frac_",col))  := sum(get(col), na.rm=TRUE)/tot_count*emp_tot/tot_count , by=c("location_id","week_start_date")]
# set to 0 any cluster which has non-NA elsewhere
for (col in names(staff_task)[grep("^count", names(staff_task))]) staff_task[,(col)  :=  ifelse(is.na(get(paste0("loc_frac_",col))),get(col),ifelse(is.na(get(col)),0,get(col)/tot_count )) , by=c("location_id","week_start_date")]

# now cast by staff.
staff_task<-dcast(staff_task, location_id + week_start_date+business_id+tot_duration+tot_count+service_types~staff_num, value.var=c(names(staff_task)[grep("^count", names(staff_task))], names(staff_task)[grep("^loc_frac_", names(staff_task))]))
# na should be only valid NAs: where staff not present or the activity not udnertaken


P<-as.matrix(staff_task[,.SD, .SDcols=grep("^count", names(staff_task))])
Q<-as.matrix(staff_task[,.SD, .SDcols=grep("^loc_frac_", names(staff_task))])
## hellinger distance.
staff_task[,dist_equal:=1/sqrt(2)*sqrt(rowSums((sqrt(P)-sqrt(Q))^2, na.rm=T))]

supp_price_index<-working[, .(mean_haircut=sum(price*(is_haircut==1))/sum(is_haircut==1), emps = uniqueN(staff_id)), by=c("location_id", "week_start_date", "location_state", "business_id", "location_city")]
firm_week<-merge(staff_task[, c("dist_equal", "location_id","week_start_date", "service_types")],supp_price_index, all.x=TRUE, by=c("location_id", "week_start_date") )


setkey(firm_week, location_id, week_start_date)
firm_week[,roll_dist_equal:=rollmean(dist_equal,12, fill=list(NA, NULL,NA)) , by=location_id]
firm_week[,roll_price:=rollmean(mean_haircut,12, fill=list(NA, NULL,NA)) , by=location_id]
ggplot(firm_week[week_start_date>=date("2015-01-01")   ], aes(x = week_start_date, y = roll_dist_equal,color=factor(location_id) )) +
  geom_line(size=0.5) + ylab("Rolling Average of Specialization Metric") + xlab("Week")+ theme(legend.position = "none")


cutD <- function(x,n) {
  cut(x, breaks=c(unique(quantile(x, probs = seq(0, 1, by = 1/n),na.rm = T, include.lowest=TRUE))))
}


stopifnot(nrow(firm_week[is.na(location_state)])==0)


### Fact 1: Specialization Rises with Price
working[date<=date('2020-10-01') & date<date('2020-11-01'),period:=1]
working[date<=date('2020-02-01') & date<date('2020-03-01'),period:=0]
recovery<-working[, .(emps=uniqueN(staff_id), apps=uniqueN(customer_id)), by=c("location_id", "period")]
recovery<-recovery[!is.na(period)]
recovery<-dcast(recovery,location_id~period, value.var=c("emps", "apps") )
recovery<-recovery[!is.na(apps_1) & !is.na(apps_0)]
recovery[, rec_emps:=(emps_1-emps_0)/emps_0]
recovery[, rec_apps:=(apps_1-apps_0)/apps_0]

firm_week<-firm_week[week_start_date<=date('2020-02-01'),]
firm_week[,emp_serv_id:=paste0(as.character(service_types), "-", as.character(emps))]
firm_week[, bins_price:=cutD(mean_haircut,20)]
plot(firm_week[,c("bins_price", "dist_equal")])

## Check: when we residualize and remove time, location and emp/service count effect.
forgraph<-data.table(r_dist=resid(feols(data=firm_week[week_start_date<=date('2020-02-01'),], dist_equal~factor(location_state)
                                        | factor(emp_serv_id)+factor(week_start_date)))
                     , r_qual=resid(feols(data=firm_week[week_start_date<=date('2020-02-01'),], mean_haircut~factor(location_state)
                                          | factor(emp_serv_id)+factor(week_start_date))))

forgraph[, bins_price:=cutD(r_qual,20)]
plot(forgraph[,c("bins_price", "r_dist")])

### regression which holds fixed emps-service margin.
firm_week[,s_dist:=(dist_equal-mean(dist_equal, na.rm=TRUE))/sd(dist_equal, na.rm=TRUE)]
firm_week[,s_price:=(mean_haircut-mean(mean_haircut, na.rm=TRUE))/sd(mean_haircut, na.rm=TRUE)]
summary(feols(data=firm_week, s_dist~s_price| factor(emp_serv_id)+factor(location_state)+factor(week_start_date)), cluster=firm_week$location_id)
# a 1 s.d. increase in price results a 0.44 s.d. increase in specialization. 

## Fact 2: Price/Quality Accounts for Around 19% of the Variation in Specialization within an Employee-Service Bucket. this corresponds roughly to 16% of total variation
summary(feols(data=firm_week, s_dist~s_price| factor(emp_serv_id)), cluster=firm_week$location_id)

## Fact 3: Specialization Strategy is Time Invariant and Firm Specific
summary(feols(data=firm_week, s_dist~ factor(location_id)), cluster=firm_week$location_id)
## firm dummies account for nearly 83% of all variation in specialization.
summary(feols(data=firm_week, s_dist~ factor(location_id)|factor(week_start_date)), cluster=firm_week$location_id)
## adding time indicators only increases r2 by 2%
summary(feols(data=firm_week, s_dist~ factor(week_start_date)), cluster=firm_week$location_id)
## adj r2. is around 0 with time indicators alone.
ggplot(firm_week[ week_start_date<= date("2020-02-01")  ], aes(x = week_start_date, y = roll_dist_equal,color=factor(location_id) )) +
  geom_line(size=0.5) + ylab("Rolling Average of Specialization Measure") + xlab("Week")+ theme(legend.position = "none")


## Fact 4: There is is a weak negative relationship between specialization level and employee recovery.
# More specialized companies recovered their apps slower.
# More specialized companies recovered their emps slower.
firms_spec<-firm_week[,.(mean_specialize=mean(dist_equal, na.rm=TRUE)), by=c("location_id", "location_state")]
firms_spec<-merge(firms_spec, recovery, by="location_id")
summary(feols(data=firms_spec, rec_apps~mean_specialize| factor(location_state)))
summary(feols(data=firms_spec, rec_emps~mean_specialize| factor(location_state)))

## Fact 5: Customer Retention Has Little to No Relationship with Specialization.
cust_retention<-unique(working[, c("app_ord", "customer_id", "location_id")])
cust_retention<-cust_retention[,.(max_ord=max(app_ord)), by=c("customer_id", "location_id")]
stopifnot(uniqueN(cust_retention)==uniqueN(cust_retention[,c("customer_id", "location_id")]))
cust_retention[, retained:=max_ord>1,]
cust_retention<-cust_retention[,.(ret_rate=sum(retained)/.N), by=c("location_id")]
firms_spec<-merge(firms_spec, cust_retention, by="location_id")
summary(feols(data=firms_spec, ret_rate~mean_specialize|factor(location_state)))
