### establish facts about employees
library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('ggplot2')
library('lubridate')
library('ggradar')
library('stringr')
library('fixest') ## using newest version
library('DescTools')
library('zoo')
library('eulerr')
library('binsreg')
setwd('C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/exploratory_analysis/')
theme_set(theme_bw(base_size=22))

firm_week<-readRDS('data/01_01_spec_metric.rds')

### create dataset that is employee-week, which has a variable capturing cumulative time spent doing each task.

setwd('C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/exploratory_analysis/')
theme_set(theme_bw())
theme_update(axis.text=element_text(size=12),
             axis.title=element_text(size=14))

working<-data.table(readRDS("../mkdata/data/tasks.rds"))
working[, week_start_date:=floor_date(date, "weeks")]
setkey(working, staff_id,location_id, date, app_id)
working$staffloc <- cumsum(!duplicated(working[,c("staff_id", "location_id")]))
emp_task_week<-working[, .( time_spent=sum(duration)),
                       by=c("staffloc","staff_id", "location_id", "week_start_date", "rep_text_cluster", "clust",
                            "business_id", "location_id", "business_id", 
                            "location_state","location_city", "location_zip")]
### cast out by cluster.
emp_task_week<-dcast(emp_task_week, location_id+business_id+staff_id+location_state+location_city+location_zip+staffloc + week_start_date~clust, value.var=c("rep_text_cluster", "time_spent"))

# expand to fill in any gaps - use first and observed weeks.
emp_task_week[, first_observed:=min(week_start_date), by="staffloc"]
emp_task_week[, last_observed:=max(week_start_date), by="staffloc"]
emp_task_week[, gap:=0]
update_info<-unique(emp_task_week[, c("staffloc", "staff_id", 
                                      "business_id", "location_id", 
                                      "location_state","location_city", "location_zip")])
all_combos<-data.table(expand.grid(week_start_date=
                                     lubridate::ymd(as.character(seq(min(emp_task_week$week_start_date),date('2021-09-06'), by = '1 week'))), staffloc=unique(emp_task_week$staffloc)))
all_combos[, tag:=1]
emp_task_week<-merge(emp_task_week,all_combos,by=c("staffloc", "week_start_date"),all.y=TRUE, fill=TRUE)
checker<-nrow(emp_task_week)
emp_task_week<-merge(emp_task_week[,-c("staff_id", 
                                       "business_id", "location_id", 
                                       "location_state","location_city", "location_zip")],update_info, by="staffloc" )
stopifnot(nrow(emp_task_week)==checker)

setkey(emp_task_week, "staffloc", "week_start_date")

### remove the excess
emp_task_week[,first_observed:=min(first_observed, na.rm=TRUE), by=staffloc]
emp_task_week[,last_observed:=max(last_observed, na.rm=TRUE), by=staffloc]
emp_task_week<-emp_task_week[week_start_date>=first_observed & week_start_date<=last_observed,]
rm(all_combos)

## what is largest gap?
emp_task_week[, gap:=is.na(gap)]
setkey(emp_task_week, "staffloc", "week_start_date")
emp_task_week[, switch:=(gap==1 & shift(gap)==0 ), by=staffloc]
emp_task_week[, gapnumber:=cumsum(switch),by=staffloc]
## make gap number NA when not gap.
emp_task_week[, gapnumber:=ifelse(gap,gapnumber, NA)]
## now count length of gaps.
emp_task_week[, gaplength:=.N, by=c("staffloc", "gapnumber")]
#hist(unique(emp_task_week[!is.na(gapnumber), c("staffloc", "gapnumber", "gaplength")])$gaplength, breaks=30)
# largest is 279 weeks.

### compute accumulated experience in each task.
for (col in names(emp_task_week)[grep("^time_spent", names(emp_task_week))]) emp_task_week[,(col)  := ifelse(is.na(get(col)),0,get(col) ) , by=staffloc]
for (col in names(emp_task_week)[grep("^time_spent", names(emp_task_week))]) emp_task_week[,(paste0("cum_",col))  := cumsum(get(col)) , by=staffloc]

### measure the balance of skills, holding fixed the demand for services and supply of labor.
### do this by building counterfactual "even assignment of tasks" then measure distance from even distribution.
### get amount of time supplied each week
temp1<-working[, .( totemp_time=sum(duration)),
                       by=c("staffloc", "location_id", "week_start_date")]
temp1[, totloc_time:=sum(totemp_time), by=c("location_id", "week_start_date")]
temp1[,totempprop:=totemp_time/totloc_time ]

temp2<-working[, .( tottask_time=sum(duration)),
               by=c("clust", "location_id", "week_start_date")]
temp2[, totloc_time:=sum(tottask_time), by=c("location_id", "week_start_date")]
temp2[,tottaskprop:=tottask_time/totloc_time ]
task_week<-dcast(temp2, location_id+week_start_date~clust, value.var=c("tottaskprop", "tottask_time"), fill=0)

task_emp<-merge(temp1,task_week, by=c("location_id", "week_start_date") )
rm(temp1, temp2)

### combine and compute hellinger distance
emp_task_week<-merge(emp_task_week,task_emp,by=c( "staffloc", "week_start_date", "location_id"), all.x=TRUE)
### generate even hours as tottaskprop*totempprop*totloc_time
for (col in 1:6) emp_task_week[,(paste0("equal_task_",col))  := totempprop*get(paste0("tottaskprop_",col))*totloc_time ]
for (col in names(emp_task_week)[grep("^equal_task_", names(emp_task_week))]) emp_task_week[,(col)  := ifelse(is.na(get(col)),0,get(col) )]
setkey(emp_task_week, "staffloc", "week_start_date")
for (col in names(emp_task_week)[grep("^equal_task_", names(emp_task_week))]) emp_task_week[,(paste0("cum_",col))  := cumsum(get(col)) , by=staffloc]

### now combine across workers who work at multiple locations.
### to do this sum the experience columns
emp_task_week[,locs:=uniqueN(location_id), by=c("staff_id", "week_start_date")]
for (col in names(emp_task_week)[grep("^cum_equal_task", names(emp_task_week))]) emp_task_week[,(col)  := sum(get(col)) , by=c("staff_id", "week_start_date")]
for (col in names(emp_task_week)[grep("^cum_time_spent", names(emp_task_week))]) emp_task_week[,(col)  := sum(get(col)) , by=c("staff_id", "week_start_date")]
cols_to_keep<-c(names(emp_task_week)[grep("^cum_time_spent", names(emp_task_week))],
                names(emp_task_week)[grep("^cum_equal_task", names(emp_task_week))],
                c("staff_id",
                  "week_start_date", "locs"))


staff_task_week<-unique(emp_task_week[,..cols_to_keep])
stopifnot(uniqueN(staff_task_week[, c("staff_id", "week_start_date")])==nrow(staff_task_week)) ## now unique by staff_id week.
### we will need to drop weeks where a staff person is present in more than one location.
staff_task_week<-staff_task_week[locs==1, ]
cols_to_keep<-c(cols_to_keep,"business_id", "location_id", 
                "location_state","location_city", "location_zip" )
staff_task_week<-emp_task_week[locs==1,..cols_to_keep ]
stopifnot(uniqueN(staff_task_week[, c("staff_id", "week_start_date")])==nrow(staff_task_week))

#table(double_locs$locs)

## get total experience

cols<-names(staff_task_week)[grep("^cum_equal_task_", names(staff_task_week))]
staff_task_week$totexperience<-rowSums(staff_task_week[, ..cols ])
P<-as.matrix(staff_task_week[,.SD, .SDcols=grep("^cum_time_spent", names(staff_task_week))])
Q<-as.matrix(staff_task_week[,.SD, .SDcols=grep("^cum_equal_task_", names(staff_task_week))])

## hellinger distance.
staff_task_week[,emp_equal:=1/sqrt(2)*sqrt(rowSums((sqrt(P/staff_task_week$totexperience)-sqrt(Q/staff_task_week$totexperience))^2,na.rm=FALSE))]

### reattach details of firm.
staff_task_week<-merge(staff_task_week, firm_week[,c("location_id", "week_start_date","emps","service_types","mean_haircut", "dist_equal") ], by=c("location_id", "week_start_date"), all.x=TRUE)

### there will be NAs when the firm had no activity that week. leave as is.
nrow(staff_task_week[is.na(emps),])/nrow(staff_task_week)


### look at individual employee: how does being in a specialized firm impact human capital?
## reg firm spec on change in emp index.
setkey(staff_task_week, staff_id, week_start_date)
staff_task_week[, emp_index_change:=c(NA,diff(emp_equal)), by=staff_id]
staff_task_week[, first_date:=min(week_start_date), by=staff_id]
staff_task_week[,emp_serv_id:=paste0(as.character(service_types), "-", as.character(emps))]
staff_task_week[, s_emp_index_change:=emp_index_change/sd(emp_index_change, na.rm=TRUE)]
staff_task_week[, s_dist_equal:=dist_equal/sd(dist_equal, na.rm=TRUE)]
res1<-feols(s_emp_index_change~s_dist_equal , data=staff_task_week)
res2<-feols(s_emp_index_change~s_dist_equal|week_start_date , data=staff_task_week)
res3<-feols(s_emp_index_change~s_dist_equal | week_start_date+staff_id, data=staff_task_week)
res4<-feols(s_emp_index_change~s_dist_equal | week_start_date+staff_id+emp_serv_id, data=staff_task_week)
uniqueN(staff_task_week[!is.na(dist_equal) & !is.na(emp_index_change)]$staff_id)
uniqueN(staff_task_week[!is.na(dist_equal) & !is.na(emp_index_change)]$location_id)

summary(res4, cluster=staff_task_week$staff_id)
esttex(res1,res2,res3,res4, fitstat=~r2, cluster=staff_task_week$staff_id, file="out/01_02_emp_pol_reg.tex", replace=TRUE, se="cluster")


## visualize by looking at experience buildup of two stylists: one at highly specialized firm other an generalized.
weekmax<-77
forradar<-staff_task_week[staff_id %in% c("08a6d02b-5280-4bbd-a912-93b0f3a9de8b",
                                        "b1015e5e-e1ff-4402-a0fb-e6995a0b324b")]
forradar<-na.omit(forradar)
forradar[, group:=ifelse(staff_id=="08a6d02b-5280-4bbd-a912-93b0f3a9de8b", "Stylist A", "Stylist B")]
extracols<-names(forradar)[grep("^cum_time_spent", names(forradar))]
forradar[,weeknum:=rank(week_start_date)-1, by=group]
forradar[,(extracols):= lapply(.SD, as.double), .SDcols=extracols]
forradar[weeknum<=weekmax,(extracols):= .SD/max(totexperience), .SDcols=extracols, by=group]
helper<-names(emp_task_week)[grep("^rep", names(emp_task_week))]
labs<-emp_task_week[,..helper]
labs<-unique(melt(labs, measure = patterns("^rep_text_cluster_"), value.name = c("rep_text_cluster"), na.rm=TRUE))
labs[variable=="rep_text_cluster_6", rep_text_cluster:="Misc."]
#for (x in seq(from=0, to=weekmax, by=5)){
#  ggradar(forradar[weeknum==x,c("group",..extracols) ], axis.labels=labs$rep_text_cluster, plot.title=paste0("Week ", x),grid.mid=0.5 ,
#          grid.max=1,group.point.size=3,group.line.width=1, values.radar=c("","", "" ))
#  ggsave(paste0("out/01_02_radar",x,".png"), width=12, height=6, units="in")
#}
setnames(forradar, "group", "empid")
setnames(forradar, "weeknum", "group")


for (x in c(seq(from=1, to=77, by=5)) ){
ggradar(forradar[(group==x ) & empid=="Stylist B",c("group",..extracols) ], axis.labels=labs$rep_text_cluster,
          grid.max=1,group.point.size=3,group.line.width=1, values.radar=c("","", "" ), plot.title =paste("Hours Worked:", round(forradar[group==x & empid=="Stylist B"]$totexperience/60) ))
ggsave(paste0("out/01_02_radar_gen_",x,".png"), width=12, height=6, units="in")

ggradar(forradar[(group==x ) & empid=="Stylist A",c("group",..extracols) ], axis.labels=labs$rep_text_cluster,
        grid.max=1,group.point.size=3,group.line.width=1, values.radar=c("","", "" ), plot.title =paste("Hours Worked:", round(forradar[group==x & empid=="Stylist A"]$totexperience/60) ))
ggsave(paste0("out/01_02_radar_spec_",x,".png"), width=12, height=6, units="in")


}