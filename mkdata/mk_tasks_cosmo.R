### create task data set.
## add new set of tasks from cosmotologist

library('data.table')
library('lubridate')
library('stringr')

tasks<-data.table(readRDS("C:/Users/jakek/blvd_dont_backup/data/compiled_trxns.rds"))
tasks[,raword:=1:.N]
# limit to just self-reported hair salons prior to covid-19
tasks<-tasks[industry%in% c("Hair Salon", "hairSalon","Barber Shop",
                                         "Blowouts & Styling") ,]
tasks[,month_num:=year(date)+month(date)/13][, week_num:=year(date)+week(date)/53]

# there is one salon with a ridiculous spike in revenue. exclude for now.
tasks<-tasks[location_id!="dae8355a-cd8f-4ae1-8d26-a839b578f9f9",]
tasks[service_performed=="GlazeOld",service_performed:="Glaze Old"]
tasks[service_performed=="HaircutOld",service_performed:="Haircut Old"]

## attach descriptions
classified<-readRDS('data/classified_descriptions.rds')
classified[raw_id==2147,`Service Description`:="BOTOX CONSULT ONLY" ]
classified[raw_id==9900,`Service Description`:="Hair Extensions Install - Bonded 18\"\"  (per bundle)" ]
classified[, service_performed:=`Service Description`]
classified[, `Service Description`:=NULL]



tasks<-merge(tasks, classified, by="service_performed", all.x=TRUE)
rm(classified)

tasks[, helper_id:=1:.N]

## some character issue. manually fix these
col<-colnames(classified)[c(1,6:11)]
tasks[service_id=="ec581975-a08a-42a5-9eae-4cefe23627bf",(col) :=classified[rep(6,nrow(tasks[service_id=="ec581975-a08a-42a5-9eae-4cefe23627bf",.SD, .SDcols=col])), .SD, .SDcols=col] ]
  
tasks[service_id=="3a15cd78-dbea-4379-ae2d-13ed26c6d228",(col) :=classified[rep(20363,nrow(tasks[service_id=="3a15cd78-dbea-4379-ae2d-13ed26c6d228",.SD, .SDcols=col])), .SD, .SDcols=col] ]

tasks[service_id=="001cb7db-72b8-4744-8fe2-157441ed8e30",(col) :=classified[rep(9900,nrow(tasks[service_id=="001cb7db-72b8-4744-8fe2-157441ed8e30",.SD, .SDcols=col])), .SD, .SDcols=col] ]
stopifnot(nrow(tasks[ is.na(service_performed)])==1)
tasks<-tasks[ !is.na(service_performed),]
summary(tasks[, .SD, .SDcols=which(colnames(tasks) %like% "taskcat")])
## split observations that are multiple tasks. 
# first compute average time spent on task among all tasks that are only in that category and that are single service appointments.
## fix duration - use total_app_time as main variable. then when not available, use duration for single service appointments.
## for multiservice appointments, divide duration to services using same methodology as multi-task services.
## practically, first replace duration with total_app time when total_app_time is available.
tasks[,app_service_count:=.N, by=app_id]
tasks[!is.na(total_app_time), duration:=total_app_time]

tasks[,avg_task1:=sum(duration*(count_cat==1 & taskcat1==1 & app_service_count==1))/sum((count_cat==1 & taskcat1==1 & app_service_count==1)),]
tasks[,avg_task2:=sum(duration*(count_cat==1 & taskcat2==1 & app_service_count==1))/sum((count_cat==1 & taskcat2==1 & app_service_count==1)),]
tasks[,avg_task3:=sum(duration*(count_cat==1 & taskcat3==1 & app_service_count==1))/sum((count_cat==1 & taskcat3==1 & app_service_count==1)),]
tasks[,avg_task4:=sum(duration*(count_cat==1 & taskcat4==1 & app_service_count==1))/sum((count_cat==1 & taskcat4==1 & app_service_count==1)),]
tasks[,avg_task5:=sum(duration*(count_cat==1 & taskcat5==1 & app_service_count==1))/sum((count_cat==1 & taskcat5==1 & app_service_count==1)),]
tasks[,avg_task6:=sum(duration*(taskcat6==1 & app_service_count==1))/sum((taskcat6==1 & app_service_count==1)),]
# then divide any multi category according to time_tot * (avg_task1)/(avg_task1+avg_task2+avg_task3)

tasks<-melt(tasks, measure.vars=list(which(str_detect(colnames(tasks), "taskcat")),
                                               which(str_detect(colnames(tasks), "avg_task"))),variable.factor=FALSE)
tasks<-tasks[value1==1,]
# treat apps that are missing total_app_time as one group.
tasks[is.na(total_app_time), helper_id:=max(helper_id), by=c("app_id")]
tasks[, new_duration:=duration*value2/sum(value2), by=c("helper_id")]
tasks[, duration:=NULL]
tasks[, duration:=new_duration]
tasks[, taskcat1:=variable=="1"]
tasks[, taskcat2:=variable=="2"]
tasks[, taskcat3:=variable=="3"]
tasks[, taskcat4:=variable=="4"]
tasks[, taskcat5:=variable=="5"]
tasks[, taskcat6:=variable=="6"]
summary(tasks[, .SD, .SDcols=which(colnames(tasks) %like% "taskcat")])


tasks<-tasks[, -c("new_duration", "value1", "value2", "variable", "helper_id")]

## create categories
stopifnot(nrow(tasks[taskcat1+ taskcat2 + taskcat3 +taskcat4 + taskcat5+taskcat6!=1,])==0)
tasks[,clust:=taskcat1+ taskcat2*2 + taskcat3*3 +taskcat4*4 + taskcat5*5+taskcat6*6]
tasks[taskcat1==1,rep_text_cluster:="Haircut/Shave"]
tasks[taskcat2==1,rep_text_cluster:="Color/Highlight/Wash"]
tasks[taskcat3==1,rep_text_cluster:="Extensions"]
tasks[taskcat4==1,rep_text_cluster:="Blowdry/Style/Treatment"]
tasks[taskcat5==1,rep_text_cluster:="Administrative"]
tasks[taskcat6==1,rep_text_cluster:="Nail/Spa/Eye/Misc."]
tasks<-tasks[,-c("taskcat1", "taskcat2", "taskcat3", "taskcat4", "taskcat5", "taskcat6")]

## create male-child flags
tasks[, male_flag:=Male==1]
tasks[is.na(Male), male_flag:=0]
tasks[, female_flag:=Female==1]
tasks[is.na(Female), female_flag:=0]
tasks[, child_flag:=Child==1]
tasks[is.na(Child), child_flag:=0]


saveRDS(tasks, "data/tasks_cosmo.rds")

## export a few example rows for presentation
write.csv(tasks[app_id %in% c('000c4d3e-6445-489d-84d3-6b2e91c39e60','00342f4b-49c0-4100-8786-7333f4308163')], "../analysis_cosmo/out/tables/mk_tasks_cosmo_examples.csv")