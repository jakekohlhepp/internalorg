### export tasks for upwork
library('data.table')

tasks<-readRDS('data/tasks.rds')
# limit to just unique descriptions
descs<-data.table(unique(tasks[,"service_performed"]))
descs[, raw_id:=1:.N]
descs[, service_performed:=paste0("'", service_performed)]
write.csv(descs[, c("raw_id", "service_performed")],'data/descriptions_for_upwork.csv', row.names = FALSE)