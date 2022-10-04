### create task data set.
## main computation burden is building classification categories.
library('data.table')
library('lubridate')
library('stringr')
library('DescTools')
library('quanteda')
quanteda_options(threads=4)
setwd('C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/exploratory_analysis/')


working_data<-data.table(readRDS("C:/Users/jakek/blvd_dont_backup/compiled_trxns_tasks.rds"))
working_data[,raword:=1:.N]
# limit to just self-reported hair salons prior to covid-19
lim_working<-working_data[industry%in% c("Hair Salon", "Barber") ,]
rm(working_data)
gc()

# remove transactions with total price less than 10.
lim_working[,tot_price:=sum(price), by="app_id"]
lim_working<-lim_working[tot_price>=10,]

lim_working[,month_num:=year(date)+month(date)/13][, week_num:=year(date)+week(date)/53]

# there is one salon with a ridiculous spike in revenue. exclude for now.
lim_working<-lim_working[location_id!="dae8355a-cd8f-4ae1-8d26-a839b578f9f9",]
lim_working[service_performed=="GlazeOld",service_performed:="Glaze Old"]
lim_working[service_performed=="HaircutOld",service_performed:="Haircut Old"]

text_to_clean<-unique(lim_working[,c("service_performed")])
text_to_clean[, clean_task:=str_to_lower(service_performed)][,clean_task:=gsub('[[:punct:]]+','',clean_task)][,clean_task:=gsub('[[:digit:]]+', '', clean_task)]
##nrow(text_to_clean)-uniqueN(text_to_clean$clean_task)

allcorpus<-dfm_trim(dfm(text_to_clean$clean_task, remove = c(stopwords(language="English"),"x") , remove_punct = TRUE, stem = TRUE),min_docfreq=20)
allcorpus<-dfm_weight(allcorpus,
                      scheme="prop")
gc()
dist_corp <- textstat_dist(allcorpus, method = "euclidean", margin="documents")
dist<-as.matrix(dist_corp)
hold<-colSums(is.na(dist)) != nrow(dist)
dist<-as.dist(dist[rowSums(is.na(dist)) != ncol(dist), colSums(is.na(dist)) != nrow(dist)])
clusters<-hclust(dist, method="ward.D2")
clusters$labels<- text_to_clean$clean_task[hold]

final_groups<-unique(data.table(clust=as.numeric(cutree(clusters, 10)), clean_task=names(cutree(clusters, 10))))
final_groups<-merge(final_groups,text_to_clean, by="clean_task", all.y=TRUE)
final_groups[is.na(clust), clust:=6]
check<-nrow(lim_working)
lim_working<-merge(lim_working, final_groups,by="service_performed")
stopifnot(nrow(lim_working)==check)
# in addition to cluster number, name group based on most frequently occuring
# mode, when tie give first in terms of alphabetic order.
Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  sort(ux[tab == max(tab)])[1]
}

lim_working[,rep_text_cluster:=Mode(service_performed), by=clust]
lim_working[, weeks_since_2000:=as.numeric(floor( (date-date("2000-01-01"))/7))]

saveRDS(lim_working,file="data/tasks.rds")
