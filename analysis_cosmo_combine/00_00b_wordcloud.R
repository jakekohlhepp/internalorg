### explore hypotheses on specialization
library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('ggplot2')
library('lubridate')
library('stringr')
library('quanteda')
library('DescTools')

working<-data.table(readRDS("../mkdata/data/tasks_cosmo.rds"))

## merge extension task with blowdry task.
working[, clust:=ifelse(clust==3,4 ,clust)]
working[, rep_text_cluster:=ifelse(clust==4,"Blowdry/Style/Treatment/Extension" ,rep_text_cluster)]
working[, clust:=frank(clust, ties.method="dense")]

## Make word bubble
clouds<-list()
for (x in 1:5){
  text_to_clean<-working[clust==x,c("service_performed")]
  corpus<-dfm_trim(dfm(text_to_clean$service_performed, remove = c(stopwords(language="English"),"x") , remove_punct = TRUE, stem = TRUE),min_docfreq=20)
  png(paste0("out/00_00b_cosmo_wordcloud_", x,".png"), width = 600, height = 600)
  textplot_wordcloud(corpus, 
                     color = rev(RColorBrewer::brewer.pal(10, "RdBu")), random_order=FALSE, random_color=FALSE)
  dev.off()
}

text_to_clean<-working[,c("service_performed")]
corpus<-dfm_trim(dfm(text_to_clean$service_performed, remove = c(stopwords(language="English"),"x") , remove_punct = TRUE, stem = TRUE),min_docfreq=20)
png("out/00_00b_cosmo_wordcloud_all.png", width = 600, height = 600)
textplot_wordcloud(corpus, 
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu")), random_order=FALSE, random_color=FALSE)
dev.off()

table(working$rep_text_cluster)
working[,tot_duration:=sum(duration, na.rm=TRUE) ]
tabulate_durations<-working[,.(frac_time=sum(duration/tot_duration, na.rm=TRUE)), by="clust"]

