### create word bubbles for tasks
library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('ggplot2')
library('lubridate')
library('stringr')
library('quanteda')
library('DescTools')
setwd('C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/analysis/')
working<-data.table(readRDS("../mkdata/data/tasks.rds"))

## Make word bubble
clouds<-list()
for (x in 1:4){
  text_to_clean<-working[clust==x,c("clean_task")]
  corpus<-dfm_trim(dfm(text_to_clean$clean_task, remove = c(stopwords(language="English"),"x") , remove_punct = TRUE, stem = TRUE),min_docfreq=20)
  png(paste0("out/01_005_wordcloud_", x,".png"), width = 600, height = 600)
  textplot_wordcloud(corpus, 
                     color = rev(RColorBrewer::brewer.pal(10, "RdBu")), random_order=FALSE, random_color=FALSE)
  dev.off()
}
table(working$clust)
table(working$rep_text_cluster)

