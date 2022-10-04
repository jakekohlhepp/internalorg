## Purpose: combine all safegraph files into one.
library('R.utils')
library('data.table')
library('SafeGraphR')
library('lubridate')
library('plyr')
## combine patterns data 
## loop over folders.

zipF <- list.files(path = "mkdata/raw/20220407_newsafegraph/", pattern = "*.gz", full.names = TRUE)

# unzip all your files

puzzle<-data.table()
for (f in zipF[13]){
  gunzip(f, remove=FALSE, overwrite=TRUE)
  fp<-gsub(".gz", "",f)
  piece<-fread(fp, colClasses = c('poi_cbg'='character'))
  puzzle<-rbind(puzzle, piece)
}


#names <- c("date_range_start", "date_range_end")

#puzzle[, (names) := lapply(.SD, ymd_hms), .SDcols = names]
#
#names <- c("opened_on", "closed_on")
#helper<-function(x){
#  ymd(paste0(x, "-01"))
#}
#helper<-Vectorize(helper)

#puzzle[, (names) := lapply(.SD, helper), .SDcols = names]

saveRDS(puzzle,file="mkdata/data/safegraph_patterns.rds")





