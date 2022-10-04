## Purpose: combine all safegraph files into one.
library('R.utils')
library('data.table')
library('SafeGraphR')
library('lubridate')
library('plyr')
## combine patterns data 
## loop over folders.

zipF <- list.files(path = "mkdata/raw/20211021_safegraph_patterns/", pattern = "*.zip", full.names = TRUE)

# unzip all your files

for (f in zipF){
unzip(f, exdir= gsub(".zip", "",f))
}

puzzle<-data.table()
for (f in zipF){
  fp<-gsub(".zip", "",f)
  gunzip(paste0(fp,"/patterns.csv.gz"), remove=FALSE, overwrite=TRUE)
  piece<-fread(paste0(fp,"/patterns.csv"), colClasses = c('poi_cbg'='character'))
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





