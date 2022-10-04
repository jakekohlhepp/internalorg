## read in safegraph places.

## Purpose: combine all safegraph files into one.
library('R.utils')
library('data.table')
library('SafeGraphR')
library('lubridate')

#gunzip("mkdata/raw/20211021_safegraphplaces/core_poi.csv.gz", remove=FALSE)

places<-fread('mkdata/raw/20211021_safegraphplaces/core_poi.csv')



saveRDS(places,file="mkdata/data/safegraph_places.rds")
