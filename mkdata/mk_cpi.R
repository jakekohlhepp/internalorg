### explore hypotheses on specialization
library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('lubridate')
library('stringr')

## process cpi data monthly.

cpi<-fread('mkdata/raw/20211015_cpi_uscity_average/CPIAUCSL.csv')
cpi[, date:=parse_date_time(DATE, "ymd")]
cpi[, DATE:=NULL]
saveRDS(cpi, file="mkdata/data/cpi.rds")