### explore hypotheses on specialization
library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('lubridate')
library('stringr')

## download and combine ppp loan data. only keep barbershops or hair salons.

## base url:https://data.sba.gov/dataset/ppp-foia
thepage = readLines('https://data.sba.gov/dataset/ppp-foia')
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
urls <- str_extract(thepage, url_pattern)
urls<-urls[!is.na(urls)]
urls<-urls[str_detect(urls, ".csv")]
parcel<-data.table()
for (f in urls){
  part<-fread(f)
  part<-part[NAICSCode%in% c("812112","812111"),]
  part[, url:=f]
  parcel<-rbind(parcel, part)
  print(paste("Completed:", f))
}


saveRDS(parcel,file="mkdata/data/ppp.rds")
