# mk census pop estimates
library('data.table')
library('lubridate')
library('stringr')

countypop_more<-fread('mkdata/raw/202403101_morecspops/co-est2022-alldata.csv')[-1]
countypop_more[, state:=ifelse(str_length(as.character(STATE))==max(str_length(as.character(STATE))),STATE,paste0("0",STATE))]
countypop_more[, county:=ifelse(str_length(as.character(COUNTY))==max(str_length(as.character(COUNTY))),COUNTY,paste0("0",COUNTY))]
countypop_more[, county:=ifelse(str_length(as.character(county))==max(str_length(as.character(county))),county,paste0("0",county))]
countypop_more[, county:=ifelse(str_length(as.character(county))==max(str_length(as.character(county))),county,paste0("0",county))]
countypop_more[, county:=paste0(state,county)]
stopifnot(nrow(countypop_more[str_length(county)!=5,])==0)
countypop_more<-countypop_more[,c("county", "POPESTIMATE2020", "POPESTIMATE2021", "POPESTIMATE2022")]

countypop_more2<-fread('mkdata/raw/202403101_morecspops/co-est2019-alldata.csv')[-1]
countypop_more2[, state:=ifelse(str_length(as.character(STATE))==max(str_length(as.character(STATE))),STATE,paste0("0",STATE))]
countypop_more2[, county:=ifelse(str_length(as.character(COUNTY))==max(str_length(as.character(COUNTY))),COUNTY,paste0("0",COUNTY))]
countypop_more2[, county:=ifelse(str_length(as.character(county))==max(str_length(as.character(county))),county,paste0("0",county))]
countypop_more2[, county:=ifelse(str_length(as.character(county))==max(str_length(as.character(county))),county,paste0("0",county))]
countypop_more2[, county:=paste0(state,county)]
stopifnot(nrow(countypop_more2[str_length(county)!=5,])==0)
countypop_more2<-countypop_more2[,c("county", "POPESTIMATE2010", "POPESTIMATE2011", "POPESTIMATE2012","POPESTIMATE2013","POPESTIMATE2014",
                                    "POPESTIMATE2015", "POPESTIMATE2016", "POPESTIMATE2017", "POPESTIMATE2018", "POPESTIMATE2019")]
countypop_more<-merge(countypop_more,countypop_more2,by="county", all=TRUE)
countypop_more<-melt(countypop_more, id.vars ="county", measure = patterns("^POPESTIMATE"),value.name = "CSPOP")
countypop_more[, year:=as.numeric(str_remove(variable, "POPESTIMATE"))]
saveRDS(countypop_more[,-c("variable")],"mkdata/data/county_census_pop.rds")