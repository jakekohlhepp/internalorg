library(tidycensus)
library('data.table')
## get census-tract income and population data

census_api_key('d921309b83d0c0c0d23180bb9c18e8d33c4a47df')


data<-data.table(get_acs(geography = "zcta", 
              variables = c(medincome = "B19013_001", pop='B01003_001', percap_income='B19301_001')
              ,year = 2020))

data[, zipcode:= gsub('ZCTA5 ', '', NAME)]

cdata<-dcast(data, GEOID + NAME +zipcode ~ variable, value.var = c("estimate", "moe"))
cdata[,location_zip:=as.numeric(zipcode)]
saveRDS(cdata,file="data/acs5_income_pop.rds")

puzzle<-data.table()
for (s in state.abb) {
  piece<-data.table(get_acs(geography = "county subdivision", state=s,
                           variables = c(medincome = "B19013_001", pop='B01003_001', percap_income='B19301_001')
                           ,year = 2020))
  piece[, state:=s]
  puzzle<-rbind(puzzle, piece)
}

saveRDS(puzzle,file="data/acs5_income_pop_subdiv.rds")

