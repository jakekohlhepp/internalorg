## extract top 600 zip codes fro period 2018-present in terms of salons.
## want to limit to just salons
## note that zip codes are mapped to cities using a lat-long method in noncensus package.

library('data.table')
library('stringr')
library('lubridate')
library('noncensus')

working_data<-data.table(readRDS("C:/Users/jakek/blvd_dont_backup/data/compiled_trxns.rds"))
working_data<-working_data[industry%in% c("Hair Salon") ,]
#working_data<-working_data[date>=date('2017-01-01') ,]
zips<-working_data[,.(trxns=.N, salons=uniqueN(location_id), emps=uniqueN(staff_id)), by=location_zip]

# agreement is for 600 zipcodes. how many do we have left?
leftovers<-600-nrow(zips)
data(zip_codes)
countyxwalk<-data.table(zip_codes)
new_zips<-countyxwalk[!(zip %in% zips$location_zip) & 
                        ((city=="Los Angeles" & state=="CA") | (city=="New York" & state=="NY"))]
stopifnot(leftovers-nrow(new_zips)>=0)

# final list is all zips where we have one trxn in the entire period plus all zips in the city of LA or NY.
final_list<-c(zips$location_zip, new_zips$zip)
final_list<-sort(final_list)
stopifnot(uniqueN(final_list)==length(final_list)) # unique
final_list<-data.frame(requested_zips=final_list)
write.csv(final_list,row.names=FALSE, file="C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/mkdata/data/yelp_request.csv")


