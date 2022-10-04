## compile epi cps monthly (https://microdata.epi.org/?fbclid=IwAR2aC_suGIq1lgjX3uKDcK01qjiVOnoyj5cDWwpS2FftCPOrynPBycM4u9U)
library('data.table')
library('haven')
library('lubridate')
library('modi')

setwd('C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/mkdata/raw/20210920_epi_cpsbasic_2000_2021/')
files<-list.files(pattern = "*.dta")

## we have two goals: first get hairstylist/barber wage distribution
save_mem<-function(x){
  temp<-data.table(read_dta(x))
  if (any(!is.na(temp$occ18))){
    temp<-temp[occ18 %in% c(4510, 4500) ,]
  } else if (any(!is.na(temp$occ10))){
    temp<-temp[occ10 %in% c(4510, 4500) ,]
  } else if (any(!is.na(temp$occ00))) {
    temp<-temp[occ00 %in% c(4510, 4500) ,]
  } else {
    temp<-temp[occ90 %in% c(458, 457),]
  }
  return(temp)
}

cps_hair <- rbindlist(lapply(files, save_mem))


saveRDS(cps_hair,file="C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/mkdata/data/cps_barbers_stylists.rds")

## then get county demographic statistics.

familyinc_dist<-function(x){
  temp<-data.table(read_dta(x))
  temp<-temp[,.(count=.N, weight_sum=sum(famwgt)), by=c("countyfips", "statefips", "year", "faminc", "age", "female")]
  return(temp)
}

cps_all <- rbindlist(lapply(files, familyinc_dist))


saveRDS(cps_all,file="C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/mkdata/data/cps_faminc_county.rds")


