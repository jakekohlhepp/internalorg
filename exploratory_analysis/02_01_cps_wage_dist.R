library('data.table')
library('lubridate')
library('stringr')

setwd('exploratory_analysis/')
## view the distribution of barbers and hair stylists in New york vs LA overtime
cps_hair<-readRDS('../mkdata/data/cps_barbers_stylists.rds')
cps_hair<-cps-hair[!is.na(wage)]

# weighted variance

stats<-cps_hair[,.(m_wage=weighted.mean(wage,basicwgt), 
                   var_wage=sum(basicwgt*(wage-weighted.mean(wage, basicwgt))^2),
                   var=var(wage),
                   iqr=IQR(wage)),
                by=c("year")]

plot(stats$year,stats$iqr )