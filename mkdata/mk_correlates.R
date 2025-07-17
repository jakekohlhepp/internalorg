library('data.table')
library('hunspell')

## mk other correlates
## generate other measures of management at firm quarter level.
working<-data.table(readRDS("mkdata/data/tasks_cosmo.rds"))


### from main data source.

## price variance across stylists within task.

## typos: count unique descriptions, count misspellings
descript<-unique(working[,"service_performed"])
is_mispelled<-Vectorize(function(x){
  return(length(hunspell(x)[[1]])!=0)
})
descript[, misspelled:=is_mispelled(service_performed)]


## determine average variance in price.

## prebooking function

## downtime - gaps within a day between appointments for an employee.


### from other sources
## tips: average tips, fraction of transactions with a tip filled in during a quarter

## chair renters - number
chair_renters<-fread('C:/Users/jakek/blvd_dont_backup/20201204_chair_renters/staff_renters.csv')

## products sold, and commissions


## cancellations





