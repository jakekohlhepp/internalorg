# use consumer expenditure survey to find the fraction of people who do not get a haircut
# do this by income.

library('data.table')


outside_option<-c()

for (y in 13:21){
  expend<-fread(gsub('19',as.character(y),'mkdata/raw/20220711_cex/intrvw19/expn19/xpb19.csv'))
  income2<-fread(gsub('19',as.character(y),'mkdata/raw/20220711_cex/intrvw19/intrvw19/fmli192.csv'))
  income3<-fread(gsub('19',as.character(y),'mkdata/raw/20220711_cex/intrvw19/intrvw19/fmli193.csv'))
  income4<-fread(gsub('19',as.character(y),'mkdata/raw/20220711_cex/intrvw19/intrvw19/fmli194.csv'))
  income5<-fread(paste0('mkdata/raw/20220711_cex/',gsub('19',as.character(y),'intrvw19/'),gsub('19',as.character(y),'intrvw19/') ,gsub('20',as.character(y+1),'fmli201.csv')))
  income<-rbind(income2, income3, income4, income5, fill=TRUE)
  stopifnot(nrow(income)==uniqueN(income[,NEWID])) # unique by newid
  
  # use imputed data.
  together<-merge(expend[, c("NEWID", "QYEAR", "SALONX", "SALONX_")],income[, c("NEWID","FSALARYM", "PSU")],by="NEWID")
  together[, SALONX:=as.numeric(SALONX)]
  stopifnot(all(together[is.na(SALONX),]$SALONX_=="A")) # all blanks are valid
  together[, nohc:=is.na(SALONX)]

  # by quarter, psu get number of people who get cut.
  outside_option<-rbind(outside_option,together[,.(nohc_count=sum(nohc),
                              max_expend=max(SALONX,na.rm=TRUE), 
                              min_spend=min(SALONX,na.rm=TRUE ),
                              count_sample=.N),
                           by=c("PSU", "QYEAR")], fill=TRUE)

}

outside_option<-rbind(outside_option,together[,.(nohc_count=sum(nohc),
                                                 max_expend=max(SALONX,na.rm=TRUE), 
                                                 min_spend=min(SALONX,na.rm=TRUE ),
                                                 count_sample=.N),
                                              by=c("PSU", "QYEAR")], fill=TRUE)


outside_option[,quarter_year:=as.numeric(as.character(QYEAR/10))]
outside_option[, quarter_year:=ifelse(round(quarter_year-floor(quarter_year),1)==0.1,floor(quarter_year)-1+0.4,quarter_year-0.1)]
outside_option[,quarter_year:=as.numeric(as.character(quarter_year))]
saveRDS(outside_option,file="mkdata/data/cex_outside.rds")


