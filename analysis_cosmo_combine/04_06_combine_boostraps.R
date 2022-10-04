library('data.table')
### combine bootstraps
### compute standard errors and save.
files <- list.files("data/",pattern="boot_res.Rdata")

counter<-0
for (file in files){
  load(paste0("data/",file))
  if (counter==0) {
    if (exists("boot_piece")) {
      boot_piece->boot_res
      rm(boot_piece)
    }
    do.call(rbind.data.frame, boot_res)->boot_full
    colnames(boot_full)<-paste0("theta",1:ncol(boot_full))
  } else{
    if (exists("boot_piece")) {
      boot_piece->boot_res
      rm(boot_piece)
    }
    do.call(rbind.data.frame, boot_res)->boot_res
    colnames(boot_res)<-paste0("theta",1:ncol(boot_res))
    
    boot_full<-rbind(boot_full, boot_res)
  }
  counter<-counter+1
}

boot_full<-data.table(boot_full)
cols<-paste0("theta",1:ncol(boot_full))

#### transform
boot_full[, theta1:=exp(theta1)]
cols<-paste0("theta",4:8)
wage_bound<-Vectorize(function(x){
  return(185/(exp(-x)+1)+15)
})
boot_mega<-copy(boot_full)
boot_full<-boot_full[1:500,]
boot_full[ , (cols) := lapply(.SD, "wage_bound"), .SDcols = cols]
cols<-paste0("theta",1:ncol(boot_full))
boot_se<-copy(boot_full)
boot_se<-boot_se[ , (cols) := lapply(.SD, "sd"), .SDcols = cols][1,]
boot_se<-as.numeric(boot_se)
save(boot_full,boot_se, file="data/04_06_all_boot.RData")


## check changes if we include 50 more.
old<-boot_se
cols<-paste0("theta",4:8)
boot_mega[ , (cols) := lapply(.SD, "wage_bound"), .SDcols = cols]
cols<-paste0("theta",1:ncol(boot_mega))
boot_se<-copy(boot_mega)
boot_se<-boot_se[ , (cols) := lapply(.SD, "sd"), .SDcols = cols][1,]
boot_se<-as.numeric(boot_se)

all(abs((boot_se-old)/old)<0.01)

