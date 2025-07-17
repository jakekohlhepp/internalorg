## purpose: retrieve gammas for some firms.

library('data.table')
library('lubridate')
library('stringr')
library('SQUAREM')
library('ggplot2')

##### purpose: perform the clustering procedure
##### can modify this directly to create the bootstrap program
staff_task<-data.table(readRDS('analysis_final/data/01_00_staff_task.rds'))

# we want threshold to stop grouping to be low for small firms and high for big firms.
within_firm_clust<-function(mat,cut_level){
  # if only one employee, return one cluster
  if (nrow(mat)<=1){
    return(c(1))
  }else{
    clust_res<-hclust(dist(mat, method="euclidean"),method='complete')
    clust_res<-cutree(clust_res,h=cut_level)
    return(clust_res)
  }
}



# for each market, set the cut_level to be such that all firms have 5 or less types.
max(staff_task[county=='17031']$min_cutlevel)
max(staff_task[county=='36061']$min_cutlevel)
max(staff_task[county=='06037']$min_cutlevel)

staff_task[,county_cutlevel:=max(min_cutlevel), by=county ]
staff_task[,type_within_firm:=within_firm_clust(as.matrix(.SD),county_cutlevel[1]), by=c("location_id", "quarter_year"), .SDcols=colnames(staff_task)[colnames(staff_task) %like% "^Btilde_raw_"]]
staff_task[,types_observed_firm:=max(type_within_firm), by=c("location_id", "quarter_year")]


all_tasks<-""
for (col in gsub("^B_","",names(staff_task)[grep("^Btilde_", names(staff_task))])) all_tasks<-paste0(all_tasks, "1")
firm_groups<-unique(staff_task[, c("types_observed_firm", "service_mix_id", "quarter_year", "location_id")])
stopifnot(uniqueN(firm_groups[, c("quarter_year", "location_id")])==nrow(firm_groups))

### restore the unsmoothed data just to get task_mix and s_index, and set group numbers.
full_unsmoothed<-readRDS("analysis_final/data/01_00_staff_task_full.rds")
full_unsmoothed<-unique(full_unsmoothed[,.SD, .SDcols=c(colnames(full_unsmoothed)[grep("^task_mix",colnames(full_unsmoothed))], "s_index", "location_id", "quarter_year", "county", "tot_duration")])
full_unsmoothed<-merge(full_unsmoothed, firm_groups[,c("location_id", "quarter_year", "types_observed_firm", "service_mix_id")], by=c("location_id", "quarter_year"), all.x=TRUE)

## restrict to the three counties and quarters
quarter_list<-c(2018.1, 2018.2, 2018.3, 2018.4, 2019.1, 2019.2, 2019.3, 2019.4, 2020.1, 2020.4, 2021.1, 2021.2)
full_unsmoothed<-full_unsmoothed[county %in% c("17031", "36061", "06037") & quarter_year %in% quarter_list,]
### to get the additional gammas we need:
## wage-adjusted skill matrix
## task-mix
## s_index


### first construct wage-adjusted skill matrix
all_results<-readRDS('analysis_final/data/02_00_parameters.rds')
market_parms<-all_results$coefficients
names(market_parms)<-all_results$parm_name


tild_theta<-vector(mode='list', length=3)
names(tild_theta)<-list("17031", "36061", "6037")

for (cnty in names(tild_theta)){
  w_mat<-matrix(c(0,market_parms[grep(paste0(cnty,":avg_labor:E"),names(market_parms))]), ncol=5, nrow=5, byrow=FALSE)
  skills<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
  rho<-market_parms[grep(paste0(cnty,":cust_price$"),names(market_parms))]
  tild_theta[[cnty]]<-w_mat+(rho)^(-1)*skills
  tild_theta[[cnty]]<-sweep(tild_theta[[cnty]],2,apply(tild_theta[[cnty]],2,min))

}






#### Get all estimated gammas
working_data<-data.table(readRDS('analysis_final/data/01_01_working.rds'))


## setup the data and main functions
source('analysis_final/preamble.R')




## set innertolerance, and choose if we want to accelerate the fixedpoint
innertol<-1e-08
outertol<-1e-04
pl_on<-TRUE

full_unsmoothed<-merge(full_unsmoothed,working_data[,.SD, .SDcols=c("location_id", "quarter_year",
                                                       "E_raw_1", "E_raw_2", "E_raw_3", "E_raw_4", "E_raw_5",
                                                       colnames(working_data)[grep("^B_raw_", colnames(working_data))] )],by=c("location_id", "quarter_year"), all.x=TRUE)

full_unsmoothed[county=="06037", county:="6037"]


## how many firm quarters were in the estimation sample?
print(paste0("Salon-quarters in estimation sample: ", nrow(full_unsmoothed[!is.na(E_raw_1),])))

print(paste0("Salon-quarters NOT in estimation sample: ", nrow(full_unsmoothed[is.na(E_raw_1),])))


## how many 
### for firms that have a s-index greater than the maximum, set gamma to 0.
# maximum s_index if no frictions.
sbound<-Vectorize(function(i){
  a1<-full_unsmoothed[i,]$task_mix_1[1]
  a2<-full_unsmoothed[i,]$task_mix_2[1]
  a3<-full_unsmoothed[i,]$task_mix_3[1]
  a4<-full_unsmoothed[i,]$task_mix_4[1]
  a5<-full_unsmoothed[i,]$task_mix_5[1]
  county<-full_unsmoothed[i,]$county[1]
  alpha<-c(a1, a2, a3, a4,a5)
  B<-matrix(0, ncol=5, nrow=5)
  for (col in 1:5){
    B[which.min(tild_theta[[county]][,col]),col]<-alpha[col]
  }
  E<-rowSums(B)
  Brel<-t(t(B/E)/alpha)
  return(sum(B*spec_log(Brel)) )
})

full_unsmoothed$s_bound<-sbound(1:nrow(full_unsmoothed))

# how many salons have an s-index beyond the bound?
print(nrow(full_unsmoothed[s_index>=s_bound]))

# mutual information has infinite marginal cost near maximum, so these have org cost of 0.
full_unsmoothed[s_index>=s_bound, gamma_invert:=0]






### invert gamma for everyone except those with 0 s-index or gamma above the bound
full_unsmoothed[, to_invert:= s_index>0 & s_index<s_bound ]
start_gamma_found<-1
max_gamma_found<-10000



findgamma<-function(i){
  a1<-full_unsmoothed[i,]$task_mix_1[1]
  a2<-full_unsmoothed[i,]$task_mix_2[1]
  a3<-full_unsmoothed[i,]$task_mix_3[1]
  a4<-full_unsmoothed[i,]$task_mix_4[1]
  a5<-full_unsmoothed[i,]$task_mix_5[1]
  county<-full_unsmoothed[i,]$county[1]
  s_actual<-full_unsmoothed[i]$s_index[1]
  get_struct<-function(gamma){
    alpha<-c(a1, a2, a3, a4,a5)
    ## this function will return matrix given gamma
    A<-exp(-1/gamma*(tild_theta[[county]]) )
    E<-rep(0.2, 5)
    A[A>=Inf]<-1e16
    A[A<=0]<-1e-16
    fxpt<-function(p){
      C<-colSums(t(A)*alpha/colSums(A*p))
      return(p*C)
    }
    #for (counter in 1:1000000){
    #  E_old<-E
    #  E<-fxpt(E_old)
    #  if (all(abs(E-E_old)<innertol)) break
    #}
    E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
    B<-t(t(A)*alpha/colSums(A*E))*E
    B[abs(B)<1e-16]<-0
    Brel<-t(t(B/E)/alpha)
    return(s_actual-sum(B*spec_log(Brel)) )
  }
  gamma<-bisection(get_struct, a=start_gamma_found, b=max_gamma_found, ftol=outertol,xtol=outertol,n=10000)
  stopifnot(gamma$conv)
  return(gamma$root)
  
}


if (pl_on){
  if (get_os()=="windows"){
    clust <- makeCluster(core_count)
    clusterExport(clust, ls())
    clusterEvalQ(clust, library("data.table"))
    clusterEvalQ(clust, library("SQUAREM"))
    
    temp<-data.table(do.call(rbind,parLapply(clust,which(full_unsmoothed$to_invert),findgamma)))
    stopCluster(clust)
  } else{
    temp<-data.table(do.call(rbind,mclapply(which(full_unsmoothed$to_invert),findgamma, mc.cores=core_count)))
  } 
  full_unsmoothed[which(full_unsmoothed$to_invert),gamma_invert:=temp$V1]
  
} else{
  for (i in which(full_unsmoothed$to_invert) ){
    full_unsmoothed[i,gamma_invert:= findgamma(i) ]
    print(i)
  }
  
}



# the salons without a gamma invert have an sindex of 0
stopifnot(all(full_unsmoothed[is.na(gamma_invert)]$s_index==0))

# firms with an s-index of 0 should have gamma invert set to infinite
# for counterfactuals we assume they choose a 0 complexity structure
full_unsmoothed[s_index==0, gamma_invert:=Inf]

stopifnot(nrow(full_unsmoothed[is.na(gamma_invert)])==0)

saveRDS(full_unsmoothed,'analysis_final/data/02_04_withgammas.rds')

### plot the distribution of gamma
ggplot(full_unsmoothed, aes(x=gamma_invert)) +
  geom_histogram(color="black", fill="lightblue", size=0.5, bins = 40)+ ylab("Salon-Quarter Count") + xlab("Org. Cost Parameter")+ theme(legend.position = "none")+
  theme_bw()+ theme(axis.text = element_text(size = 16), axis.title= element_text(size = 18))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("analysis_final/out/figures/02_04_gamma_dist.png", width=8, height=4, units="in")


