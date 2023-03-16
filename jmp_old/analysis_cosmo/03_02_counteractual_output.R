## make tables and graphs about counterfactuals
library('data.table')
library('gmm')
library('spatstat')
library('stringr')
library('knitr')
library('ggplot2')
library('kableExtra')
theme_set(theme_bw(base_size=22))
sigfig <- function(vec, n=3){ 
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig  
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg") 
  
}      # end of function   sigfig

load('data/03_00_counterfactual.RData')

##########################################################################
##########################################################################
################################### attach prices and total wages
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
bisection <- function(f, a, b, n, xtol, ftol) {
  # If the signs of the function at the evaluated points, a and b, stop the function and return message.
  if (!(f(a) < 0) && (f(b) > 0)) {
    stop('end points not of opposite sign')
  } else if ((f(a) > 0) && (f(b) < 0)) {
    stop('end points not of opposite sign')
  }
  
  for (i in 1:n) {
    c <- (a + b) / 2 # Calculate midpoint
    
    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the 
    # function and return the root.
    #if (abs(f(c))<ftol || ((b - a) / 2) < xtol) {
    if (abs(f(c))<ftol || ((b - a) / 2) < xtol) {
      return(list("root"=c, "val"=f(c),"conv"=abs(f(c))<ftol || ((b - a) / 2) < xtol  ))
    }
    
    # If another iteration is required, 
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    ifelse(sign(f(c)) == sign(f(a)), 
           a <- c,
           b <- c)
  }
  # If the max number of iterations is reached and no root has been found, 
  # return message and end function.
  return(list("root"=c, "val"=f(c),"conv"=abs(f(c))<ftol || ((b - a) / 2) < xtol  ))
}

load('data/01_01_progress.RData')

if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-32
} 

source('small_gmm_objective.R')

ny_objects<-ret_objects(coef(res_store), estim_matrix)



#### Prep Data
# weight is just scaled up share.
estim_sample[, weight:=(1-outside_share)/(sum(salon_share_subdiv)), by=c("county","quarter_year")]

## Compute amount of each type.
# this is row sum of b, times total duration times weight. divide by 60 to get in hours.
puzzle<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])*estim_sample$salon_share_subdiv[estab]*estim_sample$avg_labor[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}
tot_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), L6=sum(V6)) , by=V7 ]
setnames(tot_labor, "V7", "quarter_year")
labor_supply<-as.numeric(tot_labor[1,-1])
wages<-wage_bound(coef(res_store)[4:9])


##########################################################################
##########################################################################
##########################################################################


counter_res[str_detect(context, "Wage")==0, totalwages:=sum(labor_supply)*mean_wage]
counter_res[, totalwelfare:=totalwages+total_profit+consumer_welfare]
counter_res[, avgprice:=c(mean(estim_sample$cust_price),sapply(2:length(counter_objects), function(x) {mean(counter_objects[[x]]$mainstats$newprice)}))]
counter_res[, sdC:=c(sd(ny_objects$C),sapply(2:length(counter_objects), function(x) {sd(counter_objects[[x]]$mainstats$C)}))]
counter_res[, sdQ:=c(sd(ny_objects$Q),sapply(2:length(counter_objects), function(x) {sd(counter_objects[[x]]$mainstats$Q)}))]
counter_res[, sdQC:=c(sd(ny_objects$C-(coef(res_store)[1]*1.045)^(-1)*ny_objects$Q),sapply(2:length(counter_objects), function(x) {sd(counter_objects[[x]]$mainstats$C -(coef(res_store)[1]*1.045)^(-1)*counter_objects[[x]]$mainstats$Q )}))]
counter_res[, covered_share:=c(sum(estim_sample$salon_share_subdiv*estim_sample$weight),sapply(2:length(counter_objects), function(x) {sum(counter_objects[[x]]$mainstats$new_share *estim_sample$weight )}))]

counter_res[, avgprice:=ifelse(!str_detect(context,"Sales"),avgprice*1.045,avgprice)]
# sales tax table
setcolorder(counter_res, c("context","avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages", "totalwelfare"))

sales_tax<-melt(counter_res[context %in% c("Initial","Sales Tax Eliminated  - No Adj.", "Sales Tax Eliminated"),],
                measure.vars=names(counter_res)[-1])
sales_tax<-dcast(sales_tax, variable~ context, value.var="value")
setcolorder(sales_tax, c(1,2,4,3))
sales_tax[, pct_chnge:=(`Sales Tax Eliminated`-Initial)/Initial]

sales_tax<-sales_tax[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages", "totalwelfare")]
                    
colnames(sales_tax)<-c("Statistic", "Initial", "No Internal", "Counterfactual", "Change")
#sales_tax[, Initial:=formatC(Initial, digits = 3, format = "f")]
#sales_tax[, `No Internal`:=formatC(`No Internal`, digits = 3, format = "f")]
#sales_tax[, Counterfactual:=formatC(Counterfactual, digits = 3, format = "f")]
sales_tax[, Initial:=sigfig(Initial, 5)]
sales_tax[, `No Internal`:=sigfig(`No Internal`, 5)]
sales_tax[, Counterfactual:=sigfig(Counterfactual, 5)]
sales_tax[, Change:=paste0(formatC(Change*100, digits = 2, format = "f"),"\\%")]
sales_tax[,Statistic:=c("Avg. Price",
                        "Avg. Complexity",
                        "Avg. Quality",
                        "Avg. Hourly Wage",
                        "Std. Dev. Wage",
                        "Task Specialization",
                        "Total Profit",
                        "Consumer Welfare",
                        "Total Wages",
                        "Total Welfare"
)]

                 
kable(sales_tax, "latex", align="c", booktabs=TRUE, escape = F) %>%
  cat(., file = "out/tables/03_02_salestax_ny.tex")

## also do version that is decomposition

sales_tax<-melt(counter_res[context %in% c("Initial","Sales Tax Eliminated  - No Adj.", "Sales Tax Eliminated"),],
                measure.vars=names(counter_res)[-1])
sales_tax<-dcast(sales_tax, variable~ context, value.var="value")
setcolorder(sales_tax, c(1,2,4,3))
sales_tax[, rellocation_effect:=(`Sales Tax Eliminated  - No Adj.`-Initial)/Initial]
sales_tax[, reorg_effect:=(`Sales Tax Eliminated`-`Sales Tax Eliminated  - No Adj.`)/Initial]
sales_tax[, total_effect:=(`Sales Tax Eliminated`-Initial)/Initial]

sales_tax<-sales_tax[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages", "totalwelfare")]
sales_tax<-sales_tax[,c("variable", "rellocation_effect", "reorg_effect", "total_effect")]
colnames(sales_tax)<-c("Statistic", "Reallocation","Reorganization", "Total")

sales_tax[, Reallocation:=paste0(formatC(Reallocation*100, digits = 2, format = "f"),"\\%")]
sales_tax[, Reorganization:=paste0(formatC(Reorganization*100, digits = 2, format = "f"),"\\%")]
sales_tax[, Total:=paste0(formatC(Total*100, digits = 2, format = "f"),"\\%")]


sales_tax[,Statistic:=c("Avg. Price",
                        "Avg. Complexity",
                        "Avg. Quality",
                        "Avg. Hourly Wage",
                        "Std. Dev. Wage",
                        "Task Specialization",
                        "Total Profit",
                        "Consumer Welfare",
                        "Total Wages",
                        "Total Welfare"
)]


kable(sales_tax, "latex", align="c", booktabs=TRUE, escape = F) %>%
  cat(., file = "out/tables/03_02_salestax_effects.tex")


jobs_model<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(ny_objects$bmats[[y]]/rowSums(ny_objects$bmats[[y]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),6),rowSums(ny_objects$bmats[[y]]),
                    estim_sample[y,]$quarter_year, 1:6, estim_sample[y,]$location_id, ny_objects$gammas[y], estim_sample$s_index[y], ny_objects$C[y], estim_sample$salon_share_subdiv[y],estim_sample$cust_price[y],ny_objects$Q[y])
  jobs_model<-rbind(jobs_model,piece)
}
jobs_model<-data.table(jobs_model)
names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                     "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price", "quality")
jobs_model[,emp_tot:=tot_firm*type_frac]
jobs_model<-jobs_model[type_frac>1e-08]
jobs_model[,Source:="Initial"]


jobs_counterfactual<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(counter_objects$`Sales Tax Eliminated`$bmats[[y]]/rowSums(counter_objects$`Sales Tax Eliminated`$bmats[[y]]),rep(as.numeric(counter_objects$`Sales Tax Eliminated`$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),6),rowSums(counter_objects$`Sales Tax Eliminated`$bmats[[y]]),
                    estim_sample[y,]$quarter_year, 1:6, estim_sample[y,]$location_id, ny_objects$gammas[y],counter_objects$`Sales Tax Eliminated`$mainstats$Imodel[y], counter_objects$`Sales Tax Eliminated`$mainstats$C[y], counter_objects$`Sales Tax Eliminated`$mainstats$new_share[y], counter_objects$`Sales Tax Eliminated`$mainstats$newprice[y], counter_objects$`Sales Tax Eliminated`$mainstats$Q[y])
  jobs_counterfactual<-rbind(jobs_counterfactual,piece)
}
jobs_counterfactual<-data.table(jobs_counterfactual)
names(jobs_counterfactual)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                              "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price", "quality")
jobs_counterfactual[,emp_tot:=tot_firm*type_frac]
jobs_counterfactual<-jobs_counterfactual[type_frac>1e-08]
jobs_counterfactual[,Source:="Counterfactual"]

jobs_partial<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(counter_objects$`Sales Tax Eliminated - No Adj.`$bmats[[y]]/rowSums(counter_objects$`Sales Tax Eliminated - No Adj.`$bmats[[y]]),rep(as.numeric(counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),6),rowSums(counter_objects$`Sales Tax Eliminated - No Adj.`$bmats[[y]]),
                    estim_sample[y,]$quarter_year, 1:6, estim_sample[y,]$location_id, ny_objects$gammas[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$Imodel[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$C[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$new_share[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$newprice[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$Q[y]   )
  jobs_partial<-rbind(jobs_partial,piece)
}
jobs_partial<-data.table(jobs_partial)
names(jobs_partial)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                       "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price" , "quality")
jobs_partial[,emp_tot:=tot_firm*type_frac]
jobs_partial<-jobs_partial[type_frac>1e-08]
jobs_partial[,Source:="No Internal"]

tokeep<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6", "emp_tot","tot_firm", "Source", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share", "price", "quality")

jobs_both<-rbind(jobs_counterfactual[,..tokeep], jobs_model[,..tokeep],jobs_partial[,..tokeep] )

jobs_both[type==1, focal_task:=jobvect_1]
jobs_both[type==2, focal_task:=jobvect_2]
jobs_both[type==3, focal_task:=jobvect_3]
jobs_both[type==4, focal_task:=jobvect_4]
jobs_both[type==5, focal_task:=jobvect_5]
jobs_both[type==6, focal_task:=jobvect_6]

positions <- c("Initial", "No Internal", "Counterfactual")
jobs_both[, Source:=factor(Source, levels=positions)]
ggplot(jobs_both[type==2], aes(weight = emp_tot/sum(emp_tot),linetype=Source, color=Source)) + geom_density(aes(x = 100*focal_task),size=1.2 )+
  labs(x="Task Specialization", y="Density")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("dashed","dotted", "solid"))
ggsave("out/figures/03_02_sales_color.png", width=9, heigh=6, units="in")
ggplot(jobs_both[type!=2], aes(weight = emp_tot/sum(emp_tot),linetype=Source, color=Source)) + geom_density(aes(x = 100*focal_task),size=1.2 )+
  labs(x="Task Specialization", y="Density")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("dashed","dotted", "solid"))
ggsave("out/figures/03_02_sales_allothers.png", width=9, heigh=6, units="in")


firms<-unique(jobs_both[, c("location_id","Source", "gamma","tot_firm", "complexity", "mc", "share", "price", "quality" )])
firms[, profit:=estim_sample$CSPOP[1]*share/(1-share)/coef(res_store)[1]*1.045]
attachit<-firms[Source=="Initial"]
colnames(attachit)[-1]<-paste0(colnames(attachit)[-1], '_init')
firms<-merge(firms[Source!="Initial",], attachit, by="location_id")
firms[, emp_change:=(tot_firm-tot_firm_init)/tot_firm_init]
firms[, mc_change:=(mc-mc_init)/mc_init]
firms[, comp_change:=complexity-complexity_init ]
firms[, profit_change:=(profit-profit_init)/profit_init]
firms[, price_change:=(price-price_init)/price_init]
firms[, quality_change:=(quality-quality_init)/quality_init]
firms<-merge(firms, estim_sample[, c("task_mix2", "location_id")], all.x=TRUE, by="location_id")





### internal org.
org_model<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])
  piece<-data.frame( ny_objects$Q[estab]/coef(res_store)[1], sum(diag(ny_objects$bmats[[estab]])), estim_sample$location_id[estab])
  org_model<-rbind(org_model, piece)
}
colnames(org_model)<-c("quality", "task_spec", "location_id")

org_counterfactual<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(counter_objects$`Sales Tax Eliminated`$bmats[[estab]])
  piece<-data.frame( counter_objects$`Sales Tax Eliminated`$mainstats$Q[estab]/coef(res_store)[1], sum(diag(counter_objects$`Sales Tax Eliminated`$bmats[[estab]])), estim_sample$location_id[estab])
  org_counterfactual<-rbind(org_counterfactual, piece)
}
colnames(org_counterfactual)<-c("quality", "task_spec", "location_id")

org_partial<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(counter_objects$`Sales Tax Eliminated`$bmats[[estab]])
  piece<-data.frame( counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$Q[estab]/coef(res_store)[1], sum(diag(counter_objects$`Sales Tax Eliminated - No Adj.`$bmats[[estab]])), estim_sample$location_id[estab])
  org_partial<-rbind(org_partial, piece)
}
colnames(org_partial)<-c("quality", "task_spec", "location_id")


org_both<-merge(org_counterfactual, org_model, by="location_id")
org_counterfactual[, Source:="Full"]
org_model[, Source:="Initial"]
org_partial[, Source:="Reallocation"]
org_both2<-rbind(org_model, org_counterfactual, org_partial)

setorder(org_both2, "location_id", -"Source")
ggplot(data=org_both2[Source!="Initial"], aes(x=quality, y=task_spec, color=Source))+geom_point(size=4)+
  geom_path(aes(x =quality , y = task_spec, group = location_id), 
            arrow = arrow(length = unit(0.2, "cm"), type="closed"), size=0.8, color="black")+
  ylab("Task Specialization")+
  xlab("Service Quality")+
  theme(legend.position="bottom")
## one outlier excluded for better visualization.

ggsave("out/figures/03_02_salestax_reorg.png", width=9, heigh=6, units="in")


#### Counterfactual 2: Management software.
setcolorder(counter_res, c("context","avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages", "totalwelfare"))
software<-melt(counter_res[context %in% c("Initial", "Improvement in Management Software 2"),],
                measure.vars=names(counter_res)[-1])
software<-dcast(software, variable~ context, value.var="value")
setcolorder(software, c(1,3,2))
software[, pct_chnge:=(`Improvement in Management Software 2`-Initial)/Initial]

software<-software[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages", "totalwelfare")]

#sales_tax[, Initial:=formatC(Initial, digits = 3, format = "f")]
#sales_tax[, `No Internal`:=formatC(`No Internal`, digits = 3, format = "f")]
#sales_tax[, Counterfactual:=formatC(Counterfactual, digits = 3, format = "f")]
software[, Initial:=sigfig(Initial, 5)]
software[, `Improvement in Management Software 2`:=sigfig(`Improvement in Management Software 2`, 5)]

colnames(software)<-c("Statistic", "Initial", "Counterfactual", "Change")


software[, Change:=paste0(formatC(Change*100, digits = 2, format = "f"),"\\%")]
software[,Statistic:=c("Avg. Price",
                        "Avg. Complexity",
                        "Avg. Quality",
                        "Avg. Hourly Wage",
                        "Std. Dev. Wage",
                        "Task Specialization",
                        "Total Profit",
                        "Consumer Welfare",
                        "Total Wages",
                        "Total Welfare"
)]


kable(software, "latex", align="c", booktabs=TRUE, escape = F) %>%
  cat(., file = "out/tables/03_02_software_ny.tex")


#### Counterfactual 3: Minimum wage
load('data/03_00_counterfactual_minwage.RData')
load('data/03_00_counterfactual_minwage_noadj.RData')
trial_num<-which(minwage_res$check_binding & minwage_res$obj_value<0.01)
trial_num_noadj<-which(minwage_noadj_res$check_binding & minwage_noadj_res$obj_value<0.01)

finalminwage<-rbind(minwage_res[context=="Initial",],minwage_noadj_res[trial_num_noadj,],minwage_res[trial_num,] )
finalminwage[, unemployed:=unemployed/sum(labor_supply)]
finalminwage[, context:=c("Initial", "No Internal", "Counterfactual")]
finalminwage[, totalwages:=(sum(labor_supply)-unemployed)*mean_wage ]
finalminwage[, totalwelfare:=totalwages+total_profit+consumer_welfare]
finalminwage[, avgprice:=c(weighted.mean(estim_sample$cust_price,estim_sample$salon_share_subdiv),weighted.mean(minwage_noadj_objects[[trial_num_noadj]]$mainstats$newprice,w=minwage_noadj_objects[[trial_num_noadj]]$mainstats$new_share),weighted.mean(minwage_objects[[trial_num]]$mainstats$newprice,minwage_objects[[trial_num]]$mainstats$new_share) )]
finalminwage[, covered_share:=c(sum(estim_sample$salon_share_subdiv*estim_sample$weight),sum(minwage_noadj_objects[[trial_num_noadj]]$mainstats$new_share*estim_sample$weight ),sum(minwage_objects[[trial_num]]$mainstats$new_share*estim_sample$weight )) ]
setcolorder(finalminwage, c("context","avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "unemployed","covered_share", "total_profit", "consumer_welfare", "totalwages", "totalwelfare"))


minwag<-melt(finalminwage,
               measure.vars=names(finalminwage)[-1])
minwag<-dcast(minwag, variable~ context, value.var="value")
setcolorder(minwag, c(1,3,4,2))
minwag[, pct_chnge:=(`Counterfactual`-Initial)/Initial]

minwag<-minwag[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "unemployed","covered_share","total_profit", "consumer_welfare", "totalwages", "totalwelfare")]

minwag[, Initial:=sigfig(Initial, 5)]
minwag[, `No Internal`:=sigfig(`No Internal`, 5)]
minwag[, `Counterfactual`:=sigfig(`Counterfactual`, 5)]
colnames(minwag)<-c("Statistic", "Initial","No Internal", "Counterfactual", "Change")

minwag[, Change:=paste0(formatC(Change*100, digits = 2, format = "f"),"\\%")]
minwag[,Statistic:=c("Avg. Price",
                       "Avg. Complexity",
                       "Avg. Quality",
                       "Avg. Hourly Wage",
                       "Std. Dev. Wage",
                       "Task Specialization",
                       "Unemployment",
                        "Market Served",
                       "Total Profit",
                       "Consumer Welfare",
                       "Total Wages",
                       "Total Welfare"
)]


kable(minwag, "latex", align="c", booktabs=TRUE, escape = F) %>%
  cat(., file = "out/tables/03_02_minwage_ny.tex")

## also do version that is decomposition

minwag<-melt(finalminwage,
             measure.vars=names(finalminwage)[-1])
minwag[variable=="unemployed", value:=sum(labor_supply)-sum(labor_supply)*value]
minwag<-dcast(minwag, variable~ context, value.var="value")
setcolorder(minwag, c(1,3,4,2))

minwag[, rellocation_effect:=(`No Internal`-Initial)/Initial]
minwag[, reorg_effect:=(Counterfactual-`No Internal`)/Initial]
minwag[, total_effect:=(`Counterfactual`-Initial)/Initial]

minwag<-minwag[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "unemployed","covered_share","total_profit", "consumer_welfare", "totalwages", "totalwelfare")]
minwag<-minwag[,c("variable", "rellocation_effect", "reorg_effect", "total_effect")]
colnames(minwag)<-c("Statistic", "Reallocation","Reorganization", "Total")

minwag[, Reallocation:=paste0(formatC(Reallocation*100, digits = 2, format = "f"),"\\%")]
minwag[, Reorganization:=paste0(formatC(Reorganization*100, digits = 2, format = "f"),"\\%")]
minwag[, Total:=paste0(formatC(Total*100, digits = 2, format = "f"),"\\%")]
minwag[,Statistic:=c("Avg. Price",
                     "Avg. Complexity",
                     "Avg. Quality",
                     "Avg. Hourly Wage",
                     "Std. Dev. Wage",
                     "Task Specialization",
                     "Employment",
                     "Market Served",
                     "Total Profit",
                     "Consumer Welfare",
                     "Total Wages",
                     "Total Welfare"
)]


kable(minwag, "latex", align="c", booktabs=TRUE, escape = F) %>%
  cat(., file = "out/tables/03_02_minwage_effects.tex")


## wages and unemployment


jobs_model<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(ny_objects$bmats[[y]]/rowSums(ny_objects$bmats[[y]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),6),rowSums(ny_objects$bmats[[y]]),
               estim_sample[y,]$quarter_year, 1:6, estim_sample[y,]$location_id, ny_objects$gammas[y], estim_sample$s_index[y], ny_objects$C[y], estim_sample$salon_share_subdiv[y],estim_sample$cust_price[y])
  jobs_model<-rbind(jobs_model,piece)
}
jobs_model<-data.table(jobs_model)
names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                     "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price")
jobs_model[,emp_tot:=tot_firm*type_frac]
jobs_model<-jobs_model[type_frac>1e-08]
jobs_model[,Source:="Initial"]


jobs_counterfactual<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(minwage_objects[[trial_num]]$bmats[[y]]/rowSums(minwage_objects[[trial_num]]$bmats[[y]]),rep(as.numeric(minwage_objects[[trial_num]]$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),6),rowSums(minwage_objects[[trial_num]]$bmats[[y]]),
               estim_sample[y,]$quarter_year, 1:6, estim_sample[y,]$location_id, ny_objects$gammas[y],minwage_objects[[trial_num]]$mainstats$Imodel[y], minwage_objects[[trial_num]]$mainstats$C[y], minwage_objects[[trial_num]]$mainstats$new_share[y], minwage_objects[[trial_num]]$mainstats$newprice[y])
  jobs_counterfactual<-rbind(jobs_counterfactual,piece)
}
jobs_counterfactual<-data.table(jobs_counterfactual)
names(jobs_counterfactual)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                              "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price")
jobs_counterfactual[,emp_tot:=tot_firm*type_frac]
jobs_counterfactual<-jobs_counterfactual[type_frac>1e-08]
jobs_counterfactual[,Source:="Counterfactual"]

jobs_partial<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(minwage_noadj_objects[[trial_num_noadj]]$bmats[[y]]/rowSums(minwage_noadj_objects[[trial_num_noadj]]$bmats[[y]]),rep(as.numeric(minwage_noadj_objects[[trial_num_noadj]]$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),6),rowSums(minwage_noadj_objects[[trial_num_noadj]]$bmats[[y]]),
               estim_sample[y,]$quarter_year, 1:6, estim_sample[y,]$location_id, ny_objects$gammas[y],minwage_noadj_objects[[trial_num_noadj]]$mainstats$Imodel[y],minwage_noadj_objects[[trial_num_noadj]]$mainstats$C[y],minwage_noadj_objects[[trial_num_noadj]]$mainstats$new_share[y],minwage_noadj_objects[[trial_num_noadj]]$mainstats$newprice[y]  )
  jobs_partial<-rbind(jobs_partial,piece)
}
jobs_partial<-data.table(jobs_partial)
names(jobs_partial)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                              "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price" )
jobs_partial[,emp_tot:=tot_firm*type_frac]
jobs_partial<-jobs_partial[type_frac>1e-08]
jobs_partial[,Source:="No Internal"]

tokeep<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6", "emp_tot","tot_firm", "Source", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share", "price")

jobs_both<-rbind(jobs_counterfactual[,..tokeep], jobs_model[,..tokeep],jobs_partial[,..tokeep] )

jobs_both[type==1, focal_task:=jobvect_1]
jobs_both[type==2, focal_task:=jobvect_2]
jobs_both[type==3, focal_task:=jobvect_3]
jobs_both[type==4, focal_task:=jobvect_4]
jobs_both[type==5, focal_task:=jobvect_5]
jobs_both[type==6, focal_task:=jobvect_6]

positions <- c("Initial", "No Internal", "Counterfactual")
jobs_both[, Source:=factor(Source, levels=positions)]
ggplot(jobs_both[type==1], aes(weight = emp_tot/sum(emp_tot),linetype=Source, color=Source)) + geom_density(aes(x = 100*focal_task),size=1.2 )+
  labs(x="Task Specialization", y="Density")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("dashed","dotted", "solid"))
ggsave("out/figures/03_02_minwage_cutspec.png", width=9, heigh=6, units="in")

ggplot(jobs_both[type==2], aes(weight = emp_tot/sum(emp_tot),linetype=Source, color=Source)) + geom_density(aes(x = 100*focal_task),size=1.2 )+
  labs(x="Task Specialization", y="Density")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("dashed","dotted", "solid"))
ggsave("out/figures/03_02_minwage_colorspec.png", width=9, heigh=6, units="in")


ggplot(jobs_both[type!=2], aes(weight = emp_tot/sum(emp_tot),linetype=Source, color=Source)) + geom_density(aes(x = 100*focal_task),size=1.2 )+
  labs(x="Task Specialization", y="Density")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("dashed","dotted", "solid"))
ggsave("out/figures/03_02_minwage_binding.png", width=9, heigh=6, units="in")



### internal org.
org_model<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])
  piece<-data.frame( piece[2], diag(ny_objects$bmats[[estab]])[2]/piece[2], estim_sample$location_id[estab])
  org_model<-rbind(org_model, piece)
}
colnames(org_model)<-c("empshare_init", "task_spec_init", "location_id")

org_counterfactual<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(minwage_objects[[trial_num]]$bmats[[estab]])
  piece<-data.frame( piece[2], diag(minwage_objects[[trial_num]]$bmats[[estab]])[2]/piece[2], estim_sample$location_id[estab])
  org_counterfactual<-rbind(org_counterfactual, piece)
}
colnames(org_counterfactual)<-c("empshare", "task_spec", "location_id")

org_partial<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(minwage_noadj_objects[[trial_num_noadj]]$bmats[[estab]])
  piece<-data.frame( piece[2], diag(minwage_noadj_objects[[trial_num_noadj]]$bmats[[estab]])[2]/piece[2], estim_sample$location_id[estab])
  org_partial<-rbind(org_partial, piece)
}
colnames(org_partial)<-c("empshare", "task_spec", "location_id")
org_counterfactual[, Source:="Full"]
org_partial[, Source:=' Reallocation']
org_counterfactual<-rbind(org_counterfactual, org_partial)

org_both<-merge(org_counterfactual, org_model, by="location_id")
colnames(org_model)<-c("empshare", "task_spec", "location_id")
org_model[, Source:="Initial"]
org_both2<-rbind(org_model, org_counterfactual)

setorder(org_both2, "location_id", "Source")
org_both2[, has_some:=any(empshare>0.000001),by="location_id"]
ggplot(data=org_both2[has_some==TRUE & Source!="Initial"], aes(x=empshare, y=task_spec, color=Source))+geom_point(size=4)+
  geom_path(aes(x =empshare , y = task_spec, group = location_id), 
            arrow = arrow(length = unit(0.2, "cm"), type="closed"), size=0.8, color="black")+
  ylab("Task Specialization")+
  xlab("% of Workforce")+
  theme(legend.position="bottom")

ggsave("out/figures/03_02_minwage_subnon.png", width=9, heigh=6, units="in")


org_model<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])[-2]
  piece<-data.frame( sum(piece), sum(diag(ny_objects$bmats[[estab]])[-2])/sum(piece), estim_sample$location_id[estab])
  org_model<-rbind(org_model, piece)
}
colnames(org_model)<-c("empshare_init", "task_spec_init", "location_id")

org_counterfactual<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(minwage_objects[[trial_num]]$bmats[[estab]])[-2]
  piece<-data.frame( sum(piece), sum(diag(minwage_objects[[trial_num]]$bmats[[estab]])[-2])/sum(piece), estim_sample$location_id[estab])
  org_counterfactual<-rbind(org_counterfactual, piece)
}
colnames(org_counterfactual)<-c("empshare", "task_spec", "location_id")

org_partial<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(minwage_noadj_objects[[trial_num_noadj]]$bmats[[estab]])[-2]
  piece<-data.frame( sum(piece), sum(diag(minwage_noadj_objects[[trial_num_noadj]]$bmats[[estab]])[-2])/sum(piece), estim_sample$location_id[estab])
  org_partial<-rbind(org_partial, piece)
}
colnames(org_partial)<-c("empshare", "task_spec", "location_id")
org_counterfactual[, Source:="Full"]
org_partial[, Source:=' Reallocation'] # cheat with a space
org_counterfactual<-rbind(org_counterfactual, org_partial)
org_both<-merge(org_counterfactual, org_model, by="location_id")
colnames(org_model)<-c("empshare", "task_spec", "location_id")
org_model[, Source:="Initial"]
org_both2<-rbind(org_model, org_counterfactual)


org_both2[, has_some:=any(empshare<1),by="location_id"]
setorder(org_both2, "location_id", "Source")
ggplot(data=org_both2[has_some==TRUE & Source!="Initial"], aes(x=empshare, y=task_spec, color=Source))+geom_point(size=4)+
  geom_path(aes(x =empshare , y = task_spec, group = location_id), 
            arrow = arrow(length = unit(0.2, "cm"), type="closed"), size=0.8, color="black")+
  ylab("Task Specialization")+
  xlab("% of Workforce")+
theme(legend.position="bottom")

ggsave("out/figures/03_02_minwage_subbind.png", width=9, heigh=6, units="in")


### employment losses by firm type

firms<-unique(jobs_both[, c("location_id","Source", "gamma","tot_firm", "complexity", "mc", "share", "price" )])
firms[, profit:=estim_sample$CSPOP[1]*share/(1-share)/coef(res_store)[1]]
attachit<-firms[Source=="Initial"]
colnames(attachit)[-1]<-paste0(colnames(attachit)[-1], '_init')
firms<-merge(firms[Source!="Initial",], attachit, by="location_id")
firms[, emp_change:=(tot_firm-tot_firm_init)/tot_firm_init]
firms[, mc_change:=(mc-mc_init)/mc_init]
firms[, comp_change:=complexity-complexity_init ]
firms[, profit_change:=(profit-profit_init)/profit_init]
firms[, price_change:=(price-price_init)/price_init]
firms<-merge(firms, estim_sample[, c("task_mix2", "location_id")], all.x=TRUE, by="location_id")

ggplot(data=firms[Source=="No Internal"], aes(x=task_mix2, y=mc_change, color=rank(gamma)))+geom_point(size=8)+
  xlab("Firm Color/Highlight Task Intensity")+ylab("% Change in Marginal Cost")+ guides(color=guide_legend(title="Org. Cost Rank"))+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_mc.png", width=9, heigh=6, units="in")


ggplot(data=firms[Source=="No Internal"], aes(x=task_mix2, y=emp_change, color=rank(gamma)))+geom_point(size=8)+
  xlab("Firm Color/Highlight Task Intensity")+ylab("% Change in Employment")+ guides(color=guide_legend(title="Org. Cost Rank"))+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_emp.png", width=9, heigh=6, units="in")

ggplot(data=firms[Source=="No Internal"], aes(x=task_mix2, y=profit_change, color=rank(gamma)))+geom_point(size=8)+
  xlab("Firm Color/Highlight Task Intensity")+ylab("% Change in Profit")+ guides(color=guide_legend(title="Org. Cost Rank"))+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_profit.png", width=9, heigh=6, units="in")

ggplot(data=firms[Source=="No Internal"], aes(x=comp_change, y=emp_change, color=task_mix2))+geom_point(size=8)+
  xlab("Change in Complexity")+ylab("Change in Employment")+ guides(color=guide_legend(title="Firm Color/Highlight Task Intensity"))+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_complexity_emps.png", width=9, heigh=6, units="in")

### wages and unemployment by type
emp_wage<-jobs_both[, .(employment=sum(emp_tot)*estim_sample$weight[1]), by=c("type", "Source")]
setorder(emp_wage, "Source", "type" )
emp_wage<-dcast(emp_wage, type~Source, value.var="employment")
wage_part<-cbind(wages,as.numeric(minwage_noadj_res[trial_num_noadj,c("w1", "w2", "w3", "w4", "w5", "w6")]),as.numeric(minwage_res[trial_num,c("w1", "w2", "w3", "w4", "w5", "w6")]))
wage_part<-data.table(wage_part)
colnames(wage_part)<-c("Initial","No Internal", "Counterfactual" )
wage_part[, type:=1:6]
emp_wage<-merge(emp_wage, wage_part, by=c("type"))
setcolorder(emp_wage, c("Initial.x", "Initial.y", "No Internal.x", "No Internal.y", "Counterfactual.x", "Counterfactual.y"))
setcolorder(emp_wage, "type")
cols<-colnames(emp_wage)[-1]
emp_wage[ , (cols) := lapply(.SD, sigfig, n=5), .SDcols = cols]
colnames(emp_wage)<-c("Worker Type", rep(c("Employment", "Wage"),3))

kable(emp_wage, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Minimum Wage Counterfactual Wages and Employment', label="min_emp_wage") %>%
  add_header_above(., c(" ", "Initial" = 2, "No Internal" = 2, "Counterfactual" = 2)) %>%
  cat(., file = "out/tables/03_02_wage_emp_min.tex")
### chunks for later


temphold2<-data.table(prodrank=rep(rank(ny_objects$gammas*estim_sample$avg_labor)/length(ny_objects$gammas*estim_sample$avg_labor),3),grank=rep(rank(ny_objects$gammas)/length(ny_objects$gammas),3),c=c(minwage_noadj_objects[[trial_num_noadj]]$mainstats$C,minwage_objects[[trial_num]]$mainstats$C,minwage_noadj_objects$initial$C),type=c(rep(c("No Internal"),length(ny_objects$gammas)),rep(c("Full"),length(ny_objects$gammas)),rep(c("Start"),length(ny_objects$gammas))), size=c(minwage_noadj_objects[[trial_num_noadj]]$mainstats$new_share,minwage_objects[[trial_num]]$mainstats$new_share,minwage_noadj_objects[[trial_num_noadj]]$mainstats$salon_share_subdiv),I=c(minwage_noadj_objects[[trial_num_noadj]]$mainstats$Imodel,minwage_objects[[trial_num]]$mainstats$Imodel,minwage_noadj_objects[[trial_num_noadj]]$mainstats$s_index), CQ=c(minwage_noadj_objects[[trial_num_noadj]]$mainstats$C-minwage_noadj_objects[[trial_num_noadj]]$mainstats$Q/coef(res_store)[1],minwage_objects[[trial_num]]$mainstats$C-minwage_objects[[trial_num]]$mainstats$Q/coef(res_store)[1],minwage_noadj_objects$initial$C-minwage_objects$initial$Q/coef(res_store)[1]),price=c(minwage_noadj_objects[[trial_num_noadj]]$mainstats$newprice,minwage_objects[[trial_num]]$mainstats$newprice,minwage_noadj_objects[[trial_num_noadj]]$mainstats$cust_price))
temphold2[,labor:=estim_sample$CSPOP*rep(estim_sample$avg_labor,3)*size*estim_sample$weight]
temphold2[,labor_increase:=estim_sample$CSPOP*rep(estim_sample$avg_labor,3)*(size-estim_sample$salon_share_subdiv)]
ggplot(data=temphold2[type!="Start"], aes(x=labor_increase, y=prodrank, color=type))+geom_point(size=3)

forgraph<-cbind(temphold2[type=="Full"], temphold2[type=="Start"],temphold2[type=="No Internal"] )
colnames(forgraph)<-c(colnames(temphold2), paste0(colnames(temphold2),"_v2"),paste0(colnames(temphold2),"_v3"))
sum(forgraph$labor)->scale1
sum(forgraph$labor_v2)->scale2
sum(forgraph$labor_v3)->scale3
ggplot(forgraph)+   geom_density(aes(x = grank_v2*100, y = ..density.. *scale2, weight = labor_v2),linetype ="dotted", color="red",alpha=0.2, position="identity", size=1)+
  geom_density(aes(x = grank*100, y = ..density.. *scale1, weight = labor),linetype ="solid", color="blue",alpha=0.2, position="identity", size=1)


ggplot(temphold2[type!="No Internal"],aes(x = grank*100,y = ..density.., weight = labor, linetype =type, fill=type, color= type) ) + geom_histogram(alpha=0.2, position="identity", size=1)+
  scale_linetype_manual(values=c("solid","dashed", "dotted"))

