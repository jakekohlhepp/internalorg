## make tables and graphs about counterfactuals
library('data.table')
library('gmm')
library('spatstat')
library('stringr')
library(knitr)
library(kableExtra)
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
innertol<-1e-08
outertol<-1e-08

if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-32
} 

source('more_gmm_objective.R')

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


rank_gamma<-rank(ny_objects$gammas, ties="random")
sorted_gamma<-sort(ny_objects$gammas)

leaps<-15
gamma15<-sorted_gamma[pmax(rank_gamma-leaps,1)]
gamma15[which(ny_objects$gammas>2000)]<-ny_objects$gammas[which(ny_objects$gammas>2000)]

leaps<-16
gamma16<-sorted_gamma[pmax(rank_gamma-leaps,1)]
gamma16[which(ny_objects$gammas>2000)]<-ny_objects$gammas[which(ny_objects$gammas>2000)]

leaps<-17
gamma17<-sorted_gamma[pmax(rank_gamma-leaps,1)]
gamma17[which(ny_objects$gammas>2000)]<-ny_objects$gammas[which(ny_objects$gammas>2000)]



##########################################################################
##########################################################################
##########################################################################


counter_res[str_detect(context, "Gone")==0, totalwages:=sum(labor_supply)*mean_wage]
mod_ls<-labor_supply
mod_ls<-mod_ls[4]*0.5
counter_res[context=="Blowdry Gone", totalwages:=sum(mod_ls)*mean_wage]
mod_ls<-labor_supply
mod_ls<-mod_ls[5]*0.5
counter_res[context=="Admin. Gone", totalwages:=sum(mod_ls)*mean_wage]

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

jobs_model<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-cbind(ny_objects$bmats[[y]]/rowSums(ny_objects$bmats[[y]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),6),rowSums(ny_objects$bmats[[y]]),
               estim_sample[y,]$quarter_year)
  jobs_model<-rbind(jobs_model,piece)
}
jobs_model<-data.table(jobs_model)
names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                     "tot_firm", "type_frac", "quarter_year")
jobs_model[,emp_tot:=tot_firm*type_frac]
jobs_model<-jobs_model[type_frac>1e-08]
jobs_model[,Source:="Initial"]


jobs_counterfactual<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-cbind(counter_objects$`Improvement in Management Software 2`$bmats[[y]]/rowSums(counter_objects$`Improvement in Management Software 2`$bmats[[y]]),rep(as.numeric(counter_objects$`Improvement in Management Software 2`$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),6),rowSums(counter_objects$`Improvement in Management Software 2`$bmats[[y]]),
               estim_sample[y,]$quarter_year)
  jobs_counterfactual<-rbind(jobs_counterfactual,piece)
}
jobs_counterfactual<-data.table(jobs_counterfactual)
names(jobs_counterfactual)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                              "tot_firm", "type_frac", "quarter_year")
jobs_counterfactual[,emp_tot:=tot_firm*type_frac]
jobs_counterfactual<-jobs_counterfactual[type_frac>1e-08]
jobs_counterfactual[,Source:="Counterfactual"]

tokeep<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6", "emp_tot", "Source", "quarter_year")

jobs_both<-rbind(jobs_counterfactual[,..tokeep], jobs_model[,..tokeep])

ggplot(jobs_both,aes(x = 100*jobvect_1, w = emp_tot, fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Hair/Shave Task", y="Density")+
  theme(legend.position="bottom")
ggsave("out/figures/03_02_software_jobdist_1.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_2, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Color/Highlight/Wash Task", y="Density")+
  theme(legend.position="bottom")
ggsave("out/figures/03_02_software_jobdist_2.png", width=12, heigh=6, units="in")


ggplot(jobs_both,aes(x = 100*jobvect_3, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Extension Task", y="Density")+coord_cartesian(xlim=c(0,10))+
  theme(legend.position="bottom")
ggsave("out/figures/03_02_software_jobdist_3.png", width=12, heigh=6, units="in")



ggplot(jobs_both,aes(x = 100*jobvect_4, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Blowdry/Style/Treatment Task", y="Density")+
  theme(legend.position="bottom")
ggsave("out/figures/03_02_software_jobdist_4.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_5, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Administrative Task", y="Density")+coord_cartesian(xlim=c(0,10))+
  theme(legend.position="bottom")
ggsave("out/figures/03_02_software_jobdist_5.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_6, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Nail/Spa/Eye/Misc. Task", y="Density")+coord_cartesian(xlim=c(0,20))+
  theme(legend.position="bottom")
ggsave("out/figures/03_02_software_jobdist_6.png", width=12, heigh=6, units="in")



#### Counterfactual 3: Diffusion of Management Practices
# sales tax table
setcolorder(counter_res, c("context","avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages", "totalwelfare"))
diffusion<-melt(counter_res[context %in% c("Initial", "Mimic 1", "Mimic 20", "Mimic 40"),],
                measure.vars=names(counter_res)[-1])
diffusion<-dcast(diffusion, variable~ context, value.var="value")
diffusion[, pct_chnge:=(`Mimic 40`-Initial)/Initial]

diffusion<-diffusion[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages", "totalwelfare")]

#sales_tax[, Initial:=formatC(Initial, digits = 3, format = "f")]
#sales_tax[, `No Internal`:=formatC(`No Internal`, digits = 3, format = "f")]
#sales_tax[, Counterfactual:=formatC(Counterfactual, digits = 3, format = "f")]
diffusion[, Initial:=sigfig(Initial, 5)]
diffusion[, `Mimic 1`:=sigfig(`Mimic 1`, 5)]
diffusion[, `Mimic 20`:=sigfig(`Mimic 20`, 5)]
diffusion[, `Mimic 40`:=sigfig(`Mimic 40`, 5)]

colnames(diffusion)<-c("Statistic", "Initial", "1 Step", "Halfway", "Full", "Change")


diffusion[, Change:=paste0(formatC(Change*100, digits = 2, format = "f"),"\\%")]
diffusion[,Statistic:=c("Avg. Price",
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


kable(diffusion, "latex", align="c", booktabs=TRUE, escape = F) %>%
  cat(., file = "out/tables/03_02_diffusion_ny.tex")

ggplot(data=counter_res[4:43,], aes(x=1:40))+
  geom_line(aes(y=consumer_welfare/consumer_welfare[1]))+
  geom_line(aes(y=total_profit/total_profit[1]))+
  geom_line(aes(y=totalwages/totalwages[1]))




jobs_model<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-cbind(ny_objects$bmats[[y]]/rowSums(ny_objects$bmats[[y]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),6),rowSums(ny_objects$bmats[[y]]),
               estim_sample[y,]$quarter_year)
  jobs_model<-rbind(jobs_model,piece)
}
jobs_model<-data.table(jobs_model)
names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                     "tot_firm", "type_frac", "quarter_year")
jobs_model[,emp_tot:=tot_firm*type_frac]
jobs_model<-jobs_model[type_frac>1e-08]
jobs_model[,Source:="Initial"]


jobs_counterfactual<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-cbind(counter_objects$`Mimic 40`$bmats[[y]]/rowSums(counter_objects$`Mimic 40`$bmats[[y]]),rep(as.numeric(counter_objects$`Mimic 40`$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),6),rowSums(counter_objects$`Mimic 40`$bmats[[y]]),
               estim_sample[y,]$quarter_year)
  jobs_counterfactual<-rbind(jobs_counterfactual,piece)
}
jobs_counterfactual<-data.table(jobs_counterfactual)
names(jobs_counterfactual)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                     "tot_firm", "type_frac", "quarter_year")
jobs_counterfactual[,emp_tot:=tot_firm*type_frac]
jobs_counterfactual<-jobs_counterfactual[type_frac>1e-08]
jobs_counterfactual[,Source:="Counterfactual"]

tokeep<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6", "emp_tot", "Source", "quarter_year")

jobs_both<-rbind(jobs_counterfactual[,..tokeep], jobs_model[,..tokeep])

ggplot(jobs_both,aes(x = 100*jobvect_1, w = emp_tot, fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Hair/Shave Task", y="Density")+
  theme(legend.position="none")
ggsave("out/figures/03_02_diffusion_jobdist_1.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_2, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Color/Highlight/Wash Task", y="Density")+
  theme(legend.position="none")
ggsave("out/figures/03_02_diffusion_jobdist_2.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_3, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Extension Task", y="Density")+coord_cartesian(xlim=c(0,10))+
  theme(legend.position="none")
ggsave("out/figures/03_02_diffusion_jobdist_3.png", width=12, heigh=6, units="in")


ggplot(jobs_both,aes(x = 100*jobvect_4, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Blowdry/Style/Treatment Task", y="Density")+
  theme(legend.position="none")
ggsave("out/figures/03_02_diffusion_jobdist_4.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_5, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Administrative Task", y="Density")+coord_cartesian(xlim=c(0,10))+
  theme(legend.position="none")
ggsave("out/figures/03_02_diffusion_jobdist_5.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_6, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Nail/Spa/Eye/Misc. Task", y="Density")+coord_cartesian(xlim=c(0,20))+
  theme(legend.position="none")
ggsave("out/figures/03_02_diffusion_jobdist_6.png", width=12, heigh=6, units="in")

#### Counterfactual 4: 50 percent Reduction in blowdry Specialists
setcolorder(counter_res, c("context","avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages", "totalwelfare"))
blowdry<-melt(counter_res[context %in% c("Initial", "Blowdry Gone"),],
                measure.vars=names(counter_res)[-1])
blowdry<-dcast(blowdry, variable~ context, value.var="value")
blowdry[, pct_chnge:=(`Blowdry Gone`-Initial)/Initial]

blowdry<-blowdry[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare")]

blowdry[, Initial:=sigfig(Initial, 5)]
blowdry[, `Blowdry Gone`:=sigfig(`Blowdry Gone`, 5)]
setcolorder(blowdry, c(1,3,2,4))
colnames(blowdry)<-c("Statistic", "Initial", "Counterfactual", "Change")


blowdry[, Change:=paste0(formatC(Change*100, digits = 2, format = "f"),"\\%")]
blowdry[,Statistic:=c("Avg. Price",
                        "Avg. Complexity",
                        "Avg. Quality",
                        "Avg. Hourly Wage",
                        "Std. Dev. Wage",
                        "Task Specialization",
                        "Total Profit",
                        "Consumer Welfare"
)]


kable(blowdry, "latex", align="c", booktabs=TRUE, escape = F) %>%
  cat(., file = "out/tables/03_02_blowdrygone_ny.tex")


blowdrygraph<-data.table(C=c(ny_objects$C,counter_objects$`Admin. Gone`$mainstats$C),
                       Q=c(ny_objects$Q,counter_objects$`Admin. Gone`$mainstats$Q),
                       gammas=rep(ny_objects$gammas,2), 
                       Equilibrium=c(rep("Initial", length(ny_objects$gammas)),rep("Counterfactual", length(ny_objects$gammas))))

ggplot() +
  geom_point(data=blowdrygraph,aes(x=rank(gammas), y=C, shape=Equilibrium, color=Equilibrium), size=5)+ 
  ylab("Marginal Cost") + xlab("Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/03_02_cost_gamma_ny.png", width=12, heigh=6, units="in")

ggplot() +
  geom_point(data=blowdrygraph,aes(x=rank(gammas), y=Q, shape=Equilibrium, color=Equilibrium), size=2)+ 
  ylab("Quality") + xlab("Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_cost_gamma_ny.png", width=12, heigh=6, units="in")





jobs_model<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-cbind(ny_objects$bmats[[y]]/rowSums(ny_objects$bmats[[y]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),6),rowSums(ny_objects$bmats[[y]]),
               estim_sample[y,]$quarter_year)
  jobs_model<-rbind(jobs_model,piece)
}
jobs_model<-data.table(jobs_model)
names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                     "tot_firm", "type_frac", "quarter_year")
jobs_model[,emp_tot:=tot_firm*type_frac]
jobs_model<-jobs_model[type_frac>1e-08]
jobs_model[,Source:="Initial"]


jobs_counterfactual<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-cbind(counter_objects$`Admin. Gone`$bmats[[y]]/rowSums(counter_objects$`Admin. Gone`$bmats[[y]]),rep(as.numeric(counter_objects$`Admin. Gone`$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),6),rowSums(counter_objects$`Admin. Gone`$bmats[[y]]),
               estim_sample[y,]$quarter_year)
  jobs_counterfactual<-rbind(jobs_counterfactual,piece)
}
jobs_counterfactual<-data.table(jobs_counterfactual)
names(jobs_counterfactual)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                              "tot_firm", "type_frac", "quarter_year")
jobs_counterfactual[,emp_tot:=tot_firm*type_frac]
jobs_counterfactual<-jobs_counterfactual[type_frac>1e-08]
jobs_counterfactual[,Source:="Counterfactual"]

tokeep<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6", "emp_tot", "Source", "quarter_year")

jobs_both<-rbind(jobs_counterfactual[,..tokeep], jobs_model[,..tokeep])

ggplot(jobs_both,aes(x = 100*jobvect_1, w = emp_tot, fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Hair/Shave Task", y="Density")+
  theme(legend.position="none")
ggsave("out/figures/03_02_admingone_jobdist_1.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_2, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Color/Highlight/Wash Task", y="Density")+
  theme(legend.position="none")
ggsave("out/figures/03_02_admingone_jobdist_2.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_3, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Extension Task", y="Density")+coord_cartesian(xlim=c(0,10))+
  theme(legend.position="none")
ggsave("out/figures/03_02_admingone_jobdist_3.png", width=12, heigh=6, units="in")


ggplot(jobs_both,aes(x = 100*jobvect_4, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Blowdry/Style/Treatment Task", y="Density")+
  theme(legend.position="none")
ggsave("out/figures/03_02_admingone_jobdist_4.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_5, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Administrative Task", y="Density")+coord_cartesian(xlim=c(0,10))+
  theme(legend.position="none")
ggsave("out/figures/03_02_admingone_jobdist_5.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_6, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Nail/Spa/Eye/Misc. Task", y="Density")+coord_cartesian(xlim=c(0,20))+
  theme(legend.position="none")
ggsave("out/figures/03_02_admingone_jobdist_6.png", width=12, heigh=6, units="in")





