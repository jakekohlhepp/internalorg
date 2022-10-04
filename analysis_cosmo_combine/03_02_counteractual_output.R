## make tables and graphs about counterfactuals
library('data.table')
library('gmm')
library('spatstat')
library('stringr')
library('knitr')
library('ggplot2')
library('kableExtra')
library('formattable')
library('scales')

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

load('data/final_estimates.RData')

if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-32
} 

source('small_gmm_objective_less.R')

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
tot_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5)) , by=V6 ]
setnames(tot_labor, "V6", "quarter_year")
labor_supply<-as.numeric(tot_labor[1,-1])
wages<-wage_bound(coef(res_store)[4:8])


##########################################################################
##########################################################################
##########################################################################


counter_res[str_detect(context, "Wage")==0, totalwages:=sum(labor_supply)*mean_wage]
counter_res[, tax_rev:=estim_sample$weight[1]*estim_sample$CSPOP[1]*c(sum(estim_sample$cust_price*estim_sample$salon_share_subdiv)*0.045,
                                                                      0,0,sum(counter_objects$`Improvement in Management Software`$mainstats$new_share*counter_objects$`Improvement in Management Software`$mainstats$newprice)*0.045)]
counter_res[, totalwelfare:=totalwages+total_profit+consumer_welfare+tax_rev]
counter_res[, avgprice:=c(mean(estim_sample$cust_price),sapply(2:length(counter_objects), function(x) {mean(counter_objects[[x]]$mainstats$newprice)}))]
counter_res[, sdC:=c(sd(ny_objects$C),sapply(2:length(counter_objects), function(x) {sd(counter_objects[[x]]$mainstats$C)}))]
counter_res[, sdQ:=c(sd(ny_objects$Q),sapply(2:length(counter_objects), function(x) {sd(counter_objects[[x]]$mainstats$Q)}))]
counter_res[, sdQC:=c(sd(ny_objects$C-(exp(coef(res_store)[1])*1.045)^(-1)*ny_objects$Q),sapply(2:length(counter_objects), function(x) {sd(counter_objects[[x]]$mainstats$C -(exp(coef(res_store)[1])*1.045)^(-1)*counter_objects[[x]]$mainstats$Q )}))]
counter_res[, covered_share:=c(sum(estim_sample$salon_share_subdiv*estim_sample$weight),sapply(2:length(counter_objects), function(x) {sum(counter_objects[[x]]$mainstats$new_share *estim_sample$weight )}))]

# sales tax table
setcolorder(counter_res, c("context","avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages","tax_rev", "totalwelfare"))

sales_tax<-melt(counter_res[context %in% c("Initial","Sales Tax Eliminated  - No Adj.", "Sales Tax Eliminated"),],
                measure.vars=names(counter_res)[-1])
sales_tax<-dcast(sales_tax, variable~ context, value.var="value")
setcolorder(sales_tax, c(1,2,4,3))
sales_tax[, pct_chnge:=(`Sales Tax Eliminated`-Initial)/Initial]

sales_tax<-sales_tax[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages","tax_rev", "totalwelfare")]
                    
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
                        "Tax Revenue",
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

welfare<-copy(sales_tax[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "total_profit", "consumer_welfare", "totalwages","tax_rev", "totalwelfare")])

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

kable(sales_tax[c(1,2,3,6),c(1,4)], "latex", align="c", booktabs=TRUE, escape = F) %>%
  cat(., file = "out/tables/03_02_salestax_effects_forpres.tex")

welfare<-welfare[variable %in% c("total_profit","consumer_welfare","totalwages","tax_rev","totalwelfare")]
welfare[,change:= (`Sales Tax Eliminated`-Initial) ]
welfare[,dollarchange:= (`Sales Tax Eliminated`-Initial) ]
hold<-abs(as.numeric(welfare[variable=="totalwelfare","Initial"][1]))

welfare[, change:=change/hold]
welfare[, dollarchange:=dollar(dollarchange, prefix="\\$")]
welfare[, change:=percent(change, suffix="\\%")]
welfare<-welfare[,c("variable","dollarchange","change") ]
colnames(welfare)<-c("Source", "Change", "Percent Change")
welfare[ Source=="total_profit", Source:="Salon Profit"]
welfare[ Source=="consumer_welfare", Source:="Consumer Welfare"]
welfare[ Source=="totalwages", Source:="Wages"]
welfare[ Source=="tax_rev", Source:="Tax Revenue"]
welfare[ Source=="totalwelfare", Source:="Total Welfare"]

kable(welfare, "latex", align="c", booktabs=TRUE, escape = F) %>%
  row_spec(3, hline_after=T) %>%
  cat(., file = "out/tables/03_02_salestax_welfare.tex")



jobs_model<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(ny_objects$bmats[[y]]/rowSums(ny_objects$bmats[[y]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),5),rowSums(ny_objects$bmats[[y]]),
                    estim_sample[y,]$quarter_year, 1:5, estim_sample[y,]$location_id, ny_objects$gammas[y], estim_sample$s_index[y], ny_objects$C[y], estim_sample$salon_share_subdiv[y],estim_sample$cust_price[y],ny_objects$Q[y])
  jobs_model<-rbind(jobs_model,piece)
}
jobs_model<-data.table(jobs_model)
names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5",
                     "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price", "quality")
jobs_model[,emp_tot:=tot_firm*type_frac]
jobs_model<-jobs_model[type_frac>1e-08]
jobs_model[,Source:="Initial"]


jobs_counterfactual<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(counter_objects$`Sales Tax Eliminated`$bmats[[y]]/rowSums(counter_objects$`Sales Tax Eliminated`$bmats[[y]]),rep(as.numeric(counter_objects$`Sales Tax Eliminated`$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),5),rowSums(counter_objects$`Sales Tax Eliminated`$bmats[[y]]),
                    estim_sample[y,]$quarter_year, 1:5, estim_sample[y,]$location_id, ny_objects$gammas[y],counter_objects$`Sales Tax Eliminated`$mainstats$Imodel[y], counter_objects$`Sales Tax Eliminated`$mainstats$C[y], counter_objects$`Sales Tax Eliminated`$mainstats$new_share[y], counter_objects$`Sales Tax Eliminated`$mainstats$newprice[y], counter_objects$`Sales Tax Eliminated`$mainstats$Q[y])
  jobs_counterfactual<-rbind(jobs_counterfactual,piece)
}
jobs_counterfactual<-data.table(jobs_counterfactual)
names(jobs_counterfactual)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5",
                              "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price", "quality")
jobs_counterfactual[,emp_tot:=tot_firm*type_frac]
jobs_counterfactual<-jobs_counterfactual[type_frac>1e-08]
jobs_counterfactual[,Source:="Reorganization"]

jobs_partial<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(counter_objects$`Sales Tax Eliminated - No Adj.`$bmats[[y]]/rowSums(counter_objects$`Sales Tax Eliminated - No Adj.`$bmats[[y]]),rep(as.numeric(counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),5),rowSums(counter_objects$`Sales Tax Eliminated - No Adj.`$bmats[[y]]),
                    estim_sample[y,]$quarter_year, 1:5, estim_sample[y,]$location_id, ny_objects$gammas[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$Imodel[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$C[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$new_share[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$newprice[y],counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$Q[y]   )
  jobs_partial<-rbind(jobs_partial,piece)
}
jobs_partial<-data.table(jobs_partial)
names(jobs_partial)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5",
                       "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price" , "quality")
jobs_partial[,emp_tot:=tot_firm*type_frac]
jobs_partial<-jobs_partial[type_frac>1e-08]
jobs_partial[,Source:="Reallocation"]

tokeep<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "emp_tot","tot_firm", "Source", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share", "price", "quality")

jobs_both<-rbind(jobs_counterfactual[,..tokeep], jobs_model[,..tokeep],jobs_partial[,..tokeep] )

jobs_both[type==1, focal_task:=jobvect_1]
jobs_both[type==2, focal_task:=jobvect_2]
jobs_both[type==3, focal_task:=jobvect_3]
jobs_both[type==4, focal_task:=jobvect_4]
jobs_both[type==5, focal_task:=jobvect_5]

positions <- c("Initial", "Reallocation", "Reorganization")
jobs_both[, Source:=factor(Source, levels=positions)]
ggplot(jobs_both, aes(weight = emp_tot/sum(emp_tot),linetype=Source, color=Source)) + geom_density(aes(x = 100*focal_task),size=1.2 )+
  labs(x="Task Specialization", y="Density")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("solid","dotted", "dashed"))
ggsave("out/figures/03_02_sales_total.png", width=12, heigh=8, units="in")


colA = paste("jobvect", 1:5, sep = "_")

keytask<-data.table(task=1:5,Task=c("Cut","Color","Blowdry", "Admin.", "Misc."))
keyworker<-data.table(type=1:5, `Worker Type`=c("Cut","Color","Blowdry", "Admin.", "Misc."))


fortable<-melt(jobs_both, id.vars=c("location_id", "emp_tot", "type", "Source"), measure.vars=list(colA), value.name=c("jobvect"))
fortable[,task:=as.numeric((gsub("jobvect_", "", variable))) ]
fortable[, hours:=jobvect*emp_tot]
fortable<-merge(fortable,keytask, by="task" )
fortable<-merge(fortable,keyworker, by="type" )
fortable<-fortable[task==type, ]

ggplot(fortable[Source!="No Internal"], aes(x = `Worker Type`, fill = Source, y = hours))+
  stat_summary(geom = "bar", position = "dodge", fun.y = sum)+
  labs(y="Hours on Specialty Task")+
  theme(legend.position="bottom")
ggsave("out/figures/03_02_change_specialization.png", width=12, heigh=8, units="in")







### internal org.
org_model<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])
  piece<-data.frame( ny_objects$Q[estab]/exp(coef(res_store)[1]), sum(diag(ny_objects$bmats[[estab]])), estim_sample$location_id[estab])
  org_model<-rbind(org_model, piece)
}
colnames(org_model)<-c("quality", "task_spec", "location_id")

org_counterfactual<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(counter_objects$`Sales Tax Eliminated`$bmats[[estab]])
  piece<-data.frame( counter_objects$`Sales Tax Eliminated`$mainstats$Q[estab]/exp(coef(res_store)[1]), sum(diag(counter_objects$`Sales Tax Eliminated`$bmats[[estab]])), estim_sample$location_id[estab])
  org_counterfactual<-rbind(org_counterfactual, piece)
}
colnames(org_counterfactual)<-c("quality", "task_spec", "location_id")

org_partial<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(counter_objects$`Sales Tax Eliminated`$bmats[[estab]])
  piece<-data.frame( counter_objects$`Sales Tax Eliminated - No Adj.`$mainstats$Q[estab]/exp(coef(res_store)[1]), sum(diag(counter_objects$`Sales Tax Eliminated - No Adj.`$bmats[[estab]])), estim_sample$location_id[estab])
  org_partial<-rbind(org_partial, piece)
}
colnames(org_partial)<-c("quality", "task_spec", "location_id")


org_both<-merge(org_counterfactual, org_model, by="location_id")
org_counterfactual[, Source:="Reorganization"]
org_model[, Source:="Initial"]
org_partial[, Source:="Reallocation"]
org_both2<-rbind(org_model, org_counterfactual, org_partial)

setorder(org_both2, "location_id", "Source")
ggplot(data=org_both2[Source!="Initial"], aes(x=quality, y=task_spec, color=Source))+geom_point(size=4)+
  geom_path(aes(x =quality , y = task_spec, group = location_id), 
            arrow = arrow(length = unit(0.2, "cm"), type="closed"), size=0.8, color="black")+
  ylab("Task Specialization")+
  xlab("Service Quality")+
  theme(legend.position="bottom")

ggsave("out/figures/03_02_salestax_reorg.png", width=12, heigh=8, units="in")




ggplot(firms[Source!="Reorganization"], aes(weight = tot_firm,linetype=Source, color=Source)) + geom_density(aes(x = rank(gamma)),size=1.2 )+
  labs(x="Organization Cost Rank", y="Distribution of Labor")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("solid","dashed"))
ggsave("out/figures/03_02_sales_reallocate.png", width=12, heigh=8, units="in")


emp_wage<-jobs_both[, .(employment=sum(emp_tot)*estim_sample$weight[1], task_spec=sum(emp_tot*focal_task)/sum(emp_tot)), by=c("type", "Source")]
setorder(emp_wage, "Source", "type" )
emp_wage<-dcast(emp_wage, type~Source, value.var=c("employment","task_spec"))
wage_part<-cbind(wages,as.numeric(counter_res[context=="Sales Tax Eliminated  - No Adj.",c("w1", "w2", "w3", "w4", "w5")]),as.numeric(counter_res[context=="Sales Tax Eliminated",c("w1", "w2", "w3", "w4", "w5")]))
wage_part<-data.table(wage_part)
colnames(wage_part)<-c("Initial","Reallocation", "Reorganization" )
wage_part[, type:=1:5]
emp_wage<-merge(emp_wage, wage_part, by=c("type"))
setcolorder(emp_wage, c("type",
                        "employment_Initial","Initial","task_spec_Initial", 
                        "employment_Reallocation","Reallocation", "task_spec_Reallocation",
                        "employment_Reorganization","Reorganization", "task_spec_Reorganization"))
emp_wage<-merge(emp_wage, readRDS('data/00_00_keytask.rds')[, c("rep_text_cluster", "type")], by="type")
emp_wage[, type:=rep_text_cluster]
emp_wage[, rep_text_cluster:=NULL]

loss_gain_full<-copy(emp_wage)
loss_gain_reorg<-copy(emp_wage)
loss_gain_reloc<-copy(emp_wage)

cols<-c("Initial", "Reallocation", "Reorganization")
emp_wage[ , (cols) := lapply(.SD, dollar, prefix="\\$"), .SDcols = cols]
cols<-c("employment_Initial", "employment_Reallocation", "employment_Reorganization")
emp_wage[ , (cols) := lapply(.SD, round), .SDcols = cols]
cols<-c("task_spec_Initial", "task_spec_Reallocation", "task_spec_Reorganization")
emp_wage[ , (cols) := lapply(.SD, sigfig, n=4), .SDcols = cols]
colnames(emp_wage)<-c("Worker Type", rep(c("Hours", "Wage", "Task-Spec."),3))

kable(emp_wage, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F) %>%
  add_header_above(., c(" ", "Initial" = 3, "Reallocation" = 3, "Reorganization" = 3)) %>%
  cat(., file = "out/tables/03_02_wage_emp_salestax.tex")

kable(emp_wage[,-c(4,5,6,7,10)], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F) %>%
  add_header_above(., c(" ", "Initial" = 2, "Counterfactual" = 2)) %>%
  cat(., file = "out/tables/03_02_wage_emp_salestax_forpres.tex")


### wage losses and specialization changes by type.
loss_gain_full[, wage_change:=(Reorganization -Initial)/Initial]
loss_gain_full[, spec_change:=(task_spec_Reorganization   -task_spec_Initial )/task_spec_Initial ]
loss_gain_full[, c("type","wage_change", "spec_change")]
loss_gain_full[, wage_change:=paste0(formatC(wage_change*100, digits = 2, format = "f"),"\\%")]
loss_gain_full[, spec_change:=paste0(formatC(spec_change*100, digits = 2, format = "f"),"\\%")]
loss_gain_full<-loss_gain_full[, c("type","wage_change", "spec_change")]
colnames(loss_gain_full)<-c("Type", "Wage Change", "Task-Spec. Change")
kable(loss_gain_full, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F) %>%
  cat(., file = "out/tables/03_02_salestax_wagechanges.tex")







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
finalminwage[, context:=c("Initial", "No Internal", "Counterfactual")]
finalminwage[, totalwages:=(sum(labor_supply)-unemployed)*mean_wage ]


#### new stuff here
finalminwage[, employedwages:= (labor_supply[1]-max(unemployed))*w1+(labor_supply[2])*w2+
                                (labor_supply[3])*w3+(labor_supply[4])*w4+
                                (labor_supply[5])*w5]

finalminwage[, unemployedwages:= (max(unemployed)-unemployed)*w1]

#####


finalminwage[, totalwelfare:=totalwages+total_profit+consumer_welfare]
finalminwage[, unemployed:=unemployed/sum(labor_supply)]
finalminwage[, avgprice:=c(weighted.mean(estim_sample$cust_price,estim_sample$salon_share_subdiv),weighted.mean(minwage_noadj_objects[[trial_num_noadj]]$mainstats$newprice,w=minwage_noadj_objects[[trial_num_noadj]]$mainstats$new_share),weighted.mean(minwage_objects[[trial_num]]$mainstats$newprice,minwage_objects[[trial_num]]$mainstats$new_share) )]
finalminwage[, covered_share:=c(sum(estim_sample$salon_share_subdiv*estim_sample$weight),sum(minwage_noadj_objects[[trial_num_noadj]]$mainstats$new_share*estim_sample$weight ),sum(minwage_objects[[trial_num]]$mainstats$new_share*estim_sample$weight )) ]
setcolorder(finalminwage, c("context","avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "unemployed","covered_share", "total_profit", "consumer_welfare", "totalwages", "totalwelfare","employedwages", "unemployedwages"))


minwag<-melt(finalminwage,
               measure.vars=names(finalminwage)[-1])
minwag<-dcast(minwag, variable~ context, value.var="value")
setcolorder(minwag, c(1,3,4,2))
minwag<-minwag[variable %in% c("avgprice","avg_complexity","quality","mean_wage", "sd_wage", "task_spec", "unemployed","covered_share","total_profit", "consumer_welfare", "totalwages", "totalwelfare","employedwages", "unemployedwages")]
welfare<-copy(minwag)
minwag<-minwag[!(variable %in% c("employedwages", "unemployedwages")),]
minwag[, pct_chnge:=(`Counterfactual`-Initial)/Initial]
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

welfare<-welfare[variable %in% c("total_profit","consumer_welfare", "unemployedwages","employedwages","totalwelfare")]
welfare[,change:= (Counterfactual-Initial) ]
welfare[,dollarchange:= (Counterfactual-Initial) ]
hold<-abs(as.numeric(welfare[variable=="totalwelfare","Initial"][1]))

welfare[, change:=change/hold]
welfare[, dollarchange:=dollar(dollarchange, prefix="\\$")]
welfare[, change:=percent(change, suffix="\\%")]
welfare<-welfare[,c("variable","dollarchange","change") ]
colnames(welfare)<-c("Source", "Change", "Percent Change")
welfare[ Source=="total_profit", Source:="Salon Profit"]
welfare[ Source=="consumer_welfare", Source:="Consumer Welfare"]
welfare[ Source=="employedwages", Source:="Employed Wages"]
welfare[ Source=="unemployedwages", Source:="Unemployed Wages"]
welfare[ Source=="totalwelfare", Source:="Total Welfare"]

kable(welfare[c(1,2,4,5,3),], "latex", align="c", booktabs=TRUE, escape = F) %>%
  row_spec(4, hline_after=T) %>%
  cat(., file = "out/tables/03_02_minwage_welfare.tex")


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
  piece<-data.frame(ny_objects$bmats[[y]]/rowSums(ny_objects$bmats[[y]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),5),rowSums(ny_objects$bmats[[y]]),
               estim_sample[y,]$quarter_year, 1:5, estim_sample[y,]$location_id, ny_objects$gammas[y], estim_sample$s_index[y], ny_objects$C[y], estim_sample$salon_share_subdiv[y],estim_sample$cust_price[y])
  jobs_model<-rbind(jobs_model,piece)
}
jobs_model<-data.table(jobs_model)
names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5",
                     "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price")
jobs_model[,emp_tot:=tot_firm*type_frac]
jobs_model<-jobs_model[type_frac>1e-08]
jobs_model[,Source:="Initial"]


jobs_counterfactual<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(minwage_objects[[trial_num]]$bmats[[y]]/rowSums(minwage_objects[[trial_num]]$bmats[[y]]),rep(as.numeric(minwage_objects[[trial_num]]$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),5),rowSums(minwage_objects[[trial_num]]$bmats[[y]]),
               estim_sample[y,]$quarter_year, 1:5, estim_sample[y,]$location_id, ny_objects$gammas[y],minwage_objects[[trial_num]]$mainstats$Imodel[y], minwage_objects[[trial_num]]$mainstats$C[y], minwage_objects[[trial_num]]$mainstats$new_share[y], minwage_objects[[trial_num]]$mainstats$newprice[y])
  jobs_counterfactual<-rbind(jobs_counterfactual,piece)
}
jobs_counterfactual<-data.table(jobs_counterfactual)
names(jobs_counterfactual)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5",
                              "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price")
jobs_counterfactual[,emp_tot:=tot_firm*type_frac]
jobs_counterfactual<-jobs_counterfactual[type_frac>1e-08]
jobs_counterfactual[,Source:="Reorganization"]

jobs_partial<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-data.frame(minwage_noadj_objects[[trial_num_noadj]]$bmats[[y]]/rowSums(minwage_noadj_objects[[trial_num_noadj]]$bmats[[y]]),rep(as.numeric(minwage_noadj_objects[[trial_num_noadj]]$mainstats[y,"new_share"]*estim_sample[y,"avg_labor"]*estim_sample[y,"CSPOP"]),5),rowSums(minwage_noadj_objects[[trial_num_noadj]]$bmats[[y]]),
               estim_sample[y,]$quarter_year, 1:5, estim_sample[y,]$location_id, ny_objects$gammas[y],minwage_noadj_objects[[trial_num_noadj]]$mainstats$Imodel[y],minwage_noadj_objects[[trial_num_noadj]]$mainstats$C[y],minwage_noadj_objects[[trial_num_noadj]]$mainstats$new_share[y],minwage_noadj_objects[[trial_num_noadj]]$mainstats$newprice[y]  )
  jobs_partial<-rbind(jobs_partial,piece)
}
jobs_partial<-data.table(jobs_partial)
names(jobs_partial)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5",
                              "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share","price" )
jobs_partial[,emp_tot:=tot_firm*type_frac]
jobs_partial<-jobs_partial[type_frac>1e-08]
jobs_partial[,Source:="Reallocation"]

tokeep<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "emp_tot","tot_firm", "Source", "quarter_year", "type", "location_id", "gamma", "complexity", "mc", "share", "price")

jobs_both<-rbind(jobs_counterfactual[,..tokeep], jobs_model[,..tokeep],jobs_partial[,..tokeep] )

jobs_both[type==1, focal_task:=jobvect_1]
jobs_both[type==2, focal_task:=jobvect_2]
jobs_both[type==3, focal_task:=jobvect_3]
jobs_both[type==4, focal_task:=jobvect_4]
jobs_both[type==5, focal_task:=jobvect_5]

positions <- c("Initial", "Reallocation", "Reorganization")
jobs_both[, Source:=factor(Source, levels=positions)]
ggplot(jobs_both[type==1], aes(weight = emp_tot/sum(emp_tot),linetype=Source, color=Source)) + geom_density(aes(x = 100*focal_task),size=1.2 )+
  labs(x="Task Specialization", y="Density")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("solid","dashed", "dotted"))
ggsave("out/figures/03_02_minwage_cutspec.png", width=9, heigh=9, units="in")

ggplot(jobs_both[type==2], aes(weight = emp_tot/sum(emp_tot),linetype=Source, color=Source)) + geom_density(aes(x = 100*focal_task),size=1.2 )+
  labs(x="Task Specialization", y="Density")+
  theme(legend.position="bottom")+
  scale_linetype_manual(values=c("solid","dashed", "dotted"))
ggsave("out/figures/03_02_minwage_colorspec.png", width=9, heigh=9, units="in")



fortable<-melt(jobs_both, id.vars=c("location_id", "emp_tot", "type", "Source"), measure.vars=list(colA), value.name=c("jobvect"))
fortable[,task:=as.numeric((gsub("jobvect_", "", variable))) ]
fortable[, hours:=jobvect*emp_tot/sum(emp_tot), by=c("type", "Source")]
fortable<-merge(fortable,keytask, by="task" )
fortable<-merge(fortable,keyworker, by="type" )
fortable<-fortable[task==type, ]

ggplot(fortable[Source!="No Internal"], aes(x = `Worker Type`, fill = Source, y = hours))+
  stat_summary(geom = "bar", position = "dodge", fun.y = sum)+
  labs(y="Hours on Specialty Task")+
  theme(legend.position="bottom")
ggsave("out/figures/03_02_change_specialization_minwage.png", width=12, heigh=8, units="in")



### reorg effect: 

org_model<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])
  piece<-data.frame( piece[1], diag(ny_objects$bmats[[estab]])[1]/piece[1], estim_sample$location_id[estab])
  org_model<-rbind(org_model, piece)
}
colnames(org_model)<-c("empshare_init", "task_spec_init", "location_id")

org_counterfactual<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(minwage_objects[[trial_num]]$bmats[[estab]])
  piece<-data.frame( piece[1], diag(minwage_objects[[trial_num]]$bmats[[estab]])[1]/piece[1], estim_sample$location_id[estab])
  org_counterfactual<-rbind(org_counterfactual, piece)
}
colnames(org_counterfactual)<-c("empshare", "task_spec", "location_id")

org_partial<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(minwage_noadj_objects[[trial_num_noadj]]$bmats[[estab]])
  piece<-data.frame( piece[1], diag(minwage_noadj_objects[[trial_num_noadj]]$bmats[[estab]])[1]/piece[1], estim_sample$location_id[estab])
  org_partial<-rbind(org_partial, piece)
}
colnames(org_partial)<-c("empshare", "task_spec", "location_id")
org_counterfactual[, Source:="Full"]
org_partial[, Source:=' Reallocation']
org_counterfactual<-rbind(org_counterfactual, org_partial)
org_model[, Source:="Initial"]
org_both<-rbind(org_counterfactual, org_partial)
org_partial[, Source:=NULL]
colnames(org_partial)<-c("empshare_init", "task_spec_init", "location_id")
org_both<-merge(org_both, org_partial, by="location_id")
org_both[, changeshare:=empshare-empshare_init ]
org_both[, changespec:=task_spec-task_spec_init ]
setorder(org_both, "location_id", "Source")
ggplot(data=org_both[ Source!="Initial"], aes(x=changeshare, y=changespec, color=Source))+geom_point(size=4)+
  geom_path(aes(x =changeshare , y = changespec, group = location_id), 
            arrow = arrow(length = unit(0.2, "cm"), type="closed"), size=0.6, color="black")+
  ylab("Task Specialization")+
  xlab("% of Workforce")+
  theme(legend.position="none")+xlim(-0.01, 0.01)+ylim(-0.003, 0.003)

ggsave("out/figures/03_02_minwage_subbind.png", width=12, heigh=9, units="in")


org_model<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])
  piece<-data.frame( sum(piece[2]), sum(diag(ny_objects$bmats[[estab]])[2])/sum(piece[2]), estim_sample$location_id[estab])
  org_model<-rbind(org_model, piece)
}
colnames(org_model)<-c("empshare_init", "task_spec_init", "location_id")

org_counterfactual<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(minwage_objects[[trial_num]]$bmats[[estab]])
  piece<-data.frame( sum(piece[2]), sum(diag(minwage_objects[[trial_num]]$bmats[[estab]])[2])/sum(piece[2]), estim_sample$location_id[estab])
  org_counterfactual<-rbind(org_counterfactual, piece)
}
colnames(org_counterfactual)<-c("empshare", "task_spec", "location_id")

org_partial<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(minwage_noadj_objects[[trial_num_noadj]]$bmats[[estab]])
  piece<-data.frame( sum(piece[2]), sum(diag(minwage_noadj_objects[[trial_num_noadj]]$bmats[[estab]])[2])/sum(piece[2]), estim_sample$location_id[estab])
  org_partial<-rbind(org_partial, piece)
}
colnames(org_partial)<-c("empshare", "task_spec", "location_id")
org_counterfactual[, Source:="Full"]
org_partial[, Source:=' Reallocation']
org_counterfactual<-rbind(org_counterfactual, org_partial)
org_model[, Source:="Initial"]
org_both<-rbind(org_counterfactual, org_partial)
org_partial[, Source:=NULL]
colnames(org_partial)<-c("empshare_init", "task_spec_init", "location_id")
org_both<-merge(org_both, org_partial, by="location_id")
org_both[, changeshare:=empshare-empshare_init ]
org_both[, changespec:=task_spec-task_spec_init ]
setorder(org_both, "location_id", "Source")
ggplot(data=org_both[ Source!="Initial"], aes(x=changeshare, y=changespec, color=Source))+geom_point(size=4)+
  geom_path(aes(x =changeshare , y = changespec, group = location_id), 
            arrow = arrow(length = unit(0.2, "cm"), type="closed"), size=0.6, color="black")+
  ylab("Task Specialization")+
  xlab("% of Workforce")+
  theme(legend.position="none")+xlim(-0.01, 0.01)+ylim(-0.005, 0.005)

ggsave("out/figures/03_02_minwage_subnon.png", width=12, heigh=9, units="in")





### employment losses by firm type

firms<-unique(jobs_both[, c("location_id","Source", "gamma","tot_firm", "complexity", "mc", "share", "price" )])
firms[, profit:=estim_sample$CSPOP[1]*share/(1-share)/exp(coef(res_store)[1])]
attachit<-firms[Source=="Initial"]
colnames(attachit)[-1]<-paste0(colnames(attachit)[-1], '_init')
firms<-merge(firms, attachit, by="location_id")
firms[, emp_change:=tot_firm-tot_firm_init]
firms[, mc_change:=mc-mc_init]
firms[, comp_change:=complexity-complexity_init ]
firms[, profit_change:=profit-profit_init]
firms[, price_change:=price-price_init]
firms<-merge(firms, estim_sample[, c("task_mix1","s_index", "location_id")], all.x=TRUE, by="location_id")

init_share<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])
  piece<-c(piece,estim_sample[estab,]$location_id)
  init_share<-rbind(init_share, t(piece))
}
colnames(init_share)<-c(paste0("share",1:5), "location_id")
cols<-paste0("share",1:5)
init_share[,(cols):= lapply(.SD, as.numeric), .SDcols=cols ]
firms<-merge(firms, init_share, all.x=TRUE, by="location_id")
cor(firms[,..cols])

firms[, rank_change:=rank(emp_change/tot_firm_init,ties="random"), by="Source"]
setorder(firms, "Source","rank_change")
ggplot(data=firms[Source=="Reallocation"], aes(x=rank_change,y=emp_change/tot_firm_init*100 ))+geom_bar(stat="identity", fill="black")+xlab("Change in Employment (Rank)")+ylab("Change in Employment (%)")
ggsave("out/figures/03_02_minwage_firms_reallocate_het.png", width=12, heigh=8, units="in")


firms[, rank_change:=rank(share1,ties="random"), by="Source"]
setorder(firms, "Source","rank_change")
ggplot(data=firms[Source=="Reallocation"], aes(x=rank_change,y=emp_change/tot_firm_init*100 ))+geom_bar(stat="identity", fill="black")+xlab("% Workforce Haircut Specialists (Rank)")+ylab("Change in Employment (%)")
ggsave("out/figures/03_02_minwage_firms_reallocate_share1.png", width=12, heigh=8, units="in")
  
ggplot(data=firms[Source=="Reallocation"], aes(x=share1, y=mc_change ))+geom_point(size=5)+
  xlab("Fraction of Workforce Haircut Specialists")+ylab("Change in Marginal Cost ($)")+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_mc.png", width=12, heigh=8, units="in")

ggplot(data=firms[Source=="Reorganization"], aes(x=share1, y=mc_change ))+geom_point(size=4)+
  xlab("Fraction of Initial Workforce Haircut Specialists")+ylab("Change in Marginal Cost ($)")+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_mc_pres.png", width=12, heigh=8, units="in")

ggplot(data=firms[Source=="Reallocation"], aes(x=share1, y=emp_change))+geom_point(size=5)+
  xlab("Fraction of Workforce Haircut Specialists")+ylab("Change in Employment (Hours)")+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_emp.png", width=12, heigh=8, units="in")

ggplot(data=firms[Source=="Reorganization"], aes(x=share1, y=emp_change))+geom_point(size=4)+
  xlab("Fraction of Initial Workforce Haircut Specialists")+ylab("Change in Employment (Hours)")+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_emp_pres.png", width=12, heigh=8, units="in")

ggplot(data=firms[Source=="Reallocation"], aes(x=share1, y=profit_change))+geom_point(size=5)+
  xlab("Fraction of Workforce Haircut Specialists")+ylab("Change in Profit ($)")+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_profit.png", width=12, heigh=8, units="in")

ggplot(data=firms[Source=="Reallocation"], aes(x=share1, y=price_change))+geom_point(size=5)+
  xlab("Fraction of Workforce Haircut Specialists")+ylab("Change in Price ($)")+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_firms_price.png", width=12, heigh=8, units="in")

## get colocation of firms by task mix
#forgraph_taskmix<-melt(init_share, id.vars="location_id" )
#forgraph_taskmix<-merge(forgraph_taskmix, init_share[, c("location_id", "share1")], by="location_id")
#forgraph_taskmix[,type:=as.numeric(gsub("share", "", variable)) ]
#forgraph_taskmix<-merge(forgraph_taskmix,keyworker, by="type" )
#ggplot(data=forgraph_taskmix[type %in% c(2,3)], aes(x=value,y=share1, shape=`Worker Type`, color=`Worker Type`))+geom_point(size=4)+
#  ylab("Fraction of Workforce Haircut Specialists")+xlab("Fraction of Workforce Other Type")
ggplot(data=init_share, aes(x=share3,y=share2, size=share1))+geom_point()+scale_size_continuous(range = c(2,15), breaks=seq(from=0.2, to=0.8, by=0.2))+
  ylab("Fraction Color Specialists")+xlab("Fraction Blowdry Specialists")+labs(size = "Fraction Haircut Specialists")+ theme(legend.position="bottom")
ggsave("out/figures/03_02_minwage_firms_initialshares.png", width=12, heigh=8, units="in")


### wages and unemployment by type
emp_wage<-jobs_both[, .(employment=sum(emp_tot)*estim_sample$weight[1], task_spec=sum(emp_tot*focal_task)/sum(emp_tot)), by=c("type", "Source")]
setorder(emp_wage, "Source", "type" )
emp_wage<-dcast(emp_wage, type~Source, value.var=c("employment","task_spec"))
wage_part<-cbind(wages,as.numeric(minwage_noadj_res[trial_num_noadj,c("w1", "w2", "w3", "w4", "w5")]),as.numeric(minwage_res[trial_num,c("w1", "w2", "w3", "w4", "w5")]))
wage_part<-data.table(wage_part)
colnames(wage_part)<-c("Initial","Reallocation", "Reorganization" )
wage_part[, type:=1:5]
emp_wage<-merge(emp_wage, wage_part, by=c("type"))
setcolorder(emp_wage, c("type",
                        "employment_Initial","Initial","task_spec_Initial", 
                        "employment_Reallocation","Reallocation", "task_spec_Reallocation",
                        "employment_Reorganization","Reorganization", "task_spec_Reorganization"))
emp_wage<-merge(emp_wage, readRDS('data/00_00_keytask.rds')[, c("rep_text_cluster", "type")], by="type")
emp_wage[, type:=rep_text_cluster]
emp_wage[, rep_text_cluster:=NULL]

loss_gain_full<-copy(emp_wage)
loss_gain_reorg<-copy(emp_wage)
loss_gain_reloc<-copy(emp_wage)

cols<-c("Initial", "Reallocation", "Reorganization")
emp_wage[ , (cols) := lapply(.SD, dollar, prefix="\\$"), .SDcols = cols]
cols<-c("employment_Initial", "employment_Reallocation", "employment_Reorganization")
emp_wage[ , (cols) := lapply(.SD, round), .SDcols = cols]
cols<-c("task_spec_Initial", "task_spec_Reallocation", "task_spec_Reorganization")
emp_wage[ , (cols) := lapply(.SD, sigfig, n=4), .SDcols = cols]
colnames(emp_wage)<-c("Worker Type", rep(c("Hours", "Wage", "Task-Spec."),3))

kable(emp_wage, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F) %>%
  add_header_above(., c(" ", "Initial" = 3, "Reallocation" = 3, "Reorganization" = 3)) %>%
  cat(., file = "out/tables/03_02_wage_emp_min.tex")

kable(emp_wage[,-c(4,5,6,7,10)], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F) %>%
  add_header_above(., c(" ", "Initial" = 2, "Counterfactual" = 2)) %>%
  cat(., file = "out/tables/03_02_wage_emp_min_forpres.tex")


### wage losses and specialization changes by type.
loss_gain_full<-rbind(loss_gain_full[type =="Haircut/Shave"],loss_gain_full[type =="Haircut/Shave"],loss_gain_full[type !="Haircut/Shave"] )
loss_gain_full[type =="Haircut/Shave",type:=c("Haircut/Shave - UNEMPLOYED","Haircut/Shave - EMPLOYED") ]
loss_gain_full[type =="Haircut/Shave - UNEMPLOYED",employment_Reorganization:=employment_Initial-employment_Reorganization  ]
loss_gain_full[type =="Haircut/Shave - UNEMPLOYED",Reorganization:=0 ]
loss_gain_full[type =="Haircut/Shave - UNEMPLOYED",task_spec_Reorganization  :=NA ]
loss_gain_full[, wage_change:=(Reorganization -Initial)/Initial]
loss_gain_full[, spec_change:=(task_spec_Reorganization   -task_spec_Initial )/task_spec_Initial ]
loss_gain_full[, dollar_change:=(Reorganization -Initial)*employment_Reorganization ]
loss_gain_full[, c("type","wage_change", "dollar_change")]
loss_gain_full[,dollar_change:=dollar(dollar_change, prefix="\\$")]
loss_gain_full[, wage_change:=paste0(formatC(wage_change*100, digits = 2, format = "f"),"\\%")]
loss_gain_full<-loss_gain_full[, c("type","wage_change", "dollar_change")]
colnames(loss_gain_full)<-c("Type", "Wage Change", "Total Wages Gained/Lost")
kable(loss_gain_full, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F) %>%
  cat(., file = "out/tables/03_02_minwage_wagechanges.tex")


loss_gain_reorg[, emp_change:=(employment_Reorganization -employment_Reallocation)/employment_Initial]
loss_gain_reorg[, wage_change:=(Reorganization -Reallocation)/Initial]
loss_gain_reorg[, spec_change:=(task_spec_Reorganization   -task_spec_Reallocation )/task_spec_Initial ]
loss_gain_reorg<-loss_gain_reorg[, c("type","emp_change" ,"spec_change","wage_change")]
cols<-colnames(loss_gain_reorg)[-1]
loss_gain_reorg[ , (cols) := lapply(.SD, function(x){ paste0(ifelse(round(x*1e06)==0,"0",formatC(x*100, digits = 2, format = "f")),"\\%")}), .SDcols = cols]
colnames(loss_gain_reorg)<-c("Type", "Employment", "Task-Spec.", "Wage")
kable(loss_gain_reorg, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F) %>%
  add_header_above(., c(" ", "Reorganization Change" = 3)) %>%
  cat(., file = "out/tables/03_02_minwage_reorg.tex")


loss_gain_reloc[, emp_change:=(employment_Reallocation -employment_Initial)/employment_Initial]
loss_gain_reloc[, wage_change:=(Reallocation -Initial)/Initial]
loss_gain_reloc[, spec_change:=(task_spec_Reallocation   -task_spec_Initial )/task_spec_Initial ]
loss_gain_reloc<-loss_gain_reloc[, c("type","emp_change" ,"spec_change","wage_change")]
cols<-colnames(loss_gain_reloc)[-1]
loss_gain_reloc[ , (cols) := lapply(.SD, function(x){ paste0(ifelse(round(x*1e06)==0,"0",formatC(x*100, digits = 2, format = "f")),"\\%")}), .SDcols = cols]
colnames(loss_gain_reloc)<-c("Type", "Employment", "Task-Spec.", "Wage")
kable(loss_gain_reloc, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F) %>%
  add_header_above(., c(" ", "Reallocation Change" = 3)) %>%
  cat(., file = "out/tables/03_02_minwage_reloc.tex")

kable(loss_gain_reloc[c(1,2,3),-c(3)], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F) %>%
  add_header_above(., c(" ", "Reallocation Change" = 2)) %>%
  cat(., file = "out/tables/03_02_minwage_reloc_forpres.tex")
