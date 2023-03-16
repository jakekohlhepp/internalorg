library('data.table')
library('stats')
library('ggplot2')
theme_set(theme_bw(base_size=22))
set.seed(23299)

### functions
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))

purge_nan<-function(x)  ifelse( is.nan(x),0,x)


###

emp_probs<-rep(1,13)/13
task_probs<-c(0.41, 0.39, 0.09,0.05, 0.06)
res<-data.frame()
firm_quarter<-readRDS('data/00_00_firm_quarter.rds')
quant_taskcount<-quantile(firm_quarter$task_count, seq(from=0.05, ,to=0.95, by=0.05))
for (task_count in quant_taskcount){
  
  
  probs<-c(sapply(emp_probs,function(x){ x*task_probs}))
  
  ## assume data is generated from null. so that
  
  sim_res<-rmultinom(1, prob=probs,size=task_count)
  ## change back to matrix.
  sim_res<-sim_res/task_count
  
  B<-matrix(sim_res,ncol=length(task_probs), nrow=length(emp_probs), byrow=TRUE)
  E<-rowSums(B)
  A<-colSums(B)
  Divisor<-matrix(E,ncol=length(task_probs), nrow=length(emp_probs), byrow=FALSE)*matrix(A,ncol=length(task_probs), nrow=length(emp_probs), byrow=TRUE)
  I_tilde<-sum(B*spec_log(B/Divisor))
  
  res<-rbind(res,c(task_count,I_tilde))
}
res<-cbind(res, seq(from=0.05, ,to=0.95, by=0.05))
res<-data.table(res)
colnames(res)<-c("tasks","I_estim", "quantile")
res[, round_I_estim:=round(I_estim,digits=3)]



#### now do it using the employee counts, task mixes, and task counts in the data.


res<-data.frame()
for (id in 1:nrow(firm_quarter)){
  
  task_count<-firm_quarter[id,]$task_count
  task_probs<-as.vector(firm_quarter[id,c("task_mix1", "task_mix2", "task_mix3", "task_mix4", "task_mix5")])
  emp_count<-firm_quarter[id,]$emps
  emp_probs<-rep(1,emp_count)/emp_count
  
  probs<-c(sapply(emp_probs,function(x){ x*task_probs}))
  
  ## assume data is generated from null. so that
  
  sim_res<-rmultinom(1, prob=probs,size=task_count)
  ## change back to matrix.
  sim_res<-sim_res/task_count
  
  B<-matrix(sim_res,ncol=length(task_probs), nrow=length(emp_probs), byrow=TRUE)
  E<-rowSums(B)
  A<-colSums(B)
  Divisor<-matrix(E,ncol=length(task_probs), nrow=length(emp_probs), byrow=FALSE)*matrix(A,ncol=length(task_probs), nrow=length(emp_probs), byrow=TRUE)
  I_tilde<-sum(B*spec_log(B/Divisor))
  
  res<-rbind(res,c(I_tilde,firm_quarter[id,]$cust_price,firm_quarter[id,]$emps,firm_quarter[id,]$cust_count,firm_quarter[id,]$task_count ))
}

res<-cbind(res)
res<-data.table(res)
colnames(res)<-c("s_index", "cust_price", "emps","cust_count", "task_count")
saveRDS(res,file="data/null_simulation.rds")

cor(res)
cor(firm_quarter[,c("s_index", "cust_price", "emps","cust_count", "task_count")])
### plot against full variation in complexity
res[, source:="Null Simulation"]
firm_quarter[, source:="Observed"]

forplot<-rbind(res[,c("s_index","source")], firm_quarter[,c("s_index","source")])

ggplot(forplot, aes(x=s_index, fill=source))+geom_histogram(bins=50,position = "identity", alpha = 0.7)
ggsave("out/figures/null_hist.png", width=12, heigh=6, units="in")
forplot[, .(var=var(s_index,na.rm=TRUE)), by="source"]
