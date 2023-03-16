library('AER')
library('data.table')
library('fixest')
library('binsreg')
library('ggplot2')

theme_set(theme_bw(base_size=22))

firm_quarter<-readRDS("../mkdata/data/00_00_firm_quarter.rds")

### use most canonical instrument: sum of 
res<-ivreg(log(share)-log(outshare)~task_mix1+task_mix2+task_mix3+cust_price/estimate_percap_income, instruments=
              ~task_mix1+task_mix2+task_mix3+s_sum,
            data=firm_quarter[location_city=="Los Angeles",])
summary(res)

res2<-ivreg(log(share)-log(outshare)~task_mix1+task_mix2+task_mix3+cust_price/estimate_percap_income, instruments=
             ~task_mix1+task_mix2+task_mix3+s_sum,
           data=firm_quarter[location_city=="New York",])
summary(res2)

# recover quality
firm_quarter[location_city=="Los Angeles", qual:=resid(res)]
firm_quarter[location_city=="New York", qual:=resid(res2)]

# get rho:
firm_quarter[location_city=="Los Angeles", rho:=coef(res)["cust_price"]]
firm_quarter[location_city=="New York", rho:=coef(res2)["cust_price"]]

bin_price_specialization<-binsreg(firm_quarter[location_city=="New York"]$s_index, 
                                  firm_quarter[location_city=="New York"]$qual, 
                                  nbins=20)
bin_price_specialization$bins_plot + theme_bw(base_size=22)+
  ylab("S-Index") + xlab("Quality")
firm_quarter[, cost:=cust_price-1/rho/(1-share)]


##### test run.
firm_quarter[, qual_bar:=max(qual,na.rm=TRUE), by=c("location_city", "quarter_year")]
firm_quarter[, qual_lowerbar:=min(qual,na.rm=TRUE), by=c("location_city", "quarter_year")]

tmix<-grep("^task_mix", names(firm_quarter))

for (x in 1:24){

test<-firm_quarter[location_city=="New York" & quarter_year==2019.4,][x,]
if (test$s_index==0 | is.na(test$s_index)) {next }
alpha<-as.numeric(as.vector(test[,..tmix]))




xi<-test$qual-test$qual_lowerbar
xi_bar<-test$qual_bar-test$qual_lowerbar
xi_lower<-0
T=4 # number of tasks
N=5 # number of skill levels
theta<-seq(from=xi_lower, to=xi_bar, length=N)
d<-c(0.2,0.2, 0.2, 0.1)
m<-c(1,0.1,1,1,1,1)
w<- rep(m, N)*as.vector(sapply(1:N, function(x) {rep(theta[x], length=T)}, simplify=TRUE))+rep(d, N)
wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
delta<-2
starting<-rep(1/(N*T),N*T)
source('../analysis/ba_algorithm.R')

firmest1<-firm_estim(test$s_index )
print(c(firmest1[1:2], test$s_index, xi))
}


