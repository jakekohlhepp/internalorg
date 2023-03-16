## estimate profit using just hausman/other instruments.
## see correlation with s-index.

library('data.table')
library('AER')
library('binsreg')
set.seed(121345)
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))


### read in and limit data
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")
firm_quarter[,share_given_zip:=salon_share/zip_share]
# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year %between% c(2019.2, 2020.1) & county %in% c(6037, 36061) ]
# cannot identify when s_index is 0 or infinite
estim_sample<-estim_sample[!is.nan(s_norm) & round(s_norm,5)!=0 & round(s_norm,5)!=1 & cust_price>0]
# cannot use salons that do not record price.

table(estim_sample$county)

estim_sample[, q2:=quarter_year==2019.3]
estim_sample[, q3:=quarter_year==2019.4]
estim_sample[, q4:=quarter_year==2020.1]


estim_sample[, return_rate:=return_count/cust_count]
estim_matrix<-as.matrix(estim_sample[,c("salon_share","outside_share", "task_mix2", "task_mix3", "task_mix4",
                                         "cust_price", "q2","q3","q4", "s_2", "s_3", "s_4","d_sum", "hausman_all")])


g<-function(parms, x){
  rho<-parms[1]
  beta<-c(parms[2], parms[3], parms[4])
  c<-c(parms[5], parms[6], parms[7])
  qu<-c(parms[8], parms[9], parms[10])
  qc<-c(parms[11], parms[12], parms[13])
  ubar<-parms[14]
  cbar<-parms[15]
  moment_mat<-cbind(log(x[,1]/x[,2])-x[,3:5]%*%beta +rho*x[,6]-x[,7:9]%*%qu-ubar,
                    x[,6]-(rho*(1-x[,1]))^(-1)-x[,3:5]%*%c-x[,7:9]%*%qc-cbar  )
  Z<-cbind(1,x[,7:14])
  return(cbind(moment_mat[,1]*Z,moment_mat[,2]*Z ))
}

starting<-rep(1,15)
outgmm1<-gmm(g, x=estim_matrix, t0=starting, prewhite=0, type="twoStep",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-10,reltol=1e-10, maxit=100000))
parms<-coef(outgmm1)
rho<-parms[1]
beta<-c(parms[2], parms[3], parms[4])
c<-c(parms[5], parms[6], parms[7])
qu<-c(parms[8], parms[9], parms[10])
qc<-c(parms[11], parms[12], parms[13])
ubar<-parms[14]
cbar<-parms[15]

x<-estim_matrix
qual<-log(x[,1]/x[,2])-x[,3:5]%*%beta +rho*x[,6]-x[,7:9]%*%qu-ubar
mc<-x[,6]-(rho*(1-x[,1]))^(-1)-x[,3:5]%*%c-x[,7:9]%*%qc-cba
estim_sample[,qualp:=qual-rho*cust_price ]
estim_sample[,qual_adj:=qual-rho*mc ]
estim_sample[,p_adj:=cust_price-mc ]

plot(estim_sample[, c("s_norm", "cust_price")])
### just NY county
profit_spec_la<-binsreg(estim_sample[county==6037 & q2==1]$return_count, estim_sample[q2==1 & county==6037]$s_index,
                                  nbins=10)
profit_spec_la$bins_plot + theme_bw(base_size=22)+
  ylab("Profit ($)") + xlab("S-Index")
ggsave("out/figures/01_01_profit_sindex_la.png", width=12, heigh=6, units="in")

profit_spec_ny<-binsreg(estim_sample[county==6037 &q2==1]$qualp, estim_sample[county==6037 &q2==1]$s_index,
                                  nbins=15)

profit_spec_ny$bins_plot + theme_bw(base_size=22)+
  ylab("Profit ($)") + xlab("S-Index")
ggsave("out/figures/01_01_profit_sindex_ny.png", width=12, heigh=6, units="in")




res<-plm(log(salon_share)-log(outside_share)~task_mix2+task_mix3+task_mix4+cust_price+q2+q3+q4 | 
              s_2+s_3+s_4+hausman_all+q2+q3+q4,index="location_id",
            data=estim_sample[county ==6037 ,], model="pooling")
