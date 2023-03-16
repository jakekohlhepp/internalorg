## estimate model. only do 2021.2 la
# try particle swarm.
library('checkpoint')
#checkpoint("2020-07-16")
library('data.table')
library('gmm')

set.seed(34343)

#### general functions###########
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
wage_bound<-Vectorize(function(x){
  return(190/(exp(-x)+1)+10)
})
#################################

### read in and limit main firm-based data
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")

# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year %in% c(2021.2) & county %in% c(6037)]

# cannot identify when s_index is 0 or infinite
estim_sample<-estim_sample[!is.nan(s_norm)   & cust_price>0]
# cannot use salons that do not record price.

table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4", "task_mix5","task_mix6",
                                        "cust_price", "salon_share_subdiv","outside_share", 
                                        "hausman_all","avg_labor","avg_wage_qtr", "cust_count")])


# sales tax rate - 0% in la
tau<-1


############### tolerance preferences
innertol<-1e-03
outertol<-1e-03
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-32
} 

source('small_gmm_objective.R')
###############

############### parameters that are fixed
c0<-mean(estim_matrix[,7])
u0<-mean(log(estim_matrix[,8]/estim_matrix[,9]))

starting<-c()
starting[1]<-0.01
starting[2:3]<-0
starting[4:9]<--1
starting[10:14]<-0
starting[15:19]<-0
starting[20:25]<-80
#starting<-readRDS('data/best_starting_la.rds')
###############
test<-g(starting, estim_matrix)
stopifnot(all(test<Inf))

lower<-c(0, rep(-100,length(starting)-1))
upper<-rep(100, length(starting))
library('GA')
GA <- ga(type = "real-valued", 
         fitness =  gmm_alt,
         lower = lower, upper =upper, 
         popSize = 50, maxiter = 1000, run = 100,
         optim = TRUE,monitor=TRUE, optimArgs = list(method="Nelder-Mead"))

prelim<-hydroPSO(fn=gmm_obj, lower=lower, upper=upper, control=list(write2disk=FALSE, REPORT=1), data=estim_matrix,optimArgs=optimArgs )







