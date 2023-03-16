
## estimate model. only do 2021.2 NY.

library('data.table')
library('gmm')
set.seed(2332)

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

load('data/01_01_progress.RData')




##################################
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
############### tolerance preferences
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-32
} 

source('small_gmm_objective.R')
###############
starting<-c()
starting[1]<-0.1
starting[2:3]<-0
starting[4:9]<--2.5
starting[10:14]<-0
starting[15:19]<-0
starting[20:25]<-40




test<-g(old, estim_matrix)
oldcheck<-colMeans(test)
sum(oldcheck^2)
m1<-c(rep("cost_", (length(oldcheck)-1)/2),rep("util_", (length(oldcheck)-1)/2))
m2<-rep(c("cons", "a2","a3", "a4", "a5", "a6", 
          "sindex", "a2s","a3s", "a4s", "a5s", "a6s","haus"),2)
moment_names<-c(paste0(m1,m2), "wage")
old<-data.table(names=moment_names,values= as.numeric(oldcheck), abs_value=abs(as.numeric(oldcheck)))



trynew<-coef(res_store)
trynew[10]<-16
trynew[3]<-22
test<-g(trynew, estim_matrix)
check<-colMeans(test)
sum(check^2)
sum(check^2)-sum(oldcheck^2)
m1<-c(rep("cost_", (length(check)-1)/2),rep("util_", (length(check)-1)/2))
m2<-rep(c("cons", "a2","a3", "a4", "a5", "a6", 
          "sindex", "a2s","a3s", "a4s", "a5s", "a6s","haus"),2)
moment_names<-c(paste0(m1,m2), "wage")
tolook<-data.table(names=moment_names,values= as.numeric(check), abs_value=abs(as.numeric(check)))

View(cbind(tolook, old, tolook$abs_value-old$abs_value))