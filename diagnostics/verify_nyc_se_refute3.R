#!/usr/bin/env Rscript
suppressWarnings(suppressMessages({source("renv/activate.R"); library(data.table)}))
options(width=170)
repdir <- "results/data/bootstrap_reps"
files  <- list.files(repdir, pattern="^boot_res_.*\\.rds$", full.names=TRUE)
mt <- file.info(files)$mtime; ord<-order(mt); files<-files[ord]; mt<-mt[ord]
reps <- lapply(files, function(f) as.data.frame(readRDS(f)))
nm <- colnames(reps[[1]]); book<-c("iteration","wage_convergence","price_convergence","status","error_message")
parmcols<-setdiff(nm,book)
mat<-matrix(NA_real_,nrow=length(reps),ncol=length(parmcols),dimnames=list(NULL,parmcols))
status<-character(length(reps)); iternum<-rep(NA_integer_,length(reps))
for(i in seq_along(reps)){r<-reps[[i]];status[i]<-as.character(r[["status"]]);iternum[i]<-suppressWarnings(as.integer(r[["iteration"]]))
  for(cc in parmcols) mat[i,cc]<-suppressWarnings(as.numeric(as.character(r[[cc]])))}
okidx<-which(status=="ok")
rc<-grep("factor\\(county\\)36061:cust_price$",parmcols,value=TRUE)
e2<-grep("factor\\(county\\)36061:avg_labor:E_raw_2",parmcols,value=TRUE)

cat("=== The single rho-driving rep (NYC cust_price) ===\n")
v<-mat[okidx,rc]
o<-order(abs(v-mean(v,na.rm=TRUE)),decreasing=TRUE)
cat("NYC rho values (ok reps), sorted:\n"); print(round(sort(v),3))
cat("\nMost extreme rep: iter",iternum[okidx][o[1]],"file",basename(files[okidx][o[1]]),"rho=",round(v[o[1]],3),"\n")
cat("Point est rho=-0.0454. Median of reps:",round(median(v),4),"\n")
cat("SD with extreme:",round(sd(v),4)," SD without top-1:",round(sd(v[-o[1]]),4),
    " without top-2:",round(sd(v[-o[1:2]]),4),"\n")

cat("\n=== Rho for ALL three counties (sanity: is NYC's rho-SD an outlier-only artifact?) ===\n")
for(f in c(Cook="17031",NYC="36061",LA="6037")){
  rcf<-grep(paste0("factor\\(county\\)",f,":cust_price$"),parmcols,value=TRUE)
  vv<-mat[okidx,rcf]
  cat(sprintf("%-5s rho: range [%.3f, %.3f] sd %.4f  n_outlier(|z|>3): %d\n",
      names(which(c(Cook="17031",NYC="36061",LA="6037")==f)), min(vv),max(vv),sd(vv),
      sum(abs(scale(vv))>3)))
  cat("    sorted tail:", paste(round(sort(vv),3)[c(1,2,length(vv)-1,length(vv))],collapse=" ... "),"\n")
}

cat("\n=== DECISIVE TEST: do high-basin reps cluster in 'tighter wage convergence'? ===\n")
# wage_convergence is a code; tabulate by basin
wc<-suppressWarnings(as.numeric(sapply(reps,function(r) r[["wage_convergence"]])))
e3<-mat[,grep("factor\\(county\\)36061:avg_labor:E_raw_3",parmcols)]
basin<-ifelse(e3>500,"high","low")
cat("wage_convergence code x basin (ok reps):\n")
print(table(wconv=wc[okidx], basin=basin[okidx]))

cat("\n=== If we DOUBLE the high-basin contamination (simulate more reps drawing same mix) ===\n")
# Resample to n=789 (projected ok count) preserving empirical mixture, recompute SD distribution
set.seed(7)
for(cc in c(grep("factor\\(county\\)36061:avg_labor:E_raw_3",parmcols,value=TRUE),
            grep("factor\\(county\\)36061:avg_labor:E_raw_5",parmcols,value=TRUE), rc)){
  vv<-mat[okidx,cc]; vv<-vv[!is.na(vv)]
  sd_now<-sd(vv)
  # bootstrap SD at projected n=789 (sampling WITH replacement from current empirical dist)
  bs789<-replicate(2000, sd(sample(vv,789,replace=TRUE)))
  cat(sprintf("%-50s SD@n=43=%.1f ; resampled SD@n=789 median=%.1f CI[%.1f,%.1f]\n",
      sub("factor\\(county\\)36061:","",cc), sd_now, median(bs789), quantile(bs789,.025),quantile(bs789,.975)))
}
cat("(KEY: if resampled SD@789 ~ SD@43, then more reps do NOT lower the SE -- they just tighten its estimate.\n",
    " The SD is a property of the bootstrap DISTRIBUTION, not of n.)\n")
cat("\nDONE3\n")
