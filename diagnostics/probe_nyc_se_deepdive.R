## =============================================================================
## NYC bootstrap SE deep-dive (READ-ONLY).
## Decomposes the large NYC standard errors into the three candidate causes:
##   (1) too few reps, (2) wage-solver convergence / basin-switching,
##   (3) weak identification (flat moment surface / O(1000) wage scale).
## Writes a structured dump to diagnostics/out/nyc_se_deepdive.txt and prints it.
## =============================================================================
suppressPackageStartupMessages({ library(data.table) })
source("config.R")

reps_dir <- file.path("results", "data", "bootstrap_reps")
files <- list.files(reps_dir, pattern = "^boot_res_.*\\.rds$", full.names = TRUE)
combined <- rbindlist(lapply(files, readRDS), fill = TRUE)
setorder(combined, iteration)

nonparm <- c("iteration","wage_convergence","price_convergence","status","error_message")
pcols <- setdiff(names(combined), nonparm)
## coerce numeric (t() in bootstrap_result_row stores as character)
for (c in pcols) if (!is.numeric(combined[[c]])) combined[, (c) := as.numeric(get(c))]

pt <- as.data.table(readRDS(file.path("results","data","06_parameters.rds")))
point <- setNames(pt$coefficients, pt$parm_name)

out <- file.path("diagnostics","out"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
sink(file.path(out, "nyc_se_deepdive.txt"), split = TRUE)

hr <- function(t) cat("\n", paste(rep("=",78),collapse=""), "\n", t, "\n",
                      paste(rep("=",78),collapse=""), "\n", sep="")

hr("0. SAMPLE & STATUS")
cat("rep files on disk:", length(files), "\n")
print(combined[, .N, by=status][order(-N)])
cat("\nwage_convergence breakdown:\n"); print(combined[, .N, by=wage_convergence][order(-N)])
cat("price_convergence breakdown:\n"); print(combined[, .N, by=price_convergence][order(-N)])

## ---- helper: per-county parameter blocks ----
wage_cols <- function(cn) paste0("factor(county)",cn,":avg_labor:E_raw_",2:5)
skill_cols <- function(cn) grep(paste0("factor\\(county\\)",cn,":avg_labor:B_raw"), names(combined), value=TRUE)
rho_col   <- function(cn) paste0("factor(county)",cn,":cust_price")
counties <- c(Cook="17031", NYC="36061", LA="6037")

se_block <- function(dt, cols) {
  rbindlist(lapply(cols, function(c){
    v <- dt[[c]]; vf <- v[is.finite(v)]
    p <- as.numeric(point[[c]])
    data.table(parm=c, point=p, mean=mean(vf), sd=stats::sd(vf),
               relSE=stats::sd(vf)/abs(p), n=length(vf))
  }))
}

ok  <- combined[status=="ok"]
allr<- combined[is.na(status) | status!="error"]

hr("1. CROSS-COUNTY SE COMPARISON (status==ok reps)")
cat(sprintf("status==ok reps: %d   |   all non-error: %d\n\n", nrow(ok), nrow(allr)))
for (nm in names(counties)) {
  cn <- counties[[nm]]
  w <- se_block(ok, wage_cols(cn))
  b <- se_block(ok, skill_cols(cn))
  r <- se_block(ok, rho_col(cn))
  cat(sprintf("--- %s (%s) ---\n", nm, cn))
  cat("  WAGE (E_raw_2..5):\n")
  print(w[, .(parm=sub(".*avg_labor:","",parm), point=round(point,3), sd=round(sd,3),
              relSE=round(relSE,3))])
  cat(sprintf("    wage |point| range [%.3g, %.3g]; wage sd range [%.3g, %.3g]; median relSE=%.3g\n",
              min(abs(w$point)), max(abs(w$point)), min(w$sd), max(w$sd), median(w$relSE)))
  cat(sprintf("  SKILL B (%d params): median sd=%.4g  median relSE=%.4g  max relSE=%.4g\n",
              nrow(b), median(b$sd,na.rm=T), median(b$relSE,na.rm=T), max(b$relSE,na.rm=T)))
  cat(sprintf("  PRICE rho: point=%.4g  sd=%.4g  relSE=%.4g\n\n", r$point, r$sd, r$relSE))
}

hr("2. NYC WAGE DISTRIBUTION SHAPE (status==ok)")
nyc_w <- wage_cols("36061")
M <- as.matrix(ok[, ..nyc_w]); colnames(M) <- sub(".*avg_labor:","",nyc_w)
skew <- function(x){x<-x[is.finite(x)];m<-mean(x);mean((x-m)^3)/sd(x)^3}
kurt <- function(x){x<-x[is.finite(x)];m<-mean(x);mean((x-m)^4)/sd(x)^4}
for (j in colnames(M)) {
  x <- M[,j]; x<-x[is.finite(x)]
  qs <- quantile(x, c(0,.05,.25,.5,.75,.95,1))
  cat(sprintf("%s  point=%.2f  mean=%.2f  sd=%.2f  skew=%.2f  kurt=%.2f\n",
              j, as.numeric(point[[paste0("factor(county)36061:avg_labor:",j)]]),
              mean(x), sd(x), skew(x), kurt(x)))
  cat(sprintf("    quantiles[0,5,25,50,75,95,100]: %s\n",
              paste(sprintf("%.1f",qs), collapse="  ")))
}
cat("\nPairwise correlation of NYC wage draws across reps:\n")
print(round(cor(M), 3))

hr("3. MULTIMODALITY / BASIN-SWITCHING (NYC wage vectors, status==ok)")
## k-means k=2 on standardized 4-vector; report cluster sizes, centers, gap
Ms <- scale(M)
set.seed(1)
km <- tryCatch(kmeans(Ms, centers=2, nstart=20), error=function(e) NULL)
if (!is.null(km)) {
  cat("k=2 cluster sizes:", paste(km$size, collapse=" / "), "\n")
  cat("between_SS / total_SS =", round(km$betweenss/km$totss,3), "\n")
  cat("cluster centers (original units):\n")
  ctr <- t(sapply(1:2, function(k) colMeans(M[km$cluster==k,,drop=FALSE])))
  rownames(ctr) <- paste0("clust",1:2,"(n=",km$size,")"); print(round(ctr,1))
}
## Hartigan dip test if available
if (requireNamespace("diptest", quietly=TRUE)) {
  cat("\nHartigan dip test p-values (unimodal null) per coordinate:\n")
  for (j in colnames(M)) {
    dt2 <- diptest::dip.test(M[,j]); cat(sprintf("  %s: D=%.3f p=%.3f\n", j, dt2$statistic, dt2$p.value))
  }
} else cat("\n(diptest not installed; relying on quantiles/kmeans for modality)\n")
## flag reps near the documented 'wrong basin' (all-large-negative, ~ -1000)
neg_basin <- rowSums(M < -300) >= 3
cat(sprintf("\nreps with >=3 NYC wages < -300 (candidate 'wrong basin'): %d of %d\n",
            sum(neg_basin), nrow(M)))
if (any(neg_basin)) print(cbind(iter=ok$iteration[neg_basin], round(M[neg_basin,,drop=FALSE],0)))

hr("4. OUTLIER / TAIL DRIVERS (NYC wages, status==ok)")
for (j in colnames(M)) {
  x <- M[,j]; z <- (x-mean(x))/sd(x)
  o <- order(-abs(z))[1:min(5,length(z))]
  cat(sprintf("%s top-|z| reps: ", j))
  cat(paste(sprintf("iter%d(val=%.0f,z=%.1f)", ok$iteration[o], x[o], z[o]), collapse="  "), "\n")
}
## leave-one-out: how much does the single most extreme rep move each SE?
cat("\nLeave-one-out max impact on NYC wage SD (drop the single worst rep):\n")
for (j in colnames(M)) {
  x <- M[,j]; full <- sd(x)
  loo <- sapply(seq_along(x), function(i) sd(x[-i]))
  i <- which.min(loo)
  cat(sprintf("  %s: full sd=%.1f  min loo sd=%.1f (drop iter%d) -> %.0f%% reduction\n",
              j, full, loo[i], ok$iteration[i], 100*(full-loo[i])/full))
}

hr("5. STATUS CONDITIONING (ok vs wage_nonconverged)")
nc <- combined[status=="wage_nonconverged"]
cat(sprintf("ok n=%d   wage_nonconverged n=%d\n", nrow(ok), nrow(nc)))
## how many non-ok reps have NYC wages == the 06 warm start (gate 2/3 full revert)?
ws <- sapply(nyc_w, function(c) as.numeric(point[[c]]))
revert_flag <- function(dt) {
  if (nrow(dt)==0) return(integer(0))
  apply(as.matrix(dt[, ..nyc_w]), 1, function(r) all(abs(r-ws) < 1e-6*pmax(1,abs(ws))))
}
cat(sprintf("NYC wages == 06 warm start (within 1e-6 rel):  ok=%d/%d   nonconv=%d/%d\n",
            sum(revert_flag(ok)), nrow(ok), sum(revert_flag(nc)), nrow(nc)))
cat("\nNYC wage means by status:\n")
comp <- rbindlist(lapply(c("ok","wage_nonconverged"), function(s){
  d <- combined[status==s]; if(nrow(d)==0) return(NULL)
  as.data.table(c(list(status=s,n=nrow(d)),
    setNames(lapply(nyc_w,function(c) round(mean(d[[c]],na.rm=T),1)), sub(".*E_raw","E",nyc_w))))
}), fill=TRUE)
print(comp)
cat("\nNYC wage SD: ok-only vs all-non-error (does keeping non-ok inflate SE?):\n")
for (j in seq_along(nyc_w)) {
  c <- nyc_w[j]
  cat(sprintf("  %s: sd(ok)=%.1f   sd(all)=%.1f   ratio=%.2f\n",
      sub(".*avg_labor:","",c), sd(ok[[c]],na.rm=T), sd(allr[[c]],na.rm=T),
      sd(allr[[c]],na.rm=T)/sd(ok[[c]],na.rm=T)))
}

hr("6. SE STABILITY vs N  (is it just too few reps?)")
## cumulative SD as reps accumulate in iteration order (ok reps only)
oko <- ok[order(iteration)]
for (c in nyc_w) {
  v <- oko[[c]]; v<-v[is.finite(v)]
  cs <- sapply(seq_along(v), function(k) if(k<2) NA else sd(v[1:k]))
  idx <- unique(round(seq(2, length(v), length.out=min(8,length(v)))))
  cat(sprintf("%s cumulative sd at n=[%s]:\n   [%s]\n",
      sub(".*avg_labor:","",c), paste(idx,collapse=","),
      paste(sprintf("%.0f",cs[idx]),collapse=", ")))
}
## analytic SE-of-the-SD: se(sdhat) ~ sd / sqrt(2(n-1)); plus naive CI on final SE
cat("\nApprox precision of the SE estimate itself (se(sd) ~ sd/sqrt(2(n-1))):\n")
n_ok <- nrow(ok)
for (c in nyc_w) {
  s <- sd(ok[[c]],na.rm=T); se_s <- s/sqrt(2*(n_ok-1))
  cat(sprintf("  %s: sd=%.1f +/- %.1f (%.0f%%);  proj sd at n=1100 unchanged, CI halves to +/-%.1f\n",
      sub(".*avg_labor:","",c), s, se_s, 100*se_s/s, s/sqrt(2*(1100-1))))
}

hr("7. PRICE rho & SKILL B context (status==ok, NYC vs others)")
for (nm in names(counties)){
  cn<-counties[[nm]]; r<-se_block(ok, rho_col(cn))
  cat(sprintf("  rho %s(%s): point=%.4g sd=%.4g relSE=%.3g\n", nm,cn,r$point,r$sd,r$relSE))
}

sink()
cat("\n\nWROTE diagnostics/out/nyc_se_deepdive.txt\n")
