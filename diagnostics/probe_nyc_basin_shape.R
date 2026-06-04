## Deeper shape analysis: is the high basin a single PSO-shape basin, or label-switching
## among permutations? And how close to the known good PSO basin in sorted space?

suppressMessages(library(data.table))
DT <- readRDS("diagnostics/out/nyc_basin_quality_DT.rds")
ok <- DT[status=="ok"]

# kmeans split (redo identically)
W <- as.matrix(ok[, .(E36061_2, E36061_3, E36061_4, E36061_5)])
set.seed(1)
km <- kmeans(scale(W), 2, nstart=25)
cl_mean <- tapply(rowSums(W[,2:4]), km$cluster, mean)
high_cl <- as.integer(names(which.max(cl_mean)))
ok[, basin := ifelse(km$cluster==high_cl, "high","low")]

pso_good <- c(-22.401, 498.207, 150.942, 1826.289)  # E2,E3,E4,E5
pe_nyc   <- c(-22.07571, 164.58674, 62.84489, 116.75269)

# SORTED-space comparison: the structural model may treat skill rows symmetrically,
# so a permuted wage vector can be an equivalent optimum. Compare SORTED vectors.
sorted_dist <- function(v, ref) sqrt(sum((sort(v)-sort(ref))^2))

ok[, dist_pso_raw   := apply(.SD,1,function(r) sqrt(sum((r-pso_good)^2))), .SDcols=c("E36061_2","E36061_3","E36061_4","E36061_5")]
ok[, dist_pso_sort  := apply(.SD,1,function(r) sorted_dist(r,pso_good)),  .SDcols=c("E36061_2","E36061_3","E36061_4","E36061_5")]
ok[, dist_pe_raw    := apply(.SD,1,function(r) sqrt(sum((r-pe_nyc)^2))),  .SDcols=c("E36061_2","E36061_3","E36061_4","E36061_5")]
ok[, dist_pe_sort   := apply(.SD,1,function(r) sorted_dist(r,pe_nyc)),    .SDcols=c("E36061_2","E36061_3","E36061_4","E36061_5")]

cat("=== Distance to PSO-good basin vs Point-estimate, by basin (raw and sorted) ===\n")
print(ok[, .(n=.N,
             d2pso_raw=round(median(dist_pso_raw)),
             d2pso_sort=round(median(dist_pso_sort)),
             d2pe_raw=round(median(dist_pe_raw)),
             d2pe_sort=round(median(dist_pe_sort))), by=basin])

cat("\n=== High-basin reps SORTED wage vectors (ascending) vs sorted PSO basin ===\n")
hi <- ok[basin=="high"]
sm <- t(apply(as.matrix(hi[,.(E36061_2,E36061_3,E36061_4,E36061_5)]),1,sort))
colnames(sm) <- c("s1","s2","s3","s4")
hisort <- cbind(iter=hi$iter, round(as.data.table(sm)))
print(hisort)
cat("\nSorted PSO good basin :", round(sort(pso_good),1),"\n")
cat("Sorted point estimate :", round(sort(pe_nyc),1),"\n")
cat("Median sorted high    :", round(apply(sm,2,median),1),"\n")

# How many high-basin reps have the 'three modest + one large positive' shape (PSO shape)?
# Define: exactly one coord > 1000, the other three with |.|<600
shape_pso <- function(r){ sum(r>1000)>=1 & sum(abs(r)<700)>=3 }
hi[, pso_shape := apply(.SD,1,shape_pso), .SDcols=c("E36061_2","E36061_3","E36061_4","E36061_5")]
cat("\nHigh-basin reps matching 'one big-positive + three modest' PSO shape:",
    sum(hi$pso_shape),"of",nrow(hi),"\n")

# Two-big variants (two coords >1000): would indicate a DIFFERENT shape than PSO
hi[, n_big := apply(.SD,1,function(r) sum(r>1000)), .SDcols=c("E36061_2","E36061_3","E36061_4","E36061_5")]
cat("Distribution of #coords>1000 among high-basin reps:\n"); print(table(hi$n_big))

cat("\n=== iter109 (candidate WRONG/all-negative basin) ===\n")
print(DT[iter==109, .(iter,status,wage_conv,E36061_2,E36061_3,E36061_4,E36061_5,rho36061)])
cat("Doc-documented WRONG basin: ~(-1000,-1000,-1000,-1000) at ssq 0.0248\n")
cat("[done]\n")
