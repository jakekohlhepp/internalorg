## NYC basin-quality diagnostic
## Classify each rep as high vs low NYC wage basin; cross-tab vs convergence/status;
## compare downstream params; check shape-match to known PSO good basin.

suppressMessages(library(data.table))

repdir <- "results/data/bootstrap_reps"
files <- list.files(repdir, pattern="^boot_res_[0-9]+\\.rds$", full.names=TRUE)

NYC <- "36061"; COOK <- "17031"; LA <- "6037"
wcols <- function(fips) sprintf("factor(county)%s:avg_labor:E_raw_%d", fips, 2:5)
pcol  <- function(fips) sprintf("factor(county)%s:cust_price", fips)
bcols <- function(fips) {
  g <- expand.grid(i=1:5, j=1:5)
  sprintf("factor(county)%s:avg_labor:B_raw_%d_%d", fips, g$i, g$j)
}

num <- function(v) as.numeric(as.character(v))

rows <- lapply(files, function(f){
  x <- readRDS(f)
  it <- if ("iteration" %in% names(x)) num(x[["iteration"]]) else NA
  out <- data.table(
    file=basename(f),
    iter=it,
    status=as.character(x[["status"]]),
    wage_conv=num(x[["wage_convergence"]]),
    price_conv=num(x[["price_convergence"]])
  )
  for (fips in c(NYC, COOK, LA)) {
    wc <- wcols(fips)
    vals <- sapply(wc, function(cn) if (cn %in% names(x)) num(x[[cn]]) else NA_real_)
    for (k in 2:5) out[[sprintf("E%s_%d", fips, k)]] <- vals[k-1]
    pc <- pcol(fips)
    out[[sprintf("rho%s", fips)]] <- if (pc %in% names(x)) num(x[[pc]]) else NA_real_
    # skill-B summary: max abs and frobenius
    bc <- bcols(fips)
    bv <- sapply(bc, function(cn) if (cn %in% names(x)) num(x[[cn]]) else NA_real_)
    out[[sprintf("Bmaxabs%s", fips)]] <- max(abs(bv), na.rm=TRUE)
    out[[sprintf("Bfro%s", fips)]] <- sqrt(sum(bv^2, na.rm=TRUE))
  }
  out
})
DT <- rbindlist(rows, fill=TRUE)
setorder(DT, iter)

cat("=== N reps loaded:", nrow(DT), " status table ===\n")
print(DT[, .N, by=status])

## ---- Point estimate & known good PSO basin ----
pe <- readRDS("results/data/06_parameters.rds")
getpe <- function(nm) pe[parm_name==nm, coefficients]
pe_nyc <- c(getpe("factor(county)36061:avg_labor:E_raw_2"),
            getpe("factor(county)36061:avg_labor:E_raw_3"),
            getpe("factor(county)36061:avg_labor:E_raw_4"),
            getpe("factor(county)36061:avg_labor:E_raw_5"))
names(pe_nyc) <- paste0("E_", 2:5)
# Known good PSO basin in (E2,E3,E4,E5) order: E2=-22.4, E3=498.2, E4=150.94, E5=1826.32
pso_good <- c(E_2=-22.401, E_3=498.207, E_4=150.942, E_5=1826.289)
# Wrong basin documented: all-large-negative (~ -1000 each) and production deadend all-large-negative
cat("\n=== Point estimate NYC wages (E2..E5):", round(pe_nyc,2), "===\n")
cat("=== Known good PSO basin (E2..E5):", round(pso_good,2), "===\n")

## ---- Basin classification on status==ok reps ----
ok <- DT[status=="ok"]
cat("\n=== status==ok reps:", nrow(ok), " ===\n")

# kmeans k=2 on the four NYC wages (status==ok)
W <- as.matrix(ok[, .(E36061_2, E36061_3, E36061_4, E36061_5)])
set.seed(1)
km <- kmeans(scale(W), centers=2, nstart=25)
# label clusters: "high" = larger mean E3+E4+E5
cl_mean <- tapply(rowSums(W[,2:4]), km$cluster, mean)
high_cl <- as.integer(names(which.max(cl_mean)))
ok[, basin := ifelse(km$cluster==high_cl, "high", "low")]
cat("\nkmeans basin sizes (status==ok):\n"); print(ok[, .N, by=basin])

# centers in original units
cat("\nbasin centers (NYC wages, original units):\n")
print(ok[, .(n=.N, E2=mean(E36061_2), E3=mean(E36061_3), E4=mean(E36061_4), E5=mean(E36061_5)), by=basin])

## ---- (a) Cross-tab basin vs wage_conv / status / price_conv ----
cat("\n=== (a) CROSS-TAB basin vs convergence (status==ok subset) ===\n")
cat("basin x wage_convergence:\n"); print(dcast(ok[, .N, by=.(basin, wage_conv)], basin~wage_conv, value.var="N", fill=0))
cat("basin x price_convergence:\n"); print(dcast(ok[, .N, by=.(basin, price_conv)], basin~price_conv, value.var="N", fill=0))

# Now classify ALL non-error reps (incl wage_nonconverged) by a simple rule so we can cross-tab with status
# Rule: "high" if E5 > 600 (midpoint-ish between low-basin ~130 and high-basin ~1300) OR E3>500
DT[, basin_rule := ifelse((E36061_5 > 600) | (E36061_3 > 500), "high", "low")]
DT[, basin_rule := ifelse(E36061_2 < -300 & E36061_4 < -300, "wrong", basin_rule)]
cat("\nRule-based basin (ALL non-error reps) x status:\n")
print(dcast(DT[, .N, by=.(basin_rule, status)], basin_rule~status, value.var="N", fill=0))
cat("\nRule-based basin x wage_convergence (ALL):\n")
print(dcast(DT[, .N, by=.(basin_rule, wage_conv)], basin_rule~wage_conv, value.var="N", fill=0))

## ---- (b) Downstream params across NYC basins ----
cat("\n=== (b) DOWNSTREAM params by NYC basin (status==ok) ===\n")
cat("NYC rho, NYC skill-B magnitude:\n")
print(ok[, .(n=.N,
             rhoNYC_med=median(rho36061), rhoNYC_sd=sd(rho36061),
             BmaxabsNYC_med=median(Bmaxabs36061),
             BfroNYC_med=median(Bfro36061)), by=basin])

cat("\nOTHER counties params by NYC basin (are high-basin reps globally pathological?):\n")
print(ok[, .(n=.N,
             rhoCook_med=median(rho17031), rhoCook_sd=sd(rho17031),
             rhoLA_med=median(rho6037), rhoLA_sd=sd(rho6037),
             BmaxabsCook=median(Bmaxabs17031), BmaxabsLA=median(Bmaxabs6037),
             E_Cook3=median(E17031_3), E_LA5=median(E6037_5)), by=basin])

## ---- (c) Shape match of high basin to known PSO good basin ----
cat("\n=== (c) SHAPE MATCH: high-basin reps vs known PSO good basin (-22,498,151,1826) ===\n")
hi <- ok[basin=="high"]
cat("High-basin reps NYC wages (each row a rep):\n")
print(hi[, .(iter, E2=round(E36061_2), E3=round(E36061_3), E4=round(E36061_4), E5=round(E36061_5))])
cat("\nHigh-basin median wage vector:", round(c(median(hi$E36061_2),median(hi$E36061_3),median(hi$E36061_4),median(hi$E36061_5)),1),"\n")
cat("Known PSO good basin           :", round(pso_good,1),"\n")
cat("Point estimate (manuscript)    :", round(pe_nyc,1),"\n")

# shape descriptor: sign pattern and which coordinate is the big positive one
cat("\nPer-rep: which E coord is max, and sign pattern:\n")
hi[, maxcoord := apply(.SD, 1, function(r) c("E2","E3","E4","E5")[which.max(r)]), .SDcols=c("E36061_2","E36061_3","E36061_4","E36061_5")]
hi[, nneg := rowSums(.SD < 0), .SDcols=c("E36061_2","E36061_3","E36061_4","E36061_5")]
print(hi[, .(iter, maxcoord, nneg, E5=round(E36061_5))])

# Low basin shape for contrast
lo <- ok[basin=="low"]
cat("\nLow-basin median wage vector :", round(c(median(lo$E36061_2),median(lo$E36061_3),median(lo$E36061_4),median(lo$E36061_5)),1),"\n")

## ---- Correlation: do high-basin NYC reps coincide with extreme OTHER-county wages? ----
cat("\n=== Are high-basin NYC reps also extreme in OTHER counties (global pathology check)? ===\n")
cat("Cook E3 sd by basin / LA E5 sd by basin:\n")
print(ok[, .(CookE3_sd=sd(E17031_3), LA_E5_sd=sd(E6037_5), CookE3_med=median(E17031_3), LA_E5_med=median(E6037_5)), by=basin])

saveRDS(DT, "diagnostics/out/nyc_basin_quality_DT.rds")
cat("\n[done]\n")
