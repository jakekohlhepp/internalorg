suppressPackageStartupMessages(library(data.table))
for (f in c("14_warm_start_wages_diffusion.rds",
            "15_warm_start_wages_salestax.rds",
            "16_warm_start_wages_immigration.rds",
            "17_warm_start_wages_merger.rds")) {
  p <- file.path("results/data/counterfactuals", f)
  if (!file.exists(p)) { cat("MISSING: ", p, "\n", sep=""); next }
  x <- as.data.table(readRDS(p))
  cat("=== ", f, " ===\n", sep="")
  print(x)
  cat("\n")
}
