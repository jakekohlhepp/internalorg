## Verify the hardened tolerance env vars resolve through config.R and reach the
## counterfactual inner-solve path. Run with the SAME env the 13-17 jobs use.
source("config.R")
source("utils/counterfactuals_core.R")

cat("=== resolved CONFIG values ===\n")
cat("fixedpoint_max_iter:    ", CONFIG$fixedpoint_max_iter,
    " (class ", class(CONFIG$fixedpoint_max_iter), ")\n", sep = "")
cat("counterfactual_innertol:", CONFIG$counterfactual_innertol, "\n", sep = "")
cat("counterfactual_outertol:", CONFIG$counterfactual_outertol, "\n", sep = "")

cat("\n=== raw env vars seen by R ===\n")
cat("JMP_FIXEDPOINT_MAX_ITER   = ", Sys.getenv("JMP_FIXEDPOINT_MAX_ITER", "<unset>"), "\n", sep = "")
cat("JMP_COUNTERFACTUAL_INNERTOL = ", Sys.getenv("JMP_COUNTERFACTUAL_INNERTOL", "<unset>"), "\n", sep = "")
cat("JMP_COUNTERFACTUAL_OUTERTOL = ", Sys.getenv("JMP_COUNTERFACTUAL_OUTERTOL", "<unset>"), "\n", sep = "")

## Confirm the inner SQUAREM control that counterfactual_assignment will build.
cat("\n=== inner-solve control that counterfactual_assignment uses ===\n")
cat("SQUAREM control: maxiter = ", CONFIG$fixedpoint_max_iter,
    ", tol (passed innertol) = ", CONFIG$counterfactual_innertol, "\n", sep = "")

ok <- CONFIG$fixedpoint_max_iter == 200000L &&
  isTRUE(all.equal(CONFIG$counterfactual_innertol, 1e-12)) &&
  isTRUE(all.equal(CONFIG$counterfactual_outertol, 1e-8))
cat("\nHARDENED SETTINGS ACTIVE: ", ok, "\n", sep = "")
