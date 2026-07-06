dir.create("diagnostics/out", recursive = TRUE, showWarnings = FALSE)

current_path <- "results/data/06_parameters.rds"
old_path <- "diagnostics/out/06_parameters_HEAD_old.rds"

## Old = committed HEAD version; New = working-tree file.
cmd <- sprintf("git show HEAD:results/data/06_parameters.rds > %s", shQuote(old_path))
status <- system(cmd)
if (!identical(status, 0L)) stop("git show failed with status ", status)

old <- readRDS(old_path)
new <- readRDS(current_path)

out <- "diagnostics/out/compare_la_06_old_new.txt"
sink(out, split = TRUE)
cat("=== object classes ===\n")
print(list(old = class(old), new = class(new)))
cat("\n=== old names ===\n"); print(names(old))
cat("\n=== new names ===\n"); print(names(new))
cat("\n=== old str ===\n"); str(old, max.level = 2)
cat("\n=== new str ===\n"); str(new, max.level = 2)
sink()
cat("wrote ", out, "\n", sep = "")
