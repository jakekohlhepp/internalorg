library("data.table")
library("stringr")

params <- readRDS(file.path("results", "data", "06_parameters.rds"))
params <- as.data.table(params)

cat("Class: ", paste(class(params), collapse = ", "), "\n")
cat("Cols : ", paste(names(params), collapse = ", "), "\n")
cat("Rows : ", nrow(params), "\n\n")

skill <- params[str_detect(parm_name, "B_raw_[0-9]+_[0-9]+")]
skill[, county := str_extract(parm_name, "(?<=factor\\(county\\))[0-9]+")]
skill[, task := as.integer(str_extract(parm_name, "(?<=B_raw_)[0-9]+"))]
skill[, worker_type := as.integer(str_sub(parm_name, -1L, -1L))]

cat("Skill rows: ", nrow(skill), "\n")
cat("Counties  : ", paste(sort(unique(skill$county)), collapse = ", "), "\n\n")

county_label <- function(c) {
  c(`17031` = "Cook (Chicago)", `36061` = "New York (Manhattan)", `06037` = "Los Angeles")[c]
}

for (cnty in sort(unique(skill$county))) {
  cat("==============================================\n")
  cat("County ", cnty, " — ", county_label(cnty), "\n", sep = "")
  cat("Rows: tasks (B_raw_<row>); Cols: worker types\n")
  cat("==============================================\n")
  m <- dcast(skill[county == cnty], task ~ worker_type,
             value.var = "coefficients")
  setnames(m, "task", "task\\worker")
  print(m, digits = 4)
  cat("\n")
}
