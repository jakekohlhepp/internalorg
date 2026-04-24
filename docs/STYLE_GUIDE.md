# Coding Style Guide

This document captures the coding conventions used across this project.
All new and refactored code should follow these patterns.

## Language and Libraries

- R only. No Python, no shell scripts for data work.
- `data.table` for all data manipulation. Never `dplyr`/`tidyverse`.
- Use `library()` with quoted package names: `library('data.table')`.
- Load all libraries at the top of the file, before any code.
- `source('config.R')` immediately after libraries when config is needed.

## Naming

- **Variables**: lowercase `snake_case`. Examples: `staff_task`, `quarter_year`, `smooth_tot_duration`.
- **Flags/indicators**: suffix with `_flag` or use descriptive names like `is_zero`, `skip_to_next`.
- **Counts**: prefix or suffix with `count` or `_count`. Examples: `cust_count`, `quarter_count`.
- **Temporary columns**: prefix with `temp_` and remove when done: `temp_dur`, `temp_mean_dur`.
- **Loop indices**: single letters are fine: `i`, `y`, `q`, `col`, `cnty`.
- **Functions**: lowercase `snake_case`. Short, descriptive. Examples: `spec_log`, `purge_nan`, `distfun`, `within_firm_clust`.
- **Constants/config**: uppercase or in the `CONFIG` list.
- **Column references in data.table**: use string patterns with `grep`, `%like%`, or `str_detect` for programmatic column selection.

## Formatting

- Assignment: use `<-` for variable assignment. `=` is acceptable inside function arguments and `list()`.
- Spacing around `<-` is flexible. Both `x <- 5` and `x<-5` appear; either is fine.
- No strict line length limit. Long `data.table` chains and formula constructions can be one line.
- Use `##` for inline section comments. Use `####` or `###` for major section breaks.
- Blank lines between logical sections but not excessively.
- Indentation: 2 spaces inside functions and control flow. No tabs.

## data.table Patterns

These are the core idioms. Stick to them.

```r
# Modify columns by reference
dt[, new_col := expression]
dt[condition, col := value]

# Grouped operations
dt[, .(result = fun(x)), by = c("group1", "group2")]
dt[, col := fun(x), by = "group"]

# Programmatic column creation in loops
for (col in col_names) dt[, (paste0("new_", col)) := fun(get(paste0("old_", col)))]

# Column selection with patterns
dt[, .SD, .SDcols = grep("^B_raw_", names(dt))]
names(dt)[grep("^task_mix_", names(dt))]
colnames(dt)[colnames(dt) %like% "^Btilde_"]

# Deletion
dt[, col := NULL]
dt[, c("col1", "col2") := list(NULL, NULL)]
dt <- dt[, -c("drop1", "drop2")]

# Joins (merge, not data.table bracket syntax)
result <- merge(dt1, dt2, by = "key", all.x = TRUE)

# Reshaping
dcast(dt, formula, value.var = "col")
melt(dt, measure.vars = list(...))

# Keying and ordering
setkey(dt, col1, col2)
setorder(dt, "col1", "col2")

# Unique counts
uniqueN(dt$col)
uniqueN(dt[, c("col1", "col2")])
```

## Assertions and Data Checks

Use `stopifnot()` liberally after every data operation that could go wrong. This is not optional.

```r
# After merges: check no unexpected NAs
stopifnot(nrow(working_data[is.na(county)]) == 0)

# After filters: check expected row counts
stopifnot(nrow(tasks[price > 999999]) == 0)

# After reshaping: verify uniqueness
stopifnot(nrow(dt) == uniqueN(dt[, c("id1", "id2")]))

# Verify column sums
stopifnot(all(!is.na(B)))
stopifnot(all(is.finite(B)))
```

Place assertions inline, right after the operation they validate.
Do not group them at the end. The reader should see the check immediately
after the step it guards.

## Function Style

- Keep functions short and focused. One job per function.
- One-liner utility functions are fine on a single line:
  ```r
  spec_log <- function(x) ifelse(x == 0 | is.nan(x), 0, log(x))
  purge_nan <- function(x) ifelse(is.nan(x), 0, x)
  rowMax <- function(data) apply(data, 1, max, na.rm = TRUE)
  ```
- Longer functions use the same structure: args, body, return.
- Default arguments reference `CONFIG` directly: `function(config = CONFIG)`.
- Use `return()` explicitly at the end of multi-line functions.
- Helper functions defined at the top of the script that uses them, not in separate utility files (except `utils/logging.R` and `config.R`).

## Comments

- Comments explain **why**, not what. The code is the what.
- Use `##` for inline annotations on data decisions:
  ```r
  ## one business appears to have incorrect unit input
  ## prices jump from under 1000USD for extension to 2000USD for trim.
  tasks[business_id == 'eee49809-...', price := price/100]
  ```
- Use `####` or section dividers for major pipeline steps:
  ```r
  #### Step 4: Cluster workers within firm
  ```
- Comment blocks at the top of files describe purpose, inputs, outputs.
- Do not add docstrings/roxygen to internal helper functions unless the logic is genuinely non-obvious. The code should speak for itself.

## File Structure

- Each script is self-contained: loads its own libraries, sources its own config.
- Runner scripts (`run_all.R`, `run_prep_data.R`) handle logging and error catching.
- Individual scripts (`prep_01_cosmo_classify.R`, `01_build_data.R`) do the actual work and just use `message()` for status output.
- Data flows through `.rds` files in `mkdata/data/`.
- Scripts save with `saveRDS()` and load with `readRDS()` or `data.table(readRDS(...))`.
- Use `fread()` for CSV files, `read_excel()` for Excel.

## Formulas and Model Matrices

Build formulas programmatically using string manipulation:

```r
xnam <- paste0("factor(county):avg_labor:", col_names)
xnam <- as.formula(paste0("~", paste0(xnam, collapse = "+"), "-1"))
mm <- model.matrix(xnam, data = estim_matrix)
```

This pattern is used throughout the estimation code. Keep it consistent.

## Error Handling

- `stopifnot()` for hard invariants (data bugs that should never happen).
- `tryCatch()` in runner scripts to capture and log errors.
- `warning()` for non-fatal issues (convergence failures, missing optional data).
- Do not use `try()`. Use `tryCatch()` with explicit error handlers.

## Output and Printing

- `message()` for status updates (what step is running, row counts, file paths).
- `print()` for displaying results (coefficients, tables, convergence info).
- Pattern for status messages:
  ```r
  message("Filtered to ", nrow(tasks), " hair salon transactions")
  message("Saved mkdata/data/ppi.rds")
  message("  Quarters: ", nrow(ppi))
  ```

## Things to Avoid

- Never use `tidyverse`, `dplyr`, `tibble`, `purrr`, or pipe operators (`%>%`, `|>`).
- Never create a class/R6/S4 object. Use lists and data.tables.
- Don't wrap simple operations in abstractions. Three similar lines of code is better than a premature helper function.
- Don't add type annotations or elaborate docstrings to small functions.
- Don't create separate files for one-off utility functions. Define them at the top of the script that needs them.
- Don't use `tidyr::pivot_wider`/`pivot_longer`. Use `dcast`/`melt` from `data.table`.
