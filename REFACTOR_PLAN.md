# Refactoring Plan: `01_build_data.R` + `cluster.R`

## Audit Summary

`01_build_data.R` and `cluster.R` together form a 600+ line pipeline that is correct and well-commented, but structured as a single imperative script relying heavily on shared global state. The issues below are ordered by impact.

---

## Issue 1 — `cluster.R` is not a module; it shares global environment with `01_build_data.R`

**What:** `cluster.R` is sourced via `source('cluster.R')` at the end of `01_build_data.R`. It uses objects (`staff_task`, `staffnum_xwalk`, `CONFIG`, and several helper functions) that must already exist in the calling environment. There is no function boundary or argument passing.

**Why it matters:**
- `cluster.R` cannot be run, tested, or reasoned about in isolation.
- The `rm(list=setdiff(...))` on line 212 of `01_build_data.R` manually lists every object that `cluster.R` needs; if a new dependency is added to `cluster.R`, this line silently fails unless the list is updated.
- It is also used by the bootstrap script, which makes this fragility worse.

**Fix:** Convert the main logic of `cluster.R` into one or more explicit functions that accept `staff_task` and `staffnum_xwalk` as arguments and return results. The `source('cluster.R')` call becomes a function call. Remove the `rm(list=setdiff(...))` guard entirely.

```r
# Before (in 01_build_data.R)
rm(list=setdiff(ls(), c("staff_task", "staffnum_xwalk", "CONFIG", ...)))
source('cluster.R')

# After
source('cluster.R')   # loads functions only
result <- run_clustering_pipeline(staff_task, staffnum_xwalk, CONFIG)
verywide_expanded <- result$verywide_expanded
```

---

## Issue 2 — 15+ duplicated `for`-loop column-pattern blocks

**What:** The pattern of iterating over dynamically named columns appears throughout both scripts:

```r
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) {
  staff_task[,(paste0("B_",col)) := get(paste0("duration_",col))/smooth_tot_duration]
}
```

This pattern appears at least 15 times with slight variations (`duration_`, `smooth_duration_`, `B_`, `Btilde_`, `ratio_`, `raw_fracs_`, etc.).

**Why it matters:** Each instance is 1–2 lines but hard to read, error-prone to modify (the prefix must match in both the `gsub` and `paste0`), and the intent is buried in mechanics.

**Fix:** Extract a helper:

```r
#' Apply a column-wise transformation using a prefix pattern
#' @param dt data.table to modify in place
#' @param in_prefix Prefix of source columns (e.g., "duration_")
#' @param out_prefix Prefix of destination columns (e.g., "B_")
#' @param fn Function taking (dt, col_suffix) -> vector
transform_cols <- function(dt, in_prefix, out_prefix, fn) {
  cols <- grep(paste0("^", in_prefix), names(dt), value=TRUE)
  suffixes <- gsub(paste0("^", in_prefix), "", cols)
  for (s in suffixes) dt[, (paste0(out_prefix, s)) := fn(dt, s)]
  invisible(dt)
}
```

---

## Issue 3 — Gamma estimation loop is nearly duplicated

**What:** In `cluster.R`, the gamma estimation loop body (including the inner functions `simil_function`, `wrapper_func`, `find_vecmax`, `recurse_down`, `outer_check`) appears twice:
- Lines 236–272: runs only on 2019, for display purposes (network plot).
- Lines 274–387: runs on all years, computes `gamma_normalized`.

The inner functions are re-defined inside the `for (cnty in ...)` loop on both passes.

**Fix:** Define `simil_function`, `wrapper_func`, `find_vecmax`, `recurse_down`, and `outer_check` once outside the loop. Separate the display/plot logic from the estimation logic. The two loops can then share the same inner functions.

---

## Issue 4 — `quarter_list` is hard-coded in `01_build_data.R`

**What:** Line 164:
```r
quarter_list <- c(2018.1, 2018.2, 2018.3, 2018.4, 2019.1, 2019.2, 2019.3, 2019.4, 2020.1, 2020.4, 2021.1, 2021.2)
```

This is a model parameter (COVID quarters 2020 Q2/Q3 are excluded) that should be centralized.

**Fix:** Add to `CONFIG`:
```r
estimation_quarters = c(2018.1, 2018.2, 2018.3, 2018.4,
                         2019.1, 2019.2, 2019.3, 2019.4,
                         2020.1,          2020.4,           # exclude Q2/Q3 COVID
                         2021.1, 2021.2)
```

---

## Issue 5 — Diagnostic / exploratory code mixed with production code

**What:** Several calls are clearly diagnostic and not part of the data pipeline:
- `nrow(working[duration==0,])/nrow(working)` (line 45) — prints a fraction, no assignment
- `print(nrow(temp[is_zero==1]))` (line 79)
- `hist(...)` and `print(table(...))` in `cluster.R` (lines 25–26)
- `uniqueN(...)` on lines 127, 128 (print-only, no assignment)
- Commented-out `#View(...)` and `#png(...)` blocks
- `for (cnty in CONFIG$counties_padded) print(max(...))` (cluster.R line 20)

**Fix:** Wrap in `if (CONFIG$verbose_logging)` or consolidate into a summary logging step at the end of each stage. The `CONFIG$verbose_logging` flag already exists for this purpose.

---

## Issue 6 — `01_build_data.R` has no function structure

**What:** The script is ~220 lines of top-level imperative code with a single comment-based section structure. There are no function definitions (except `spec_log`, `purge_nan`, `distfun`, `within_firm_min`).

**Fix:** Wrap each pipeline stage in a function. This makes each stage independently testable and the top-level script a readable sequence of calls:

```r
source('config.R')
source('cluster.R')

working       <- load_and_clean_data(CONFIG)
staff_task    <- build_staff_task_matrix(working, CONFIG)
staff_task    <- apply_smoothing_and_filter(staff_task, CONFIG)
verywide_expanded <- run_clustering_pipeline(staff_task, staffnum_xwalk, CONFIG)
saveRDS(verywide_expanded, "mkdata/data/01_working.rds")
```

---

## Issue 7 — `smooth_e_frac` initialized to 0 and accumulated in a loop, but never reset between uses

**What:** `smooth_e_frac` is set to 0 and then accumulated across task columns in a loop (appears in both `01_build_data.R` and `cluster.R`). This works but is confusing — it looks like a running total that could accumulate incorrectly if the loop runs twice.

**Fix:** Replace with `rowSums`:
```r
staff_task[, smooth_e_frac := rowSums(.SD), .SDcols = grep("^B_", names(staff_task))]
```

---

## Suggested Refactoring Order

| Priority | Issue | Effort | Risk |
|----------|-------|--------|------|
| 1 | Issue 4: move `quarter_list` to CONFIG | Low | None |
| 2 | Issue 5: wrap diagnostics in `verbose_logging` | Low | None |
| 3 | Issue 7: replace `smooth_e_frac` loop with `rowSums` | Low | Low |
| 4 | Issue 2: extract `transform_cols` helper | Medium | Low |
| 5 | Issue 3: deduplicate gamma loop | Medium | Medium |
| 6 | Issue 6: wrap stages in functions | Medium | Medium |
| 7 | Issue 1: convert `cluster.R` to a proper module | High | High — bootstrap script depends on current behavior |

Issue 1 (cluster.R as module) should be done last because `cluster.R` is also sourced by the bootstrap script, and changing its interface requires updating both callers simultaneously.
