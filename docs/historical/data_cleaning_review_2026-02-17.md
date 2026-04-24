# Data Cleaning Review Report

**Date**: 2026-02-17
**Reviewer**: Claude Code
**Scope**: Raw transaction data structure and cleaning pipeline

---

## Executive Summary

Reviewed the salon transaction data pipeline from raw data (`compiled_trxns.rds`) through processed tasks (`00_tasks_cosmo.rds`) to working data (`01_working.rds`). Identified 3 critical issues, 3 moderate issues, and 4 minor improvements related to data quality and cleaning logic.

---

## Data Overview

| Stage | File | Rows | Description |
|-------|------|------|-------------|
| Raw | compiled_trxns.rds | 19.3M | All industries, all transactions |
| Filtered | (intermediate) | 12.1M | Hair salons only |
| Processed | 00_tasks_cosmo.rds | 13.7M | Melted for multi-category services |
| Working | 01_working.rds | ~100K | Staff-quarter panel |

**Date Range**: 2008-02-06 to 2021-07-31
**Locations**: 512 hair salons
**Staff**: 10,669
**Customers**: 2.1M

---

## Critical Issues

### 1. Extreme Price Values Not Filtered

**Location**: `00_mk_tasks_cosmo.R` and `01_build_data.R`

**Status**: **FIXED (Mar 2026)**

**Resolution**: The $999,999.99 row is now removed explicitly (line 70-71). Two businesses with unit errors have business-specific corrections (lines 76-81: cents→dollars). An assertion `stopifnot(tasks$price<10000)` (line 82) ensures no remaining extreme values. A general cap/winsorization was not added but the data quality tail is now bounded.

~~**Problem**~~:
- ~~7,693 rows have prices > $5,000~~
- ~~1 row has price = $999,999.99~~
- ~~These flow into revenue calculations and distort `cust_price = revenue/cust_count`~~

---

### 2. Negative Duration Not Caught Early

**Location**: `00_mk_tasks_cosmo.R`

**Status**: **FIXED (Mar 2026)**

**Resolution**: The script now explicitly validates that `duration` has no negative values: `stopifnot(uniqueN(tasks[duration<0 ,"app_id"])==0)` (line 206). The 5 negative values were in `total_app_time`, not `duration`. The script now uses `duration` directly (the `total_app_time` overwrite was removed), so the negative `total_app_time` values never propagate.

---

### 3. Duration Overwrite Logic May Inflate Averages

**Location**: `00_mk_tasks_cosmo.R:139`

**Status**: **FIXED (Mar 2026)**

**Resolution**: The `total_app_time` overwrite line (`tasks[!is.na(total_app_time), duration := total_app_time]`) has been completely removed. The script now uses the original `duration` column throughout. Assertions confirm `duration` is always available and non-negative. The average computation for multi-task allocation now uses clean `duration` values only.

> **Update (Apr 2026)**: Removing the `total_app_time` overwrite exposed a separate
> issue: 14 salons contain legacy-import data with placeholder durations (0 or 5 min)
> that the overwrite had partially masked. These are now handled by a dedicated
> legacy-import detection and drop step. See `docs/legacy_import_data_issue.md`.

---

## Moderate Issues

### 4. Hardcoded Row Numbers in Encoding Fixes

**Location**: `00_mk_tasks_cosmo.R:105-123`

**Problem**: References specific row numbers in `classified` data.table:
```r
classified[rep(6, n_fix), .SD, .SDcols = col]
classified[rep(20363, n_fix), .SD, .SDcols = col]
classified[rep(9900, n_fix), .SD, .SDcols = col]
```

These could break if `classified_descriptions.rds` is regenerated with different row ordering.

**Recommendation**: Match by service description or raw_id instead:
```r
# Instead of row number references
fix_row <- classified[raw_id == 6]  # or match by service name
tasks[service_id == "ec581975-...", (col) := fix_row[rep(1, n_fix), .SD, .SDcols = col]]
```

---

### 5. Zero Duration Imputation Edge Case

**Location**: `01_build_data.R:50-51`

**Problem**:
- ~272K rows (2%) have zero duration
- Imputation uses quarterly cluster means
- If a quarter-cluster combination has only zeros, `temp_mean_dur` is NA

**Current Code**:
```r
working[, temp_mean_dur := mean(temp_dur, na.rm=TRUE), by=c("quarter_year", "clust")]
working[, duration := ifelse(duration==0, temp_mean_dur, duration)]
```

**Recommendation**: Add fallback to global mean:
```r
working[, global_mean_dur := mean(temp_dur, na.rm=TRUE), by="clust"]
working[, duration := ifelse(duration == 0,
                             ifelse(is.na(temp_mean_dur), global_mean_dur, temp_mean_dur),
                             duration)]
```

---

### 6. ZIP-to-County Mapping Threshold

**Location**: `01_build_data.R:64`

**Problem**:
```r
data <- data[afact > 0.50 | count == 1]
```
Uses 50% threshold to map ZIPs to counties. ZIPs split across county boundaries could be misassigned to the wrong county, affecting the geographic analysis.

**Recommendation**: Consider stricter threshold:
```r
data <- data[afact > 0.75 | count == 1]  # Require 75% overlap
```

Or add logging to track ambiguous cases:
```r
ambiguous <- data[afact <= 0.75 & count > 1]
message("ZIPs with ambiguous county mapping: ", nrow(ambiguous))
```

---

## Minor Issues / Improvements

### 7. Inconsistent taskcat6 Average Computation

**Location**: `00_mk_tasks_cosmo.R:157-158`

**Problem**: `avg_task6` doesn't require `count_cat==1` unlike other categories:
```r
# taskcat1-5 use: count_cat == 1 & taskcatN == 1 & app_service_count == 1
tasks[, avg_task6 := sum(duration * (taskcat6 == 1 & app_service_count == 1)) /
                     sum((taskcat6 == 1 & app_service_count == 1))]
```

Comment says "different logic for miscellaneous category" but rationale unclear.

**Recommendation**: Document why taskcat6 is treated differently, or make consistent with other categories.

---

### 8. Pre-2015 Data Quality

**Problem**:
- ~8% of transactions (1.5M rows) are pre-2015
- Data density and quality may differ from main 2015-2021 period
- Date range starts as early as 1998

**Recommendation**: Consider date filtering if pre-2015 data is not needed:
```r
# In 00_mk_tasks_cosmo.R, after line 60
tasks <- tasks[date >= as.Date("2015-01-01"), ]
message("Filtered to post-2015: ", nrow(tasks), " rows")
```

---

### 9. Extreme Duration Values

**Problem**:
- 7,922 rows have duration > 480 minutes (8 hours)
- 32 rows have duration > 24 hours
- Maximum: 262,655 minutes (~182 days)

These seem unrealistic for single services and likely represent data entry errors.

**Recommendation**: Add duration cap similar to price cap:
```r
# In 00_mk_tasks_cosmo.R
duration_cap <- 480  # 8 hours maximum
tasks[duration > duration_cap, duration := duration_cap]
```

---

### 10. Multi-Location Staff Not Validated

**Problem**:
- 893 staff work at 3+ locations
- Maximum: 29 locations for one staff member
- Could indicate data quality issues or legitimate franchise/chain employees

**Recommendation**: Add logging to monitor:
```r
# In 00_mk_tasks_cosmo.R or 01_build_data.R
staff_loc_count <- tasks[, uniqueN(location_id), by=staff_id]
message("Staff at 1 location: ", nrow(staff_loc_count[V1 == 1]))
message("Staff at 2 locations: ", nrow(staff_loc_count[V1 == 2]))
message("Staff at 3+ locations: ", nrow(staff_loc_count[V1 >= 3]))
```

---

## Summary Table

| Priority | Issue | File | Line | Impact | Status (Mar 2026) |
|----------|-------|------|------|--------|--------------------|
| Critical | Extreme prices not filtered | 00_mk_tasks_cosmo.R | - | Distorts revenue/price metrics | **FIXED** - $1M row removed, business-specific corrections, `stopifnot(price<10000)` |
| Critical | Negative duration propagates | 00_mk_tasks_cosmo.R | 177 | Invalid data in output | **FIXED** - `stopifnot(uniqueN(tasks[duration<0])==0)` added |
| Critical | Duration overwrite timing | 00_mk_tasks_cosmo.R | 139 | Inflates task averages | **FIXED** - `total_app_time` overwrite removed entirely; uses `duration` |
| Moderate | Hardcoded row numbers | 00_mk_tasks_cosmo.R | 105-123 | Fragile to data changes | Open |
| Moderate | Zero duration edge case | 01_build_data.R | 50-51 | Potential NA values | Open |
| Moderate | ZIP threshold too loose | 01_build_data.R | 64 | County misassignment | Open |
| Minor | taskcat6 inconsistent | 00_mk_tasks_cosmo.R | 157 | Methodology unclear | Open |
| Minor | Pre-2015 data quality | 00_mk_tasks_cosmo.R | - | Data quality variance | Open |
| Minor | No duration cap | 00_mk_tasks_cosmo.R | - | Unrealistic values persist | Open |
| Minor | Multi-location staff | - | - | Unvalidated edge cases | Open |

---

## Data Quality Statistics

### Missing Values (Raw Data)
| Column | Missing Count | Percentage |
|--------|---------------|------------|
| total_app_time | 6,778,651 | 35.1% |
| location_city | 611,539 | 3.2% |
| location_state | 611,539 | 3.2% |
| location_zip | 712,423 | 3.7% |
| service_performed | 1 | 0.0% |

### Duration Distribution (Processed Data)
| Metric | Value |
|--------|-------|
| Minimum | -24,821,940 |
| Maximum | 1,440 |
| Mean | 44.8 |
| Zero count | 272,408 |
| Negative count | 5 |

### Price Distribution (Processed Data)
| Metric | Value |
|--------|-------|
| Minimum | $0 |
| Maximum | $999,999.99 |
| Mean | $81.96 |
| > $1,000 | 15,648 rows |
| > $5,000 | 7,693 rows |

---

## Appendix: Data Skill Created

A companion skill file was created at `.claude/skills/salon-transaction-data.md` documenting:
- Complete data schema
- Task category definitions
- Known data quality issues
- Processing pipeline overview
- File size and performance considerations
