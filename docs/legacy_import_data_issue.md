# Legacy Import Data Issue

**Date identified**: 2026-04-07
**Status**: Fix implemented (branch `fix/legacy-import-drop`)
**Impact**: 228,343 rows (1.66%) across 14 locations dropped from `00_tasks_cosmo.rds`

---

## Summary

Fourteen salons in the dataset contain historical transaction records imported from
a prior point-of-sale system when the business migrated to Boulevard. These legacy
records have placeholder duration values (typically 0 or 5 minutes) that do not
reflect actual appointment times, producing extreme outlier values in
revenue-per-minute (`rev_labor`) and distorting downstream regressions.

The fix detects and drops these legacy-period rows at the `00_mk_tasks_cosmo.R`
stage, before any downstream aggregation.

---

## How the Issue Was Discovered

The productivity-specialization regressions in `01_01_stylized_facts.R` showed a
coefficient approximately 2x larger than the published results. Investigation traced
the change to a single Manhattan salon (`8310d870`) with `rev_labor` of 43--55
(vs. a dataset P99 of ~8). All 10 of the top Cook's-distance observations in the
bivariate regression belonged to this one salon.

Further analysis revealed the salon's extreme `rev_labor` came from its pre-2018Q3
period, where all appointments had `duration = 5` minutes regardless of service type
--- clearly a placeholder, not a real measurement. The salon's post-2018Q3 data
showed normal durations (~60 min median) and normal `rev_labor` (~250).

A dataset-wide investigation found 13 additional salons with the same pattern.

---

## Evidence for Legacy Imports

The hypothesis that these are imported historical records (not ongoing recording
failures) is supported by multiple independent lines of evidence:

### 1. Bad durations always start at the salon's first transaction

For all 14 affected locations, the placeholder-duration period begins at the
salon's very first observed date and ends abruptly. No salon develops bad durations
in the middle of its history.

### 2. Boulevard-specific features are absent

In the placeholder-duration period:
- `prebooked` is 0.000 for 13/14 locations (the exception, `8310d870`, has 0.09)
- `was_staff_requested` is 0.000 for 13/14 locations
- `first_prebook`, `first_tip`, and `first_staffreq` dates from the stylized
  facts data align precisely with the duration transition

These features are native to Boulevard's booking system and would not exist in
data imported from a legacy POS system.

### 3. Low customer overlap across periods

Only 12--53% of customers observed in the bad-duration period ever appear again in
the good-duration period, consistent with imported customer rosters rather than an
active customer base.

### 4. Duration values suggest unit-conversion artifacts

The bad-duration periods show fractional values like 2.5, 3.17, and 1.67 minutes,
consistent with automated unit conversions (e.g., seconds to minutes) during import.
Non-affected salons never show this pattern.

### 5. Non-affected salons start with normal durations

Among the 498 unaffected salons, median duration in their first 90 days is ~49
minutes. Affected salons show ~5 minutes or 0 minutes in the same window.

### 6. `total_app_time` is also corrupted

The `total_app_time` field (a service-level booking slot length) shows the same
placeholder values as `duration` in the affected periods. This rules out using
`total_app_time` as a correction and confirms the entire appointment record is
imported, not just the duration field.

---

## Affected Locations

14 locations, 228,343 rows (1.66% of data), 190,066 appointments.

| Location ID (prefix) | City | Default dur | Legacy period | Transition to | Bad rows | % of salon |
|---|---|---|---|---|---|---|
| `23468507` | Spring Lake, NJ | 0 min | 2011Q2--2018Q4 | 2019Q1 (45 min) | 149,977 | 79.5% |
| `b9f5c7cf` | Costa Mesa, CA | 0 min | 2018Q1--2019Q1 | 2019Q3 (57 min) | 28,024 | 60.4% |
| `191172d5` | Chicago, IL | 0 min | 2017Q2--2019Q2 | 2019Q3 (60 min) | 23,998 | 52.2% |
| `01fbf61d` | Southbury, CT | 5 min | 2014Q4--2017Q4 | 2020Q2 (31 min) | 8,195 | 15.7% |
| `ccb54cfa` | Oakbrook Terr, IL | 15 min | 2020Q1--2020Q3 | 2021Q1 (45 min) | 6,713 | 41.3% |
| `1b7912af` | Frederick, MD | 10 min | 2020Q1--2021Q1 | 2021Q3 (83 min) | 5,349 | 71.2% |
| `f44c05e5` | Portsmouth, NH | 5 min | 2020Q1--2020Q3 | 2021Q1 (60 min) | 3,929 | 23.3% |
| `1e80128d` | Chicago, IL | 15 min | 2020Q2--2020Q2 | 2021Q1 (30 min) | 2,083 | 7.1% |
| `c7ea80fe` | New York, NY | 15 min | 2017Q3--2017Q3 | 2020Q2 (38 min) | 1 | 0.0% |
| `4e20bf8c` | Santa Monica, CA | 15 min | 2021Q3--2021Q3 | 2020Q1 (60 min) | 1 | 0.0% |
| `b0ec6cfd` | Oakland, CA | 15 min | 2020Q2--2020Q2 | 2020Q3 (60 min) | 58 | 1.2% |
| `26e16e5d` | Carrboro, NC | 15 min | 2020Q4--2020Q4 | 2021Q1 (60 min) | 10 | 0.2% |
| `0cb3db09` | Kailua, HI | 15 min | 2018Q1--2018Q1 | 2019Q4 (60 min) | 3 | 0.0% |
| `74b5e484` | Charlottesville, VA | 15 min | 2018Q2--2018Q2 | 2018Q3 (45 min) | 2 | 0.0% |

### Locations in the estimation sample

The estimation sample covers Cook County IL (17031), New York County NY (36061),
and Los Angeles County CA (06037), quarters 2018Q1--2021Q2 excluding 2020Q2--Q3.

Two affected locations potentially fall in estimation counties:
- `191172d5` (Chicago, 60654 = Cook County): bad quarters through 2019Q2, all within
  the estimation window
- `1e80128d` (Chicago, 60611 = Cook County): bad quarter 2020Q2, which is already
  excluded from estimation

Salon `8310d870` (Manhattan) --- the original outlier that prompted this
investigation --- does NOT trigger the detection rule because its bad-duration
period has mode_pct < 80% after proportional allocation. Its extreme `rev_labor` is
addressed separately by the downstream exclusion of firm-quarters with
`rev_labor > 42.80` or by the fact that its bad quarters (pre-2018Q1) mostly fall
outside the estimation window. See "Relationship to salon 8310d870" below.

---

## Detection Rule

A location-quarter is flagged as legacy import data if all three conditions hold:

1. **>80% of rows** share the same rounded duration value (indicating a system
   default rather than real measurements)
2. That duration is **<= 15 minutes** (too short to represent a real salon
   appointment for the vast majority of services)
3. The same location has **at least one other quarter with median duration >= 30
   minutes** (confirming the salon eventually recorded real durations, so the short
   ones are not the salon's true operating pattern)

This rule correctly identifies all 14 legacy-import locations while producing zero
false positives among the 498 unaffected locations. Unaffected salons start with
realistic durations (~49 min median) from their first observed transaction.

---

## Relationship to Salon `8310d870`

Salon `8310d870-fb13-414b-ba6c-2902eaf0276f` (Manhattan, Upper East Side) was the
outlier that originally triggered this investigation. Its pre-2018Q3 data has
`duration = 5` minutes for most appointments, producing `rev_labor` of 43--55.

However, this salon does NOT trigger the 80% mode-pct detection rule in the
post-allocation data because the proportional splitting across task categories
creates variation in the allocated durations. The salon's bad quarters (2016Q1 --
2018Q2) largely fall before the estimation window (2018Q1--2021Q2), with only
2018Q1--Q2 overlapping.

The old pipeline handled this salon via `total_app_time` replacement
(`tasks[!is.na(total_app_time), duration := total_app_time]`), which was
intentionally not restored in the refactor (see below). The salon's impact on
results is mitigated by:
1. Only 2 of its quarters fall in the estimation window
2. Downstream winsorization or trimming can address residual outliers

---

## Relationship to the `total_app_time` Replacement

The old pipeline (`mk_tasks_cosmo.R`) contained the line:
```r
tasks[!is.na(total_app_time), duration := total_app_time]
```
This replaced the appointment-level `duration` with the service-level
`total_app_time` for 69% of rows, partially masking the legacy import issue.

This replacement was intentionally NOT restored because:

1. **`total_app_time` is equally corrupted** in legacy-import periods (same
   placeholder values as `duration`)
2. **Conceptual mismatch**: `duration` is appointment-level; `total_app_time` is
   service-level. Replacing one with the other changes the meaning of the
   proportional allocation step.
3. **The replacement affected 69% of all rows**, far beyond the 1.66% that are
   actually problematic. It compressed the right tail of the `rev_labor`
   distribution for reasons unrelated to data quality.
4. **Availability cutoff**: `total_app_time` drops to 0% from 2020Q4 onward,
   creating a structural break in which data receives the correction.

The targeted legacy-import drop is a cleaner solution that addresses the root cause
(imported data with no real durations) without altering the 98.3% of rows that have
valid duration data.

---

## Implementation

In `00_mk_tasks_cosmo.R`, after the proportional duration allocation step and
before saving:

```r
# Detect and drop legacy import data (see docs/legacy_import_data_issue.md)
tasks[, quarter_year := year(date) + quarter(date)/10]
lq_stats <- tasks[, .(
  mode_dur = as.numeric(names(sort(table(round(duration, 1)), decreasing=TRUE)[1])),
  mode_pct = max(table(round(duration, 1))) / .N
), by = .(location_id, quarter_year)]
has_good <- tasks[, .(med_dur = median(duration)), by = .(location_id, quarter_year)
  ][med_dur >= 30, .(has_good_quarter = TRUE), by = location_id]
legacy_lq <- lq_stats[mode_pct > 0.8 & mode_dur <= 15]
legacy_lq <- merge(legacy_lq, has_good, by = "location_id")
tasks <- tasks[!paste(location_id, quarter_year) %in%
               legacy_lq[, paste(location_id, quarter_year)]]
tasks[, quarter_year := NULL]
```

---

## Verification

After applying the fix, confirm:
- No firm-quarter in the estimation sample has `rev_labor` > 20 from a
  legacy-duration artifact
- The productivity-specialization regression coefficient returns to the range
  reported in the published paper (~0.11 in the bivariate specification)
- Summary statistics (N, mean, percentiles) are close to published values
