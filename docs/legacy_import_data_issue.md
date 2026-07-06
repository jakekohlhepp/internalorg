# Legacy Import Data Issue

**Date identified**: 2026-04-07
**Status**: Active in `00_mk_tasks_cosmo.R` (legacy_cutoffs block)
**Impact**: 388,569 rows (2.82%) across 10 locations dropped from `00_tasks_cosmo.rds`
**Detection method**: hard-coded location/transition-date list from
`docs/historical/duration_reliability_2026-04-08.md`

---

## Summary

Ten salons in the dataset carry historical transaction records that were imported
from a prior point-of-sale system when the salon migrated to the platform. These
imported records have placeholder duration values (typically 0, 5, or 15 minutes)
that do not reflect actual appointment times, producing extreme outliers in
revenue-per-minute (`rev_labor`) and distorting downstream regressions.

`00_mk_tasks_cosmo.R` drops every row dated *before* each salon's transition
month, using a hard-coded list of `(location_id, transition_start)` pairs. The
list and the transition dates come from the monthly short-duration /
platform-feature-onset analysis in `docs/historical/duration_reliability_2026-04-08.md`.

This is a more aggressive trim than an earlier quarter-level detection rule
considered (and described in a previous version of this note): the transition
rule drops *all* of a salon's pre-transition history rather than just the
quarters that individually trip a placeholder-duration mode test.

---

## How the Issue Was Discovered

The productivity-specialization regressions in the stylized-facts script showed a
bivariate S-Index coefficient of roughly 0.22 — about twice the manuscript value
of 0.11. The change traced to a single salon (`8310d870`) with
`rev_labor` of 43--55 (vs. a dataset P99 of ~8). All 10 of the top
Cook's-distance observations in the bivariate regression belonged to this one
salon.

Further analysis revealed `8310d870`'s extreme `rev_labor` came from its
pre-2018Q3 period, where appointments had `duration = 5` minutes regardless of
service type --- clearly a placeholder. The salon's post-2018Q3 data showed
normal durations (~60 min median) and normal `rev_labor` (~250).

A dataset-wide investigation in `docs/historical/duration_reliability_2026-04-08.md`
found 9 additional salons with the same "switch from a placeholder duration to
real durations at a single transition month, accompanied by the first appearance
of platform-native booking features" pattern. Across those 10 salons, the
pre-transition history accounts for 81.6% of all `duration <= 5` appointments
and 95.2% of `duration <= 5 & revenue >= $100` appointments.

---

## Evidence for Legacy Imports

The hypothesis that these are imported historical records (not ongoing recording
failures) is supported by:

### 1. Sharp transition month

For each affected salon, monthly short-duration share drops sharply from very
high to near-zero at a single month, and the share stays low for the remainder
of the salon's history.

### 2. Platform-native features turn on at the transition

`prebooked` and `was_staff_requested` are essentially absent before the
transition month and switch on at or just after it. Both fields are native to
the platform's booking system and would not exist in records imported from a
legacy POS.

### 3. Duration values consistent with unit-conversion artifacts

The pre-transition periods contain fractional duration values like 2.5, 3.17,
and 1.67 minutes that are consistent with automated unit conversions
(e.g. seconds to minutes) during import. Unaffected salons never show this
pattern.

### 4. `total_app_time` is corrupted in lockstep with `duration`

In the bad period, `total_app_time` shows the same placeholder values as
`duration`. This rules out swapping in `total_app_time` as a correction (the
old pipeline's approach) and confirms the *appointment record* is imported,
not just the duration field.

### 5. Unaffected salons start with realistic durations

Across the 498 unaffected locations, median duration in their first 90 days is
roughly 49 minutes. The 10 affected salons show roughly 5 minutes or 0 minutes
in the same window.

---

## Affected Locations (drop counts)

10 locations, 388,569 rows (2.82% of data after proportional duration allocation,
13,760,364 rows total), 310,668 appointments. Counts are post-melt: a single
multi-task appointment can produce several rows.

| Location ID (prefix) | Transition | First date | Last pre-date | Rows dropped | Appts dropped |
|---|---|---|---|---|---|
| `23468507` | 2020-02-01 | 2011-06-21 | 2020-01-31 | 173,099 | 139,669 |
| `8310d870` | 2018-09-01 | 2016-01-05 | 2018-08-31 |  56,726 |  33,916 |
| `b9f5c7cf` | 2019-08-01 | 2018-01-11 | 2019-07-31 |  34,980 |  32,315 |
| `01fbf61d` | 2020-04-01 | 2014-09-25 | 2020-03-31 |  34,036 |  28,786 |
| `fb282d29` | 2019-01-01 | 2016-04-05 | 2018-12-30 |  26,574 |  23,182 |
| `a413d2fc` | 2018-07-01 | 2015-02-12 | 2018-06-30 |  24,989 |  21,118 |
| `191172d5` | 2019-08-01 | 2017-04-14 | 2019-07-06 |  24,197 |  20,265 |
| `f44c05e5` | 2020-12-01 | 2020-01-02 | 2020-11-25 |   9,276 |   7,691 |
| `ec64ba65` | 2018-11-01 | 2016-07-10 | 2018-10-31 |   2,457 |   2,056 |
| `f4938ca8` | 2019-04-01 | 2018-08-21 | 2019-03-30 |   2,235 |   1,670 |

### Locations in the estimation sample

The estimation sample covers Cook County IL (17031), New York County NY (36061),
and Los Angeles County CA (06037), quarters 2018Q1–2021Q2 excluding 2020Q2–Q3.

Two of the affected locations sit in estimation counties and overlap the
estimation window:

- `8310d870` (New York County): pre-transition runs 2016Q1–2018Q2; the
  overlap with the estimation window is 2018Q1–Q2, two quarters that the trim
  now removes.
- `191172d5` (Chicago, IL = Cook County): pre-transition runs 2017Q2–2019Q2;
  the overlap is 2018Q1–2019Q2, six quarters that the trim now removes.

The other eight salons sit outside the estimation counties (the trim still
affects them for the full-sample stylized facts in `02_stylized_facts.R`).

---

## Detection Method (as implemented)

`00_mk_tasks_cosmo.R` does not run an in-script detection rule. It applies a
hard-coded list of 10 `(location_id, transition_start)` pairs produced by the
offline transition-analysis documented in
`docs/historical/duration_reliability_2026-04-08.md`. For each listed location,
every row with `date < transition_start` is dropped. Other locations are kept
in full.

The offline transition analysis flagged a salon as legacy-like when:

1. The salon had a very high share of `duration <= 5` appointments at the start
   of its observed history.
2. That share dropped sharply at a single month and stayed near zero afterward.
3. Platform-native features (`prebooked`, `was_staff_requested`) turned on at
   or just after that month.

The 10 salons that met all three conditions account for 81.6% of all
`duration <= 5` appointments and 95.2% of the appointments with
`duration <= 5 & revenue >= $100` --- i.e., almost all of the most suspicious
records in the raw data.

An earlier draft of this note (now superseded) proposed an automated
quarter-level detection rule (mode_pct > 0.8, mode_dur <= 15 min, plus a
"good" quarter with median >= 30 min). That rule would have flagged 14
location-quarters with 228k rows. The transition-date rule that is actually
implemented is more aggressive: it covers a different (mostly disjoint) set of
salons and removes the *entire* pre-transition history per salon rather than
just the bad quarters.

---

## Relationship to Salon `8310d870`

Salon `8310d870-fb13-414b-ba6c-2902eaf0276f` was
the outlier that originally triggered this investigation. Its pre-2018Q3 data
has `duration = 5` for most appointments, producing `rev_labor` of 43--55.

Under the implemented transition rule (`transition_start = 2018-09-01`), every
row dated before 2018-09-01 is dropped from this salon: 56,726 task-rows /
33,916 appointments, covering 2016-01-05 through 2018-08-31. The salon's
2018-09-01-onward data is retained at face value.

This makes the new pipeline strictly tougher on `8310d870` than the old
`mk_tasks_cosmo.R`, which had attempted to repair its durations by overwriting
with `total_app_time` (a fix that didn't actually work: `total_app_time` is
itself corrupted in the legacy period).

---

## Relationship to the `total_app_time` Replacement

The old pipeline (`mk_tasks_cosmo.R`) contained the line:
```r
tasks[!is.na(total_app_time), duration := total_app_time]
```
This replaced appointment-level `duration` with service-level `total_app_time`
for ~69% of rows, partially masking the legacy import issue.

The replacement was intentionally not restored in the refactor because:

1. **`total_app_time` is equally corrupted in legacy-import periods** — same
   placeholder values as `duration`, so the swap doesn't fix the bad rows.
2. **Conceptual mismatch** — `duration` is appointment-level; `total_app_time`
   is service-level. Replacing one with the other changes the meaning of the
   proportional duration allocation step.
3. **The replacement affected ~69% of all rows**, far beyond the small fraction
   that are actually problematic. It compressed the right tail of the
   `rev_labor` distribution for reasons unrelated to data quality.
4. **Availability cutoff** — `total_app_time` drops to 0% from 2020Q4 onward,
   creating a structural break in which rows receive the "correction".

The targeted transition-date drop addresses the root cause (imported data with
no real durations) directly, without altering the ~97% of rows that have valid
duration data.

---

## Implementation

In `00_mk_tasks_cosmo.R`, after proportional duration allocation (the melt
step) and before the final task-category cleanup:

```r
n_before_legacy <- nrow(tasks)
legacy_cutoffs <- data.table(
  location_id = c(
    "8310d870-fb13-414b-ba6c-2902eaf0276f",
    "191172d5-2e41-4cf7-b94f-5b15145f18ec",
    "b9f5c7cf-38d0-42ec-9669-2304d7abfedd",
    "f44c05e5-5c88-4ab5-99ab-32ac1ee1d470",
    "23468507-cec2-458d-a053-ac09060d4721",
    "01fbf61d-01ef-4c3a-9a88-3e64cc8f99fe",
    "fb282d29-cc1f-41ab-87b1-b33cdba2ea3f",
    "ec64ba65-7a8e-49be-a8ca-e04dd0be4ff2",
    "a413d2fc-b120-4b5f-b13c-c5fac327866d",
    "f4938ca8-a8cf-49a7-9358-1df7ca32e336"
  ),
  transition_start = as.Date(c(
    "2018-09-01", "2019-08-01", "2019-08-01", "2020-12-01", "2020-02-01",
    "2020-04-01", "2019-01-01", "2018-11-01", "2018-07-01", "2019-04-01"
  ))
)

tasks <- merge(tasks, legacy_cutoffs, by = "location_id", all.x = TRUE)
n_legacy <- nrow(tasks[!is.na(transition_start) & date < transition_start])
tasks <- tasks[is.na(transition_start) | date >= transition_start]
tasks[, transition_start := NULL]

message("Dropped ", n_legacy, " legacy-import rows (",
        round(100 * n_legacy / n_before_legacy, 2), "%) from ",
        nrow(legacy_cutoffs), " locations")
```

The run-time message reports the values cited above (388,569 rows, 2.82%, 10
locations) when run against the current `compiled_trxns.rds`.

---

## Verification (run 2026-05-15)

Re-running `02_stylized_facts.R` against the post-trim `00_tasks_cosmo.rds`:

- The full-sample stylized-facts panel has 4,495 firm-quarters
  (manuscript: 4,599; loss of 104 firm-quarters from the 10-salon
  pre-transition history that is now excluded).
- Labor-productivity dispersion (`results/out/tables/02_dispersion.tex`):
  max `rev_labor` is **15.04**, down from the manuscript's 42.80 (which was
  pinned at the old `rev_labor < 42.80` cap, dominated by `8310d870`'s
  pre-2018Q3 placeholder durations). Mean falls from 1.81 to 1.61; median
  is essentially unchanged (1.38 → 1.35); P75 1.94 vs. 2.05. S-index
  distribution is unchanged.
- Bivariate `std_rev_labor ~ std_sindex` (col 1 of
  `02_productivity_sindex.tex` vs. `01_01_productivity_sindex.tex`):
  - manuscript: 0.110* (SE 0.056), R² 0.058
  - new      : 0.283*** (SE 0.050), R² 0.076
- The coefficient is roughly 2.6× the manuscript value, more precisely
  estimated, and stays in the 0.17–0.31 range across all seven fixed-effect
  specifications (vs. 0.07–0.14 in the manuscript). The earlier expectation
  that the coefficient would "return to ~0.11 after the trim" (a guess based
  on an intermediate state of the refactor) was wrong: the manuscript's small
  coefficient was depressed by the `total_app_time` overwrite that compressed
  the right tail of `rev_labor`. After the swap is removed AND the legacy
  pre-period is dropped, the S-Index/productivity relationship is strictly
  stronger than in the published paper.
- Teamwork bivariate coefficient (col 1 of `02_productivity_teamwork.tex`):
  0.066** → 0.239***, same direction.
- S-Index → teamwork coefficient in `02_management_practices.tex` (col 1)
  is essentially unchanged: 0.6551 → 0.6564.

The drop counts above come from re-running the `00_mk_tasks_cosmo.R`
preprocessing through line 320 against the current `compiled_trxns.rds`
(2026-05-15). To re-verify, run `00_mk_tasks_cosmo.R` and look for the
`message("Dropped X legacy-import rows ...")` line near the end of the legacy
block.
