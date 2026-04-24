# Refactoring Plan: Analysis Scripts (01_01, 01_02, 01_03)

*Last updated: 2026-03-12 (Opus review)*

## Current State

All three scripts are pre-refactoring. They were written before the main pipeline
was restructured and have never been updated to match it. Key symptoms:

| Problem | 01_01 | 01_02 | 01_03 |
|---------|-------|-------|-------|
| Wrong input paths (`analysis_final/`, `tasks_cosmo.rds`) | yes | yes (via 01_01 output) | yes |
| Wrong output paths (`analysis_final/out/`) | yes | yes | yes |
| Duplicates preprocessing from `01_build_data.R` | yes | yes (ZIPâ†’county again) | no |
| Hard-coded absolute paths to raw data | yes | no | no |
| Not wired into `run_all.R` | yes | yes | yes |
| Reads an intermediate that may not exist at correct path | yes | yes | yes |
| Missing library dependencies not in `renv.lock` | yes | yes | no |

---

## Dependency Map

```
00_tasks_cosmo.rds  â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”
                                                                   â”‚
01_staff_task_full.rds (from 01_build_data.R)  â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–ºâ”‚
                                                                   â–¼
[prep: tips, chair renters, products]  â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–º 01_01_stylized_facts.R
                                                                   â”‚
                                                       saves: 01_01_stylized_facts_data.rds
                                                                   â”‚
                                                     â”Œâ”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”˜
                                                     â–¼
                                ZCTA shapefiles â”€â”€â–º 01_02_spatial_corr.R
                                                     â”‚
                                              saves: 9 map PNGs + 1 coverage map

01_staff_task_full.rds  â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â–º 01_03_summary_stats_esample.R
                                                                   â”‚
                                                            saves: 01_03_summary_stats_structural.tex
```

`01_02` depends on `01_01`'s output. `01_03` is fully independent of `01_01` and `01_02`.

---

## Issues in Detail

### Issue A â€” Input file path mismatches

| Script | Current path | Correct path |
|--------|-------------|--------------|
| `01_01` line 40 | `mkdata/data/tasks_cosmo.rds` | `mkdata/data/00_tasks_cosmo.rds` |
| `01_01` line 240 | `analysis_final/data/01_00_staff_task_full.rds` | `mkdata/data/01_staff_task_full.rds` |
| `01_02` line 10 | `analysis_final/data/01_01_stylied_facts_data.rds` | `results/data/01_01_stylized_facts_data.rds` |
| `01_03` line 5 | `analysis_final/data/01_00_staff_task_full.rds` | `mkdata/data/01_staff_task_full.rds` |

### Issue B â€” Output directory mismatches

All scripts write to `analysis_final/out/tables/` and `analysis_final/out/figures/`.
These should be `results/out/tables/` and `results/out/figures/`. Neither subdirectory
currently exists under `results/out/`; they need to be created (or created on the fly
by the scripts using `dir.create(..., recursive=TRUE)`).

`01_01` also saves its intermediate to `analysis_final/data/01_01_stylied_facts_data.rds`
(note typo: "stylied"). The corrected path is `results/data/01_01_stylized_facts_data.rds`.

### Issue C â€” Duplicated preprocessing in `01_01`

Lines 43â€“91 of `01_01_stylized_facts.R` re-run:
- Task cluster 3â†’4 merge
- Zero-duration imputation
- ZIP â†’ county mapping (full GEOCORR logic)
- Zero-revenue firm-quarter drop

This is identical to `01_build_data.R` lines 33â€“83. However, `01_01` cannot simply
read from `01_staff_task_full.rds` and skip this, because it needs the **raw
transaction-level data** to build visit-level variables (repeat customers, pre-booking,
tips, multi-staff visits, etc.) that are not in any current pipeline output.

**Resolution:** Extract the shared cleaning steps (cluster merge, duration impute,
ZIPâ†’county, revenue filter) into a shared helper in `utils/clean_transactions.R`.
Both `01_build_data.R` and `01_01` call it. The ZIPâ†’county GEOCORR logic in
particular appears in `01_build_data.R`, `01_01`, and `01_02` (three copies).

**Important caveat:** `01_02` re-runs the ZIPâ†’county mapping (lines 40â€“49) but
does so for a different reason: it needs to join the GEOCORR zip-to-county mapping
to the ZCTA shapefile (`my_sf`) for the spatial plots. However, since `01_01`'s
output already contains `county` at the zip level, `01_02` can get county from
there instead of reloading the GEOCORR files â€” but only for zips in the data.
For the shapefile join (all zips nationally), it still needs the GEOCORR crosswalk.
So the GEOCORR block in `01_02` **cannot be fully removed**; it can be replaced
with a shared helper that returns the zipâ†’county mapping.

### Issue D â€” Hard-coded absolute raw data paths in `01_01`

`01_01` reads six data sources at paths beginning with
`C:/Users/jakek/blvd_dont_backup/`. These are:

| Data | Lines | Description |
|------|-------|-------------|
| Chair renters | 157 | `20201204_chair_renters/staff_renters.csv` |
| Tip data pull 1 | 167â€“171 | `20200909_raw/Marketing Intern Data/*.csv` (multiple files via `list.files`) |
| Tip data pull 2 | 175 | `20201214_tip_more/bi_tip_lines_2017-2020.csv` |
| Tip data pull 3 | 177 | `20210809_alldata_refresh_withzip/tip_amount_export.csv` |
| Product/discount data 1 | 297 | `20210809_alldata_refresh_withzip/product_sales_export.csv` |
| Product/discount data 2 | 299 | `2017_2020_product_sales/2017_2020_product_sales.csv` |

Note that `prep_00_compile_transactions.R` already reads from two of the same
`blvd_dont_backup` subdirectories (`20200909_raw/Marketing Intern Data/` and
`20210809_alldata_refresh_withzip/`). The existing `run_prep_data.R` already
resolves these paths via `CONFIG$raw_data_base`. So the pattern is established;
these additional data sources just need to follow it.

These need to be handled in one of two ways:
- **Option A (preferred):** Add prep scripts that read from `CONFIG$raw_data_base`,
  clean the data, and write `.rds` files to `mkdata/data/`. `01_01` then reads those.
- **Option B (minimal):** Replace the hard-coded paths with
  `file.path(CONFIG$raw_data_base, ...)`, keeping the loading logic inside `01_01`
  but making the base path configurable.

Option A is consistent with how qcew, CEX, and census data are handled. But the
tip-merging logic in `01_01` (lines 167â€“213) is complex â€” it reconciles three
overlapping data pulls with unit conversion (cents vs dollars) and conflict
resolution. Moving this into a prep script means the prep script inherits that
complexity, which is fine but non-trivial.

`01_01` also makes a **live API call** to `tidycensus` (lines 593â€“604) to pull
ACS zip-code income data for 2011â€“2021. This should be extracted into a prep
script with a cached `.rds` output, for reproducibility and to avoid network
dependency at analysis time. Note: the ACS data is only used for a single
regression at the very end (lines 606â€“609) â€” it may be worth asking whether
this regression is still needed before writing the prep script.

### Issue E â€” Negative-duration assertion mismatch

`01_01` line 52: `stopifnot(nrow(working[duration<0])==5)` â€” expects 5 rows.
`01_build_data.R` line 42: `stopifnot(nrow(working[duration<0])==0)` â€” expects 0.

These scripts read from **different input files**:
- `01_01` reads `tasks_cosmo.rds` (old name, probably an older data extract)
- `01_build_data.R` reads `00_tasks_cosmo.rds` (current pipeline output)

The discrepancy likely reflects that the older raw data had 5 negative-duration rows
which were subsequently cleaned upstream (in `00_mk_tasks_cosmo.R` or
`prep_00_compile_transactions.R`). Once `01_01` is updated to read
`00_tasks_cosmo.rds`, the negative durations should already be gone, and the
assertion should change to match `01_build_data.R` (expect 0).

**Action:** After the path fix, verify by inspecting `00_tasks_cosmo.rds` and
update the assertion accordingly. If negative durations still exist, the cleaning
should happen in the shared helper from Issue C.

### Issue F â€” `01_01` reads the *full-sample* `staff_task_full`, not the filtered one

`01_01` reads `s_index` and `task_mix_*` from `01_staff_task_full.rds` (line 240),
which covers all firms (not just the 3-county estimation sample). This is intentional
â€” the stylized facts section covers the full national sample. `01_build_data.R`
saves `01_staff_task_full.rds` before any county/quarter filtering (line 162), so
this file is the correct one to use. No change needed here except the path fix.

### Issue G â€” `01_03` hard-codes the `quarter_list`

`01_03` line 14 repeats the same hard-coded `quarter_list` as `01_build_data.R`.
Once `quarter_list` is moved to `CONFIG` (see `REFACTOR_PLAN.md` Issue 4),
`01_03` should use `CONFIG$estimation_quarters`.

**Review follow-up (2026-04-13):** This issue was re-raised in code review after
the analysis-script refactor work. The concern is configuration drift between
`01_build_data.R` and `01_03_summary_stats_esample.R` if the estimation window
changes in one place but not the other. This note is documentation only; no code
change is implied by this entry.

### Issue H â€” `01_02` county padding inconsistency

`01_02` line 49 does `county := as.character(as.integer(county))`, which strips
leading zeros. This is why it then compares against `'6037'` instead of `'06037'`
throughout the script. `CONFIG$counties_padded` uses `"06037"`.

When the GEOCORR block in `01_02` is replaced with a shared helper, the county
values should stay zero-padded. The hardcoded `'6037'`, `'36061'`, `'17031'`
filter strings should be replaced with `CONFIG$counties` (which already strips
leading zeros: `c("17031", "36061", "6037")`) â€” or better, the script should
use padded counties consistently and compare against `CONFIG$counties_padded`.

**Note:** `CONFIG` actually has **both** `counties` and `counties_padded` â€”
they differ only for LA County (`"6037"` vs `"06037"`). This dual representation
is itself a smell; consider normalizing to always use padded 5-digit FIPS codes
and having only one list.

### Issue I â€” `01_02` plot code is massively repetitive

`01_02` generates 9 county-variable maps by copy-pasting a ~10-line block for
each of 3 counties Ã— 3 variables (`rev_labor`, `s_index`, `return_cust`). This
should be a loop or a function:

```r
plot_county_map <- function(sf_data, county_code, variable, filename) {
  county_sf <- sf_data[sf_data$county == county_code & sf_data$coverage == 1, ]
  vals <- county_sf[[variable]]
  cats <- frank(vals, na.last = FALSE, ties.method = "dense")
  my_colors <- c("white", colorRampPalette(c("white", "black"))(max(cats, na.rm=TRUE) - 1))
  my_colors <- my_colors[cats]
  png(filename, width = 700, height = 700)
  plot(st_geometry(county_sf), col = my_colors, bg = "white", lwd = 0.25)
  dev.off()
}

for (cnty in CONFIG$counties_padded) {
  for (var in c("rev_labor", "s_index", "return_cust")) {
    plot_county_map(my_sf, cnty, var, sprintf("results/out/figures/01_02_spatial_%s_%s.png", cnty, var))
  }
}
```

### Issue J â€” Missing R packages not in `renv.lock`

`01_01` loads libraries not currently in the `run_all.R` package list:
`qdapDictionaries`, `quanteda`, `DescTools`, `stringdist`, `knitr`, `kableExtra`,
`fixest`, `pander`, `stargazer`, `showtext`, `mediation`, `tidycensus`.

`01_02` loads: `sf`, `ggthemes`.

These need to be added to `renv.lock` (via `renv::install()` + `renv::snapshot()`)
before the scripts can run in the managed environment.

### Issue K â€” `01_01` Kentucky salon exclusion is duplicated

Both `01_01` (line 320) and `01_03` (line 9) hard-code the exclusion of salon
`fb686b3a-a166-469b-88ea-3467a68e2f53`. This should be centralized â€” either as
`CONFIG$excluded_locations` or in the shared cleaning helper.

### Issue L â€” `01_01` excludes quarter 2021.3

`01_01` line 322: `firm_quarter <- firm_quarter[quarter_year != 2021.3,]`. This
is a partial-quarter exclusion that is separate from the COVID exclusion in
`01_build_data.R`. If the estimation pipeline already excludes this quarter
(it's not in the `quarter_list`), this is only relevant for the full-sample
stylized facts. Should be documented or moved to CONFIG.

---

## Step-by-Step Tasks

Tasks are ordered so that each step is unblocked by prior ones.

### Step 0 â€” Prerequisites from `REFACTOR_PLAN.md`

Before touching the analysis scripts, complete these from the `01_build_data.R`
refactoring plan:

- **Issue 4:** Move `quarter_list` to `CONFIG$estimation_quarters`. This unblocks
  `01_03` and keeps the quarter filter in one place.
- **Issue 5:** Wrap diagnostics in `CONFIG$verbose_logging`. Sets the pattern for
  the analysis scripts.

### Step 1 â€” Create output directories and add packages

Create `results/out/tables/`, `results/out/figures/`, `results/data/`.

Best done once in `run_all.R` at the top, alongside the existing `CONFIG$log_dir`
creation:

```r
ensure_directory("results/out/tables")
ensure_directory("results/out/figures")
ensure_directory("results/data")
```

Install and snapshot the additional packages (Issue J):
```r
renv::install(c("fixest", "stargazer", "sf", "ggthemes", "showtext",
                "knitr", "kableExtra", "pander", "qdapDictionaries",
                "quanteda", "DescTools", "stringdist", "mediation",
                "tidycensus"))
renv::snapshot()
```

### Step 2 â€” Extract shared cleaning helper (Issue C)

Create `utils/clean_transactions.R` with:

```r
#' Clean raw transaction data (shared between 01_build_data.R and 01_01)
#'
#' Steps:
#'   1. Merge task cluster 3 â†’ 4
#'   2. Drop negative durations / impute zero durations
#'   3. ZIP â†’ county mapping via GEOCORR
#'   4. Merge county population
#'   5. Drop zero-revenue firm-quarters
#'
#' @param working data.table of raw transactions (from 00_tasks_cosmo.rds)
#' @param config CONFIG list
#' @return Cleaned data.table with county, quarter_year, year columns added
clean_raw_transactions <- function(working, config) { ... }

#' Load ZIP â†’ county crosswalk from GEOCORR files
#'
#' @return data.table with columns: location_zip (numeric), county (character, padded)
load_zip_county_xwalk <- function() { ... }
```

Update `01_build_data.R` to call `clean_raw_transactions()` instead of inline code.

### Step 3 â€” Handle extra raw data (Issue D)

**Recommendation: Option B first, Option A later.** Option B (replace hard-coded
paths with `file.path(CONFIG$raw_data_base, ...)`) can be done in minutes and
unblocks all downstream work. Option A (separate prep scripts) is better long-term
but is a larger lift for the tip-merging logic.

For Option B, add to `config.R`:
```r
# Analysis-only raw data subdirectories (relative to raw_data_base)
chair_renter_subpath = "20201204_chair_renters/staff_renters.csv",
tip_pull_1_subpath   = "20200909_raw/Marketing Intern Data",
tip_pull_2_subpath   = "20201214_tip_more/bi_tip_lines_2017-2020.csv",
tip_pull_3_subpath   = "20210809_alldata_refresh_withzip/tip_amount_export.csv",
product_pull_1_subpath = "20210809_alldata_refresh_withzip/product_sales_export.csv",
product_pull_2_subpath = "2017_2020_product_sales/2017_2020_product_sales.csv",
```

For the ACS/tidycensus call: cache the result and skip re-pulling if the cached
file exists. This is a lightweight version of Option A for just that one source.

### Step 4 â€” Centralize exclusions (Issue K)

Add to `config.R`:
```r
# Known data anomalies to exclude
excluded_locations = c("fb686b3a-a166-469b-88ea-3467a68e2f53"),  # KY salon

# Partial quarters to exclude from analysis (beyond the COVID exclusion)
excluded_quarters_analysis = c(2021.3),
```

### Step 5 â€” Refactor `01_03_summary_stats_esample.R`

Smallest script; simplest fix. Changes needed:
1. Add `source('config.R')` at top
2. Update input path: â†’ `mkdata/data/01_staff_task_full.rds`
3. Update output path: â†’ `results/out/tables/`
4. Replace hard-coded `quarter_list` with `CONFIG$estimation_quarters`
5. Replace hard-coded county filter with `CONFIG$counties_padded`
6. Replace hard-coded Kentucky exclusion with `CONFIG$excluded_locations`

### Step 6 â€” Refactor `01_01_stylized_facts.R`

This is the most complex script. Changes needed:

1. Add `source('config.R')` and `source('utils/clean_transactions.R')` at top.
2. Replace lines 40â€“91 (data cleaning) with:
   ```r
   working <- data.table(readRDS("mkdata/data/00_tasks_cosmo.rds"))
   working <- clean_raw_transactions(working, CONFIG)
   ```
3. Replace `analysis_final/data/01_00_staff_task_full.rds` â†’ `mkdata/data/01_staff_task_full.rds`.
4. Replace hard-coded `blvd_dont_backup` paths with `file.path(CONFIG$raw_data_base, CONFIG$<subpath>)`.
5. Replace the live `tidycensus` API call with a cached read.
6. Fix the negative-duration assertion (Issue E) â€” should become `stopifnot(nrow(working[duration<0])==0)` after using the cleaned data.
7. Update all output paths to `results/out/tables/` and `results/out/figures/`.
8. Fix the typo: `01_01_stylied_facts_data.rds` â†’ `01_01_stylized_facts_data.rds`, saved to `results/data/`.
9. Replace Kentucky exclusion with `CONFIG$excluded_locations`.
10. Replace `quarter_year != 2021.3` with `CONFIG$excluded_quarters_analysis`.
11. Wrap diagnostic prints in `if (CONFIG$verbose_logging)`.

### Step 7 â€” Refactor `01_02_spatial_corr.R`

1. Add `source('config.R')` and `source('utils/clean_transactions.R')` at top.
2. Update input path: â†’ `results/data/01_01_stylized_facts_data.rds`.
3. Replace the GEOCORR block (lines 40â€“49) with a call to `load_zip_county_xwalk()`.
   Keep the shapefile join â€” `01_02` needs county on all ZCTAs nationally, not just
   those in the transaction data.
4. Fix the county padding: use `as.character(county)` without the `as.integer()` strip.
   Compare against `CONFIG$counties_padded` (or `CONFIG$counties` with a note on padding).
5. Update all output paths: â†’ `results/out/figures/`.
6. Replace the 9 copy-pasted plot blocks with a loop (Issue I).
7. Use `CONFIG$counties_padded` instead of hard-coded county strings.

### Step 8 â€” Wire into `run_all.R`

The analysis scripts should run **after** `01_build_data.R` (Step 2 in the current
`run_all.R`) and **before** `02_estimation.R` (Step 3). They do not affect the
estimation inputs, but logically belong between data construction and estimation.

Add them using the same `tryCatch` + `needs_rerun` + logging pattern that the
existing steps use. Use `source(script, local = new.env())` to isolate.

`01_01` and `01_03` are independent of each other and could be run in either order.
`01_02` must come after `01_01`.

Also add the analysis scripts' dependencies to the `run_all.R` step-definition
pattern so `needs_rerun` works. The `01_01` step depends on
`01_01_stylized_facts.R`, `config.R`, `utils/clean_transactions.R`, plus the
prep data files.

**Note:** The Step 6 example in the original Sonnet plan used a `run_if_needed()`
function that doesn't exist in the current `run_all.R`. The actual pattern uses
`needs_rerun()` (from `utils/logging.R`) inside `if` blocks with `tryCatch`.
Match the existing pattern rather than inventing a new one.

### Step 9 â€” Update README

Update the README workflow table to include the analysis scripts with their correct
inputs, outputs, and descriptions. Also add the new prep scripts if Option A was
chosen.

---

## Summary of New/Modified Files

| File | Action | Step |
|------|--------|------|
| `config.R` | Add `estimation_quarters`, `excluded_locations`, `excluded_quarters_analysis`, raw data subpaths | Steps 0, 3, 4 |
| `utils/clean_transactions.R` | **New.** Shared cleaning helper + `load_zip_county_xwalk()` | Step 2 |
| `01_build_data.R` | Call `clean_raw_transactions()` instead of inline cleaning | Step 2 |
| `01_03_summary_stats_esample.R` | Path fixes, use CONFIG | Step 5 |
| `01_01_stylized_facts.R` | Major refactor: paths, shared helper, configurable raw paths, assertions | Step 6 |
| `01_02_spatial_corr.R` | Path fixes, shared helper, loop over counties/variables | Step 7 |
| `run_all.R` | Add analysis steps, `ensure_directory` calls | Step 8 |
| `README.md` | Update workflow table | Step 9 |
| `renv.lock` | Snapshot additional packages | Step 1 |

---

## Open Questions

1. **Option A vs B for raw data** (Issue D): Prep scripts are cleaner but the
   tip-merging logic is non-trivial. Recommend Option B first (CONFIG paths),
   upgrade to Option A later if the code stabilizes.

2. **Negative duration discrepancy** (Issue E): Need to confirm whether
   `00_tasks_cosmo.rds` contains negative durations. If it does, the shared
   cleaning helper should drop them. If it doesn't, the assertion in `01_01`
   simply changes to expect 0.

3. **Full-sample vs. estimation-sample scope for `01_01`**: `01_01` runs on the
   full national sample (not just 3 counties). Confirm this scope is intentional.

4. **`results/data/` vs `mkdata/data/`**: `01_01`'s output is an analysis
   intermediate, not a pipeline input. Placing it in `results/data/` separates
   analysis outputs from pipeline intermediates. Confirm this is the right convention.

5. **Is the ACS income regression at the end of `01_01` still needed?** If not,
   the `tidycensus` dependency and `prep_10_acs_income.R` can be dropped entirely.

6. **County padding normalization**: Should `CONFIG` be simplified to use only
   padded 5-digit FIPS codes (dropping the `counties` list and keeping only
   `counties_padded`)? This would require a one-time update of `02_estimation.R`
   and other downstream code that uses the unpadded versions.
