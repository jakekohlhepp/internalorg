# Summary: 00_mk_tasks_cosmo.R

## Purpose

`00_mk_tasks_cosmo.R` is a **data preprocessing script** that transforms raw salon transaction data into a cleaned, task-classified dataset. It is a prerequisite step that must be run before the main estimation pipeline.

## What the Script Does

### 1. Data Loading and Initial Filtering (Lines 1-16)
- Loads raw transaction data from `compiled_trxns.rds`
- Filters to keep only hair-related businesses (Hair Salon, Barber Shop, Blowouts & Styling)
- Creates temporal indices (month_num, week_num) for time-series analysis
- Excludes one outlier salon with anomalous revenue spikes

### 2. Service Classification Merge (Lines 17-41)
- Loads a pre-built classification file (`classified_descriptions.rds`) that maps service descriptions to task categories (taskcat1-6)
- Merges this classification onto the transaction data
- Handles edge cases where character encoding issues prevented automatic merging

### 3. Duration Allocation for Multi-Task Services (Lines 42-75)
- The key challenge: a single "service" may involve multiple task types (e.g., "Cut & Color")
- Solution approach:
  1. Compute average duration for each task category using single-task, single-service appointments
  2. For multi-task services, allocate total duration proportionally based on these averages
  3. "Melt" the data so each task component becomes its own row

### 4. Task Category Finalization (Lines 76-97)
- Creates the `clust` variable (integer 1-6) identifying task type
- Creates human-readable labels (`rep_text_cluster`):
  - 1 = Haircut/Shave
  - 2 = Color/Highlight/Wash
  - 3 = Extensions
  - 4 = Blowdry/Style/Treatment
  - 5 = Administrative
  - 6 = Nail/Spa/Eye/Misc.
- Creates customer demographic flags (male_flag, female_flag, child_flag)

### 4b. Legacy Import Detection (after allocation)
- Detects location-quarters where >80% of rows share the same short duration (<= 15 min),
  indicating placeholder values from data imported from a prior POS system
- Only flags locations that also have quarters with normal durations (>= 30 min median),
  confirming the short durations are artifacts, not the salon's true operating pattern
- Drops flagged rows (228K rows, 1.66% of data, across 14 locations)
- See `docs/legacy_import_data_issue.md` for full details

### 5. Output
- Saves processed data to `mkdata/data/00_tasks_cosmo.rds`
- Exports example rows to CSV for presentation purposes

## How It Fits in the Pipeline

```
                    00_mk_tasks_cosmo.R
                          |
                          v
              mkdata/data/00_tasks_cosmo.rds  (OUTPUT)
                          |
         (copied/moved to mkdata/data/00_tasks_cosmo.rds)
                          |
                          v
                   run_all.R
                          |
          +---------------+---------------+
          |               |               |
          v               v               v
    01_build_data.R    |     02_estimation.R
          |               |               |
          v               v               v
    (clustering)    (preamble.R)    (GMM estimation)
```

### Dependency Chain:
1. **00_mk_tasks_cosmo.R** (this file) - Creates `mkdata/data/00_tasks_cosmo.rds`
2. **01_build_data.R** - Reads `mkdata/data/00_tasks_cosmo.rds`, builds working dataset with clustering
3. **02_estimation.R** - Runs GMM estimation using the working dataset

### Key Outputs Used Downstream:
| Column | Used In | Purpose |
|--------|---------|---------|
| `clust` | 01_build_data.R | Task type identifier for worker assignment matrix |
| `duration` | 01_build_data.R | Computing task mix shares and specialization index |
| `price` | 01_build_data.R | Revenue calculations |
| `location_id` | All | Firm identifier |
| `staff_id` | 01_build_data.R | Worker clustering |
| `date` | 01_build_data.R | Quarter-year grouping |

## Data Transformations

### Input Schema (from compiled_trxns.rds)
- Transaction-level data with one row per service performed
- Contains: service descriptions, prices, durations, appointment IDs, staff IDs, location info

### Output Schema (00_tasks_cosmo.rds)
- Task-level data (one row per task component of each service)
- Key columns:
  - `clust`: Task category (1-6)
  - `rep_text_cluster`: Human-readable task name
  - `duration`: Allocated time for this task (in minutes)
  - `price`: Service price
  - `staff_id`, `location_id`, `customer_id`: Identifiers
  - `date`, `month_num`, `week_num`: Temporal indices

---

## Critical Errors and Issues

### 1. **Hardcoded File Path (CRITICAL)** -- FIXED (Mar 2026)
~~**Line 8:**~~
```r
# OLD: tasks<-data.table(readRDS("C:/Users/jakek/jmp_dont_backup/data/compiled_trxns.rds"))
# NEW: raw_data_file <- file.path(CONFIG$raw_data_base, "compiled_trxns.rds")
```
**Resolution**: Now uses `CONFIG$raw_data_base` which reads from the `.Renviron` file via `Sys.getenv("raw_data_base")`.

### 2. **Magic Numbers / Hardcoded Row Indices (HIGH)**
**Lines 34-38:**
```r
tasks[service_id=="ec581975-...",(col) :=classified[rep(6,...)...]
tasks[service_id=="3a15cd78-...",(col) :=classified[rep(20363,...)...]
tasks[service_id=="001cb7db-...",(col) :=classified[rep(9900,...)...]
```
- **Issue**: Uses hardcoded row numbers (6, 20363, 9900) that assume a specific order in `classified`
- **Impact**: If `classified_descriptions.rds` changes order, wrong classifications will be applied
- **Recommendation**: Look up by a stable key rather than row number:
  ```r
  tasks[service_id=="...", (col) := classified[raw_id == target_id, .SD, .SDcols=col]]
  ```

### 3. **Inconsistent Output Path (MEDIUM)** -- FIXED (Mar 2026)
**Resolution**: Script now saves directly to `mkdata/data/00_tasks_cosmo.rds` (line 310). The `analysis_cosmo` CSV export is commented out (lines 316-318).

### 4. **Fragile Data Quality Assertion (MEDIUM)**
**Line 39:**
```r
stopifnot(nrow(tasks[ is.na(service_performed)])==1)
```
- **Issue**: Expects exactly 1 NA row; will fail if data changes
- **Impact**: Script will crash if raw data has different number of unmatched services
- **Recommendation**: Log warning instead of hard stop, or make the assertion more flexible

### 5. **Outlier Removal Without Logging (LOW)**
**Line 16:**
```r
tasks<-tasks[location_id!="dae8355a-cd8f-4ae1-8d26-a839b578f9f9",]
```
- **Issue**: Silently excludes one salon; not documented what "ridiculous spike" means
- **Impact**: Hard to audit or replicate exclusion decision
- **Recommendation**: Add comment with criteria used (e.g., "revenue > 10x median") and log the exclusion

### 6. **Potential Division by Zero (LOW)**
**Lines 50-55:**
```r
tasks[,avg_task1:=sum(duration*...)/sum(...),]
```
- **Issue**: If no single-category, single-service appointments exist for a task type, division by zero occurs
- **Impact**: Will produce NaN which may propagate through calculations
- **Recommendation**: Add check for empty denominator

### 7. **No Input Validation (LOW)**
- **Issue**: Script assumes input file exists and has expected columns
- **Impact**: Cryptic error messages if input is missing or malformed
- **Recommendation**: Add existence checks and column validation at script start

---

## Summary Statistics

Based on the code logic, the expected output should contain:
- **6 task categories** (taskcat1-6, later merged to 5 in 01_build_data.R)
- Transactions from **pre-COVID period** (based on filtering in downstream scripts)
- Multiple rows per original service if that service spans multiple task categories
