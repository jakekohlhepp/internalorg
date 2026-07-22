# R Estimation Pipeline Coding Style

This skill documents the coding conventions and style used in the JMP estimation pipeline for hair salon worker specialization analysis.

## File Naming Conventions

### Data Preparation Scripts (run before main pipeline)
- Pattern: `prep_##_descriptive_name.R`
- Examples:
  - `prep_01_cosmo_classify.R` - Service classification
  - `prep_02_censuspop.R` - Census population data
  - `prep_00_compile_transactions.R` - External data compilation (rarely run)

### Main Pipeline Scripts
- Pattern: `##_descriptive_name.R`
- Examples:
  - `00_mk_tasks_cosmo.R` - Task data creation (from compiled transactions)
  - `01_build_data.R` - Data construction and clustering
  - `06_estimation.R` - GMM estimation

### Supporting Scripts
- `config.R` - Centralized configuration (CONFIG object)
- `preamble.R` - Core functions and GMM helpers
- `cluster.R` - Hierarchical clustering implementation
- `run_all.R` - Pipeline orchestrator
- `run_prep_data.R` - Data preparation orchestrator

## Directory Structure

```
refactor_estimation/
├── Main Scripts
│   ├── run_all.R               # Master pipeline script
│   ├── run_prep_data.R         # Data preparation script
│   ├── config.R                # Configuration
│   ├── 00_mk_tasks_cosmo.R     # Step 0: Task data
│   ├── 01_build_data.R         # Step 1: Data build
│   ├── 06_estimation.R         # Step 6: Estimation
│   ├── preamble.R              # Core functions
│   └── cluster.R               # Clustering
│
├── prep_*.R                    # Data preparation scripts
│
├── mkdata/
│   ├── raw/                    # Raw source files (CSV, XLSX)
│   │   ├── 20220526_cosmo_classify/
│   │   ├── 20220711_cex/
│   │   └── ...
│   └── data/                   # ALL intermediate data files (.rds)
│       ├── classified_descriptions.rds  # prep_01
│       ├── county_census_pop.rds        # prep_02
│       ├── 00_tasks_cosmo.rds           # 00_mk_tasks_cosmo
│       ├── 01_working.rds               # 01_build_data
│       ├── minwage.xlsx                 # manual input
│       └── seeit_bb.rds                 # manual input
│
├── results/
│   ├── data/                   # Final outputs
│   │   └── 06_parameters.rds
│   └── out/                    # Figures and tables
│
├── tests/testthat/             # Unit tests
├── utils/                      # Utility functions
│   └── logging.R
└── logs/                       # Execution logs
```

## Path Conventions

### All paths should be relative to the project root
- **CORRECT**: `mkdata/data/ppi.rds`
- **INCORRECT**: `../mkdata/data/ppi.rds`

### Raw data locations
- Raw files: `mkdata/raw/{date_source}/filename`
- Example: `mkdata/raw/20220711_cex/intrvw19/expn19/xpb19.csv`

### Intermediate data locations
- ALL intermediate data: `mkdata/data/`
- This includes prep script outputs, pipeline intermediates, and manual inputs

### Final outputs
- Pipeline parameters: `results/data/06_parameters.rds`
- Figures and tables: `results/out/`

## Header Documentation Template

Each script should start with a roxygen2-style header:

```r
#' =============================================================================
#' SCRIPT TITLE
#' =============================================================================
#' Brief description of what the script does (1-2 sentences).
#'
#' Input:  path/to/input1.rds (description)
#'         path/to/input2.csv (description)
#' Output: path/to/output.rds
#'
#' Output Schema:
#'   column1: description
#'   column2: description
#'
#' Dependencies: other_script.R (if any)
#' =============================================================================
```

## Inline Comment Style

### Section headers
```r
#' -----------------------------------------------------------------------------
#' SECTION NAME
#' -----------------------------------------------------------------------------
```

### Block comments for complex logic
```r
# Explanation of what the following code block does
# and why it's necessary (multiple lines if needed)
```

### Line comments for non-obvious operations
```r
data[, new_col := complex_operation]  # Brief explanation
```

## data.table Conventions

### Preferred syntax
```r
# Use := for assignment
dt[, new_col := value]

# Use .() for list creation in j
dt[, .(col1, col2)]

# Use .SD with .SDcols for column operations
dt[, lapply(.SD, mean), .SDcols = numeric_cols]
```

### Chaining
```r
# OK for 2-3 operations
dt[condition, ][, new := val]

# For longer chains, use intermediate variables
step1 <- dt[condition, ]
step2 <- step1[, .(aggregated)]
```

## Configuration (CONFIG object)

All magic numbers and configurable parameters should be in `config.R`:

```r
CONFIG <- list(
  # County FIPS codes
  counties = c(6037, 17031, 36061),
  counties_padded = c("06037", "17031", "36061"),

  # Model parameters
  n_worker_types = 5,

  # Paths
  raw_data_path = "C:/Users/jakek/jmp_dont_backup/data",
  log_dir = "logs"
)
```

## Error Handling

### Use stopifnot for assertions
```r
stopifnot(nrow(data) > 0)
stopifnot(all(!is.na(data$key_col)))
```

### Use tryCatch for recoverable errors
```r
tryCatch({
  source("script.R")
}, error = function(e) {
  warning("Script failed: ", e$message)
})
```

## Testing

- Test files go in `tests/testthat/`
- Name pattern: `test_feature_name.R`
- Use skip functions when data files are missing:
```r
test_that("data loads correctly", {
  skip_if_not(file.exists("data/01_working.rds"))
  # test code
})
```

## Version Control

### Files to track
- All .R scripts
- config.R
- renv.lock
- Documentation (.md files)

### Files to ignore
- mkdata/data/*.rds (generated)
- analysis_final/data/*.rds (generated)
- logs/*.log (generated)
- Large raw data files (>100MB)
