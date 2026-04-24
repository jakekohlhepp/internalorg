# Prep Data Pipeline

This note documents the current prep stage that runs before `00_mk_tasks_cosmo.R`
and the downstream estimation workflow.

## Entry Point

Run:

```r
source("run_prep_data.R")
```

`run_prep_data.R`:

- locates the project root before sourcing anything
- activates `renv/activate.R` when present
- checks required inputs before rerunning a step
- forces a rerun when an expected output file is missing
- writes a summary log to `logs/run_prep_data.log`

## Step Map

| Step | Script | Main output |
|------|--------|-------------|
| PREP 00 | `prep_00_compile_transactions.R` | `<raw_data_base>/compiled_trxns.rds` |
| PREP 01 | `prep_01_cosmo_classify.R` | `mkdata/data/classified_descriptions.rds` |
| PREP 02 | `prep_02_censuspop.R` | `mkdata/data/county_census_pop.rds` |
| PREP 03 | `prep_03_county_msa_xwalk.R` | `mkdata/data/county_msa_xwalk.rds` |
| PREP 04 | `prep_04_consumerexpenditure.R` | `mkdata/data/cex_outside.rds` |
| PREP 05 | `prep_05_ppi.R` | `mkdata/data/ppi.rds` |
| PREP 06 | `prep_06_qcew.R` | `mkdata/data/qcew_county.rds` |
| PREP 07 | `prep_07_nyc_rent.R` | `mkdata/data/nyc_rent_zip_quarterly.rds` |

## Environment Inputs

- `raw_data_base` points to machine-specific confidential raw transaction files.
- `JMP_RAW_DATA_PATH` optionally overrides the local raw/prep source folder used by
  `CONFIG$raw_data_path`.
- `config.R` centralizes both path settings.

## Rebuild Rules

A prep step reruns if any of the following is true:

- the expected output file is missing
- the script changed since its last successful run
- a tracked dependency changed since its last successful run
- `CONFIG$force_rerun` is `TRUE`

## Downstream Use

- `classified_descriptions.rds` and `<raw_data_base>/compiled_trxns.rds` feed `00_mk_tasks_cosmo.R`.
- `county_census_pop.rds` feeds `01_build_data.R`.
- `cex_outside.rds`, `county_msa_xwalk.rds`, and `qcew_county.rds` feed `cluster.R` during `01_build_data.R`.
- `ppi.rds` and `minwage.xlsx` feed `04_estimation_sample.R`, whose saved artifact is consumed by `05_iv_spec_comparison.R` and `06_estimation.R`.
- `nyc_rent_zip_quarterly.rds` feeds downstream rent/market-structure analysis.
- `00_tasks_cosmo.rds` and `01_staff_task_full.rds` also feed the standalone analysis scripts (`02_stylized_facts.R`, `03_spatial_corr.R`).
