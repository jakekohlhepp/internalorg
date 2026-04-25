# Data Dependencies

This note documents the active end-to-end dependency graph for the current
pipeline.

## Architecture Summary

The main data split is explicit:

- `01_build_data.R` writes both:
  - `mkdata/data/01_staff_task_full.rds` for full-sample descriptive work
  - `mkdata/data/01_working.rds` for the estimation-base branch
- `04_estimation_sample.R` takes the estimation-base branch and adds
  estimation-only enrichments such as PPI, minimum wages, and instruments
- `02_stylized_facts.R` and `03_spatial_corr.R` stay on the full-sample
  descriptive branch
- `07_bootstrap.R` through `12_validate.R` produce post-estimation artifacts
  (bootstrap draws, display tables, inverted gammas, substitution patterns,
  validation outputs, and the counterfactual input `12_data_for_counterfactuals.rds`)
- `run_counterfactuals.R` orchestrates the `13_*.R` through `19_*.R` scenario
  scripts on top of `results/data/06_parameters.rds`

## Flowchart

```mermaid
flowchart TD
    subgraph PREP[Prep pipeline]
        p00[prep_00_compile_transactions.R] --> compiled[<raw_data_base>/compiled_trxns.rds]
        p01[prep_01_cosmo_classify.R] --> classified[mkdata/data/classified_descriptions.rds]
        p02[prep_02_censuspop.R] --> census[mkdata/data/county_census_pop.rds]
        p03[prep_03_county_msa_xwalk.R] --> msa[mkdata/data/county_msa_xwalk.rds]
        p04[prep_04_consumerexpenditure.R] --> cex[mkdata/data/cex_outside.rds]
        p05[prep_05_ppi.R] --> ppi[mkdata/data/ppi.rds]
        p06[prep_06_qcew.R] --> qcew[mkdata/data/qcew_county.rds]
        p07[prep_07_nyc_rent.R] --> rent[mkdata/data/nyc_rent_zip_quarterly.rds]
    end

    reviewed[check2_truly_new_sorted_for_classification.csv] --> mk
    compiled --> mk[00_mk_tasks_cosmo.R]
    classified --> mk
    mk --> tasks[mkdata/data/00_tasks_cosmo.rds]

    tasks --> build[01_build_data.R]
    census --> build
    cex --> cluster[cluster.R]
    msa --> cluster
    qcew --> cluster
    build --> cluster

    build --> stafffull[mkdata/data/01_staff_task_full.rds]
    cluster --> working[mkdata/data/01_working.rds]

    ppi --> esample[04_estimation_sample.R]
    minwage[mkdata/data/minwage.xlsx] --> esample
    working --> esample
    stafffull --> esample
    esample --> esample_out[mkdata/data/04_estimation_sample.rds]
    esample --> summary04[results/out/tables/04_summary_stats_structural.tex]

    esample_out --> ivspec[05_iv_spec_comparison.R]
    ivspec --> iv1[results/out/tables/05_standard_iv_comparison.tex]
    ivspec --> iv2[results/out/tables/05_standard_hausman_fe_comparison.tex]
    ivspec --> iv3[results/out/tables/05_nested_fe_comparison.tex]

    esample_out --> est[06_estimation.R]
    starts[mkdata/data/seeit_bb.rds] --> est
    preamble[preamble.R helper functions] --> est
    est --> params[results/data/06_parameters.rds]

    tasks --> a02[02_stylized_facts.R]
    stafffull --> a02
    a02 --> a02out[results/data/02_stylized_facts_data.rds]
    a02out --> a03[03_spatial_corr.R]

    esample_out --> boot[07_bootstrap.R]
    params --> boot
    boot --> bootout[results/data/07_bootstrap.rds]

    params --> disp[08_display_estimates.R]
    esample_out --> disp
    bootout --> disp
    disp --> disptabs[results/out/tables/08_*.tex]

    params --> gam[09_invert_gammas.R]
    esample_out --> gam
    stafffull --> gam
    gam --> gamout[results/data/09_withgammas.rds]

    params --> sub10[10_substitution.R]
    gamout --> sub10
    sub10 --> sub10out[results/out/tables/10_*.tex]

    params --> sub11[11_substitution_prod.R]
    gamout --> sub11
    sub11 --> sub11out[results/out/tables/11_substitute_prod.tex]

    params --> val[12_validate.R]
    gamout --> val
    stafffull --> val
    val --> valout[results/data/12_data_for_counterfactuals.rds]
    val --> valtabs[results/out/tables/12_validate_corr.tex]

    valout --> cf[run_counterfactuals.R]
    params --> cf
    cf --> cf13[13_counterfactual_prep.R]
    cf13 --> cfwages[counterfactuals/05_00_initial_wages.rds]
    cf13 --> cfdata[counterfactuals/05_00_working_data.rds]
    cfwages --> cfscen[14..17 scenario scripts]
    cfdata --> cfscen
    cfscen --> cfsum[18_counterfactual_summary.R]
    cfscen --> cfplot[19_counterfactual_figures.R]
    cfsum --> cftables[results/out/tables/05_06_*.tex]
    cfplot --> cffigs[results/out/figures/05_07_*.png]
```

## Current Managed Workflow

### Prep runner: `run_prep_data.R`

| Step | Script | Primary output |
|------|--------|----------------|
| 00 | `prep_00_compile_transactions.R` | `<raw_data_base>/compiled_trxns.rds` |
| 01 | `prep_01_cosmo_classify.R` | `mkdata/data/classified_descriptions.rds` |
| 02 | `prep_02_censuspop.R` | `mkdata/data/county_census_pop.rds` |
| 03 | `prep_03_county_msa_xwalk.R` | `mkdata/data/county_msa_xwalk.rds` |
| 04 | `prep_04_consumerexpenditure.R` | `mkdata/data/cex_outside.rds` |
| 05 | `prep_05_ppi.R` | `mkdata/data/ppi.rds` |
| 06 | `prep_06_qcew.R` | `mkdata/data/qcew_county.rds` |
| 07 | `prep_07_nyc_rent.R` | `mkdata/data/nyc_rent_zip_quarterly.rds` |

### Main runner: `run_all.R`

| Step | Script or component | Primary output |
|------|---------------------|----------------|
| 0 | `00_mk_tasks_cosmo.R` | `mkdata/data/00_tasks_cosmo.rds` |
| 1 | package restore/setup inside `run_all.R` | local `renv` library state |
| 2 | `01_build_data.R` + `cluster.R` | `mkdata/data/01_staff_task_full.rds`, `mkdata/data/01_working.rds` |
| 3 | `04_estimation_sample.R` | `mkdata/data/04_estimation_sample.rds`, `results/out/tables/04_summary_stats_structural.tex` |
| 4 | `05_iv_spec_comparison.R` | `results/out/tables/05_standard_iv_comparison.tex`, `results/out/tables/05_standard_hausman_fe_comparison.tex`, `results/out/tables/05_nested_fe_comparison.tex` |
| 5 | `06_estimation.R` + `preamble.R` | `results/data/06_parameters.rds` |
| 6 | `07_bootstrap.R` | `results/data/07_bootstrap.rds`, `results/data/07_boot_weights.rds` |
| 7 | `08_display_estimates.R` | `results/out/tables/08_org_price.tex`, `results/out/tables/08_time_effects.tex`, `results/out/tables/08_model_fit.tex`, `results/out/tables/08_wages_skills_<county>.tex` |
| 8 | `09_invert_gammas.R` | `results/data/09_withgammas.rds`, `results/out/figures/09_gamma_dist.png` |
| 9 | `10_substitution.R` | `results/out/tables/10_substitute.tex`, `results/out/figures/10_*.png` |
| 10 | `11_substitution_prod.R` | `results/out/tables/11_substitute_prod.tex` |
| 11 | `12_validate.R` | `results/data/12_data_for_counterfactuals.rds`, `results/out/tables/12_validate_corr.tex`, `results/out/figures/12_*.png` |
| 12 | `run_counterfactuals.R` | see counterfactual runner below |

### Counterfactual runner: `run_counterfactuals.R`

Orchestrates the `13_*.R` through `19_*.R` scenario scripts on top of
`results/data/06_parameters.rds` and `results/data/12_data_for_counterfactuals.rds`.
Shared helpers live in `utils/counterfactuals_core.R`.

| Step | Script | Primary output |
|------|--------|----------------|
| CF 00 | `13_counterfactual_prep.R` | `counterfactuals/05_00_initial_wages.rds`, `counterfactuals/05_00_working_data.rds` |
| CF 02 | `14_counterfactual_diffusion.R` | `counterfactuals/05_02_wages_diffusion.rds`, `counterfactuals/05_02_prod_diffusion.rds` |
| CF 03 | `15_counterfactual_sales_tax.R` | `counterfactuals/05_03_wages_salestax.rds`, `counterfactuals/05_03_prod_salestax.rds`, `counterfactuals/05_03_prod_initial.rds` |
| CF 04 | `16_counterfactual_immigration.R` | `counterfactuals/05_04_wages_immigration.rds`, `counterfactuals/05_04_prod_immigration.rds` |
| CF 06A | `17_counterfactual_merger.R` | `counterfactuals/05_06_wages_merger.rds`, `counterfactuals/05_06_prod_merger.rds` |
| CF 06B | `18_counterfactual_summary.R` | `results/out/tables/05_06_tot_counterfactuals.tex`, `results/out/tables/05_06_bytype_counterfactuals.tex` |
| CF 07 | `19_counterfactual_figures.R` | `results/out/figures/05_07_*.png` |

## Standalone Analysis Scripts

These scripts remain standalone by design. They operate on the descriptive,
full-sample branch rather than the estimation-only branch.

| Script | Main input | Main output |
|--------|------------|-------------|
| `02_stylized_facts.R` | `mkdata/data/00_tasks_cosmo.rds`, `mkdata/data/01_staff_task_full.rds` | `results/data/02_stylized_facts_data.rds` plus tables and figures |
| `03_spatial_corr.R` | `results/data/02_stylized_facts_data.rds` | `results/out/figures/03_spatial_cor_*.png`, `03_coverage.png` |

## Archived Non-Pipeline Scripts

One-off diagnostics are kept in `archive/experimental/`. They are not part of
the managed workflow.
