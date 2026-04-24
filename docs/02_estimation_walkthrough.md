# `05_estimation.R` Walkthrough

## Purpose

`05_estimation.R` is the structural-estimation stage of the active pipeline. It
takes the estimation-ready artifact assembled by `04_estimation_sample.R` and
estimates model parameters so that:

- the demand side matches observed market shares, and
- the assignment side matches observed worker-type shares across salons.

The script saves its final parameter table to `results/data/05_parameters.rds`.

## Main Inputs

- `mkdata/data/04_estimation_sample.rds`
  - Built by `04_estimation_sample.R`
  - Contains:
    - `working_data`: the estimation-base panel from `01_working.rds`, enriched
      with PPI, minimum wages, and instruments
    - `estim_matrix`: the slim estimation matrix consumed by the GMM routines
    - `min_wage_levels`: county-quarter minimum wages used in the constrained
      auxiliary regression
- `mkdata/data/seeit_bb.rds`
  - Starting values for the nonlinear solver
- `config.R`
  - Defines counties, model dimensions, tolerances, and parallel settings
- `preamble.R`
  - Now defines shared numerical helpers and the
    `build_estimation_setup(...)` function
  - It no longer mutates the session when sourced

## Architecture

The main structural split is:

1. `01_build_data.R`
   - writes the full-sample descriptive panel `01_staff_task_full.rds`
   - writes the estimation-base panel `01_working.rds`
2. `04_estimation_sample.R`
   - adds estimation-only enrichments such as PPI, minimum wages,
     `dye_instrument`, `org_cost`, and `log_rel_mkt`
   - writes `04_estimation_sample.rds`
3. `05_estimation.R`
   - reads the saved 04 artifact
   - calls `build_estimation_setup(...)`
   - runs the nonlinear and constrained estimation blocks

That means the estimation stage no longer depends on hidden globals created by
sourcing `preamble.R`.

## High-Level Flow

### 1. Load the saved estimation sample

`05_estimation.R` starts by reading `mkdata/data/04_estimation_sample.rds`.
This gives it:

- `working_data`
- `estim_matrix`
- `min_wage_levels`

Those are the only data objects the script needs before estimation begins.

### 2. Build the analytic setup objects

After sourcing `preamble.R`, the script calls:

`build_estimation_setup(working_data, estim_matrix, config = CONFIG)`

This shared helper constructs:

- `beta`: analytic IV demand coefficients
- `beta_2`: the closed-form price-side coefficient block used to label the
  nonlinear starting values
- model matrices used by the analytic first-stage setup

The helper returns a list explicitly instead of writing into the caller's
environment.

### 3. Prepare the nonlinear unknowns

The script reads `seeit_bb.rds` and relabels those starting values to the
county-by-worker-type wage-premium coefficients:

- `factor(county)17031:avg_labor:E_raw_2`
- ...
- `factor(county)6037:avg_labor:E_raw_5`

With 3 counties and 4 non-normalized worker types, the current nonlinear block
has 12 unknowns.

### 4. Solve the assignment-side moments

The main nonlinear estimation uses:

- `BBsolve(...)` as the outer solver
- `objective_gmm(...)` as the moment function

For any candidate parameter vector `theta`, `objective_gmm(...)` does the
following:

1. Build a county-specific cost matrix with `build_cost_matrix(...)`
2. For each observation, use the salon's task mix `alpha` and specialization
   index `s_index`
3. Solve for the `gamma` that reproduces that `s_index`
4. At that `gamma`, recover the model-implied worker shares `E`
5. Compare model-implied `E` to observed `E_raw_*`
6. Return the county-by-worker-type moment matrix

`BBsolve` then searches for the `theta` that drives the mean moments to zero.

## The Numerical Core

### Cost matrix

`build_cost_matrix(...)` constructs a county-specific matrix:

`tild_theta[i, j] = wage_premium[i, j] + skill[i, j] / rho`

where:

- `rho` is county-specific price sensitivity from the analytic demand estimate
- worker type 1 is normalized to have zero wage premium
- the matrix is normalized so the minimum cost in each task column is zero

### Solving worker allocation for a fixed `gamma`

`solve_equilibrium(...)` takes:

- a cost matrix
- task shares `alpha`
- a regularization parameter `gamma`

and solves for:

- worker shares `E`
- assignment matrix `B`

It uses a fixed-point iteration accelerated by `SQUAREM`.

### Inverting `gamma` from `s_index`

`find_gamma_for_sindex(...)` chooses `gamma` so that the model's implied
entropy matches the observed `s_index`.

It does this in two steps:

1. Compute the corner solution, where each task is assigned to the cheapest
   worker type
2. If the observed `s_index` is attainable, run bisection over `gamma` until
   the model's entropy matches the data

## Post-GMM Auxiliary Regression

After the nonlinear wage-premium block converges, the script estimates another
set of parameters in a price-adjustment regression.

It first constructs:

- `wb_i`: wage-bill terms implied by the estimated worker premia
- `gamma_invert`: observation-level `gamma` values implied by the final
  nonlinear estimates
- `p_adj`: adjusted price after removing worker-premium and organization-cost
  components

It then:

1. runs an unconstrained regression for starting values
2. parses coefficient names to identify county-quarter wage terms
3. merges `min_wage_levels` from the saved 04 artifact to build lower bounds
4. runs bounded `optim(..., method = "L-BFGS-B")`

The output of this step is the second coefficient block, stored in `coef_vect2`.

## Final Output

The saved output `results/data/05_parameters.rds` is a long table with:

- `demand = TRUE`
  - coefficients from the analytic demand estimate `beta`
- `demand = FALSE`
  - nonlinear wage-premium estimates from `BBsolve`
  - bounded auxiliary-regression estimates from `optim`

## What To Read First In The Code

If reading the source directly, this order is usually easiest:

1. `config.R`
2. `04_estimation_sample.R`
3. `preamble.R` functions:
   - `build_estimation_setup(...)`
   - `build_cost_matrix(...)`
   - `solve_equilibrium(...)`
   - `find_gamma_for_sindex(...)`
   - `objective_gmm(...)`
   - `get_gammas(...)`
4. `05_estimation.R`

## Serious Issues And Risks

### 1. Non-convergence still does not stop the pipeline

`05_estimation.R` warns when `BBsolve` fails to converge, then continues using
`res_store$par` anyway. It does the same after the bounded `optim(...)` call.
That means `results/data/05_parameters.rds` can still be written even when one
of the estimation stages failed.

### 2. Starting values are still an external artifact

`seeit_bb.rds` remains a manual starting-point file. Reproducibility still
depends on that artifact being present and aligned with the current
specification.

## Bottom Line

Conceptually, `05_estimation.R` now does three things:

1. load the explicit estimation artifact written by `04_estimation_sample.R`
2. estimate demand-side and assignment-side coefficient blocks
3. recover additional bounded price-side coefficients in a follow-up
   regression

The main architectural improvement is that the stage no longer depends on
hidden side effects from `preamble.R`.
