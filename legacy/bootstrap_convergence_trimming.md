# Bootstrap convergence-failure trimming for NYC: methodology & legitimacy

**Question this note settles:** the bootstrap NYC standard errors are huge (skill-matrix
median SD ≈ 390, driven by ~3 reps / 97% of the variance). Is removing those reps a
legitimate fix or *artificial shrinkage* of the SEs?

## Diagnosis (why NYC reps vary so wildly)

- 06c Hessian spectrum: NYC is a **saddle / near-flat ridge** along the **E_5 (and E_3)**
  axis (`docs/wage_identification.md`; smallest eigenvalue ≈ −1.6e-5; multiple minima at
  similar ssq). LA/Cook are clean.
- Ridge test (`smoke_nyc_ridge_test.R`, 2026-06-06): from the point estimate, moving a NYC
  wage coord off its value makes the per-county wage-moment ssq jump from ~1e-9 to a
  **saturated plateau** — E_2→0.121, E_3→0.020, E_4→0.015 — and stay there. **E_5 is the
  exception: ssq stays ~0.008 even at E_5=1500** (genuinely soft / under-identified).
- So the wild scatter is **two different things**:
  1. **Convergence failures** — reps whose optimizer parked in an inferior/saturated basin
     (ssq 0.02–0.12 ≫ the 1e-9 good basin). These drive the skill-matrix SD blow-up.
  2. **Genuine weak identification of E_5** — a real flat direction; the scatter there is
     honest uncertainty, NOT a failure.

## Critical refinement (2026-06-07): only the WAGE block can have "convergence failures"

The wage params (`E_raw`) are solved by a GMM **optimizer** (`objective_gmm` / `beta_2`),
so they can land in wrong basins — these are trimmable failures (verified: E_raw_2 SE
collapses on trimming; E_raw_5 stays, genuine).

The skill matrix **`B_raw` is NOT a GMM/optimizer parameter** — it is `demand==TRUE` (75
params), estimated as exogenous cost-shifter coefficients in the **demand IV** (`ivreg` via
`rank_aware_solve`; `05_iv_spec_comparison.R`, `utils/constrained_demand_iv.R`). It is a
**closed-form regression coefficient**, so there is no basin to "fail" into. Its bootstrap
blow-up (≈3 reps drive 97% of the skill-B SD; values ±1e4–1e5) is the genuine sampling
behavior of an **IV coefficient under a near rank-deficient / collinear NYC skill design**
(NYC/LA skill rows correlate 0.77–0.97; `rank_aware_solve` exists precisely because the
`avg_labor:B_raw` design is ill-conditioned). A few resamples tip the design toward
singularity → coefficients explode.

**Therefore trimming the skill-B SEs is illegitimate (artificial shrinkage).** Only the wage
block is trimmable. Report the skill matrix (and E_5) via identification-robust / near-
singularity methods, not a trimmed Wald SE. (The wage-ssq trimmer cannot even move the
skill-B SE — B_raw is not in its objective; a `smoke_B_failure_test.R` that assumed B was a
GMM param errored on exactly this, which is how we found it.)

## The bright line (legitimate vs cheating)

- **Legitimate:** excluding reps where the estimator *was not successfully computed* (failed
  solution verification — here ssq ≫ 0, i.e. the moment conditions were not zeroed). Such a
  point is **not an estimate**. Precedent: McCullough & Vinod, *The Numerical Reliability of
  Econometric Software* (1999 JEL) and *Verifying the Solution from a Nonlinear Solver*
  (2003 AER); standard practice of estimating the bootstrap variance from the **converged**
  reps.
- **Cheating (artificial shrinkage):** dropping reps because their *estimates* are extreme
  (valid tail draws). The literature is explicit that discarding/replacing resamples until
  optimization is "easy" "induces incomplete coverage of the sampling distribution and
  therefore incorrect inferences." Trimming on |estimate| would be this.
- We stay on the legitimate side ONLY by (a) keying the rule on **ssq (solution failure),
  not on the estimate value**, with evidence the trimmed reps are inferior basins (above),
  and (b) **full disclosure**: always report SE *with and without* removal and the rule/counts.

## Cleaner alternative the literature prefers (over trimming)

Re-solve, don't drop: start every bootstrap rep from the point estimate / use per-rep
multistart so the optimizer reaches the good basin. SEs computed this way are **conservative
(if anything over-estimated), not shrunk** (*Statistical Inference with Local Optima*,
arXiv:1807.04431; Wood, "bootstrap restarting" for spurious local minima, 2001). Note the
bootstrap here **already warm-starts from `06_parameters`** yet still fails — so the honest
upgrade is per-rep multistart / a more robust wage solver, not post-hoc removal.

## E_5 is special — neither trimming nor the bootstrap is valid there

E_5 is genuinely weakly identified, and under weak identification the **bootstrap itself is
invalid** (Andrews & Guggenberger, *Identification- and singularity-robust inference*, QE
2019). Trimming cannot fix this. Report E_5 (and any E_5-loaded skill entries) with
**identification-robust** inference (Stock–Wright S-set / profiled-criterion CI), not a Wald SE.

## Decision adopted

1. **Minimum bar:** report bootstrap SE **with and without** convergence-failure removal,
   removal disclosed as solution-verification exclusion (ssq > τ=1e-2; τ sits in the gap
   between the 1e-9 good basin and the 0.015–0.12 saturated corners; it deliberately KEEPS
   the soft E_5 ridge). Tooling: `trim_bootstrap_convergence_failures.R` →
   `results/data/07b_bootstrap_se_trimmed.rds`, audit in
   `diagnostics/out/bootstrap_convergence_audit.rds`.
2. **Stronger (headline):** re-solve failed reps (multistart from the point estimate).
3. **E_5:** identification-robust CI, reported separately.

Do **not** publish a silently-trimmed SE as the headline — that is the shrinkage to avoid.
