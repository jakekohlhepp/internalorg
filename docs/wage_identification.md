# Wage-stage local identification diagnostic

This document records the Hessian and perturbation tests on the per-county
wage solution produced by [06_estimation.R](../06_estimation.R), and
explains the new pipeline step [06c_wage_identification.R](../06c_wage_identification.R)
that automates them. Companion to
[wage_solver_stability.md](wage_solver_stability.md) and
[parameter_comparison.md](parameter_comparison.md).

## Motivation

The smoke runs in `wage_solver_stability.md` showed that PSO with multiple
seeds and seven different polishers all converged to the same NYC point at
ssq = 0.00128336, with the E_5 coordinate drifting along a flat direction.
That suggested either a true local minimum on a flat ridge, or a shallow
saddle that no polisher could escape. The Hessian test settles the
question, and the perturbation test confirms it constructively.

## Method

For each of the three counties at the wage solution `x*`:

1. **Hessian.** Compute the 4×4 Hessian of `objective_county_ssq` at `x*`
   numerically using `numDeriv::hessian` (Richardson extrapolation, central
   differences). Step size is `0.05 × parscale_w` for that county (200 ×
   0.05 = 10 for NYC, 20 × 0.05 = 1 for Cook and LA).
2. **Eigendecomposition.** Symmetric eigendecomp; classify the solution as
   - *strict local minimum* if all eigenvalues > 1e−9,
   - *flat ridge* if the smallest is in [-1e−9, 1e−9] and others are positive,
   - *saddle* if any eigenvalue < −1e−9.
3. **Perturbation test (only for ridge / saddle).** Perturb `x*` along the
   smallest-eigenvalue eigenvector `v` at steps `s ∈ {±1, ±10, ±50, ±100}`,
   and run NM polish from each `x* + s·v`. If any polish reaches an ssq
   materially below `f(x*)`, the saddle (or "ridge that hides another
   minimum") is constructively confirmed.

## Results — job 49401310, post-PSO solution

### Hessian eigenvalue spectra

| County | f(x*) | λ₁ (max) | λ₂ | λ₃ | λ₄ (min) | Verdict |
|---|---|---|---|---|---|---|
| Cook (17031) | 1.24e−16 | 1.04e−4 | 3.60e−6 | 1.78e−7 | **4.32e−8** | strict local min (cond ≈ 2,406) |
| NYC (36061) | 0.00128 | 1.57e−4 | 2.20e−5 | 1.04e−6 | **−3.17e−7** | **saddle** |
| LA (6037) | 1.51e−7 | 5.15e−3 | 1.14e−3 | 9.96e−5 | **1.81e−6** | strict local min (cond ≈ 2,851) |

Cook and LA are clean. NYC has one negative eigenvalue at the perturbation
threshold; the corresponding eigenvector is `v = (0.033, 0.575, 0.184, 0.796)`
— heavy on E_5 and E_3.

### NYC perturbation test

Perturbations of `x*` along `v`, then NM polish:

| step `s` | f(x* + s·v) | polish ssq | polish convergence | polish par |
|---|---|---|---|---|
| −100 | 0.04739 | 0.001328 | 10 | (−23.5, +496.4, +151.3, +2250.0) |
| **−50** | 0.03049 | **0.00128338** | 0 | (−22.4, +498.1, +150.9, +1981.8) |
| −10 | 0.00315 | 0.001780 | 10 | (−22.8, +492.1, +150.8, +1912.5) |
| **−1** | 0.00130 | 0.001292 | 10 | (−22.5, +497.6, +151.0, +1914.0) |
| **+1** | 0.00130 | **0.00128336** | 0 | (−22.4, +498.2, +150.9, +1249.9) |
| **+10** | 0.00495 | **0.00119084** | 10 | (−24.1, +495.8, +151.9, **+342.75**) |
| +50 | 0.04469 | 0.02305 | 0 | (+7.0, +633.2, +149.4, +1791.6) |
| +100 | 0.04236 | 0.02305 | 0 | (+7.1, +605.5, +149.4, +1867.9) |

The polish at step +10 achieves **ssq = 0.001190842**, about 7% below the
original `f(x*) = 0.001283382`. **Saddle confirmed.**

### What's actually going on

Look at the `polish_par` column. The polished points are essentially the
same in (E_2, E_3, E_4) — all near (−22, +498, +151) — but E_5 spans
**+343, +1250, +1826, +1981, +2250** with ssq values 0.00119, 0.00128, 0.00128,
0.00128, 0.00133. So the original "flat ridge" interpretation was incomplete:
NYC has **multiple distinct local minima at very similar ssq** along the
E_5 axis. Direct moves along `v` cross between them; sufficient perturbation
plus polish lands in a different (sometimes lower) one.

The lowest minimum we have constructively reached so far is

```
NYC wages ≈ (−24.1, +495.8, +151.9, +342.75)   at ssq = 0.001190842
```

This is closer to the manuscript's reported NYC values
`(−166, −142, −70, +660)` in the sense that **the E_5 magnitude is now
the same order as the manuscript's +660** (vs the previous +1826), even
though it's still a different sign pattern.

## Practical implications

1. **The current `06_parameters.rds` is not at the global minimum.** It
   sits at a saddle / shallow-local-min with a known downhill direction.
   Re-running 06_estimation with the patched `seeit_bb.rds` (NYC =
   `(−24.1, +495.8, +151.9, +342.75)` for E_2..E_5) and the dual-polish PSO
   would produce a slightly lower ssq.
2. **Bootstrap E_5 standard errors will be enormous** regardless of which
   local minimum the point estimate sits in. Each bootstrap replicate's
   solver is likely to land at a different one of these shallow minima.
   Standard-error reporting for E_5 (and to a lesser extent E_3) needs to
   be interpreted as identifying which basin the data anchored, not as
   precision around a single point.
3. **The diagnostic should run on every estimation pass.** Step 5c in
   [run_all.R](../run_all.R) wires it in immediately after 06_estimation
   and before 07_bootstrap. The flag `RUN_WAGE_IDENTIFICATION` (default
   TRUE) lets you skip if needed.

## Outputs

`06c_wage_identification.R` writes:
- [results/data/06c_wage_identification.rds](../results/data/06c_wage_identification.rds)
  — the raw per-county Hessian, eigenvalues, eigenvectors, and (where
  triggered) the perturbation table. Use this for downstream tooling that
  needs the eigenvectors as numerical objects.
- [results/out/tables/06c_wage_eigenvalues.tex](../results/out/tables/06c_wage_eigenvalues.tex)
  — per-county spectrum + verdict, ready to `\input` in the paper.
- [results/out/tables/06c_wage_perturbation.tex](../results/out/tables/06c_wage_perturbation.tex)
  — perturbation tests for any county whose Hessian was not strictly
  positive definite. Empty stub if all counties are strict local minima.

## Reproduction

```sh
sbatch -p general -n 1 --cpus-per-task=8 --mem=16g -t 1:00:00 \
  -J 06c_wage_id -o logs/06c_wage_id_%j.out \
  --wrap='set -euo pipefail; cd "$SLURM_SUBMIT_DIR"; \
          module load r/4.4.0; \
          export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1; \
          Rscript 06c_wage_identification.R'
```

or, as part of the full pipeline:

```r
source("run_all.R")
```

Runtime is ~30–40 minutes (numDeriv Hessian + 8 NM polishes for NYC).

## Recommended next step (open)

The natural follow-up is to (a) seed `seeit_bb.rds` with the lower NYC
basin found by the perturbation polish at step +10, (b) re-run 06 with
`wage_optimizer_mode = pso` (the dual-polish design will pick this up via
`polish_seed`), and (c) re-run 06b on the new solution. If 06b again
reports a saddle, repeat. Convergence to a strict local minimum is the
stopping criterion. This loop has not been automated; it would naturally
fit as a configurable `min_optim` / `pso` outer-iteration mode.
