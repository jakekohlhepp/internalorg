# Petrin-Train Two-Step Standard Errors (07_bootstrap.R)

As of 2026-06-10, `07_bootstrap.R` no longer runs a Bayesian (Dirichlet-weight)
bootstrap of the full pipeline. It implements the two-step inference procedure
of Petrin & Train, "Omitted Product Attributes in Discrete Choice Models"
(NBER WP 9452, 2003), published as "A Control Function Approach to Endogeneity
in Consumer Choice Models" (JMR, 2010). Deployment mechanics (array sizing,
env vars, convergence gates) are unchanged and live in
[bootstrap_slurm.md](bootstrap_slurm.md).

## The procedure

**First stage — demand-side 2SLS (`demand == TRUE` rows).** Standard errors
are *analytical*: the CR1 cluster-robust sandwich for the 2SLS estimator,
clustered at `location_id`,

```
V = c * (X'PzX)^-1 [ Σ_g s_g s_g' ] (X'PzX)^-1,   s_g = X̂_g' û_g,
c = G/(G-1) * (N-1)/(N-K),
```

with `X̂ = Pz X` and `û = y − Xβ̂` (residuals at the 2SLS coefficients on the
original regressors). Implemented in
[`cluster_vcov_2sls` in utils/estimation_pipeline.R](../utils/estimation_pipeline.R);
it matches `sandwich::vcovCL(fit, cluster = location_id, type = "HC1")` on the
equivalent `ivreg` fit (the same estimator/SE convention
`05_iv_spec_comparison.R` reports). Saved to
`results/data/07_first_stage_vcov.rds` (point estimates, vcov, SEs, cluster
counts).

**Second stage — structural wage + price parameters.** Each replication `r`
draws a full demand-coefficient vector

```
beta^(r) ~ N(beta_hat, V_clustered)
```

and re-runs the second-stage estimation (wage solver + L-BFGS-B price step)
on the ORIGINAL, unweighted estimation sample, conditioning on `beta^(r)`.
The wage warm start stays at the 06 point estimates, exactly as before. The
reported second-stage SE (in `08_display_estimates.R`) is the standard
deviation of each second-stage parameter across replications.

This matches the way Petrin & Train propagate first-stage sampling error into
the second stage. In the 2003 WP they perturb the first stage by resampling
its data ("we repeatedly estimate the price regressions with bootstrapped
samples ... and re-estimate the [second stage] with the new residuals"); the
draws-from-`N(β̂, V̂)` variant used here is the parametric, asymptotically
equivalent version of that perturbation (the bootstrap distribution of the
first-stage estimator converges to `N(β̂, V̂)`), with `V̂` clustered at
`location_id` to mirror a location-level block bootstrap. The JMR 2010
version notes the bootstrap and the Murphy-Topel / Karaca-Mandic-Train
asymptotic formulas gave nearly identical SEs in their application. The
Murphy-Topel sandwich — which adds the conditional second-stage variance and
the same-sample cross terms — is implemented in `07c_murphy_topel.R`; see
[murphy_topel_proposal.md](murphy_topel_proposal.md) and
`JMP_STRUCTURAL_SE_SOURCE`.

## Known caveat: the conditional second-stage variance term

Petrin & Train's total second-stage variance **adds** the across-draw
variance to the second stage's own conditional sampling variance (the
"traditional formulas ... appropriate when µ is observed without error",
WP 9452 p.13 fn.). The across-draw SD reported here captures only the
first-stage propagation term, so it is a *lower bound* on the strict
Petrin-Train SE. The full correction (conditional second-stage variance +
propagation + same-sample cross terms) is available analytically via the
Murphy-Topel sandwich in `07c_murphy_topel.R`
([murphy_topel_proposal.md](murphy_topel_proposal.md)); switch the reported
structural SEs to it with `JMP_STRUCTURAL_SE_SOURCE=murphy_topel`.

Relatedly: unlike the old Dirichlet scheme, second-stage sampling noise from
the data is **not** resampled at all — every rep sees the same observations
with uniform weights, and the only source of cross-rep variation is the
first-stage draw.

## Practical notes

- **Determinism.** The full `bootstrap_reps × K` draw matrix is regenerated
  inside every array task from `JMP_BOOTSTRAP_SEED` (`set.seed` +
  `MASS::mvrnorm`), then each task uses only its own row — the same
  determinism contract the Dirichlet weights had. Only non-array runs (and
  the combine-only pass) persist `07_first_stage_vcov.rds` and
  `07_boot_draws.rds`, so 1100 concurrent tasks never race on shared files.
  `load_or_create_parameter_draws` invalidates a stored draw file whose seed,
  rep count, parameter names, or point estimates no longer match (e.g. after
  a 06 re-run).
- **Rank-deficient vcov.** With G location clusters and K demand parameters,
  the clustered meat has rank ≤ G. If G < K the draws live in a
  G-dimensional subspace; `cluster_vcov_2sls` warns when this binds.
- **Weak first stages propagate.** A demand coefficient with a large
  clustered SE (e.g. NYC's price coefficient ρ — see the NYC weak-ID memory)
  produces draws that move the second-stage wage surface a lot, and a draw
  that flips the sign of a county's ρ can make a rep's wage solve land in a
  bad basin or fail outright. Those reps surface through the usual
  `status` / `wage_convergence` machinery; this is the procedure working as
  designed, not a bug.
- **Monotone-constrained demand (06b) is unsupported.** `07_bootstrap.R`
  stops if `skill_monotone_orientation != "none"`: the analytical 2SLS
  sandwich is not the sampling distribution of the constrained QP estimator.
- **Rep count.** `CONFIG$bootstrap_reps` stays at 1100 (env-overridable via
  `JMP_BOOTSTRAP_REPS`). Petrin-Train used 50 draws; 100–200 is typical in
  applied work. Since per-rep cost is hours of wage solving, trimming the
  array (e.g. `--array=1-200%200`) is a legitimate budget lever now — the
  draw-based SD converges much faster than a full nonparametric bootstrap.

- **Stale pre-rewrite reps are refused, not mixed.** Every new rep row
  carries `procedure = "petrin_train_draws"`. The resume ("skip if ok")
  logic re-runs any rep file without that stamp, and the combine pass
  `stop()`s if any stamped-less file remains — so a `bootstrap_reps/`
  directory full of old Dirichlet reps cannot silently leak into the new SE
  distribution. Archive or delete the old `boot_res_*.rds` files before
  submitting the new array.

## File map

| File | Role |
|---|---|
| `results/data/07_first_stage_vcov.rds` | `list(beta, vcov, se, cluster_variable, n_clusters, n_obs, rank, small_sample_adjustment, type, estimator)` |
| `results/data/07_boot_draws.rds` | `list(seed, beta, parm_names, draws)`; `draws` is reps × K |
| `results/data/bootstrap_reps/boot_res_<i>.rds` | one row per rep: drawn demand values + re-estimated wage/price values + `status` / convergence codes (format unchanged), plus a `procedure = "petrin_train_draws"` stamp |
| `results/data/07_bootstrap.rds` | combined reps (format unchanged) |
| `results/data/07_boot_weights.rds` | **obsolete** (pre-2026-06 Dirichlet weights); no longer read or written |

`08_display_estimates.R` reads `07_first_stage_vcov.rds` for the demand-row
SEs and uses the across-rep SD only for the wage/price rows. If the vcov file
is missing it falls back to across-rep SDs for everything (with a message);
note the demand columns' across-rep SD ≈ the analytical SE by construction,
since the demand columns of the rep file contain the draws themselves — a
useful sanity check.
