# Parameter comparison: latest run vs. manuscript draft

Compares the parameter estimates in [results/data/06_parameters.rds](../results/data/06_parameters.rds)
(produced by job 48610268, written 2026-05-07) with the values reported in
the JMP draft [docs/draft_jmp_context.pdf](draft_jmp_context.pdf) (compiled
2025-06-30, 631-replication bootstrap SEs).

The current run was the third successful structural estimation since the
move to `min_optim` mode; it is the run whose wage parameters fed the
bootstrap currently in flight at the time of writing. Subsequent multi-start
and PSO experiments suggest NYC's wage block sits at a non-global local
minimum — see "Why NYC differs" at the bottom.

## Headline numbers

| metric | manuscript | current run |
|---|---|---|
| Wage moment objective (global) | implicit (matched) | 0.00423 |
| Price moment objective (L-BFGS-B value) | not reported in draft | 23.66 |
| Wage moment objective for NYC (county) | implicit | 0.0248 (large) |
| Wage moment objective for LA (county) | implicit | 1.24e-7 (small) |
| Wage moment objective for Cook (county) | implicit | 5.3e-16 (zero) |

The total parameter count is **243** (manuscript) vs **243** (current run) — the
specifications are identical.

## 1. Price sensitivity (Table 8)

Manuscript ρ values are reported as positive (the absolute price coefficient);
in the current parameter file the `factor(county):cust_price` entries are
stored with their natural sign (negative).

| county | manuscript ρ | manuscript s.e. | current \|cust_price\| | Δ (latest − ms) | within ~1 s.e.? |
|---|---|---|---|---|---|
| Cook (17031) | 0.027 | 0.010 | 0.0335 | +0.0065 | ~0.65 s.e. ✓ |
| LA (6037) | 0.016 | 0.004 | 0.0290 | +0.0130 | ~3.3 s.e. ✗ |
| NYC (36061) | 0.018 | 0.014 | 0.0490 | +0.0310 | ~2.2 s.e. ✗ |

All three counties are now **more price-elastic** than the manuscript reports.
Cook is plausibly within sampling error, but LA and NYC are well outside the
manuscript's 631-replication confidence intervals. Direction is the same
(downward-sloping demand) and the cross-county ranking is preserved — NYC most
elastic, LA least — so qualitative interpretation is unchanged. Likely
candidates for the magnitude shift are (a) the move from BBsolve to `min_optim`
changing how the wage stage interacts with the price stage, and (b) the data
prep refresh between manuscript draft and current run; sample sizes per county
appear stable but worth double-checking against Table 7 if numbers rotate
again.

## 2. Skill-set wages (Tables 9-11)

Manuscript wages are relative to Skill Set 1 (which is the omitted reference;
4 free wages per county × 3 counties = 12 entries — matching the 12
`avg_labor:E_raw_w` entries in the parameter file). The manuscript explicitly
notes that skill-set indices are arbitrary, so a position-by-position
comparison is misleading; sorting within each county is the right comparison.

| county | manuscript (sorted) | current run (sorted) |
|---|---|---|
| Cook (17031) | −127.6, −122.7, −80.3, **+537.7** | +6.9, +57.3, +198.3, +350.7 |
| LA (6037) | −7.2, +21.0, +59.8, **+536.8** | +13.3, +40.6, +71.3, **+544.5** |
| NYC (36061) | −166.2, −141.7, −70.1, **+660.4** | **−1058.9, −947.9, −868.9, −674.7** |

Three observations:

1. **LA is the most consistent**. Three of four wages are nearly identical
   in shape (−7/21/60/537 vs 13/41/71/544), and crucially the large positive
   "premium" skill set is preserved at +544 vs the manuscript's +537. Job
   48610268's restart loop was the mechanism that let this LA solution
   appear; before the restart logic, the wage stage rejected LA and pinned
   E_raw_w=0.

2. **Cook lost its premium skill set**. The manuscript has Cook with a
   +538 high-wage skill set; the current run has a much narrower spread
   (7→351) with no extreme positive. Cook converged at machine precision
   (ssq ≈ 5e−16) so this is not a numerical artifact — the solution is
   exact under the current specification, just in a different basin from
   the manuscript.

3. **NYC is qualitatively wrong.** Manuscript NYC has 3 negatives plus a
   single big positive (+660); the current run has all four wages
   negative and 5–10× the magnitude in absolute value. NYC's wage moment
   ssq (0.0248) is also five orders of magnitude above Cook's and LA's,
   confirming the moment conditions are not satisfied at this point. See
   "Why NYC differs" below.

## 3. Skill parameters θ (Tables 9-11, task columns)

The manuscript reports a 5×5 θ matrix per county. Here the rows are worker
types and columns are tasks; columns are listed in the manuscript as
*Administrative, Blowdry/Style, Color/Wash, Haircut/Shave, Nail/Misc*. The
current parameter file stores the same content as `avg_labor:B_raw_w_t` with
numeric indices, and the data-driven cluster labeling means the row/column
permutation is not stable across runs — only the per-county *range* and
*shape* are directly comparable.

| county | manuscript θ range | current run θ range | one large outlier? |
|---|---|---|---|
| Cook (17031) | [−37.6, +56.2] | [−32.2, +33.6] | ms: +56 (Sk2/Nail), curr: +33.6 (W3/Bl), −32.2 (W3/Nail) |
| LA (6037) | [−61.6, +13.3] | [−52.9, +17.5] | both have a single large negative (~−55) |
| NYC (36061) | [−29.2, +47.3] | [−55.7, +82.3] | ms: +47, curr: +82 / −55 — current has wider tails |

Cook and LA θ matrices are roughly the right shape and magnitude. NYC θ has
notably wider tails in the current run (one entry at +82.3, one at −55.7),
consistent with the wage-block discrepancy: with NYC wages off by ~1000,
the model compensates via skill parameters when fitting market shares.

## 4. County-quarter levels (Appendix Table A4)

Three blocks: Demand Level (33 — one quarter dropped as reference), Cost Level
(36), Wage Level (36).

### Demand Level

Manuscript reports Demand Level for 11 quarters per county (2018Q1 dropped).

| county | manuscript range | current range | comment |
|---|---|---|---|
| Cook | [−3.5, −0.4] | [−3.09, −0.11] | matches well, same sign pattern across quarters |
| NYC | [−0.81, +1.81] | [−1.18, +1.51] | matches well |
| LA | [−1.57, +0.04] | [−1.72, +1.12] | one quarter drifts positive in current run |

Demand levels are the closest match across all parameter blocks.

### Cost Level

| county | manuscript range | current range |
|---|---|---|
| Cook | [−867, +748] | [−346, +1122] |
| NYC | [−601, +1149] | [−1161, +386] |
| LA | [−248, +1416] | [+151, +961] |

Same order of magnitude. Specific quarter values differ — Cook and LA Cost
Levels in the current run are noticeably smaller-mean than the manuscript;
NYC's cost levels are flipped in direction (manuscript averages positive;
current averages negative). This is consistent with the NYC wage-block
discrepancy bleeding into the price-stage residuals.

### Wage Level

This is where NYC's wage problem becomes most visible.

| county | manuscript range | current range | ratio (current / ms midpoint) |
|---|---|---|---|
| Cook | [136, 404] (mean ~250) | [6.4, 626] (mean ~108) | ~0.4× |
| LA | [17.7, 270] | [10.5, 405] | comparable |
| NYC | [178, 565] (mean ~250) | **[1071, 1527]** (mean ~1180) | **~4.7×** |

NYC's per-quarter Wage Level in the current run is **roughly five times the
manuscript value**, in every quarter. This is the price-stage's compensation
for the off-basin wage block: when the underlying skill-set wages are
incorrect by ~1000, the per-quarter Wage Level shift required to match
market shares moves accordingly.

## 5. Material costs (Appendix Table A4)

48 entries (4 task categories × 12 quarters; one task is the dropped
reference). Both runs show the same general pattern: large negative
shocks at COVID quarters (2020.1, 2020.4, 2021.1) and noisier values
elsewhere. Magnitude ranges are within 2× of the manuscript across all
four task indices. Direct entry-by-entry comparison is not informative
because the task ordering is data-driven.

## Why NYC differs

The structural wage stage minimizes ‖g_wage‖² per county. The current
NYC solution is at **a true stationary point of that objective** (the
gradient is essentially zero — confirmed by both BFGS and L-BFGS-B
converging at the same point with code 0). It is **not** a Nelder-Mead
artefact. The objective at NYC's local minimum is 0.0248, so the moment
conditions are not zeroed.

Subsequent experiments (job 48962785, smoke `pso_nyc`) showed:

1. NM multi-start in `[-1000, +1000]^4` (which is what the production code
   currently does for NYC) finds the same plateau or a worse one for every
   random initialization. The seed start is the best NM can do.
2. **PSO in `[-2000, +2000]^4`** found a different basin at ssq ≈ 0.00128 —
   roughly 20× lower — with NYC wages of approximately
   `(−22, +498, +151, +1826)`. The shape (one large positive, others mild)
   matches the manuscript's NYC pattern (`(−166, −142, −70, +660)`).
   Magnitudes in the PSO basin are larger, but qualitatively closer.
3. The `min_optim_multistart_scale_by_county` setting at the time of the
   production run (= 5 × parscale = 1000) was not wide enough to sample
   the PSO-discovered basin (E5 ≈ 1826 lies outside the box).

Implications:

* The current NYC wage block in 06_parameters.rds is **the best the production
  code can find** under the current settings, but it is not the global
  minimum.
* Widening NYC's multi-start scale to 2000 would likely allow `min_optim` to
  sample the PSO basin in subsequent runs.
* Even at the PSO basin, ssq remains ~1e−3 — three orders of magnitude
  above `obj_tol = 1e-6`. Whether this means NYC's moment system genuinely
  cannot be zeroed (e.g., due to a saturated worker-type assignment) or
  there is a still-better basin further out is not yet established. A
  larger PSO box and/or more particles is the next sensible smoke test.

## Recommended next actions

1. Bump `min_optim_multistart_scale_by_county["36061"]` to 2000 (or have it
   default to e.g. 10 × parscale), then rerun 06.
2. If the NYC solution at the wider scale still ssq ≈ 0.001, decide
   whether to keep `obj_tol = 1e-6` strict or to loosen the strict-exit
   guard for NYC specifically.
3. Re-comparison after both: many of the demand and cost-level discrepancies
   here are consequences of the NYC wage block being off; once NYC moves
   into the manuscript-shape basin, those quarter-level shifts should
   shrink as well.
