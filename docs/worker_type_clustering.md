# Worker-type clustering: cut height rule

## What `cluster.R` does

`assign_worker_types_baseline` in [`cluster.R`](../cluster.R) maps every
staff-firm-quarter to one of `CONFIG$n_worker_types` latent types. The flow is:

1. Build `Btilde_raw_{t}` for each staff-firm-quarter: the worker's share of
   the firm's task-`t` minutes, divided by the firm's overall task-`t` share.
2. For each firm-quarter, run complete-linkage `hclust` on
   `Btilde_raw_*` and read off `min_cutlevel` -- the smallest dendrogram
   cut height that splits the firm into at most `n_worker_types` clusters.
3. Cut every firm-quarter at a single county-wide height
   (`county_cutlevel`), then match the resulting within-firm clusters to a
   reference firm-quarter so types are comparable across firms.

Steps 1 and 2 are unambiguous. Step 3 is governed by a single rule, which
is the subject of this note.

## Cut height rule and the `cutlevel_quantile` config knob

The county cut height is set from a configurable quantile of the per-firm-quarter
`min_cutlevel`:

```r
county_cutlevel    <- quantile(min_cutlevel, CONFIG$cutlevel_quantile, type = 7)
effective_cutlevel <- pmax(county_cutlevel, min_cutlevel)  # per firm-quarter
```

`effective_cutlevel` is what `cutree(h = ...)` is actually called with for
each firm-quarter.

- `CONFIG$cutlevel_quantile = 1.0` reproduces the legacy `max(min_cutlevel)`
  rule -- a single outlier firm-quarter sets the height for every salon in
  the county.
- `CONFIG$cutlevel_quantile = 0.9` (current default) lets the firm with the
  90th-percentile `min_cutlevel` set the county height. The top decile fall
  back to their own `min_cutlevel` via the `pmax`, so no firm ever exceeds
  `n_worker_types` clusters.

Under complete linkage, `pmax(county_cutlevel, min_cutlevel)` is equivalent
to "cut the dendrogram at `county_cutlevel`, then complete-linkage-merge the
excess clusters until at most `n_worker_types` remain": the merges that
would happen between `county_cutlevel` and `min_cutlevel` are exactly the
next complete-linkage merges by construction. So the implementation does
not need a separate merge loop.

The same rule is reapplied in [`09_invert_gammas.R`](../09_invert_gammas.R)
when it reclusters the full panel to invert gammas for firm-quarters
outside the estimation sample.

## Why the rule matters for LA

`max(min_cutlevel)` has the side effect that the firm with the most
heterogeneous staff sets the cut height for the whole county. The county
cut height under the legacy rule was:

| County | Legacy max | New p90 |
|---|---|---|
| LA (06037) | 0.958 | 0.689 |
| Chicago (17031) | 0.550 | 0.346 |
| NYC (36061) | 0.990 | 0.690 |

In LA, the max was driven by a single 2018 Q1 firm-quarter; only 1 of 512
LA firm-quarters sat at the max, and only 3 within 5% of it. A
`county_cutlevel` of 0.958 collapses 142 of 512 LA firm-quarters to a
single observed worker type -- those firms then fail the
`types_observed_firm > 1` filter and drop out of the estimation sample.

Under `cutlevel_quantile = 0.9`, the LA cut drops to 0.689. The number of
LA firm-quarters that resolve into 5 types rises from 14 to 62; the
1-type firm-quarters fall from 142 to 113. The eligible reference-firm
pool (`types_observed_firm == n_worker_types &
service_mix_id == "11111"`) expands from 14 firm-quarters / 4 salons to
59 firm-quarters / 13 salons in LA, which makes the reference-firm anchor
much less fragile.

The reference *firm* identity is stable for all three counties under the
rule change; only the reference *quarter* shifts for LA and NYC.

## Restoring legacy behavior

Set `cutlevel_quantile = 1.0` in `config.R`. All upstream artifacts
(`mkdata/data/01_*.rds`, `results/data/09_withgammas.rds`) must be
regenerated to reflect the change.

## Saved artifact additions

`mkdata/data/01_worker_type_lookup.rds` now contains, in addition to the
prior fields:

- `effective_cutlevels`: per firm-quarter `(min_cutlevel, county_cutlevel,
  effective_cutlevel)` table -- useful for diagnostic plots and for
  auditing how each firm-quarter was clustered.
- `cutlevel_quantile`: the scalar quantile in effect when the artifact
  was built.
