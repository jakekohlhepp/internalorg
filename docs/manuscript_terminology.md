# Manuscript Terminology Glossary

Canonical terms from the manuscript ("The Inner Beauty of Firms," Jacob
Kohlhepp, June 30 2025 draft — `docs/draft_jmp_context.pdf`, 75 pp.), extracted
2026-07-06 so that pipeline figure/table text (`19_counterfactual_figures.R`,
`18_counterfactual_summary.R`, `22_skill_parameter_units.R`) stays aligned
with the paper's language. Page numbers are the printed page numbers, which
match the PDF page numbers.

## Quick alignment table

| Repo concept | Paper's term |
|---|---|
| `realloc` solve mode | **Reallocation Equilibrium** (prices `p_j` adjust, task assignments `B_j` fixed at baseline) |
| `reorg` solve mode | **Reorganization Equilibrium** (also "Full (Reorganization) Equilibrium") |
| `s_index` | **S-Index** in figures/tables; "s-index" in running text; long form "task specialization index", notation `I(B_j)` |
| labor productivity | **Labor productivity** ("Prod. Change" in tables) = labor-weighted average **endogenous quality** |
| immigration CF | **Low-Wage Immigration** (increase in labor supply of the lowest-wage skill set); shocked type = **"immigrant skill set"** |
| merger CF | **Increase in Market Concentration** (table row: **"Incr. Concentration"**; the word "merger" is never used — half of salons are *removed*) |
| market share / weight | **Market share** only (never "customer share" or "market weight"); axis "Market Share Change" |
| price | **Price** (axis "Price Change"); no "customer price"/"equilibrium price" |
| wages | **Wage**, one per worker skill set, `w_i`/`w(i)`; "market clearing wages"; table wages relative to Skill Set 1 |
| worker types | **Skill Set 1–5** ("worker skill sets", numbering arbitrary); described ad hoc, e.g. "low-skill generalist" |
| organization cost | **Organization Cost (Gamma)**, `γ_j`; axis "Log Organization Cost (Gamma)" |
| units of people | Establishments are **salons**; counted people are **customers** |

## 1. Reallocation vs. reorganization equilibrium

- Definition, p. 36 (Section 7): "To make this point, I first define the
  reallocation equilibrium. It is the outcome when firms are allowed to adjust
  prices (p_j) but organization structures (B_j) are fixed at the initial
  equilibrium choices. ... I define the reorganization equilibrium as the
  outcome when firms are allowed to fully adjust their task assignments. It is
  the full equilibrium described in Section 4.1."
- First named p. 23 (after Prop. 3): "I call this benchmark the reallocation
  equilibrium, and use it extensively in Section 7."
- Informal contrasts: salons "prevented from internally reorganizing" vs
  "allowed to internally reorganize" (p. 38); "when reorganization is shut
  down", "if reorganization is neglected" (pp. 39-40).
- Table 14 column spanners: "Reallocation" / "Reorganization", sub-columns
  "S-Index Change" and "Prod. Change".
- Figure captions: "Figure A1: Low Wage Immigration: Reallocation
  Equilibrium"; "Figure A2: Low Wage Immigration: Full (Reorganization)
  Equilibrium" (pp. 69-70).
- Baseline: the **"initial equilibrium"** / **"baseline equilibrium"**
  (Table 14 note: "Effects are percent changes from the baseline
  equilibrium.").

## 2. Specialization measure

- Definition 2, p. 11: "The task specialization index (s-index) of
  establishment j with organization B_j is I(B_j) := D_KL(B_j||G(B_j)) ...
  the unit of measurement is the nat." `G(B_j)` is the "generalist
  benchmark".
- Casing: lowercase "s-index" in text; "S-Index" in table rows and figure
  axes; "Task Specialization Index (S-index)" in figure titles.
- Aggregate: "the percentage change in the average s-index (weighted by total
  labor)"; prose "aggregate task specialization" (p. 38).

## 3. Labor productivity / endogenous quality

- Model-based counterfactual object, p. 35 (Section 6.3): labor productivity
  of skill set i is "the average of endogenous quality across all
  establishments weighted by the amount of labor employed by that salon in
  equilibrium." The B-weighted-skill sum itself is **endogenous quality**
  (xi_j, "the average quality of the tasks performed", p. 20).
- Descriptive (Section 3) measure: "Revenue per Minute" — "total revenue
  divided by the total duration of all services" (p. 12).
- The paper does not say "quality output per hour" or "quality units."

## 4. Worker skill sets

- "Worker skill sets", numbered Skill Set 1-5; "worker skill sets are
  numbered arbitrarily" (p. 33). Adjective form: "skill-set-3 workers".
- Ad hoc descriptions by task profile: "Skill Set 2 in New York County is a
  high-wage color-blow-dry/style specialist" (p. 33); "Skill Set 1, which
  Table 10 shows is a low-skill generalist" (LA, p. 37).
- Task categories (Table 2): Haircut/Shave; Color/Highlight/Wash;
  Blowdry/Style/Treatment/Extensions; Administrative; Nail/Spa/Eye/Misc.

## 5. Low-wage immigration counterfactual

- Setup, p. 37: "**Low-Wage Immigration.** There is a 10% increase in the
  total labor supply of the worker skill set with the lowest wage in each
  market. I use wages from fully solving the model rather than estimated
  wages." (Current pipeline: shock sized as 5% of county labor; the target
  rule — lowest baseline wage — is unchanged.)
- "I focus on low-wage rather than low-skill immigration because workers
  differ in multiple dimensions of skill" (p. 37). Never "low-skill
  immigration", never "influx".
- Shocked type: the **"immigrant skill set"** / "low wage skill set" (p. 39).
- Appendix A.15 panel titles state takeaways: "(a) Immigrant Skill Set
  Employed at Low γ", "(b) High γ Reduce Prices", "(c) High γ Gain, Low γ
  Lose Market Share", "(d) Specialized Jobs Lost" (reallocation);
  "(a) Incorporation of Immigrant Skill Set", "(b) All Reduce Prices",
  "(c) Most Gain Market Share", "(d) Specialized Jobs Created"
  (reorganization). Pie-scatter axes: "Initial Immigrant Skill Set" /
  "Additional Immigrant Skill Set".

## 6. Increase in market concentration counterfactual

- Setup, pp. 37-38: "**Increase in Market Concentration.** Half of the salons
  in each market are removed. Because each salon in the data represents a
  number of actual salons in each market, this change is implemented by
  reducing the number of actual salons that each salon in the data
  represents." Footnote: "This is similar to merging salons with the same
  characteristics."
- Table 14 row: "Incr. Concentration"; prose "Increased concentration".
  The word "merger" is not used in the paper's own framing.
- Other counterfactual rows: "Sales Tax", "Management Diffusion".

## 7-10. Market shares, wages, decomposition language, prices

- **Market share** always (axis "Market Share Change"); never "market weight".
- Wages: `w_i`/`w(i)` per skill set; "market clearing wages" (p. 68); county
  tables "Worker Wages and Skill Parameters for [County] Salons" with wages
  relative to Skill Set 1; Table A3 "Counterfactual Productivity and Wage
  Effects by Worker Skill Set" reports percent changes from the baseline
  equilibrium.
- No formal within/between decomposition vocabulary; the contrast is
  "reallocation of labor across firms" vs firms "internally changing" /
  "internal reorganization". Recurrent job framing: "specialized jobs are
  destroyed/created" (pp. 5, 39).
- **Price** plainly ("These salons respond by reducing prices"); a salon's
  price is quarterly revenue over customers (p. 10); rho is "consumer price
  sensitivity" (p. 20).

## Figure/table caption style

- Short title-case titles with bold "Figure N:"/"Table N:" prefixes; appendix
  items A1, A2, ...
- Sub-panel titles state the takeaway ("(c) High γ Gain, Low γ Lose Market
  Share"), not just the content.
- Explanations live in a separate bold "Note:" block under the figure/table,
  1-3 full sentences, sentence case.
- Greek spelled out parenthetically in axis labels: "Log Organization Cost
  (Gamma)".
