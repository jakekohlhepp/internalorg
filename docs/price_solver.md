# Equilibrium Price Solver: Notes, Contraction Analysis, and Verification

This note documents the Lambert‑W price solver used in the counterfactual
pipeline, why the rearrangement is correct, what its contraction guarantees
actually are, how it compares to Duarte (2025, *Economics Letters*), and how
its correctness was verified end‑to‑end.

## Provenance and attribution

This file consolidates a Claude‑driven walkthrough of the price solver.
Authorship of the underlying material is split as follows.

**User‑authored (Jake Kohlhepp, JMP work).**
- The entire estimation and counterfactual codebase, including the
  `best_respond()` function reproduced below.
- The decision, after reviewing the analysis, to leave the solver as is.
- The final form of [tests/manual_check_price_solver.R](../tests/manual_check_price_solver.R):
  - the `sink()`‑based logging shim that writes
    `tests/manual_check_price_solver.log`,
  - the switch from a hand‑rolled Newton Lambert‑W to `lamW::lambertW0`
    (matching the package the counterfactual scripts actually use),
  - the **WEIGHTED‑PROFIT FOC** extension to Test D, which checks a third
    interpretation of `wgt`,
  - Test E, the 100‑firm heterogeneous‑quality stress test.

**Claude‑authored (this assistant).**
- Locating `best_respond()` in the pipeline and the supporting prose below.
- The Lambert‑W derivation from the Bertrand–Nash FOC.
- The contraction analysis (Jacobian, row‑sum bound, conditions for it being
  a contraction).
- The comparison with Morrow & Skerlos (2011) and Duarte (2025) and the
  pro/con assessment of switching.
- The initial scaffold of `tests/manual_check_price_solver.R`: the
  `best_respond()` reproduction, the `foc_residual()` reference, and Tests
  A–C plus the original (internal‑consistency) form of Test D.

User direction in this thread: questions about the algorithm, contraction
guarantees, and Duarte (2025); explicit instruction `leave as is`; explicit
instruction to deeply examine the solver and build a small test.

## Where the solver lives

`best_respond()` is duplicated verbatim in four counterfactual scripts:

- [14_counterfactual_diffusion.R:117-125](../14_counterfactual_diffusion.R) — also at lines 184‑196 and 316‑323
- [15_counterfactual_sales_tax.R](../15_counterfactual_sales_tax.R)
- [16_counterfactual_immigration.R](../16_counterfactual_immigration.R)
- [17_counterfactual_merger.R](../17_counterfactual_merger.R)

It is the inner price step inside the larger county‑level wage / labor‑market
clearing loop driven by `BBsolve` + `pracma::broyden`. The wage loop calls
`solve_org` (entropy‑regularized internal organization, solved with SQUAREM in
[utils/structural_solver.R:341-347](../utils/structural_solver.R)), feeds the
resulting $(Q, C)$ into `best_respond()`, and uses the implied product shares
to compute the labor‑clearing residual that wages have to zero out.

## The code chunk

```r
best_respond <- function(p0, Q, C, wgt) {
  old_p <- p0
  for (i in 1:10000000) {
    new_p <- -1/rho[cnty] + C - lambertW0(
      exp(-1 + Q + rho[cnty]*C) /
      (1 + sum(wgt*exp(Q + rho[cnty]*old_p)) - exp(Q + rho[cnty]*old_p))
    ) / rho[cnty]
    if (all(abs(new_p - old_p) < outertol)) break
    old_p <- new_p
  }
  return(new_p)
}
```

`rho[cnty]` is the negative price coefficient in logit demand (in the active
draft, `rho < 0`); `Q` is the quality index; `C` is marginal cost; `wgt` is
the per‑observation replication weight; `lambertW0` is the principal branch of
the Lambert W function from the `lamW` package.

## Derivation of the Lambert‑W rearrangement

Demand is plain (multinomial) logit with outside option:

$$s_j(p) = \frac{e^{Q_j + \rho p_j}}{1 + \sum_k w_k e^{Q_k + \rho p_k}}.$$

For a single‑product Bertrand–Nash firm $j$ the FOC is the textbook

$$p_j = c_j - \frac{1}{\rho\,(1 - s_j)}.$$

Define the inclusive‑value‑outside‑of‑$j$ aggregate

$$D_j(p) \;=\; 1 \;+\; \sum_k w_k e^{Q_k + \rho p_k} \;-\; e^{Q_j + \rho p_j},$$

so that $1 - s_j = D_j / (1 + \sum_k w_k e^{Q_k + \rho p_k})$ and therefore
$1/(1 - s_j) = 1 + e^{Q_j + \rho p_j}/D_j$. The FOC becomes

$$p_j \;=\; c_j \;-\; \frac{1}{\rho} \;-\; \frac{e^{Q_j + \rho p_j}}{\rho\, D_j}.$$

Set $y_j \equiv -\rho(p_j - c_j) - 1$. Substituting $\rho p_j = \rho c_j - 1 - y_j$:

$$y_j \, e^{y_j} \;=\; \frac{e^{-1 + Q_j + \rho c_j}}{D_j(p)}.$$

By the defining identity $W_0(z) e^{W_0(z)} = z$,

$$y_j \;=\; W_0\!\left(\frac{e^{-1 + Q_j + \rho c_j}}{D_j(p)}\right),$$

which back‑substitutes to the formula in the code:

$$p_j \;=\; c_j \;-\; \frac{1}{\rho} \;-\; \frac{1}{\rho}\, W_0\!\left(\frac{e^{-1 + Q_j + \rho c_j}}{D_j(p)}\right).$$

The rearrangement is **algebraically exact for every weight vector**. What
weights change is only how strongly $D_j$ depends on $p_j$ (through the
$(w_j - 1) e^{Q_j + \rho p_j}$ contribution that is absent when $w_j = 1$),
i.e. the iteration's contraction modulus, not the equilibrium it finds.

## Contraction analysis

Call the operator $T$, with $T_j(p) = c_j - 1/\rho - W_0(u_j(p))/\rho$, where
$u_j(p) = e^{-1 + Q_j + \rho c_j}/D_j(p)$. Using $W_0'(u) = W_0/[u(1+W_0)]$
and writing $\sigma_k = w_k e^{Q_k + \rho p_k}/D_j$:

- **Cross‑price** ($k \neq j$): $\partial T_j/\partial p_k = \sigma_k\, W_0/(1+W_0)$.
- **Own price** ($k = j$): $\partial T_j/\partial p_j = (w_j - 1)\, \sigma_j\, W_0/(1+W_0)$.

The $L^\infty$ row‑sum bound is therefore

$$L_j(p) \;=\; \frac{W_0(u_j)}{1 + W_0(u_j)} \cdot \frac{D_j - 1 + (w_j - 2)\,w_j\,e^{Q_j+\rho p_j}\,\mathbf{1}_{w_j \neq 1}}{D_j}.$$

Two regimes emerge:

1. **$w_j = 1$ for every $j$.** The own‑price term vanishes and
   $L_j(p) = (W_0/(1+W_0)) \cdot ((D_j-1)/D_j)$ — a product of two
   factors each strictly in $(0, 1)$. The map is a **local $L^\infty$
   contraction at every point**, hence locally unique fixed point with
   exponential convergence. On any bounded invariant set the supremum is also
   strictly less than 1, giving Banach contraction on that set. The constant
   can approach 1 in dense markets (large $D_j$) or high‑margin markets
   (large $W_0$), so convergence is exponential but can be slow.

2. **$w_j \neq 1$.** The own‑price derivative is no longer zero. The clean
   global proof is lost. Empirically the iteration still converges in the
   tests — Tests C (10 iters) and E (11 iters) both finish quickly — but no
   uniform contraction rate is guaranteed.

3. **Multi‑product ownership / non‑Bertrand conduct.** Outside the
   single‑product Bertrand–Nash assumption the rearrangement does not apply
   at all: the FOC has additional cross‑product margin terms that the
   Lambert‑W formula does not encode. There is no own conduct/multi‑product
   extension.

## Comparison with Duarte (2025)

Duarte, *Economics Letters* 250 (2025), "Extending fixed‑point methods for
equilibrium computation in markets with differentiated products" ([RePEc](https://ideas.repec.org/a/eee/ecolet/v250y2025ics0165176525001120.html);
[ScienceDirect](https://www.sciencedirect.com/science/article/abs/pii/S0165176525001120)),
extends the Morrow & Skerlos (2011) $\zeta$‑markup reformulation of the BLP
pricing problem beyond Bertrand–Nash to Cournot and linear‑pricing /
double‑marginalization conduct. The $\zeta$ map iterates
$p^{(t+1)} = c + \zeta(p^{(t)})$, where $\zeta$ is built from a
$\Lambda$/$\Gamma$ decomposition of the demand Jacobian and an ownership
matrix $\Omega$.

The Lambert‑W form in this codebase is the **single‑product, plain‑logit,
Bertrand–Nash special case**: the FOC admits a closed‑form inversion via
$W_0$, so each price update is $O(n)$ and exact per step rather than
requiring Jacobian assembly. The two methods are siblings — both are
reformulations of the FOC into a fixed point with small contraction modulus
— but the Lambert‑W form is narrower.

### Pros of switching to Duarte's $\zeta$ method

1. **Correctness for the merger counterfactual.** The Lambert‑W formula has
   no place for an ownership matrix $\Omega$. After a merger, what
   `best_respond()` computes is still single‑product Bertrand prices, not
   prices internalized across the merged firm's product portfolio. The
   $\zeta$ map plugs in $\Omega$ directly. If only one counterfactual is
   migrated, this is the one.
2. **Conduct generality.** Duarte's contribution is showing the same
   iteration is a contraction for Cournot and double‑marginalization. If a
   conduct comparison ever enters the JMP, $\zeta$ extends "for free" while
   Lambert‑W has no analogue.
3. **Mixed‑logit ready.** With random coefficients the closed‑form inversion
   breaks; $\zeta$ averages over draws.
4. **Removes the $w_j = 1$ contraction caveat.** $\zeta$ does not depend on
   weights for its theorem.
5. **The skeleton already exists.** The commented‑out block at
   [14_counterfactual_diffusion.R:199-210](../14_counterfactual_diffusion.R)
   labelled `## from conlon` is essentially a Morrow–Skerlos $\zeta$ update.
   Reviving it is much smaller than starting from scratch.

### Cons

1. **Per‑iteration cost is higher.** $O(n)$ Lambert‑W vs. $O(n^2)$ Jacobian
   assembly. For hundreds of salons per market this is fine, but the closed
   form is genuinely faster and more numerically stable.
2. **Single‑product plain logit is exactly where Lambert‑W is at its best.**
   Diffusion / sales‑tax / immigration counterfactuals are this regime; the
   switch buys nothing operationally.
3. **It does not fix the actual bottleneck.** The hand‑tuned `BBsolve`
   ladders + `broyden` polish in
   [14_counterfactual_diffusion.R:240-247](../14_counterfactual_diffusion.R)
   solve the *outer* labor‑market clearing fixed point. Replacing the inner
   price step does not help the outer loop.
4. **More moving parts to verify.** Ownership matrix, demand‑Jacobian
   decomposition, and a contraction proof for endogenous costs (which here
   come from the entropy organization step, not exogenous primitives).
5. **Numbers move.** Anything that changes the post‑merger equilibrium
   requires re‑computing standard errors (`07_vcov.R`) and rewriting.
6. **Migration touches four files.** `best_respond()` is duplicated, so
   either factor it into [utils/counterfactuals_core.R](../utils/counterfactuals_core.R)
   first or accept four edits.

### Decision

Per user direction: **leave the solver as is.** The case for switching is
strong only in the merger counterfactual, where the cost is modeling
(single‑product Bertrand internalization) rather than computational. If that
counterfactual is ever revisited as a multi‑product story, factor
`best_respond()` out of the four scripts into a shared helper, keep the
Lambert‑W path as default, and add a $\zeta$ path that activates when an
ownership matrix with off‑diagonal entries is supplied.

## Correctness verification

The driver is [tests/manual_check_price_solver.R](../tests/manual_check_price_solver.R),
runnable with

```
Rscript tests/manual_check_price_solver.R
```

It writes a transcript to `tests/manual_check_price_solver.log`. The script
reproduces `best_respond()` byte‑for‑byte from
`14_counterfactual_diffusion.R` and uses the same `lamW::lambertW0`
implementation as the pipeline.

### Test setup

- **A — Monopolist (closed form).** $J = 1$, $w = 1$, $\rho = -2$. Because
  $D_j \equiv 1$ when $w = 1$, the FOC has the closed form
  $p^* = c - 1/\rho - W_0(e^{-1 + Q + \rho c})/\rho$. Iteration must
  converge in one step.
- **B — Symmetric duopoly.** $J = 2$, $w = (1, 1)$, $Q$ and $c$ symmetric.
  Reference computed with `uniroot()` on the 1‑D reduced FOC.
- **C — Asymmetric 3‑firm market.** $J = 3$, $w = (1, 1, 1)$, distinct
  $Q, c$. Cross‑checked with `rootSolve::multiroot()` on the full $3 \times 3$
  FOC system.
- **D — Weighted 3‑firm market.** $J = 3$, $w = (2, 3, 1.5)$. Three FOC
  residuals are computed against the converged price:
  - the **implied** FOC, which is the algebraic identity Lambert‑W is
    inverting (internal consistency check),
  - the **textbook** single‑product FOC residual,
  - the **weighted‑profit** FOC, which assumes each $w_j$ multiplies firm
    $j$'s own demand into its profit and changes the own derivative.
- **E — 100‑firm heterogeneous market.** Stress test with $\rho = -1.25$,
  $Q$ ranging over $[-10, 11]$, $C$ over $[0.04, 1.59]$, all $w_j = 1$.

### Results

| Test | Reference | abs error | FOC residual | Iters |
|---|---|---|---|---|
| A — Monopolist | analytical closed form | **0** | $0$ | 2 (one‑shot) |
| B — Symmetric duopoly | `uniroot` 1‑D root | $1.9 \times 10^{-12}$ | $2.2 \times 10^{-12}$ | 8 |
| C — Asymmetric 3‑firm | `rootSolve::multiroot` on full FOC | $1.6 \times 10^{-12}$ | $1.9 \times 10^{-12}$ | 9 |
| D — Weighted 3‑firm (implied FOC) | self‑consistency | — | $3.7 \times 10^{-13}$ | 10 |
| D — Weighted 3‑firm (textbook FOC) | $1/[\rho(1-s_j)]$ form | — | $4.1 \times 10^{-13}$ | — |
| D — Weighted 3‑firm (weighted‑profit FOC) | $1/[\rho(1-w_j s_j)]$ form | $1.6 \times 10^{-1}$ | $2.2 \times 10^{-1}$ | — |
| E — 100‑firm market | self‑consistency at scale | — | $1.1 \times 10^{-11}$ | 11 |

**Verdict.** Under the textbook single‑product Bertrand–Nash interpretation,
the solver is correct to machine precision. Convergence is fast — one shot
for the monopolist, ≤ 11 iterations for 100 firms, all with `outertol = 1e-10`.

### Open question raised by Test D

Tests A–C, E, and the implied/textbook columns of D all confirm correctness
under one model: each row is a singleton firm and $w_j$ is a replication
weight that scales the row's contribution to the *inclusive value* but does
not multiply the firm's own demand inside its own profit. The
**weighted‑profit FOC** in Test D probes the alternative interpretation in
which $w_j$ multiplies the firm's own demand inside its profit, so the FOC
becomes $p_j = c_j - 1/[\rho(1 - w_j s_j)]$. Under that interpretation
`best_respond()` produces price gaps of order $10^{-1}$ — i.e., a different
equilibrium.

Which interpretation is intended depends on what `weight` represents in
[14_counterfactual_diffusion.R:128](../14_counterfactual_diffusion.R) and the
analogous lines in 15/16/17. In the current draft `weight` is the row's
sample/replication weight (each "firm" is an estimation row representing
multiple similar salons that price independently), so the singleton/inclusive‑value
interpretation is the right one and `best_respond()` is correct. If a future
revision ever reinterprets `weight` as a within‑firm product mass, the
weighted‑profit FOC would apply and the formula would need to be re‑derived.
The test file documents this fork explicitly so the choice is not silent.

## How to re‑run

```
Rscript tests/manual_check_price_solver.R
```

Output streams to stdout and to `tests/manual_check_price_solver.log`.
Requires `lamW` and (for Tests C and D's cross‑checks) `rootSolve` from the
`renv` library.
