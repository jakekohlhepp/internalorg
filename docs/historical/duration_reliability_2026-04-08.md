# Duration Reliability Investigation

**Date**: 2026-04-08  
**Scope**: Reliability of the raw `duration` field in `compiled_trxns.rds`, with special attention to very short durations and whether they appear to come from legacy system imports.


## Update (Same Day)

A follow-up transition-based test provides an important refinement to the conclusions below.

The earlier version of this note used a coarse first-6-versus-after-6 comparison across all salons. That statistic turns out to be poorly suited to long imported histories, because a salon can have years of legacy-import data after its first 6 months and still transition cleanly later.

Using a stronger test based on monthly short-duration shares and the first appearance of Boulevard-native booking features (`prebooked` and `was_staff_requested`), a small set of salons does look strongly consistent with legacy imports.

Key refinement:

- `10` salons show a clear legacy-like pattern: extremely high short-duration shares at the start, a clean transition to normal durations, and the onset of Boulevard-native booking features at or just after the transition.
- These `10` salons account for `280,331` of `343,605` appointments with `duration <= 5` minutes, or about `81.6%` of the entire short-duration left tail.
- They account for `46,533` of `48,887` appointments with `duration <= 5` and revenue at least `$100`, or about `95.2%` of the most suspicious cases.

So the corrected interpretation is:

- my earlier aggregate statement was too broad
- Claude's legacy-import explanation is substantially right for the severe offenders
- the bad left tail is mostly driven by a relatively small set of legacy-like salon histories, not by a broad cross-salon persistence problem

The remaining sections below are still useful background, but they should now be read with this refinement in mind.

---

## Executive Summary

The raw `duration` field looks usable for most appointments, but it has a real left-tail quality problem.

- The middle of the distribution is plausible: the 5th percentile is 15 minutes and the median is 45 minutes.
- The left tail is not fully credible: 3.29% of appointments have `duration <= 5`, and 48,887 appointments have `duration <= 5` with revenue at least `$100`.
- The bad left-tail cases are not mostly harmless add-ons. They include common core services such as women's cuts, color, highlights, blow dry, and brow shaping.
- The short-duration problem is not mostly an early-history artifact within salon. Some salons show sharp early spikes, but most of the large-problem salons remain problematic long after their first observed months.
- `total_app_time` is not a reliable general repair for these worst cases. In suspicious very-short appointments, it almost always matches the same bad 5-minute value.

Bottom line: `duration` is still the better primary field than `total_app_time`, but the extreme left tail should not be treated as fully reliable, and the problem appears to be driven largely by persistent salon-specific recording behavior rather than a one-time legacy import.

---

## Data and Method

The audit used the raw transaction file:

- `C:/Users/jakek/jmp_dont_backup/compiled_trxns.rds`

The analysis was restricted to:

- `Hair Salon`
- `hairSalon`
- `Barber Shop`
- `Blowouts & Styling`

Appointments were collapsed to the `app_id` level using:

- `duration = duration[1]`
- `revenue = sum(price)`
- `location_id = location_id[1]`
- `date = date[1]`

This collapse is appropriate because earlier checks showed that `duration` is constant within `app_id`, while `total_app_time` can vary within appointment.

For the legacy-import question, the main diagnostic was the share of appointments with:

- `duration <= 5`

within each salon over time. I compared the first 6 and first 12 months of a salon's observed history to later months.

---

## Overall Distribution

At the appointment level:

- Total appointments: `10,433,124`
- Share with `duration <= 5`: `3.29%`
- Share with `duration <= 10`: `3.66%`
- Share with `duration <= 15`: `13.55%`

Selected quantiles of appointment `duration`:

| Percentile | Duration |
|------------|----------|
| 0% | 0 |
| 1% | 0 |
| 5% | 15 |
| 10% | 15 |
| 25% | 30 |
| 50% | 45 |

This pattern suggests that the center of the distribution is reasonable, but the extreme lower tail contains suspicious values.

---

## Evidence That The Left Tail Is Problematic

Very short durations are sometimes attached to clearly implausible revenue totals.

- Appointments with `duration <= 5`: `343,605`
- Appointments with `duration <= 5` and revenue `>= $100`: `48,887`

Examples from the raw data include appointments with `duration = 5` and revenue in the hundreds or thousands of dollars, including:

- women's haircut
- single process color
- highlights
- blow dry
- brow shaping
- extension-related services
- offsite event / travel fee bundles

These are not isolated edge cases involving only obscure add-ons. The dominant service lines in `duration <= 5 & revenue >= 100` appointments were:

| Service line | Count |
|--------------|-------|
| Women's Hair Cut | 9,573 |
| Brow Shaping | 8,805 |
| Single Process | 4,195 |
| Hi-Lites | 3,321 |
| Brow Tinting | 2,967 |
| Single Process & Hi-Lites | 2,695 |
| Blow Dry After Color | 2,399 |
| Blow Dry | 2,195 |

This is strong evidence that at least some salons are recording placeholder or otherwise non-literal durations in the left tail.

---

## Does `total_app_time` Fix The Worst Cases?

Usually no.

Among the `48,887` suspicious appointments with `duration <= 5` and revenue `>= $100`:

- `92.78%` had non-missing `total_app_time`
- among those with non-missing `total_app_time`, `max(total_app_time)` equaled `duration` in `99.98%` of cases
- `max(total_app_time)` was greater than `duration` in only `0.01%` of cases

This means the worst short-duration cases are usually not cases where `duration` is bad but `total_app_time` provides a clean correction. Instead, both fields typically encode the same short value.

So while `duration` is still preferable overall, `total_app_time` is not a general solution to the left-tail data-quality problem.

---

## Do Short Durations Mostly Disappear Over Time Within Salon?

Mostly no.

To test the legacy-import hypothesis, I measured the share of `duration <= 5` appointments within each salon over its observed history.

### 6-Month Window

Eligible salons:

- at least 200 appointments
- at least 20 short appointments
- at least 12 observed months

Results for `133` eligible salons:

- median short-share in first 6 months: `0.47%`
- median short-share after month 6: `0.29%`
- share of salons with higher short-share in first 6 months than later: `51.88%`
- share of salons with a drop of at least 10 percentage points: `8.27%`
- share of salons with at least 80% of all short appointments concentrated in first 6 months: `11.28%`

### 12-Month Window

Eligible salons:

- at least 200 appointments
- at least 20 short appointments
- at least 24 observed months

Results for `83` eligible salons:

- median short-share in first 12 months: `0.19%`
- median short-share after month 12: `0.30%`
- share of salons with higher short-share in first 12 months than later: `48.19%`
- share of salons with a drop of at least 10 percentage points: `12.05%`
- share of salons with at least 80% of all short appointments concentrated in first 12 months: `4.82%`

These summary results do not support the claim that most small-duration records are early legacy-import artifacts that disappear once salons are live in the platform.

---

## What Does Look Legacy-Like?

A minority of salons do show a pattern that is consistent with onboarding or migration.

Examples from the strongest first-6-month drops include salons where:

- the first 6 months have very high short-duration shares
- later periods have much lower shares
- the salon first appears on a common start date such as `2020-01-02`

This pattern is suggestive of a batch migration or import event for some salons.

However, these cases are the minority, not the rule.

Using a strict "legacy-like" rule:

- first-6-month short-share at least `20%`
- after-6-month short-share at most `2%`
- at least 20 short appointments in the first 6 months

the share of eligible salons satisfying that rule was:

- `0%` in the 6-month sample
- `1.2%` in the 12-month sample with the analogous rule

So genuine legacy-import behavior exists, but it does not explain most of the bad short-duration records.

---

## What The Data Mostly Suggests Instead

The dominant pattern is persistent salon-specific recording behavior.

Several salons with the biggest short-duration problems remain problematic long after their first observed months. For example, some of the largest offenders have:

- nearly all appointments at `<= 5` minutes in the first 6 months
- still very large short-duration shares well after month 6
- median appointment duration of `0` or `5` minutes at the salon level

That pattern is much more consistent with:

- a persistent default duration setting
- salon-specific data-entry conventions
- a POS integration that records nominal or placeholder times

than with a one-time imported legacy history that later cleans itself up.

---

## Implications For Cleaning

The current decision to prefer `duration` over `total_app_time` still looks correct. The evidence does not support reviving the old practice of broadly substituting `total_app_time` for `duration`.

But the left tail of `duration` should not be taken at face value.

The best next step is likely salon-specific cleaning or flagging, not a universal time-variable swap. In particular:

1. Flag salons with persistently high shares of `duration <= 5`.
2. Treat `duration <= 5` as suspicious only when paired with substantive revenue or service descriptions that imply longer appointments.
3. Avoid using `total_app_time` as the default repair field, since it usually shares the same bad short value in the worst cases.
4. Consider salon-level exclusion, winsorization, or targeted replacement rules for establishments with obvious placeholder-duration behavior.

---

## Practical Conclusion

If the question is whether `duration` is perfect, the answer is no.

If the question is whether the short-duration problem mostly reflects legacy imports that disappear over time within salon, the answer is also no.

The investigation points to a narrower and more actionable conclusion:

- `duration` is broadly usable
- the extreme short tail is often not credible
- the problem is mainly driven by persistent salon-specific recording patterns, with only limited evidence of true legacy-import fadeout
