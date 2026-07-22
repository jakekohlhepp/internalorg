# Salon Transaction Data Structure

This skill documents the structure and quirks of the salon transaction data used in the JMP estimation pipeline.

## Data Pipeline Overview

```
Raw Transactions (compiled_trxns.rds)
    │ ~19M rows, all industries
    ▼
00_mk_tasks_cosmo.R
    │ Filters to hair salons
    │ Merges with service classifications
    │ Melts multi-category services
    ▼
Processed Tasks (00_tasks_cosmo.rds)
    │ ~14M rows (expanded due to melting)
    ▼
01_build_data.R
    │ Aggregates to staff-quarter level
    │ Performs hierarchical clustering
    ▼
Working Data (01_working.rds)
    │ Staff-quarter panel
    ▼
02_estimation.R
    │ GMM estimation
    ▼
Parameters (02_parameters.rds)
```

## Raw Transaction Data Schema

| Column | Type | Description |
|--------|------|-------------|
| `location_id` | UUID | Salon location identifier |
| `app_id` | UUID | Appointment identifier |
| `customer_id` | UUID | Customer identifier |
| `staff_id` | UUID | Employee identifier |
| `service_id` | UUID | Service type identifier |
| `service_performed` | string | Service description text |
| `app_datetime` | string | Appointment timestamp |
| `app_tz` | string | Timezone |
| `duration` | integer | Service duration in minutes |
| `was_staff_requested` | boolean | Whether customer requested specific staff |
| `app_stage` | string | Always "final" after preprocessing |
| `prebooked` | boolean | Whether appointment was prebooked |
| `price` | numeric | Service price |
| `total_app_time` | integer | Total appointment time (may differ from sum of durations) |
| `location_city` | string | City name |
| `location_state` | string | State abbreviation |
| `location_zip` | integer | ZIP code |
| `business_id` | UUID | Business identifier |
| `industry` | string | Industry classification |
| `date` | Date | Appointment date |

## Industry Values (Hair-Related)

The pipeline filters to these industries:
- `Hair Salon` (primary)
- `hairSalon` (alternate coding)
- `Barber Shop`
- `Blowouts & Styling`

## Task Category Classification

Services are classified into 6 task categories via `classified_descriptions.rds`:

| Category | Name | Description |
|----------|------|-------------|
| taskcat1 | Haircut/Shave | Cutting, trimming, shaving |
| taskcat2 | Color/Highlight/Wash | Chemical treatments, coloring, washing |
| taskcat3 | Extensions | Hair extensions (merged with taskcat4 in pipeline) |
| taskcat4 | Blowdry/Style/Treatment | Styling, treatments, finishing |
| taskcat5 | Administrative | Consultations, no-shows, adjustments |
| taskcat6 | Nail/Spa/Eye/Misc | Non-hair services |

**Important**: In `01_build_data.R`, Extensions (taskcat3) are merged into Blowdry/Style (taskcat4), resulting in 5 final task types.

### Multi-Category Services

Some services belong to multiple categories:
- ~63% are single-category
- ~14% are two-category
- ~1% are three+ category

Multi-category services are "melted" into separate rows, with duration allocated proportionally based on average durations for each category.

## Known Data Quality Issues

### Duration Problems

1. **Negative durations**: ~5 rows have impossible negative values (data entry errors)
2. **Zero durations**: ~2% of rows have zero duration
3. **Extreme durations**: Thousands of rows exceed 8 hours (480 minutes)
4. **Duration vs total_app_time mismatch**: For multi-service appointments, sum(duration) often differs from total_app_time

### Price Problems

1. **Extreme prices**: Some rows have prices > $5,000 (likely data entry errors)
2. **Zero prices**: ~25% of raw transactions have $0 price (free services, add-ons, or missing data)

### Missing Data

1. **total_app_time**: ~35% missing (only available from 2021 data refresh)
2. **location info**: ~3% missing city/state/zip
3. **service_performed**: 1 row with NA

### Temporal Coverage

- Date range spans many years but density varies
- Pre-2015 data is sparse (~8% of transactions)
- Most data concentrated in 2017-2021

## Appointment Structure

- Most appointments (87%) have a single service
- ~13% have 2-5 services
- <1% have 6+ services
- Maximum observed: 26 services in one appointment

## Geographic Coverage

Data covers all 50 US states but concentration varies. The estimation focuses on three counties:
- Cook County, IL (FIPS: 17031)
- Los Angeles County, CA (FIPS: 06037)
- New York County (Manhattan), NY (FIPS: 36061)

## Staff Patterns

- Most staff work at one location (~85%)
- Some staff work at 2 locations (~10%)
- Few staff work at 3+ locations (~5%)
- Maximum observed: 29 locations for one staff member

## Customer Patterns

- Most customers visit one location (~94%)
- Some visit 2 locations (~5%)
- Few visit 3+ locations (~1%)

## Data Cleaning Applied

### In prep_00_compile_transactions.R
- Combines multiple data pulls
- Removes duplicates (prioritizes original data pull)
- Fixes date format inconsistencies
- Standardizes city name typos
- Filters to "final" appointment stage

### In 00_mk_tasks_cosmo.R
- Filters to hair-related industries
- Excludes one anomalous high-revenue location
- Fixes service name encoding issues
- Merges with service classifications
- Allocates duration for multi-category services
- Drops rows with NA service_performed (1 row)

### In 01_build_data.R
- Merges Extensions into Blowdry/Style category
- Drops 5 rows with negative duration
- Imputes zero durations using quarterly cluster means
- Aggregates to staff-quarter level

## Processing Quirks

### Duration Allocation Logic

For services spanning multiple categories:
```
new_duration = original_duration * (avg_time_this_category / sum_avg_times_all_categories)
```

Average times are computed from single-category, single-service appointments only.

### Month/Week Numbering

- `month_num = year + month/13` (continuous index, e.g., 2019.077 for Jan 2019)
- `week_num = year + week/53`

These are used for time series ordering, not calendar calculations.

## File Size Considerations

| File | Approximate Size | Load Time |
|------|------------------|-----------|
| compiled_trxns.rds | 10+ GB | 2-3 minutes |
| 00_tasks_cosmo.rds | ~2 GB | 30-60 seconds |
| 01_working.rds | ~100 MB | <5 seconds |

For faster loading of the raw file, consider converting to `qs` or `fst` format.
