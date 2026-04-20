#' =============================================================================
#' STEP 01_02: Spatial Correlation of Productivity, Specialization, Return Rate
#' =============================================================================
#' For each of the three focal counties, plots a ZIP-code choropleth map of:
#'   - rev_labor   (revenue per minute, a productivity proxy)
#'   - s_index     (task-specialization index)
#'   - return_cust (fraction of customers who are returning from a prior quarter)
#'
#' Also produces a state-level map showing how many salons are observed in
#' each U.S. state (sample coverage).
#'
#' Input:
#'   - results/data/01_01_stylized_facts_data.rds
#'     (produced by 01_01; contains firm-quarter panel with location_zip,
#'      county, rev_labor, s_index, pastrepeat_rate, location_state)
#'   - 20220727_countypop/geocorr2022_2220801561.csv        (GEOCORR ZCTA->county crosswalk)
#'   - 20240415_census_zcta_shapefiles/cb_2018_us_zcta510_500k.shp
#'     (Census cartographic-boundary ZCTAs for all U.S.)
#'
#' Output:
#'   - results/out/figures/01_02_spatial_cor_<FIPS>_{rev,sindex,return_rate}.png
#'     (9 files: 3 counties x 3 variables)
#'   - results/out/figures/01_02_coverage.png (state-level salon count map)
#' =============================================================================

# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------
library('data.table')
library('fixest')
library('sf')
library('ggplot2')
library('ggthemes')
library('stringr')

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
source('config.R')

ensure_directory("results/out/figures")

stylized_path <- file.path("results", "data", "01_01_stylized_facts_data.rds")
zcta_county_path <- file.path(CONFIG$raw_data_path,
                              "20220727_countypop/geocorr2022_2220801561.csv")
zcta_shape_path <- file.path(CONFIG$raw_data_path,
                             "20240415_census_zcta_shapefiles/cb_2018_us_zcta510_500k.shp")
assert_required_files(c(stylized_path, zcta_county_path, zcta_shape_path))

# -----------------------------------------------------------------------------
# Load 01_01 output and aggregate to the ZIP level
# -----------------------------------------------------------------------------
working_data <- data.table(readRDS(stylized_path))

## sanity regression: within-ZIP correlation of productivity and specialization.
## Printed for the record only — not saved to disk.
if (isTRUE(CONFIG$verbose_logging)) {
  print(summary(feols(data = working_data,
                      rev_labor ~ s_index | location_zip,
                      cluster = ~ location_id)))
}

## state-level observation counts (for the coverage map)
state_data_count <- working_data[, .(salons = uniqueN(location_id), count = .N),
                                 by = "location_state"]

get_state <- Vectorize(function(y) {
  if (y %in% state.abb) {
    return(str_to_lower(state.name[which(y == state.abb)]))
  } else if (y == "DC") {
    return("district of columbia")
  } else {
    return(NA)
  }
})

state_data_count[, region := get_state(location_state)]
## pad with any state that has zero salons so the map fills in grey
state_data_count <- merge(
  data.table(region = get_state(state.abb)),
  state_data_count,
  all.x = TRUE
)
state_data_count[is.na(count), count := 0]
state_data_count[is.na(salons), salons := 0]

## one row per ZIP code: mean rev_labor, mean s_index, mean return rate.
## `coverage = 1` flags ZIPs present in the data (used later for white-vs-colored
## fill on the choropleths).
working_data <- working_data[, .(
  return_cust = mean(pastrepeat_rate),
  rev_labor = mean(rev_labor),
  s_index = mean(s_index),
  salons = uniqueN(location_id),
  count = .N
), by = "location_zip"]
setnames(working_data, "location_zip", "ZCTA5CE10")
working_data[, coverage := 1]

# -----------------------------------------------------------------------------
# ZIP -> county crosswalk (needed because the shapefile covers ALL U.S. ZCTAs,
# not just those present in the transaction data, and the choropleth is
# drawn county-by-county)
# -----------------------------------------------------------------------------
zcta_county <- fread(zcta_county_path)[-1]  # first row is a repeated header
zcta_county[, count := uniqueN(county), by = zcta]
## keep the primary county only when a ZIP spans multiple
zcta_county <- zcta_county[afact > 0.50 | count == 1]
stopifnot(uniqueN(zcta_county$zcta) == nrow(zcta_county))
zcta_county[, ZCTA5CE10 := as.character(zcta)]
## normalize to 5-digit FIPS with leading zeros (LA is "06037") so the filter
## matches CONFIG$counties_padded downstream. GEOCORR files sometimes return
## county as an integer, dropping the leading zero on LA.
zcta_county[, county := sprintf("%05d", as.integer(county))]

# -----------------------------------------------------------------------------
# Merge shapefile with crosswalk and with per-ZIP variables
# -----------------------------------------------------------------------------
my_sf <- read_sf(zcta_shape_path)
my_sf <- merge(my_sf, zcta_county[, c("ZCTA5CE10", "county")],
               by = "ZCTA5CE10", all.x = TRUE)
my_sf <- merge(my_sf, working_data, by = "ZCTA5CE10", all.x = TRUE)

# -----------------------------------------------------------------------------
# County choropleths: one PNG per (county, variable) pair
# -----------------------------------------------------------------------------
## Colors are quantile-ranked greyscale: ZIPs missing the variable get white,
## the darkest ZIP gets black, and the rest are linearly interpolated.
plot_county_map <- function(sf_data, county_fips, variable, out_png) {
  county_sf <- sf_data[sf_data$county == county_fips & sf_data$coverage == 1, ]
  vals <- county_sf[[variable]]
  ramp <- c("white",
            colorRampPalette(c("white", "black"))(uniqueN(vals) - 1))
  cats <- frank(vals, na.last = FALSE, ties.method = "dense")
  png(out_png, width = 700, height = 700)
  plot(st_geometry(county_sf), col = ramp[cats], bg = "white", lwd = 0.25)
  dev.off()
  message("01_02: wrote ", out_png)
}

## map variable name -> suffix used in the output filename
variable_suffix <- list(
  rev_labor = "rev",
  s_index = "sindex",
  return_cust = "return_rate"
)

for (county_fips in CONFIG$counties_padded) {
  for (var in names(variable_suffix)) {
    out_png <- sprintf(
      "results/out/figures/01_02_spatial_cor_%s_%s.png",
      county_fips, variable_suffix[[var]]
    )
    plot_county_map(my_sf, county_fips, var, out_png)
  }
}

# -----------------------------------------------------------------------------
# State-level coverage map (number of salons by state)
# -----------------------------------------------------------------------------
states_map <- map_data("state")
ggplot(state_data_count, aes(map_id = region)) +
  geom_map(aes(fill = salons), map = states_map, color = "black") +
  scale_fill_gradientn(colours = c("white", "black")) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_map() +
  theme(legend.text = element_text(size = 18),
        legend.title = element_text(size = 18))
ggsave("results/out/figures/01_02_coverage.png", width = 12, height = 6, units = "in")
message("01_02: wrote results/out/figures/01_02_coverage.png")
