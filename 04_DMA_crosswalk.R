# 04_DMA_crosswalk
# Builds a DMA-level shapefile from Census county boundaries + the Kaggle
# DMA/FIPS crosswalk, for later comparison against Google Trends and
# wildfire smoke data.
#

# NEED TO DEBUG: year 2020 vs 2022 for CT


# load packages
library(tidyverse)
library(sf)
library(tigris)
library(janitor)


# 1. settings
options(tigris_use_cache = TRUE) # use census cartographic boundry files for lower resolution

# year of county boundaries
COUNTY_YEAR <- 2020

# Read in DMA crosswalk
DMA_CROSSWALK <- "data/raw/DMA_FIPS_County_Mapping.csv"

# Output directory
OUTPUT_DIR <- "data/processed"

# 2. create output directory
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# 3. read in KAGGLE DMA crosswalk
dma_crosswalk <- read_csv(
  DMA_CROSSWALK, 
  show_col_types = FALSE, 
  na = c("", "NA", "#N/A")) %>%
  clean_names() %>%
  # create 5 digit standardized FIPS
  mutate(
    fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0")
  )

stopifnot(
  "Duplicate FIPS codes in crosswalk" = nrow(count(dma_crosswalk, fips) %>% filter(n > 1)) == 0
)

# 4. Patch known pre-2022 FIPS changes not reflected in the crosswalk.
# These are real renames/splits, not vintage differences, so they're
# needed regardless of COUNTY_YEAR.
fips_patches <- tribble(
  ~old_fips, ~new_fips,
  "02261",   "02063",  # Valdez-Cordova (AK) -> Chugach
  "02261",   "02066",  # Valdez-Cordova (AK) -> Copper River
  "02270",   "02158",  # Wade Hampton (AK) -> Kusilvak
  "46113",   "46102"   # Shannon (SD) -> Oglala Lakota
)


patched_rows <- fips_patches %>%
  left_join(dma_crosswalk, by = c("old_fips" = "fips")) %>%
  select(-old_fips) %>%
  rename(fips = new_fips)

dma_crosswalk <- dma_crosswalk %>%
  filter(!fips %in% fips_patches$old_fips) %>%
  bind_rows(patched_rows)


# Bedford City (51515) is different: it wasn't renamed, it was MERGED
# into Bedford County (51019) in 2013, which already exists as its own
# row in the crosswalk with its own DMA assignment. Remapping
# 51515 -> 51019 (like the renames above) would create a duplicate FIPS
# rather than fill a gap, since 51019 is already present. Dropped instead.
dma_crosswalk <- dma_crosswalk %>%
  filter(fips != "51515")

stopifnot(
  "Patch step introduced duplicate FIPS codes" =
    nrow(count(dma_crosswalk, fips) %>% filter(n > 1)) == 0
)

# 5. Census county geometry

# exclude non-contiguous areas
nonContig_stateFIPS <- c("02","60","66","15","72","78","69")

# low resolution is fine
counties <- tigris::counties(year = COUNTY_YEAR, cb = TRUE) %>%
filter(!(STATEFP %in% nonContig_stateFIPS)) %>% 
  mutate(fips = GEOID)


# 7. join dma crosswalk to county geometry
counties_dma <- counties %>% left_join(dma_crosswalk, by = "fips")

# mismatch
unmatched_counties <- counties_dma %>%
  filter(is.na(dmaindex)) %>%
  st_drop_geometry() %>%
  select(fips, STATEFP, COUNTYFP, NAME)

message("Census counties without a DMA match: ", nrow(unmatched_counties))
if (nrow(unmatched_counties) > 0) print(unmatched_counties)

crosswalk_not_in_census <- dma_crosswalk %>% filter(!fips %in% counties$fips)
message("Crosswalk rows with no matching county geometry: ", nrow(crosswalk_not_in_census))
if (nrow(crosswalk_not_in_census) > 0) print(crosswalk_not_in_census)

# 8. dissolve counties into DMA polygons
# group by dmaindex only
dma_sf <- counties_dma %>%
  filter(!is.na(dmaindex)) %>%
  group_by(dmaindex) %>%
  summarise(
    dma = first(na.omit(dma)),
    google_dma = first(na.omit(google_dma)),
    n_counties = n(),
    .groups = "drop"
  ) %>%
  st_make_valid()

message("\nDMA polygons created: ", nrow(dma_sf))
stopifnot("Unexpected number of DMAs -- check for label-based grouping regressions" = nrow(dma_sf) < 215)
stopifnot("Invalid geometries remain after st_make_valid()" = sum(!st_is_valid(dma_sf)) == 0)

county_dma_check <- counties_dma %>%
  st_drop_geometry() %>%
  filter(!is.na(dmaindex)) %>%
  count(fips) %>%
  filter(n > 1)
stopifnot("A county is assigned to more than one DMA" = nrow(county_dma_check) == 0)


# 9. QA plot
ggplot(dma_sf) +
  geom_sf(aes(fill = as.factor(dmaindex)), linewidth = 0) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = paste0(nrow(dma_sf), " DMA polygons"))


# 10. Save output
st_write(dma_sf, file.path(OUTPUT_DIR, "dma_boundaries.shp"), append = FALSE, quiet = TRUE)
saveRDS(dma_sf, file.path(OUTPUT_DIR, "dma_boundaries.rds"))

message("\nSaved: ", file.path(OUTPUT_DIR, "dma_boundaries.shp"))
message("Saved: ", file.path(OUTPUT_DIR, "dma_boundaries.rds"))


# FOUND AN ISSUE: 205 DMAS is GOOD! we are excluding 4- from alaska and hawaii.
# The issue is that there are 209 dmas in kaggle- not 210.
