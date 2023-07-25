################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  # raster,
  terra,
  magrittr,
  furrr,
  tidyverse
)

# Load data --------------------------------------------------------------------
OISST_data <-
  readRDS(file = here("data", "raw", "daily_sst", "ncdcOisst21Agg_LonPM180.rds"))


## PROCESSING ##################################################################

# Rasterize --------------------------------------------------------------------
plan("multisession", workers = 12)
rasters <- OISST_data %>%
  group_by(t) %>%
  nest() %>%
  head() %>% 
  mutate(r = future_map(data, rast, type = "xyz", crs = "EPSG:4326"))

## EXPORT ######################################################################

# Define wraper function -------------------------------------------------------
my_write <- function(x, filename) {
  filename <-
    here(
      "data",
      "raw",
      "daily_sst",
      "rasters",
      paste0("ncdcOisst21Agg_LonPM180_", filename, ".tif")
    )
  
  writeRaster(x = x,
              filename = filename,
              overwrite = T)
}

# Export files -----------------------------------------------------------------
rasters %>%
  select(t, r) %$%
  future_walk2(.x = r, .y = t, .f = my_write)

plan(sequential)