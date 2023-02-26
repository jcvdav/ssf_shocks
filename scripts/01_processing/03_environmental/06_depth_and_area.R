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
library(here)
library(raster)
library(exactextractr)
library(sf)
library(tidyverse)

# Load data --------------------------------------------------------------------
turfs <- st_read(dsn = here("data",
                            "processed",
                            "turf_polygons.gpkg"))

bathymetry <- raster(here("data", "raw", "GEBCO_03_Feb_2023_58106760a1c2", "gebco_2022_n33.0_s23.0_w-119.0_e-110.0.tif"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
bathymetry[bathymetry > 0] <- NA
bathymetry <- -bathymetry
area <- raster::area(bathymetry)

# X ----------------------------------------------------------------------------
S <- stack(bathymetry, area)
names(S) <- c("depth", "area")

# X ----------------------------------------------------------------------------
extracted <- exact_extract(x = S,
                           y = turfs, 
                           fun = "sum", 
                           append_cols = c("eu_rnpa", "fishery"),
                           progress = T)

# X ----------------------------------------------------------------------------
calcs <- extracted %>% 
  mutate(relative_volume = sum.depth / sum.area) %>% 
  select(eu_rnpa, fishery, depth = sum.depth, area = sum.area, relative_volume)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = calcs,
        file = here("data", "processed", "depth_and_area.rds"))



