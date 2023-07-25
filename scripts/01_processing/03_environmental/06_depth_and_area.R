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
  terra,
  exactextractr,
  sf,
  tidyverse
)

# Load data --------------------------------------------------------------------
turfs <- st_read(dsn = here("data",
                            "processed",
                            "turf_polygons.gpkg"))

bathymetry <- rast(here("data", "raw", "GEBCO_03_Feb_2023_58106760a1c2", "gebco_2022_n33.0_s23.0_w-119.0_e-110.0.tif"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
bathymetry[bathymetry > 0] <- NA
bathymetry <- -bathymetry
area <- cellSize(bathymetry)
volume <- bathymetry*area

# X ----------------------------------------------------------------------------
S <- c(bathymetry, area, volume)
names(S) <- c("depth", "area", "volume")

# X ----------------------------------------------------------------------------
extracted <- exact_extract(x = S,
                           y = turfs, 
                           fun = "sum", 
                           append_cols = c("eu_rnpa", "fishery"),
                           progress = T)

# X ----------------------------------------------------------------------------
calcs <- extracted %>% 
  mutate(relative_volume = sum.volume / sum.area) %>% 
  select(eu_rnpa, fishery, depth = sum.depth, area = sum.area, volume = sum.volume, relative_volume)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = calcs,
        file = here("data", "processed", "depth_and_area.rds"))



