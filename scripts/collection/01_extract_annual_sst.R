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
library(raster)
library(sf)

fedecoop <- sf::st_read(dsn = file.path(mex_data_path, "fedecoop", "fedecoop_polygons.gpkg")) %>% 
  st_transform(crs = 4326)

sst <- list.files(
  path = here::here("../data_remotes/data/sst/processed_annual"),
  pattern = "tif",
  full.names = T) |>
  stack() |>
  extract(y = fedecoop, df = T, fun = "mean", na.rm = T)



