################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# Oct 30, 2022
#
# Extracts mean annual SST for lobster TURFS. Annual SST data come from
# https://github.com/jcvdav/remotes/tree/main/scripts/sst
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(raster)
library(sf)
library(exactextractr)
library(tidyverse)

# Load data --------------------------------------------------------------------
turfs <-
  sf::st_read(dsn = file.path(
    mex_data_path,
    "concesiones",
    "processed",
    "lobster_turf_polygons.gpkg"
  ))

anual_sst <- list.files(
  path = here::here("../data_remotes/data/sst/processed_annual"),
  pattern = "tif",
  full.names = T) %>%
  stack()

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
sst_extract <- exact_extract(
  x = anual_sst,
  y = turfs,
  fun = "mean",
  append_cols = c("coop_name", "eu_rnpa")
)

# X ----------------------------------------------------------------------------
sst_ts <- sst_extract %>% 
  pivot_longer(cols = contains("erdMWsst"),
               names_to = "year",
               values_to = "mean_sst") %>%
  mutate(year = as.numeric(str_extract(year, "[:digit:]+"))) %>%
  select(coop_name, eu_rnpa, year, mean_sst)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(
  object = sst_panel,
  file = here("data", "processed", "annual_mean_sst_by_turf.rds")
)
