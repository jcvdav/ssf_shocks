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
library(sf)
library(tidyverse)

# Load data --------------------------------------------------------------------
# Landings data for reference
landings <- readRDS(file = here("data", "processed", "annual_fishery_panel.rds"))

# Polygons
turfs <-
  sf::st_read(dsn = file.path(
    mex_data_path,
    "concesiones",
    "processed",
    "lobster_permit_and_concessions_polygons.gpkg"))

## PROCESSING ##################################################################
# Filter out -------------------------------------------------------------------
project_turfs <- turfs %>% 
  filter(eu_rnpa %in% unique(landings$eu_rnpa))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
st_write(obj = project_turfs,
         dsn = here("data", "processed", "project_turfs.gpkg"),
         delete_dsn = TRUE)
