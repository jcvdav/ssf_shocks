################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# RE RUN THIS ONCE YOU HAVE CLEANED THE COOPERATIVE NAMES
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  tidyverse
)

# Load data --------------------------------------------------------------------
env_panel <- readRDS(here("data", "processed", "annual_environmental_panel.rds"))
fish_panel <- readRDS(here("data", "processed", "annual_fishery_panel.rds"))
cpi <- readRDS(here("data", "processed", "cpi_t_rates.rds"))
centroids <- st_read(here("data", "processed", "centroids.gpkg")) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(lon = X, lat = Y)


## PROCESSING ##################################################################

# Combine ----------------------------------------------------------------------
annual_env_fish_panel <- fish_panel %>% 
  left_join(cpi, by = "year") %>% 
  left_join(env_panel, by = c("year", "fishery", "eu_rnpa")) %>% 
  left_join(centroids, by = c("fishery", "eu_rnpa")) %>%
  mutate(value = value * rate) %>% 
  select(eu_rnpa, eu_name, year, fishery, everything()) %>% 
  select(-rate)


## EXPORT ######################################################################

# Export the panel  ------------------------------------------------------------
saveRDS(object = annual_env_fish_panel,
        file = here("data", "estimation_panels", "env_fish_panel.rds"))

