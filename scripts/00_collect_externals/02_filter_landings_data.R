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
library(tidyverse)

# Load data --------------------------------------------------------------------
# Landings data
landings <- readRDS(
  file = file.path(
    "/Users/juancarlosvillasenorderbez/GitHub/",
    "data_mex_fisheries",
    "data",
    "mex_landings",
    "clean",
    "mex_annual_landings_by_eu.rds"
  )
)

eu_rnpas <-
  sf::st_read(dsn = here("data", "processed", "turf_polygons.gpkg")) %>% 
  sf::st_drop_geometry() %>% 
  pull(eu_rnpa) %>% 
  unique()

eu_rnpas <- c(eu_rnpas, "0203002125")

# Define species I want --------------------------------------------------------
spp <- c(
  "LANGOSTA",
  # "PEPINO DE MAR",
  "ERIZO"
)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
filtered <- landings %>%
  filter(main_species_group %in% spp,
         between(year, 2003, 2021),                                             # Most permits were granted in 2002
         eu_rnpa %in% eu_rnpas) %>% 
  mutate(main_species_group = case_when(
    main_species_group == "LANGOSTA" ~ "lobster",
    main_species_group == "ERIZO" ~ "urchin")) %>% 
  left_join(periods, by = "year") %>% 
  mutate(ifelse(eu_rnpa == "0203002125", "0203126552", eu_rnpa)) %>% 
  group_by(eu_rnpa, main_species_group) %>% 
  mutate(n = n_distinct(period)) %>% 
  ungroup() %>% 
  filter(n == 3) %>% 
  group_by(eu_rnpa, main_species_group, period) %>% 
  mutate(n2 = n()) %>% 
  ungroup() %>% 
  group_by(eu_rnpa, main_species_group) %>% 
  filter(all(n2 >= 3)) %>%
  select(-c(n, n2)) %>% 
  mutate(landed_weight = landed_weight / 1e3,
         value = value / 1e6) %>%
  group_by(eu_rnpa, main_species_group) %>% 
  mutate(norm_landed_weight = (landed_weight - mean(landed_weight)) / sd(landed_weight),
         norm_value = (value - mean(value)) / sd(value)) %>%
  ungroup() %>% 
  mutate(balanced = year >= 2008) %>% 
  select(eu_rnpa,
         year,
         balanced,
         fishery = main_species_group,
         landed_weight, value,
         norm_landed_weight, norm_value)
  
## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = filtered,
        file = here("data", "processed", "annual_fishery_panel.rds"))

