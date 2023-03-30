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
# Landings data
landings <- readRDS(
  file = file.path(
    "/Users/juancarlosvillasenorderbez/GitHub/",
    "data_mex_fisheries",
    "data",
    "mex_landings",
    "clean",
    "mex_monthly_landings_by_eu.rds"
  )
)

turf_polygons <- st_read(dsn = here("data", "raw", "turf_polygons.gpkg"))

eu_rnpas <- turf_polygons %>% 
  filter(!eu_rnpa == "0203008198") %>% # This EU all of a suddedn caught 100-times more than usual. It's either poaching or an error
  st_centroid() %>% 
  bind_cols(st_coordinates(.)) %>% 
  sf::st_drop_geometry() %>% 
  filter(Y > 25) %>% # Filter polygons below 25 N
  select(fishery, eu_rnpa) %>% 
  distinct()

# Define species I want --------------------------------------------------------
spp <- c(
  "LANGOSTA",
  "PEPINO DE MAR",
  "ERIZO"
)

# Define fishing seasons -------------------------------------------------------
# These com from the official NOMS
# Lobster:
# Cucumber:
# Urchin: 
seasons <- expand_grid(main_species_group = spp,
                       month = 1:12) %>% 
  mutate(open = case_when((main_species_group == "LANGOSTA" & (month >= 9 | month <= (2 + 1))) ~ T,
                          (main_species_group == "PEPINO DE MAR" & between(month, 3, (9 + 1))) ~ T,
                          (main_species_group == "ERIZO" & (month >= 7 | month <= (2 + 1))) ~ T,
                          T ~ F),
         within_year = main_species_group == "PEPINO DE MAR") %>% 
  filter(open) %>% 
  select(main_species_group, month, within_year)

## PROCESSING ##################################################################

# Filter landings data ---------------------------------------------------------
# Data criteria:
# - Keep only landings reported within the period of open season + 1 month after
# - Landings of the target species of interest (see spp)
# - We must be able to attribute landings to a polygon
# - No later than 2021 (2022 data are partial)
# - At least 3 observations before, during, and after
# - Data for 2011, 2012, and 2013 (three immediate years prior)

filtered_landings <- landings %>%
  inner_join(seasons, by = c("main_species_group", "month")) %>% 
  mutate(main_species_group = case_when(
    main_species_group == "LANGOSTA" ~ "lobster",
    main_species_group == "ERIZO" ~ "urchin",
    main_species_group == "PEPINO DE MAR" ~ "sea_cucumber")) %>%
  inner_join(eu_rnpas, by = c("main_species_group" = "fishery", "eu_rnpa")) %>% 
  mutate(year = ifelse(!within_year & month <= 2, year -1, year)) %>% 
  filter(year <= 2021) %>%
  group_by(year, eu_rnpa, main_species_group) %>% 
  summarize(landed_weight = sum(landed_weight),
            value = sum(value)) %>% 
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
  group_by(eu_rnpa, main_species_group) %>% 
  filter(sum(year %in% c(2011, 2012, 2013)) == 3) %>% 
  ungroup() %>% 
  group_by(eu_rnpa, main_species_group) %>% 
  mutate(norm_landed_weight = (landed_weight - mean(landed_weight[year <= 2013])) / sd(landed_weight[year <= 2013]),
         norm_value = (value - mean(value)) / sd(value),
         log_landed_weight = log(landed_weight),
         norm_log_landed_weight = (log_landed_weight - mean(log_landed_weight[year <= 2013])) / sd(log_landed_weight[year <= 2013])) %>%
  ungroup() %>% 
  select(eu_rnpa,
         year,
         fishery = main_species_group,
         landed_weight, value,
         norm_landed_weight, norm_log_landed_weight, norm_value)

# Filter TURF polygons ---------------------------------------------------------
# We keep polygons that meet all the criteria for landings
# Find the EU's that meet all the criteria for landings
eu_rnpas_with_landings <- filtered_landings %>% 
  select(fishery, eu_rnpa) %>% 
  distinct()

filtered_polygons <- turf_polygons %>% 
  drop_na(eu_rnpa) %>% 
  inner_join(eu_rnpas_with_landings, by = c("eu_rnpa", "fishery"))
  
## EXPORT ######################################################################

# Landings
saveRDS(object = filtered_landings,
        file = here("data", "processed", "annual_fishery_panel.rds"))

# Polygons
st_write(obj = filtered_polygons,
         dsn = here("data", "processed", "turf_polygons.gpkg"),
         delete_dsn = T)
