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
    "mex_monthly_landings_by_eu.rds"
  )
)

eu_rnpas <-
  sf::st_read(dsn = here("data", "processed", "turf_polygons.gpkg")) %>% 
  sf::st_drop_geometry() %>% 
  filter(!eu_rnpa == "0203008198") %>% # This EU all of a suddedn caught 100-times more than usual. It's either poaching or an error
  pull(eu_rnpa) %>% 
  unique()

eu_rnpas <- c(eu_rnpas, "0203002125")

# Define species I want --------------------------------------------------------
spp <- c(
  "LANGOSTA",
  "PEPINO DE MAR",
  "ERIZO"
)


seasons <- expand_grid(main_species_group = spp,
                       month = 1:12) %>% 
  mutate(open = case_when((main_species_group == "LANGOSTA" & (month >= 9 | month <= 2)) ~ T,
                          (main_species_group == "PEPINO DE MAR" & between(month, 3, 9)) ~ T,
                          (main_species_group == "ERIZO" & (month >= 7 | month <= 2)) ~ T,
                          T ~ F),
         within_year = main_species_group == "PEPINO DE MAR") %>% 
  filter(open) %>% 
  select(main_species_group, month, within_year)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
filtered <- landings %>%
  inner_join(seasons, by = c("main_species_group", "month")) %>% 
  filter(eu_rnpa %in% eu_rnpas) %>% 
  mutate(main_species_group = case_when(
    main_species_group == "LANGOSTA" ~ "lobster",
    main_species_group == "ERIZO" ~ "urchin",
    main_species_group == "PEPINO DE MAR" ~ "sea_cucumber")) %>% 
  mutate(year = ifelse(!within_year & month <= 2, year -1, year)) %>% 
  filter(between(year, 2002, 2021)) %>%                                              # Most permits were granted in 2002)
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
  mutate(norm_landed_weight = (landed_weight - mean(landed_weight[year <= 2013])) / sd(landed_weight[year <= 2013]),
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

