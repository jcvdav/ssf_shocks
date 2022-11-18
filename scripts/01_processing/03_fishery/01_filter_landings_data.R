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

# Define species I want --------------------------------------------------------
spp <- c(
  "LANGOSTA"#,
  #,"ABULON",
  # "PEPINO DE MAR",
  # "ERIZO"
)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
filtered <- landings %>%
  filter(main_species_group %in% spp,
         between(year, 2003, 2021) # Most permits were granted in 2002
         # year < 2022
         ) %>% 
  inner_join(coop_eurnpa, by = "eu_rnpa") %>%
  left_join(periods, by = "year") %>% 
  group_by(coop_name, main_species_group) %>% 
  mutate(n = n_distinct(period)) %>% 
  ungroup() %>% 
  filter(n == 3) %>% 
  group_by(coop_name, main_species_group, period) %>% 
  mutate(n2 = n()) %>% 
  ungroup() %>% 
  filter(n2 > 3) %>%
  select(-c(n, n2)) %>% 
  group_by(coop_name, main_species_group) %>% 
  mutate(norm_landed_weight = (landed_weight - mean(landed_weight)) / sd(landed_weight),
         norm_value = (value - mean(value)) / sd(value)) %>%
  ungroup() %>% 
  mutate(balanced = year >= 2008) %>% 
  select(coop_name,
         eu_rnpa,
         year,
         balanced,
         main_species_group,
         landed_weight, value,
         norm_landed_weight, norm_value)
  
  

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = filtered,
        file = here("data", "processed", "annual_fishery_panel.rds"))

