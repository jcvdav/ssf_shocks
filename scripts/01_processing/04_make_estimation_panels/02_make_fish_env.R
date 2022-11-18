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
env_panel <- readRDS(here("data", "processed", "annual_environmental_panel.rds"))
fish_panel <- readRDS(here("data", "processed", "annual_fishery_panel.rds"))
cpi <- readRDS(here("data", "processed", "cpi_t_rates.rds"))


## PROCESSING ##################################################################

# Combine ----------------------------------------------------------------------
annual_env_fish_panel <- fish_panel %>% 
  left_join(cpi, by = "year") %>% 
  left_join(env_panel, by = c("year", "coop_name", "eu_rnpa")) %>% 
  mutate(value = value * rate) %>% 
  select(-rate)


## EXPORT ######################################################################

# Export the panel  ------------------------------------------------------------
saveRDS(object = annual_env_fish_panel,
        file = here("data", "estimation_panels", "env_fish_panel.rds"))
