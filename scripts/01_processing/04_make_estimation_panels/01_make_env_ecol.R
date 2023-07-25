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
  tidyverse
)

# Load data --------------------------------------------------------------------
env_panel <- readRDS(here("data", "processed", "annual_environmental_panel.rds")) %>% 
  select(-fishery) %>% 
  filter(eu_rnpa == "0301000089")
eco_panel <- readRDS(here("data", "processed", "annual_ecological_panel.rds"))

## PROCESSING ##################################################################

# Combine ----------------------------------------------------------------------
annual_env_eco_panel <- eco_panel %>% 
  inner_join(env_panel, by = c("year", "period", "period_long", "eu_rnpa"))


## EXPORT ######################################################################

# Export the panel  ------------------------------------------------------------
saveRDS(object = annual_env_eco_panel,
        file = here("data", "estimation_panels", "env_eco_panel.rds"))

