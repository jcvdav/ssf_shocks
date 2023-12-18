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
  modelsummary,
  tidyverse
)
# Load data --------------------------------------------------------------------
mhw <- readRDS(file = here("data", "processed", "mhw_by_turf.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
# Median duration
event_info <- mhw %>%
  mutate(events = map(mhw, ~.x$event)) %>%
  select(eu_rnpa, events) %>%
  unnest(events)

datasummary(duration + intensity_mean + intensity_max + intensity_cumulative ~ mean + sd + min + max + median,
            event_info)
