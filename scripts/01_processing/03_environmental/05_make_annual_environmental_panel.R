################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Loads the MHW data and creates a panel environmental measures of interest
# at the coop-year level
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(lubridate)
library(tidyverse)

# Load data --------------------------------------------------------------------
mhw_by_turf <- readRDS(here("data", "processed", "mhw_by_turf.rds"))
## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
annual_sst_panel <-
  mhw_by_turf %>%
  select(eu_name, eu_rnpa, data) %>%
  unnest(data) %>%
  mutate(year = year(t)) %>%
  group_by(eu_name, eu_rnpa, year) %>%
  summarize(
    temp_mean = mean(temp),
    temp_sd = sd(temp),
    temp_max = max(temp),
    temp_min = min(temp)
  ) %>%
  ungroup() %>%
  group_by(eu_name) %>% 
  mutate(temp_mean_lag = lag(temp_mean),
         temp_long_term = mean(temp_mean)) %>% 
  ungroup()

# X ----------------------------------------------------------------------------
annual_mhw_panel <-
  mhw_by_turf %>%
  select(eu_name, eu_rnpa, summary) %>%
  unnest(summary) %>% 
  group_by(eu_name) %>% 
  mutate(mhw_events_lag = lag(mhw_events),
         mhw_days_lag = lag(mhw_days),
         mhw_int_cumulative_lag = lag(mhw_int_cumulative)) %>% 
  ungroup()

# X ----------------------------------------------------------------------------
annual_env_panel <-
  left_join(annual_sst_panel,
            annual_mhw_panel,
            by = c("eu_name", "eu_rnpa", "year")) %>%
  replace_na(replace = list(
    mhw_events = 0,
    mhw_days = 0,
    mhw_int_cumulative = 0
  )) %>%
  left_join(periods, by = "year") %>%
  select(eu_name,
         eu_rnpa,
         year,
         contains("period"),
         contains("temp"),
         contains("mhw"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(
  object = annual_env_panel,
  file = here("data", "processed", "annual_environmental_panel.rds")
)
