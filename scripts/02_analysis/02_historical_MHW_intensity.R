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
# For individual MHW event stats
raw_mhw <- readRDS(file = here("data/processed/mhw_by_turf.rds")) %>%
  mutate(event = map(mhw, function(x){x$event})) %>%
  select(eu_rnpa, fishery, event) %>%
  unnest(event)

# For net exposure info
mhw <- readRDS(file = here("data", "processed", "annual_environmental_panel.rds"))

## PROCESSING ##################################################################
# Part 1 What are the typical duration and intensities of individual events? ---
raw_mhw %>% 
  select(duration, intensity_mean) %>%
  summarize_all(function(x){paste0(round(mean(x, na.rm = T), 2), "(", round(sd(x, na.rm = T), 2), ")")})

# Part 2 How does the MHW regime compare to pre-regime exposure? ---------------
# Subset data
comp <- mhw %>% 
  filter(year <= 2016,
         mhw_events > 0)
# Build table
comp %>% 
  select(-c(year, eu_rnpa, fishery, period, contains("temp_"))) %>% 
  group_by(period_long) %>% 
  summarize_all(function(x){paste0(round(mean(x, na.rm = T), 2), "(", round(sd(x, na.rm = T), 2), ")")}) %>% 
  select(period_long, mhw_events, mhw_days, mhw_int_mean, mhw_int_cumulative)

# T.test
t.test(log(mhw_int_cumulative) ~ period_long, data = comp, alternative = "less")
