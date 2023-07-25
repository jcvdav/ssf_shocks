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
mhw <- readRDS(file = here("data", "processed", "mhw_by_turf.rds"))

# Mean stats for the regime
mhw %>% 
  select(eu_rnpa, summary) %>% 
  unnest(summary) %>% 
  select(-eu_rnpa) %>%
  filter(between(year, 2014, 2016)) %>% 
  # group_by(year) %>% 
  summarize_all(function(x){paste0(round(mean(x), 3), "; ", round(sd(x), 3))})

# How does the regime compare to historical MHWs?
comp <- mhw %>% 
  select(eu_rnpa, fishery, summary) %>% 
  unnest(summary) %>% 
  filter(year <= 2016) %>% 
  left_join(periods, by = "year")

comp %>% 
  select(-c(year, eu_rnpa, fishery, period)) %>% 
  group_by(period_long) %>% 
  summarize_all(function(x){paste0(round(mean(x, na.rm = T), 3), "; ", round(sd(x, na.rm = T), 3))})

t.test(mhw_int_cumulative ~ period_long, data = comp, alternative = "less")
