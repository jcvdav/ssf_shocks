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
mhw <- readRDS(file = here("data", "processed", "annual_environmental_panel.rds"))

# How does the regime compare to historical MHWs?
comp <- mhw %>% 
  filter(year <= 2016,
         mhw_events > 0)

comp %>% 
  select(-c(year, eu_rnpa, fishery, period, contains("temp_"))) %>% 
  group_by(period_long) %>% 
  summarize_all(function(x){paste0(round(mean(x, na.rm = T), 2), "(", round(sd(x, na.rm = T), 2), ")")}) %>% 
  select(period_long, mhw_events, mhw_days, mhw_int_mean, mhw_int_cumulative)

t.test(mhw_int_cumulative ~ period_long, data = comp, alternative = "less")
t.test(log(mhw_int_cumulative) ~ period_long, data = comp, alternative = "less")
