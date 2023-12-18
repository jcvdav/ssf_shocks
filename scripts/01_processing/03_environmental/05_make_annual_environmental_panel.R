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
pacman::p_load(
  here,
  lubridate,
  tidyverse
)

# Load data --------------------------------------------------------------------
mhw_by_turf <- readRDS(here("data", "processed", "mhw_by_turf.rds"))

std_normal <- function(x, year, cutoff_year = 2013) {
  (x - mean(x[year <= cutoff_year], na.rm = T)) / sd(x[year <= cutoff_year], na.rm = T)
}

cv <- function(x, year, cutoff_year = 2013){
  sd(x[year <= cutoff_year], na.rm = T) / mean(x[year <= cutoff_year], na.rm = T)
}

warming_rate <- function(temp, year) {
  as.numeric(coef(lm(temp ~ year))[2])
}
## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
annual_sst_panel <-
  mhw_by_turf %>%
  select(fishery, eu_rnpa, data) %>%
  unnest(data) %>%
  mutate(year = year(t)) %>%
  group_by(fishery, eu_rnpa, year) %>%
  summarize(
    temp_mean = mean(temp),
    temp_sd = sd(temp),
    temp_max = max(temp),
    temp_min = min(temp)
  ) %>%
  ungroup() %>%
  group_by(eu_rnpa, fishery) %>% 
  mutate(temp_mean_lag = lag(temp_mean),
         temp_long_term = mean(temp_mean),
         temp_cv = cv(temp_mean, year),
         norm_temp_mean = std_normal(temp_mean, year),
         warming_rate_mean = warming_rate(temp_mean, year),
         warming_rate_max = warming_rate(temp_max, year)) %>% 
  ungroup()

# X ----------------------------------------------------------------------------
annual_mhw_panel <-
  mhw_by_turf %>%
  select(fishery, eu_rnpa, summary) %>%
  unnest(summary) %>% 
  complete(year = 1982:2021,
           nesting(fishery, eu_rnpa),
           fill = list(mhw_events = 0,
                       mhw_days = 0,
                       mhw_int_mean = 0,
                       mhw_int_max = 0,
                       mhw_int_cumulative = 0,
                       net_mhw = 0)) %>% 
  group_by(eu_rnpa, fishery) %>% 
  mutate(mhw_int_cumulative_lag = lag(x = mhw_int_cumulative),
         mhw_int_cumulative_lag3 = lag(x = mhw_int_cumulative, n = 3),
         norm_mhw_events = std_normal(mhw_events, year),
         norm_mhw_days = std_normal(mhw_days, year),
         norm_mhw_int_cumulative = std_normal(mhw_int_cumulative, year),
         norm_mhw_int_cumulative_lag = std_normal(mhw_int_cumulative_lag, year),
         norm_mhw_int_cumulative_lag3 = std_normal(mhw_int_cumulative_lag3, year)) %>% 
  ungroup()

# X ----------------------------------------------------------------------------
annual_env_panel <-
  left_join(annual_sst_panel,
            annual_mhw_panel,
            by = c("fishery", "eu_rnpa", "year")) %>%
  left_join(periods, by = "year") %>%
  select(fishery,
         eu_rnpa,
         year,
         contains("period"),
         contains("temp"),
         contains("mhw"),
         contains("rate"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(
  object = annual_env_panel,
  file = here("data", "processed", "annual_environmental_panel.rds")
)
