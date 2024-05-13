################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# We next calculate the conditional probabilities P((MHW≥observed duration) | MHW occurs)
# that a given heatwave equals or exceeds the duration (intensity/cumulative intensity) of 
# the observed one, under the condition that a (arbitrary long/intense) heatwave occurs.
# We determine this probability by binomial sampling of events, that is, dividing the
# number of heatwaves exceeding the observed duration (intensity/cumulative intensity) by
# the number of all heatwaves that have occurred, for both present-day and pre-industrial
# climate.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  cowplot,
  tidyverse
)

# Load data --------------------------------------------------------------------
future_mhw <- readRDS(file = here("data", "processed", "future_mhw_by_turf.rds"))

env_panel <- readRDS(file = here("data", "processed", "annual_environmental_panel.rds"))

centroids <- st_read(here("data", "processed", "centroids.gpkg"))

## PROCESSING ##################################################################

data <- future_mhw %>% 
  mutate(climatology = map(mhw, ~.x$climatology)) %>%
  select(model, ssp, eu_rnpa, fishery, climatology) %>%
  unnest(climatology) %>%
  mutate(year = lubridate::year(t))

# P(MHW Occurs) ----------------------------------------------------------------

p_mhw_occurs_future <- data %>%
  filter(year >= 2022) %>% 
  group_by(model, ssp, eu_rnpa, fishery, year) %>%
  summarize(mhw = 1 * any(event)) %>%
  ungroup() %>%
  group_by(model, ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(mhw)) %>%
  ungroup() %>% 
  group_by(ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(p_at_least_one)) %>%
  ungroup() %>% 
  mutate(type = "forecast")

# Hindcast
p_mhw_occurs_hindcast <- data %>%
  filter(year <= 2021) %>% 
  group_by(model, ssp, eu_rnpa, fishery, year) %>%
  summarize(mhw = any(event)) %>%
  ungroup() %>%
  group_by(model, ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(mhw)) %>%
  ungroup() %>% 
  group_by(ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(p_at_least_one)) %>%
  ungroup() %>% 
  mutate(type = "hindcast")

references <- env_panel %>% 
  filter(period == 1) %>% 
  group_by(fishery, eu_rnpa) %>% 
  filter(mhw_int_cumulative == max(mhw_int_cumulative, na.rm = T)) %>% 
  ungroup() %>% 
  select(fishery, eu_rnpa, year, max = mhw_int_cumulative)

con_p_mhw_threshold_future <- future_mhw %>% 
  select(model, ssp, eu_rnpa, fishery, summary) %>%
  unnest(summary) %>%
  filter(year >= 2022) %>% 
  left_join(references, by = c("eu_rnpa", "fishery")) %>% 
  mutate(exceeds = 1 * (mhw_int_cumulative >= max)) %>% 
  group_by(fishery, eu_rnpa, ssp, model) %>% 
  summarize(p_exceeds = mean(exceeds), .groups = "drop") %>% 
  ungroup() %>% 
  group_by(fishery, eu_rnpa, ssp) %>% 
  summarize(mean = mean(p_exceeds),
            sd = sd(p_exceeds),
            n = n()) %>% 
  ungroup() %>% 
  mutate(se = sd / sqrt(n),
         fishery = str_to_sentence(str_replace(fishery, "_", " ")),
         fishery = fct_relevel(fishery, c("Lobster", "Urchin", "Sea cucumber")))

## EXPORT ######################################################################

saveRDS(object = p_mhw_occurs_future,
        file = here("data", "output", "p_mhw_occurs_future.rds"))

saveRDS(object = p_mhw_occurs_hindcast,
        file = here("data", "output", "p_mhw_occurs_hindcast.rds"))

saveRDS(object = con_p_mhw_threshold_future,
        file = here("data", "output", "con_p_mhw_threshold_future.rds"))

saveRDS(object = references,
        file = here("data", "output", "reference_mhw_by_TURF.rds"))