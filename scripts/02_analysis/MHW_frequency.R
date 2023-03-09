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
# We first calculate the probabilities
# P(MHW occurs pre-industrial) and P(MHW occurs present-day), with which at least
# one heatwave occurs in a year by dividing the number of years in
# which at least one heatwave occurred by the number of all years.
# We next calculate the conditional probabilities P((MHW≥observed duration) | MHW occurs)
# that a given heatwave equals or exceeds the duration (intensity/cumulative intensity) of 
# the observed one, under the condition that a (arbitrary long/intense) heatwave occurs.
# We determine this probability by binomial sampling of events, that is, dividing the
# number of heatwaves exceeding the observed duration (intensity/cumulative intensity) by
# the number of all heatwaves that have occurred, for both present-day and pre-industrial
# climate.
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------
mhw <- readRDS(file = here("data", "processed", "mhw_by_turf.rds"))

baseline <- readRDS(here("data", "estimation_panels", "env_fish_panel.rds")) %>%
  select(eu_rnpa, fishery) %>%
  distinct()


## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
mhw %>% 
  select(eu_rnpa, summary) %>% 
  unnest(summary) %>% 
  select(-eu_rnpa) %>%
  filter(between(year, 2014, 2017)) %>% 
  group_by(year) %>%
  summarize_all(function(x){paste(round(mean(x), 3), round(sd(x), 3))})

mhw %>% 
  select(eu_rnpa, summary) %>% 
  unnest(summary) %>% 
  select(-eu_rnpa) %>%
  filter(between(year, 2014, 2017)) %>% 
  # group_by(year) %>% 
  summarize_all(function(x){tibble(mean = mean(x), sd = sd(x))})

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

get_cond_p <- function(threshold, data){
  # browser()
  n_at_or_above_threshold <- sum(data$intensity_cumulative >= threshold)
  cond_p <- n_at_or_above_threshold / length(data$intensity_cumulative)
  
  back <- tibble(threshold,
                 cond_p)
  
  return(back)
}

mhw_by_turf <- mhw %>% 
  inner_join(baseline, by = c("eu_rnpa", "fishery")) %>% 
  mutate(mhw = map(mhw, ~.x$event)) %>% 
  select(eu_rnpa, fishery, mhw) %>% 
  expand_grid(threshold = seq(1, 900, by = 5)) %>% 
  mutate(cond_p = map2(.x = threshold, .y = mhw, .f = ~get_cond_p(.x, .y)))

mhw_by_turf %>% 
  select(fishery, eu_rnpa, cond_p) %>%
  unnest(cond_p) %>%
  ggplot(aes(x = threshold, y = cond_p, group = eu_rnpa)) +
  geom_line() +
  facet_wrap(~fishery, ncol = 2)


events_by_fishery <- mhw %>% 
  inner_join(baseline, by = c("eu_rnpa", "fishery")) %>% 
  mutate(climatology = map(mhw, ~.x$climatology)) %>% 
  select(eu_rnpa, fishery, climatology) %>% 
  unnest(climatology) %>% 
  mutate(year = lubridate::year(t)) %>% 
  group_by(eu_rnpa, fishery, year) %>% 
  summarize(at_least_one_mhw = any(event)) %>% 
  ungroup() %>% 
  group_by(eu_rnpa, fishery) %>% 
  summarize(p = sum(at_least_one_mhw) / n_distinct(year)) %>% 
  ungroup()

events_by_fishery %>% 
  group_by(fishery) %>%
  summarize(p_m = mean(p),
            p_sd = sd(p))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------