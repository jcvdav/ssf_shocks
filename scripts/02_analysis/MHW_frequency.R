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
#
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
library(furrr)
library(tidyverse)

# Load data --------------------------------------------------------------------
mhw <- readRDS(file = here("data", "processed", "mhw_by_turf.rds"))

future_mhw <- readRDS(file = here("data", "processed", "future_mhw_by_turf.rds"))

# X ----------------------------------------------------------------------------
events_by_eu <- mhw %>% 
  mutate(climatology = map(mhw, ~.x$climatology)) %>% 
  select(eu_rnpa, climatology) %>% 
  unnest(climatology) %>% 
  mutate(year = lubridate::year(t)) %>% 
  group_by(eu_rnpa, year) %>% 
  summarize(at_least_one_mhw = any(event)) %>% 
  ungroup() %>% 
  group_by(eu_rnpa) %>% 
  summarize(p = mean(at_least_one_mhw)) %>% 
  ungroup()

events_by_fishery <- mhw %>% 
  mutate(climatology = map(mhw, ~.x$climatology)) %>% 
  select(fishery, climatology) %>% 
  unnest(climatology) %>% 
  mutate(year = lubridate::year(t)) %>% 
  group_by(fishery, year) %>% 
  summarize(at_least_one_mhw = any(event)) %>% 
  ungroup() %>% 
  group_by(fishery) %>% 
  summarize(p = mean(at_least_one_mhw)) %>% 
  ungroup()

events_by_eu_and_fishery <- mhw %>% 
  mutate(climatology = map(mhw, ~.x$climatology)) %>% 
  select(eu_rnpa, fishery, climatology) %>% 
  unnest(climatology) %>% 
  mutate(year = lubridate::year(t)) %>% 
  group_by(eu_rnpa, fishery, year) %>% 
  summarize(at_least_one_mhw = any(event)) %>% 
  ungroup() %>% 
  group_by(eu_rnpa, fishery) %>% 
  summarize(p = mean(at_least_one_mhw)) %>% 
  ungroup()

mhw %>% 
  mutate(climatology = map(mhw, ~.x$climatology)) %>% 
  select(eu_rnpa, climatology) %>% 
  unnest(climatology) %>% 
  mutate(year = lubridate::year(t)) %>% 
  group_by(eu_rnpa, year) %>% 
  summarize(at_least_one_mhw = any(event)) %>% 
  ungroup() %>% 
  summarize(p = mean(at_least_one_mhw)) %>% 
  ungroup()

# Conditional on there being a heatwave, what is the probability that it will be
# as big as X?

get_cond_p_event <- function(threshold, data){
  n_at_or_above_threshold <- sum(data$intensity_cumulative >= threshold)
  cond_p <- n_at_or_above_threshold / length(data$intensity_cumulative)
  
  back <- tibble(threshold,
                 cond_p)
  
  return(back)
}

get_cond_p_year <- function(threshold, data){
  n_at_or_above_threshold <- sum(data$mhw_int_cumulative >= threshold)
  cond_p <- n_at_or_above_threshold / length(data$mhw_int_cumulative)
  
  back <- tibble(threshold,
                 cond_p)
  
  return(back)
}

max_single_mhw <- map_dbl(mhw$mhw, ~max(.x$event$intensity_cumulative)) %>%
  max()
max_single_mhw <- round(max_single_mhw / 10) * 10

max_annual_int <- map_dbl(mhw$summary, ~max(.x$mhw_int_cumulative)) %>%
  max()
max_annual_int <- round(max_annual_int / 10) * 10

plan(multisession, workers = 12)
con_mhw_as_big <- mhw %>% 
  mutate(mhw = map(mhw, ~.x$event)) %>% 
  select(eu_rnpa, fishery, mhw) %>% 
  expand_grid(threshold = seq(1, max_single_mhw, by = 5)) %>% 
  mutate(cond_p = future_map2(.x = threshold, .y = mhw, .f = ~get_cond_p_event(.x, .y))) %>% 
  select(fishery, eu_rnpa, cond_p) %>%
  unnest(cond_p)

con_int_as_much <- mhw %>% 
  select(eu_rnpa, fishery, summary) %>% 
  expand_grid(threshold = seq(1, max_annual_int, by = 5)) %>% 
  mutate(cond_p = future_map2(.x = threshold, .y = summary, .f = ~get_cond_p_year(.x, .y))) %>% 
  select(fishery, eu_rnpa, cond_p) %>%
  unnest(cond_p)

future_con_int_as_much <- future_mhw %>% 
  select(model, ssp, eu_rnpa, fishery, summary) %>% 
  expand_grid(threshold = seq(1, max_annual_int, by = 5)) %>% 
  mutate(cond_p = future_map2(.x = threshold, .y = summary, .f = ~get_cond_p_year(.x, .y %>% filter(year >= 2022)))) %>% 
  select(ssp, model, fishery, eu_rnpa, cond_p) %>%
  unnest(cond_p)
plan(sequential)


p1 <- ggplot(data = con_mhw_as_big, 
       mapping = aes(x = threshold, y = cond_p)) +
  geom_line(aes(group = eu_rnpa), alpha = 0.5, linewidth = 0.2, color = "cadetblue") +
  stat_summary(geom = "ribbon",
               fun.data = mean_se) +
  stat_summary(geom = "line",
               fun = "mean",
               color = "#E41A1C",
               linewidth = 0.5) +
  facet_wrap(~fishery, ncol = 1) +
  labs(x = "Cumulative MHW Intensity")

p2 <- ggplot(data = con_int_as_much, 
       mapping = aes(x = threshold, y = cond_p)) +
  geom_line(aes(group = eu_rnpa), alpha = 0.5, linewidth = 0.2, color = "cadetblue") +
  stat_summary(geom = "ribbon",
               fun.data = mean_se) +
  stat_summary(geom = "line",
               fun = "mean",
               color = "#E41A1C",
               linewidth = 0.5) +
  facet_wrap(~fishery, ncol = 1) +
  labs(x = "Cumulative MHW Intensity")

ggplot(data = future_con_int_as_much,
       mapping = aes(x = threshold, y = cond_p, color = ssp)) +
  stat_summary(geom = "line",
               fun = "mean") +
  facet_wrap(~fishery)

cowplot::plot_grid(p1, p2)

mhw_dat <- future_mhw %>% 
  select(model, ssp, eu_rnpa, fishery, summary) %>% 
  unnest(summary) %>% 
  group_by(eu_rnpa, fishery) %>% 
  mutate(threshold = max(mhw_int_cumulative[year <= 2021])) %>% 
  ungroup() %>% 
  mutate(exceeds = mhw_int_cumulative >= threshold) %>% 
  filter(year >= 2022)

int_models <- fixest::feglm(exceeds ~ year | eu_rnpa + fishery,
                            panel.id = ~eu_rnpa + year,
                            data = dat,
                            family = binomial,
                            split = ~ssp, vcov = "DK")

modelsummary::modelsummary(int_models, stars = T)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

0.975 * 0.03333
