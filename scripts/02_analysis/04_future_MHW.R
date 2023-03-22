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
library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------

future_mhw <- readRDS(file = here("data", "processed", "future_mhw_by_turf.rds"))

env_panel <- readRDS(file = here("data", "processed", "annual_environmental_panel.rds"))

## PROCESSING ##################################################################

# P(MHW Occurs) ----------------------------------------------------------------

p_mhw_occurs_future <- future_mhw %>% 
  mutate(climatology = map(mhw, ~.x$climatology)) %>%
  select(model, ssp, eu_rnpa, fishery, climatology) %>%
  unnest(climatology) %>%
  mutate(year = lubridate::year(t)) %>%
  filter(year >= 2022) %>% 
  group_by(model, ssp, eu_rnpa, fishery, year) %>%
  summarize(mhw = any(event)) %>%
  ungroup() %>%
  group_by(model, ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(mhw)) %>%
  ungroup() %>% 
  group_by(ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(p_at_least_one)) %>%
  ungroup()

# P (MHW >= Threshold | MHW Occurs) --------------------------------------------
# Define thresholds
references <- env_panel %>% 
  filter(period == 1) %>% 
  group_by(fishery, eu_rnpa) %>% 
  summarize(mean = mean(mhw_int_cumulative),
            max = max(mhw_int_cumulative))

# Calculate, for each ssp, the probability that mhw_int_cum in the future exceeds the mean of what we saw

# Conditional on there being a heatwave, what is the probability that it will be
# as big as X?

con_p_mhw_threshold_future <- future_mhw %>% 
  select(model, ssp, eu_rnpa, fishery, summary) %>%
  unnest(summary) %>%
  filter(year >= 2022) %>% 
  left_join(references, by = c("eu_rnpa", "fishery")) %>% 
  mutate(exceeds = 1 * (mhw_int_cumulative >= max))


## VISUALIZE ###################################################################

fishery_summary <- con_p_mhw_threshold_future %>% 
  group_by(fishery, eu_rnpa, ssp, model) %>% 
  summarize(p_exceeds = mean(exceeds), .groups = "drop") %>% 
  ungroup() %>% 
  group_by(fishery, eu_rnpa, ssp) %>% 
  summarize(mean = mean(p_exceeds))

my_jitter <- position_jitterdodge(jitter.width = 0.1,
                                  jitter.height = 0,
                                  dodge.width = 0.75,
                                  seed = 1)

ggplot(fishery_summary,
       aes(x = fishery, y = mean, color = ssp, fill = ssp)) +
  geom_hline(yintercept = 1/40) +
  geom_point(aes(group = ssp),
             size = 1,
             alpha = 0.5,
             position = my_jitter) + 
  stat_summary(geom = "linerange",
               fun.data = mean_cl_normal,
               color = "black",
               linewidth = 0.5,
               position = my_jitter) +
  stat_summary(geom = "pointrange",
               fun.data = mean_se,
               fatten = 4,
               linewidth = 2,
               position = my_jitter) +
  scale_color_manual(values = c("royalblue3", "orange2", "darkred")) +
  labs(x = "Fishery",
       y = expression("P((Cum. Int. ">=bar(MHW~Cum.~Int.)[i]~") | MHW Occurs)"))
















## VISUALIZE ###################################################################
ggplot(p_mhw_occurs_future,
       mapping = aes(x = fishery,
                     y = p_at_least_one,
                     color = ssp)) +
  geom_jitter(size = 0.5) +
  stat_summary(geom = "pointrange",
               fun.data = mean_se,
               position = position_jitter(width = 0.2, height = 0, seed = 2))

ggplot(future_events_by_eu_and_fishery,
       mapping = aes(x = p_at_least_one,
                     y = Y, 
                     color = ssp)) +
  geom_point() +
  facet_wrap(~fishery)







future_con_int_as_much <- future_mhw %>% 
  select(model, ssp, eu_rnpa, fishery, summary) %>% 
  expand_grid(threshold = seq(1, max_annual_int, by = 5)) %>% 
  mutate(cond_p = future_map2(.x = threshold, .y = summary, .f = ~get_cond_p_year(.x, .y %>% filter(year >= 2022)))) %>% 
  select(ssp, model, fishery, eu_rnpa, cond_p) %>%
  unnest(cond_p)




ggplot(data = future_con_int_as_much,
       mapping = aes(x = threshold, y = cond_p, color = ssp)) +
  stat_summary(geom = "line",
               fun = "mean") +
  facet_wrap(~fishery)


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


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------






a <- read_csv(here::here("../recovery_time", "data", "Projected_MHW_Cumulative_Intensity.csv"))
b <- con_p_mhw_threshold_future %>%
  group_by(model, ssp, year) %>% 
  summarize(mhw_int_cumulative = mean(mhw_int_cumulative)) %>% 
  select(year, ssp, model, mhw_int_cumulative) %>%
  spread(ssp, mhw_int_cumulative) %>%
  arrange(model)


ggplot(data = a, aes(x = Year, y = ssp126)) +
  geom_line()
ggplot(data = b, aes(x = year, y = ssp126)) +
  geom_line()
