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

# Load data --------------------------------------------------------------------
mhw <- readRDS(file = here("data", "processed", "mhw_by_turf.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

mhw$summary[[1]]$mhw_events

mhw %>% 
  select(eu_rnpa, summary) %>% 
  unnest(summary) %>% 
  select(-eu_rnpa) %>%
  filter(between(year, 2014, 2017)) %>% 
  # group_by(year) %>% 
  summarize_all(mean)

mhw %>% 
  select(eu_rnpa, summary) %>% 
  unnest(summary) %>% 
  select(-eu_rnpa) %>%
  filter(between(year, 2014, 2017)) %>% 
  # group_by(year) %>% 
  summarize_all(sd)

mhw %>% 
  select(eu_rnpa, summary) %>% 
  unnest(summary) %>% 
  distinct() %>%
  ggplot(aes(x = year, y = mhw_int_cumulative)) + 
  stat_summary(geom = "ribbon", fun.data = mean_sdl, na.rm = T) +
  stat_summary(geom = "line", fun = mean)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------