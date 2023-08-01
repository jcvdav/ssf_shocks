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
# We follow Laufkötter et al., 2020 Science DOI: 10.1126/science.aba0690
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load data --------------------------------------------------------------------

env_panel <- readRDS(file = here("data", "processed", "annual_environmental_panel.rds"))

## PROCESSING ##################################################################

# P(MHW occurs) ----------------------------------------------------------------
# We first calculate P(MHW occurs): the probability with which at least one
# heatwave occurs in a year by dividing the number of years in  which at least
# one heatwave occurred by the number of all years.
# We do this for every Economic Unit and fishery
p_mhw_occurs <- env_panel %>% 
  group_by(eu_rnpa, fishery, year) %>% 
  summarize(at_least_one_mhw = mhw_events > 0) %>% 
  ungroup() %>% 
  group_by(eu_rnpa, fishery) %>% 
  summarize(p_at_least_one = mean(at_least_one_mhw)) %>% 
  ungroup()

p_mhw_occurs %>% 
  select(p_at_least_one) %>% 
  summarize_all(function(x){paste0("Mean = ", round(mean(x * 100), 2), "(SD = ", round(sd(x * 100), 2), ")")})

p_mhw_occurs %>%
  select(fishery, p_at_least_one) %>% 
  group_by(fishery) %>%
  summarize_all(function(x){paste0("Mean = ", round(mean(x), 3), "(SD = ", round(sd(x), 3), ")")})


between_fisheries <- p_mhw_occurs %>%
  select(fishery, p_at_least_one)

model <- lm(p_at_least_one ~ fishery, data = between_fisheries)
car::Anova(model, type = "III")

# P (MHW >= Threshold | MHW Occurs) --------------------------------------------
# We ow calculate P((MHW ≥ Thershold) | MHW occurs): The probability that
# MHW Cumulative Intensity of exceeds a threshold.  We determine this by
# dividing the number of years with heatwaves exceeding a given threshold by
# the number of all heatwaves that have occurred. We evaluate this for values
# of MHW Cum Int between 0 and the largest value observed across all data

# Define a function
get_cond_p_cum_int <- function(threshold, data){
  # Number of years with MHW Cum INT >= threshold
  n_at_or_above_threshold <- sum(data$mhw_int_cumulative >= threshold)
  # Divide by total number of years with at least one MHW
  cond_p <- n_at_or_above_threshold / length(data$mhw_int_cumulative)
  
  # Build 1 X 2 a tibble to return
  back <- tibble(threshold,
                 cond_p)
  
  # Return
  return(back)
}

# Find historicla maximum
max_annual_int_cumulative <- env_panel %>%
  filter(period == 1) %>%
  pull(mhw_int_cumulative) %>%
  max()

# Round it up
max_annual_int_cumulative <- ceiling(max_annual_int_cumulative / 10) * 10

# Filter to keep only years with at least 1 MWH and then calculate conditionals
con_p_mhw_threshold <- env_panel %>% 
  filter(mhw_events > 0) %>%
  select(eu_rnpa, fishery, mhw_int_cumulative) %>%
  nest(data = mhw_int_cumulative) %>% 
  expand_grid(threshold = seq(1, max_annual_int_cumulative, by = 5)) %>% 
  mutate(cond_p = map2(.x = threshold,
                       .y = data,
                       .f = ~get_cond_p_cum_int(.x, .y))) %>% 
  select(fishery, eu_rnpa, cond_p) %>%
  unnest(cond_p) %>% 
  filter(cond_p > 0)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = p_mhw_occurs, here("data", "output", "p_mhw_occurs.rds"))
saveRDS(object = con_p_mhw_threshold, here("data", "output", "con_p_mhw_threshold.rds"))
