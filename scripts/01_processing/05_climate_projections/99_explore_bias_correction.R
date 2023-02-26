################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Explore bias correction
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
# Load packages ----------------------------------------------------------------
library(here)
library(lubridate)
library(tidyverse)

# Load data --------------------------------------------------------------------
RS_sst_ts <-
  readRDS(file = here("data", "processed", "daily_mean_sst_by_turf.rds"))

CMIP_sst_ts <-
  readRDS(file = here("data", "processed", "ssp245_daily_mean_sst_by_turf.rds"))

## PROCESSING ##################################################################

# Test things ------------------------------------------------------------------
testing <- function(year_out, data) {
  
  inside_data <- data %>% 
    filter(!year(t) == year_out)
  
  test_data <- data %>% 
    filter(year(t) == year_out)
  
  gpt_scaling_factor <- mean(inside_data$rs) / mean(inside_data$cmip)
  scaling_factor <- mean(inside_data$rs / inside_data$cmip)
  model1 <- lm(rs ~ 0 + cmip, data = inside_data)
  model2 <- lm(rs ~ cmip, data = inside_data)
  model3 <- lm(rs ~ log(cmip), data = inside_data)
  model4 <- lm(log(rs) ~ cmip, data = inside_data)
  model5 <- lm(log(rs) ~ log(cmip), data = inside_data)
  
  fixed <- test_data %>% 
    mutate(year = year(t),
           month = month(t),
           week = week(t)) %>% 
    mutate(pred_lin_lin = predict(model1, newdata = .),
           pred_lin_int = predict(model2, newdata = .),
           pred_lin_log = predict(model3, newdata = .),
           pred_log_lin = exp(predict(model4, newdata = .) + (summary(model4)$sigma)^2/2),
           pred_log_log = exp(predict(model5, newdata = .) + (summary(model5)$sigma)^2/2),
           pred_scale_gpt = cmip * gpt_scaling_factor,
           pred_scale = cmip * scaling_factor)
  
  rss <- fixed %>% 
    select(cmip, contains("pred")) %>% 
    summarize_all(.funs = ~(sum((.x - fixed$rs)^2, na.rm = T))) %>% 
    pivot_longer(cols = c(cmip, contains("pred")),
                 names_to = "model",
                 values_to = "value") %>% 
    mutate(variable = "rss")
  
  rmse <- fixed %>% 
    select(cmip, contains("pred")) %>% 
    summarize_all(.funs = ~(sqrt(mean((.x - fixed$rs) ^ 2, na.rm = T)))) %>% 
    pivot_longer(cols = c(cmip, contains("pred")),
                 names_to = "model",
                 values_to = "value") %>% 
    mutate(variable = "rmse")
  
  results <- bind_rows(rss,
                       rmse) %>% 
    mutate(year = year_out)
  
  return(results)
}

fix <- daily_sst_ts %>% 
  filter(year(t) >= 2015 ,
         year(t) <= 2021) %>% 
  mutate(source = ifelse(source == 1, "rs", "cmip")) %>% 
  pivot_wider(names_from = source, values_from = temp) %>% 
  drop_na()

c(2015:2021) %>% 
  map_dfr(testing, data = fix) %>% 
  ggplot(aes(x = model, y = value, color = year)) +
  geom_point() +
  facet_wrap(~variable, scales = "free_y", ncol = 1)

# X ----------------------------------------------------------------------------
daily_sst_ts <- bind_rows(RS_sst_ts,
                          CMIP_sst_ts,
                          .id = "source")

bias_correction <- function(data) {
  inside_data <- data %>% 
    pivot_wider(names_from = source, values_from = temp) %>% 
    drop_na()
  
  model <- fixest(rs ~ cmip, data = inside_data, vcov = "nw")
  
}

overlap_data <- daily_sst_ts %>% 
  filter(year(t) >= 2015 ,
         year(t) <= 2021) %>% 
  mutate(id = paste(fishery, eu_rnpa, "_")) %>% 
  mutate(source = ifelse(source == 1, "rs", "cmip")) %>%
  select(t, fishery, eu_rnpa, source, temp) %>% 
  pivot_wider(names_from = source, values_from = temp) %>% 
  drop_na()

correction_models <- overlap_data %>% 
  group_by(fishery, eu_rnpa) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(rs ~ cmip, data = .x))) %>% 
  select(-data)

# NOTES TO DO:
# Make a table of correction_models and export it

tidy_predict <- function(object, data){
  data %>% 
    mutate(temp = predict(object, newdata = .)) %>%
    select(t, temp)
}

nested_RS <- RS_sst_ts %>% 
  filter(year(t) <= 2014) %>% 
  group_by(eu_rnpa, fishery) %>% 
  nest()

corrected <- CMIP_sst_ts %>% 
  rename(cmip = temp) %>% 
  group_by(eu_rnpa, fishery) %>% 
  nest() %>% 
  inner_join(correction_models, by = c("fishery", "eu_rnpa")) %>% 
  mutate(corrected = map2(.x = model,
                          .y = data,
                          .f = ~tidy_predict(object = .x,
                                             data = .y))) %>% 
  select(-data) %>% 
  inner_join(nested_RS, by = c("fishery", "eu_rnpa")) %>% 
  mutate(corrected_long = map2(data, corrected, bind_rows)) %>% 
  select(eu_rnpa, fishery, corrected_long)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------