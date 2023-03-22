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
library(here)
library(lme4)
library(fixest)
library(tidyverse)

# Load data --------------------------------------------------------------------
data <-
  readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds"))


## ESTIMATE ####################################################################
models <- data %>% 
  group_by(fishery) %>%
  nest() %>% 
  expand_grid(
    dep = c("norm_landed_weight"),
    indep = c(
      "norm_mhw_int_cumulative",
      "norm_temp_mean",
      "norm_mhw_days",
      "norm_mhw_events"
    )
  ) %>% 
  mutate(re_fml = paste0(dep, "~ year +", indep, "+ (0 + ", indep, "| eu_rnpa)"),
         fe_fml = paste0(dep, "~", "-1 + year +", indep, ":eu_rnpa")) %>% 
  mutate(re_model = map2(.x = re_fml, .y = data, lmer),
         fe_model = map2(.x = fe_fml, .y = data,
                         .f = ~feols(fml = as.formula(.x),
                                     data = .y,
                                     panel.id = ~eu_rnpa + year,
                                     vcov = "DK")))

# ## EXPORT ######################################################################
saveRDS(object = models,
        file = here("data", "output", "effect_on_fishery_models.rds"))
