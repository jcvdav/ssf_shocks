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
library(modelsummary)
library(lme4)
library(tidyverse)

# Load data --------------------------------------------------------------------
data <-
  readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
reg_data <- data %>%
  group_by(eu_rnpa, fishery) %>% 
  mutate_at(
    .vars = vars(temp_mean, mhw_int_cumulative, mhw_days, mhw_events),
    .funs = ~ (.x - mean(.x)) / sd(.x)
  ) %>% 
  ungroup()
# mutate_at(.vars = vars(temp_mean, mhw_int_cumulative, mhw_days, mhw_events), .funs = log1p)
# mutate_at(.vars = vars(temp_mean, mhw_int_cumulative, mhw_days, mhw_events), .funs = ihs)


## ESTIMATE ####################################################################
models <- reg_data %>% 
  group_by(fishery) %>%
  nest() %>% 
  expand_grid(
    dep = c("landed_weight"),
    indep = c("temp_mean", "mhw_int_cumulative", "mhw_days", "mhw_events")
  ) %>% 
  mutate(fml = paste(dep, "~", indep, "+ (", indep, "| eu_rnpa)")) %>% 
  mutate(model = map2(.x = fml, .y = data, lmer))

# The old approach (shown below) is now commented out because it's easier to
# apply all models at once
# # Landings ---------------------------------------------------------------------
# land_temp_mod <-
#   lme4::lmer(landed_weight ~ temp_mean + (temp_mean |
#                                             eu_name), data = reg_data)
# land_int_mod <-
#   lme4::lmer(landed_weight ~ mhw_int_cumulative + (mhw_int_cumulative |
#                                                      eu_name),
#              data = reg_data)
# land_days_mod <-
#   lme4::lmer(landed_weight ~ mhw_days + (mhw_days |
#                                            eu_name), data = reg_data)
# land_event_mod <-
#   lme4::lmer(landed_weight ~ mhw_events + (mhw_events |
#                                              eu_name), data = reg_data)
# 
# # Revenues ---------------------------------------------------------------------
# rev_temp_mod <-
#   lme4::lmer(value ~ temp_mean + (temp_mean |
#                                     eu_name), data = reg_data)
# rev_int_mod <-
#   lme4::lmer(value ~ mhw_int_cumulative + (mhw_int_cumulative |
#                                              eu_name),
#              data = reg_data)
# rev_days_mod <-
#   lme4::lmer(value ~ mhw_days + (mhw_days |
#                                    eu_name), data = reg_data)
# rev_event_mod <-
#   lme4::lmer(value ~ mhw_events + (mhw_events |
#                                      eu_name), data = reg_data)
# 
# # Group models -----------------------------------------------------------------
# land_mods <- list(land_temp_mod,
#                   land_int_mod,
#                   land_days_mod,
#                   land_event_mod) %>%
#   set_names(c("SST", "MHW int", "MHW days", "MHW events"))
# 
# rev_mods <- list(rev_temp_mod,
#                  rev_int_mod,
#                  rev_days_mod,
#                  rev_event_mod) %>%
#   set_names(c("SST", "MHW int", "MHW days", "MHW events"))
# 
# ## EXPORT ######################################################################
# saveRDS(object = land_mods,
#         file = here("data", "output", "lobster_landing_models.rds"))
# 
# saveRDS(object = rev_mods,
#         file = here("data", "output", "lobster_value_models.rds"))
