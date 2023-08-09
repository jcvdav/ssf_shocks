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
pacman::p_load(
  here,
  fixest,
  tidyverse
)

# Load data --------------------------------------------------------------------
data <-
  readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds")) %>% 
  filter(!eu_rnpa == "0203127311") %>% 
  mutate(bef = period %in% c("0", "1"))

## ESTIMATE ####################################################################
models <- data %>% 
  group_by(fishery) %>%
  nest() %>% 
  expand_grid(
    dep = c("norm_live_weight"),
    indep = c(
      "norm_mhw_int_cumulative",
      "norm_mhw_int_cumulative_lag",
      "norm_mhw_int_cumulative_lag3"
    )
  ) %>% 
  mutate(fe_fml = paste0(dep, "~ year + ", indep, ":bef:eu_rnpa | eu_rnpa")) %>% 
  mutate(fe_model = map2(.x = fe_fml, .y = data,
                         .f = ~feols(fml = as.formula(.x),
                                     data = .y,
                                     panel.id = ~eu_rnpa + year,
                                     vcov = "NW"
                                     # waiting for vcov-hac SE errors to be available by fixest 
                                     #vcov = vcov_conley(lat = ~lat,
                                                        # lon = ~lon,
                                                        # cutoff = 100,
                                                        # distance = "spherical")
                         ))
         )

# ## EXPORT ######################################################################
saveRDS(object = models,
        file = here("data", "output", "effect_on_fishery_models.rds"))
