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
  lme4,
  fixest,
  tidyverse
)

# Define functions -------------------------------------------------------------
# Define a way to compute heteroskedastic and spatio-temporally autocorrelation
# consistent standard errors a la Conley, 1999:
# From https://github.com/lrberge/fixest/issues/350#issuecomment-1671226930
vcov_conley_hac <- function(x) {
  
  # Spatial portion
  vcov_conley <- 
    vcov_conley(
      x = x,
      lat = ~lat,
      lon = ~lon,
      cutoff = 100)
  
  # Panel portion
  vcov_hac <- 
    vcov_NW(
      x = x,
      unit = ~eu_rnpa,
      time = ~year,
      lag = 5)
  # Heteroskedasticity
  vcov_robust <-
    vcov_cluster(
      x = x,
      cluster = ~eu_rnpa)
  
  
  vcov_conley_hac <- vcov_conley + vcov_hac - vcov_robust
  
  return(vcov_conley_hac)
}

# Load data --------------------------------------------------------------------
data <-
  readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds"))


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
  mutate(fe_fml = paste0(dep, "~ year + ", indep, ":eu_rnpa"),
         re_fml = paste(dep, "~ year +", indep, " + (0 + ", indep, " | eu_rnpa)")) %>% 
  mutate(fe_model = map2(.x = fe_fml, .y = data,
                         .f = ~feols(fml = as.formula(.x),
                                     data = .y,
                                     panel.id = ~eu_rnpa + year,
                                     vcov = function(x)vcov_conley_hac(x))),
         re_model = map2(.x = re_fml, .y = data,
                         .f = ~lmer(formula = .x,
                                    data = .y)),
         regime_model = map(data, ~feols(fml = norm_live_weight ~ year + norm_mhw_int_cumulative:bef:eu_rnpa | eu_rnpa,
                                         data = .x %>% mutate(bef = (period %in% c("0", "1"))),
                                         panel.id = ~eu_rnpa + year,
                                         vcov = function(x)vcov_conley_hac(x))))

## EXPORT ######################################################################
saveRDS(object = models,
        file = here("data", "output", "effect_on_fishery_models.rds"))
