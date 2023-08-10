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
  cowplot,
  broom,
  lme4,
  magrittr,
  tidyverse
)

# Define functions -------------------------------------------------------------
ranef_table <- function(model){
  coef <- fixef(model)[3]
  model %>%
    ranef() %>% 
    as_tibble() %>%
    filter(str_detect(term, "norm_mhw_int_cumulative")) %>% 
    mutate(estimate = coef + condval) %>% 
    select(eu_rnpa = grp, re_estimate = estimate, re_se = condsd)
}

fixef_table <- function(model) {
  model %>%
    broom::tidy() %>% 
    filter(str_detect(term, ":")) %>% 
    mutate(term = str_extract(term, "[:digit:]{10}")) %>% 
    select(eu_rnpa = term, fe_estimate = estimate, fe_se = std.error)
}

combine <- function(fe_model, re_model) {
  fe <- fixef_table(fe_model)
  re <- ranef_table(re_model)
  
  coefs <- left_join(fe, re, by = c("eu_rnpa"))
  return(coefs)
}

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
coefs <- models %>% 
  select(fishery, indep, fe_model, re_model) %>% 
  mutate(coefs = map2(fe_model, re_model, combine)) %>% 
  select(fishery, indep, coefs) %>% 
  unnest(coefs) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
         indep = case_when(indep == "norm_mhw_int_cumulative" ~ "hat(beta[i])",
                           indep == "norm_mhw_int_cumulative_lag" ~ "hat(beta[i])(lag = 1)",
                           indep == "norm_mhw_int_cumulative_lag3" ~ "hat(beta[i])(lag = 3)"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p <- ggplot(coefs, aes(x = fe_estimate, y = re_estimate)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_errorbarh(aes(xmin = fe_estimate - fe_se,
                     xmax = fe_estimate + fe_se)) +
  geom_errorbar(aes(ymin = re_estimate - re_se,
                    ymax = re_estimate + re_se)) +
  geom_point() + 
  facet_grid(indep~fishery) +
  coord_equal() +
  labs(x = "FE Estimate",
       y = "RE Estimate")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "figS06_fe_vs_re_coefficients",
                    width = 15,
                    height = 12)
