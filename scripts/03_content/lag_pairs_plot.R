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
  GGally,
  tidyverse
)

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

## PROCESSING ##################################################################
data <- models %>% 
  mutate(coefs = map(fe_model, broom::tidy)) %>% 
  select(fishery, indep, coefs) %>% 
  unnest(coefs) %>% 
  filter(str_detect(term, ":befTRUE:eu_rnpa")) %>% 
  mutate(eu_rnpa = str_extract(term, "[:digit:]{10}")) %>% 
  select(eu_rnpa, indep, estimate, fishery) %>%
  mutate(indep = case_when(indep == "norm_mhw_int_cumulative" ~ "hat(beta[i])",
                           indep == "norm_mhw_int_cumulative_lag" ~ "hat(beta[i])(lag == 1)",
                           indep == "norm_mhw_int_cumulative_lag3" ~ "hat(beta[i])(lag == 3)")) %>% 
  pivot_wider(names_from = indep, values_from = estimate)

## VISUALIZE ###################################################################
lag_pairs_plot <- ggpairs(data = data,
                          columns = 3:5,
                          aes(color = fishery),
                          labeller = "label_parsed",
                          upper = list(color = "transparent")) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

## EXPORT ######################################################################
startR::lazy_ggsave(
  plot = lag_pairs_plot,
  filename = "lag_pairs_plot",
  width = 18,
  height = 12
)
