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
  broom,
  tidyverse
)

# Define functions -------------------------------------------------------------
fixef_table <- function(model, match = ":", names) {
  model %>%
    broom::tidy() %>% 
    filter(str_detect(term, match)) %>% 
    mutate(term = str_extract(term, "[:digit:]{10}")) %>% 
    select(eu_rnpa = term, fe_estimate = estimate, fe_se = std.error) %>% 
    magrittr::set_names(names)
}

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
data <- models %>% 
  filter(indep == "norm_mhw_int_cumulative") %>% 
  select(fishery, fe_model, regime_model) %>% 
  mutate(fe_coef = map(fe_model, fixef_table, names = c("eu_rnpa", "fe_est", "fe_se")),
         regime_coef = map(regime_model, fixef_table, match = "befTRUE", names = c("eu", "regime_est", "regime_se"))) %>% 
  select(fishery, contains("coef")) %>% 
  unnest(cols = contains("coef")) %>% 
  select(-eu) %>% 
  mutate(fishery = str_to_title(str_replace_all(fishery, "_", " ")))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p <- ggplot(data = data,
            aes(x = fe_est,
                y = regime_est)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(ymin = regime_est - regime_se,
                    ymax = regime_est + regime_se)) +
  geom_errorbarh(aes(xmin = fe_est - fe_se,
                     xmax = fe_est + fe_se)) +
  geom_point() +
  facet_grid(~fishery) +
  coord_equal() +
  labs(x = "Main text estimates",
       y = "Allowing for regime shift")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "figS04_regime_change_test_plot.R",
                    width = 18,
                    height = 6)
