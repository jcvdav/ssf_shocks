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
  sf,
  modelsummary,
  tidyverse
)

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

centroids <- st_read(here("data", "processed", "centroids.gpkg")) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(lon = X, lat = Y) %>% 
  mutate(lat_dist = ((lat - 25) * 111.1) / 1e3)

hist_landings <- readRDS(here("data", "processed", "mean_historical_eu_landings.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

hist_mean_t <- models %>% 
  filter(indep == "norm_mhw_int_cumulative") %>% 
  select(fishery, data) %>% 
  unnest(data) %>% 
  select(fishery, eu_rnpa, temp_long_term, temp_cv, live_weight_cv) %>% 
  distinct()

coef_data <- models %>%
  filter(indep == "norm_mhw_int_cumulative") %>% 
  mutate(coefs = map(fe_model, broom::tidy)) %>% 
  select(fishery, coefs) %>% 
  unnest(coefs) %>% 
  group_by(fishery) %>% 
  filter(str_detect(term, "norm_mhw_int_cumulative")) %>% 
  mutate(eu_rnpa = str_extract(term, "[:digit:]+")) %>% 
  select(-term) %>% 
  left_join(centroids, by = c("fishery", "eu_rnpa")) %>% 
  left_join(hist_mean_t, by = c("fishery", "eu_rnpa")) %>% 
  left_join(hist_landings, by = c("fishery", "eu_rnpa")) %>% 
  left_join(slopes, by = c("fishery", "eu_rnpa")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " "))) %>% 
  mutate(img = here("data", "img", paste0(fishery, ".png")),
         p_fill = 1* (p.value <= 0.05) * estimate,
         eu_name = fct_reorder(eu_name, estimate))

# Fit models -------------------------------------------------------------------
three_models <- feols(estimate ~ sw(lat_dist, temp_cv, live_weight_cv, slope) | fishery,
      weights = ~live_weight,
      data = coef_data,
      vcov = vcov_conley(lat = ~lat,
                         lon = ~lon,
                         cutoff = 100,
                         distance = "spherical")) %>%
  set_names(c("Biogeographic",
              "Climate refiguia", 
              "Adaptation"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
modelsummary(three_models,
             stars = panelsummary:::econ_stars(),
             coef_rename = c(lat_dist = "Slope",
                             temp_cv = "Slope",
                             live_weight_cv = "Slope"),
             gof_omit = c("IC|RMSE|R2"),
             title = "Regression coefficients testing for the biogeographic, climate refugia, and adaptation hypothesis.",
             notes = "All models include fixed-effects by fishery and use spatial standard errors with a 100 km buffer.")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(obj = coef_data,
        file = here("data", "output", "effect_on_fishery_and_biophysical.rds"))
