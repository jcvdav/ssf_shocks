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
  panelsummary,
  tidyverse
)

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

centroids <- st_read(here("data", "processed", "centroids.gpkg")) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(lon = X, lat = Y) %>% 
  mutate(lat_dist = ((lat - 25) * 111.1) / 1e2)

hist_landings <- readRDS(here("data", "processed", "mean_historical_eu_landings.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

hist_mean_t <- models %>% 
  filter(indep == "norm_mhw_int_cumulative") %>% 
  select(fishery, data) %>% 
  unnest(data) %>% 
  select(fishery, eu_rnpa, temp_long_term, temp_cv, live_weight_cv, warming_rate_mean, warming_rate_max) %>% 
  distinct()

coef_data <- models %>%
  filter(indep == "norm_mhw_int_cumulative") %>% 
  mutate(coefs = map(fe_model, broom::tidy)) %>% 
  select(fishery, coefs) %>% 
  unnest(coefs) %>% 
  # group_by(fishery) %>% # This group by was included in the original submission, and it should not have been there at all
  filter(str_detect(term, "norm_mhw_int_cumulative")) %>% 
  mutate(eu_rnpa = str_extract(term, "[:digit:]+")) %>% 
  select(-term) %>% 
  left_join(centroids, by = c("fishery", "eu_rnpa")) %>% 
  left_join(hist_mean_t, by = c("fishery", "eu_rnpa")) %>% 
  left_join(hist_landings, by = c("fishery", "eu_rnpa")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " "))) %>% 
  mutate(img = here("data", "img", paste0(fishery, ".png")),
         p_fill = 1* (p.value <= 0.05) * estimate,
         eu_name = fct_reorder(eu_name, estimate))

# Fit models -------------------------------------------------------------------
three_models <- feols(estimate ~ sw(lat_dist, temp_cv, live_weight_cv) + turf_area_norm | fishery,
      data = coef_data %>% 
        # Re-scale regressors to be between 0 and 1 for ease of interpretation
        mutate(lat_dist = scales::rescale(lat_dist, to = c(0, 1)),
               temp_cv = scales::rescale(temp_cv, to = c(0, 1)),
               live_weight_cv = scales::rescale(live_weight_cv, to = c(0, 1)),
               turf_area_norm = scales::rescale(turf_area, to = c(0, 1))),
      vcov = vcov_conley(lat = ~lat,
                         lon = ~lon,
                         cutoff = 100,
                         distance = "spherical")) %>%
  set_names(c("Biogeographic",
              "Climate refugia", 
              "Adaptation"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
modelsummary(three_models,
             stars = panelsummary:::econ_stars(),
             coef_rename = c(lat_dist = "Slope",
                             temp_cv = "Slope",
                             live_weight_cv = "Slope"),
             gof_omit = c("IC|RMSE|R2"),
             output = here("results", "tab", "tab01_biophysical_vs_effect.tex"),
             title = "\\label{tab:biophysical_vs_effect}Regression coefficients testing for the biogeographic, climate refugia, and adaptation hypothesis.",
             notes = "All models include fixed-effects by fishery and use spatial standard errors with a 100 km buffer. Regressors were rescaled to 0-1 range to help comparison of coefficients between drivers.",
             threeparttable = T,
             escape = F)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(obj = coef_data,
        file = here("data", "output", "effect_on_fishery_and_biophysical.rds"))


## TEMPORARY CODE FOR RESONSE TO REVIEWERS

old_three_models <- feols(estimate ~ sw(lat_dist, temp_cv, live_weight_cv) | fishery,
                      data = old_coef_data,
                      vcov = vcov_conley(lat = ~lat,
                                         lon = ~lon,
                                         cutoff = 100,
                                         distance = "spherical")) %>%
  set_names(c("Biogeographic",
              "Climate refugia", 
              "Adaptation"))

three_models_without_turf_area <- feols(estimate ~ sw(lat_dist, temp_cv, live_weight_cv) | fishery,
                                        data = coef_data %>% 
                                          # Re-scale regressors to be between 0 and 1 for ease of interpretation
                                          mutate(lat_dist = scales::rescale(lat_dist, to = c(0, 1)),
                                                 temp_cv = scales::rescale(temp_cv, to = c(0, 1)),
                                                 live_weight_cv = scales::rescale(live_weight_cv, to = c(0, 1)),
                                                 turf_area_norm = scales::rescale(turf_area, to = c(0, 1))),
                                        vcov = vcov_conley(lat = ~lat,
                                                           lon = ~lon,
                                                           cutoff = 100,
                                                           distance = "spherical")) %>%
  set_names(c("Biogeographic",
              "Climate refugia", 
              "Adaptation"))







modelsummary(list("Panel A: Original (incorrect) estimates" = old_three_models,
                  "Panel B: Updated (correct) estimates" = three_models_without_turf_area,
                  "Panel C: Updated (correct) estimates with TURF area" = three_models),
             stars = panelsummary:::econ_stars(),
             coef_rename = c(lat_dist = "Slope",
                             temp_cv = "Slope",
                             live_weight_cv = "Slope"), coef_omit = "turf",
             gof_omit = c("IC|RMSE|R2|FE|N|Std"), shape = "rbind",
             threeparttable = T)
