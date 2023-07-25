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
  sf,
  ggimage,
  cowplot,
  magrittr,
  tidyverse
)

# Load data --------------------------------------------------------------------
models <- readRDS(file = here("data", "output", "effect_on_fishery_models.rds"))

centroids <- st_read(here("data", "processed", "centroids.gpkg")) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(lon = X, lat = Y)

depth_info <- readRDS(file = here("data", "processed", "depth_and_area.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

hist_mean_t <- models %>% 
  filter(indep == "norm_mhw_int_cumulative") %>% 
  select(fishery, data) %>% 
  unnest(data) %>% 
  select(fishery, eu_rnpa, temp_long_term, temp_cv, landed_weight_cv) %>% 
  distinct()

coef_data <- models %>%
  filter(indep == "norm_mhw_int_cumulative",
         fishery == "lobster") %>% 
  mutate(coefs = map(fe_model, broom::tidy)) %>% 
  select(fishery, coefs) %>% 
  unnest(coefs) %>% 
  group_by(fishery) %>% 
  filter(str_detect(term, ":")) %>% 
  mutate(eu_rnpa = str_extract(term, "[:digit:]+")) %>% 
  select(-term) %>% 
  left_join(centroids, by = c("fishery", "eu_rnpa")) %>% 
  left_join(hist_mean_t, by = c("fishery", "eu_rnpa")) %>% 
  left_join(depth_info, by = c("fishery", "eu_rnpa")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
         fishery = fct_relevel(fishery, c("Lobster", "Urchin", "Sea cucumber"))) %>% 
  mutate(img = here("data", "img", paste0(fishery, ".png")),
         p_fill = (p.value <= 0.05) * estimate)


scatter_plot <- function(data, variable, lab, img = F) {
  p <- data %>% 
    ggplot(aes(x = {{variable}},
               y = estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.5) + 
    geom_smooth(method = "lm", linetype = "dashed", linewidth = 0.5, color = "black") +
    geom_errorbar(aes(ymin = estimate + std.error,
                      ymax = estimate - std.error),
                  width = 0) +
    geom_point(aes(fill = p_fill), shape = 21, size = 2) +
    scale_fill_gradient2(low = "#E41A1C", mid = "gray", high = "steelblue", midpoint = 0) +
    labs(x = lab,
         y = expression(hat(beta[i]))) +
    guides(fill = guide_colorbar(title = expression(hat(beta[i])),
                                 frame.colour = "black",
                                 ticks.colour = "black",)) +
    theme(legend.position = c(0, 0),
          legend.justification = c(0, 0),
          legend.direction = "horizontal")
  
  if(img) {
    p <- p + 
      geom_image(data = select(data, fishery, img) %>% distinct(),
                 mapping = aes(image = img), 
                 x = c(32, 32, 31.5),
                 y = c(0.65, 0.95, 0.4),
                 inherit.aes = F,
                 size = 0.15)
  }
  
  return(p)
}

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
# Need to fiddle witht eh coordinates for the images
lat <- scatter_plot(coef_data, lat, "°Latitude (Centroid)") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))
temp_cv <- scatter_plot(coef_data, temp_cv, "CV SST") +
  theme(legend.position = "None")
land_cv <- scatter_plot(coef_data, landed_weight_cv, "CV Landings") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))

p <- plot_grid(temp_cv, land_cv, ncol = 2, labels = "AUTO")

# Others -----------------------------------------------------------------------
scatter_plot(coef_data, depth, "Mean depth (m)")
scatter_plot(coef_data, area, "Area")

coef_data %>%
  select(fishery, eu_rnpa, lat, temp_cv) %>%
  distinct() %>%
  ggplot(aes(x = lat, y = temp_cv)) +
  geom_point() +
  geom_text(aes(label = eu_rnpa), size = 2, nudge_x = 0.01) +
  facet_wrap(~fishery)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "biophysical_vs_effect",
                    width = 20,
                    height = 14)
### Lat for slides
ggplot(coef_data,
       aes(x = estimate, y = lat)) +
  geom_vline(xintercept = 0, linewidth = 0.5) + 
  geom_smooth(method = "lm", linetype = "dashed", linewidth = 0.5, color = "black") +
  geom_errorbarh(aes(xmin = estimate + std.error,
                    xmax = estimate - std.error),
                height = 0) +
  geom_point(aes(fill = p_fill), shape = 21, size = 2) +
  scale_fill_gradient2(low = "#E41A1C", mid = "gray", high = "steelblue", midpoint = 0) +
  labs(x = expression(hat(beta[i])),
       y = "° Latitude (Centroid)") +
  guides(fill = guide_colorbar(title = expression(hat(beta[i])),
                               frame.colour = "black",
                               ticks.colour = "black",)) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal")
