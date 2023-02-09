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
library(sf)
library(lme4)
library(cowplot)
library(tidyverse)
library(broom)

# Define functions -------------------------------------------------------------
extractr <- function(obj, pattern = "mhw|temp"){
  as.data.frame(ranef(obj)) %>%
    filter(str_detect(term, pattern))
}

get_mu <- function(obj){
  coef <- fixef(obj)[2]
  vars <- obj %>% vcov() %>% diag()
  sd <- sqrt(vars[2])
  tibble(x = coef, xmin = coef - sd, xmax = coef + sd)
}

# Load data --------------------------------------------------------------------
centroids <- st_read(here("data", "processed", "centroids.gpkg")) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(lon = X, lat = Y)
# land_mods <- readRDS(file = here("data", "output", "lobster_landing_models.rds"))
# 
# rev_mods <- readRDS(file = here("data", "output", "lobster_value_models.rds"))
# 
# ## PROCESSING ##################################################################
# 
# # Extract RE coefficients ------------------------------------------------------
# land_coefs <- land_mods %>% 
#   map_df(extractr, .id = "measure") %>%
#   mutate(variable = "Landings")
# 
# rev_coefs <- rev_mods %>% 
#   map_df(extractr, .id = "measure") %>%
#   mutate(variable = "Revenues")
# 
# coef_data <- 
#   bind_rows(land_coefs, rev_coefs) %>% 
#   mutate(grp = fct_reorder(grp, -condval)) %>% 
#   group_by(measure, variable) %>% 
#   mutate(condval_col = condval / max(condval))

# Extract central coefficient of independent variable --------------------------
# land_mu_data <- land_mods %>% 
#   map_df(get_mu, .id = "measure") %>%
#   mutate(variable = "Landings")
# 
# rev_mu_data <- rev_mods %>% 
#   map_df(get_mu, .id = "measure") %>%
#   mutate(variable = "Revenues")
# 
# mu_data <- 
#   bind_rows(land_mu_data, rev_mu_data)

mu_data <- models %>%
  mutate(mu = map(model, get_mu)) %>%
  select(fishery, dep, indep, mu) %>% 
  unnest(mu)

coef_data <- models %>%
  mutate(coeff = map(model, extractr)) %>%
  select(fishery, dep, indep, coeff) %>%
  unnest(coeff) %>% 
  left_join(centroids, by = c("grp" = "eu_rnpa", "fishery")) %>% 
  left_join(mu_data, by = c("dep", "indep", "fishery"))
  

## VISUALIZE ###################################################################

# Plot landings coefficients ---------------------------------------------------
land_coef_plot <- coef_data %>% 
  filter(dep == "landed_weight",
         indep == "mhw_int_cumulative") %>% 
  mutate(grp = fct_reorder(grp, -condval)) %>%
  ggplot(aes(y = grp, x = x + condval)) +
  geom_rect(data = filter(mu_data, indep == "MHW int"),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = F, fill = "gray80") +
  geom_vline(data = filter(mu_data, indep == "MHW int"),
             aes(xintercept = x), color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = x + condval - condsd,
                     xmax = x + condval + condsd),
                 height = 0) +
  geom_point(aes(fill = condval), shape = 21, size = 2) + 
  facet_wrap( ~ fishery, scale = "free_x", nrow = 1) +
  scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue") +
  labs(x = "RE of MHW Cum. Int.",
       y = NULL) +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank(),
        axis.text.y = element_text(size = 5))

coef_sst_plot <- coef_data %>% 
  filter(dep == "landed_weight",
         indep == "mhw_int_cumulative") %>% 
  left_join(data %>% 
              group_by(eu_rnpa, fishery) %>% 
              summarize(sd = sd(temp_mean),
                        temp_long_term = mean(temp_mean)) %>% 
              ungroup(), by = c("grp" = "eu_rnpa", "fishery")) %>% 
  ggplot(aes(x = temp_long_term, y = condval)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_errorbar(aes(ymin = condval -condsd,
                    ymax = condval + condsd),
                width = 0) +
  geom_errorbarh(aes(xmin = temp_long_term - sd, xmax = temp_long_term + sd)) +
  geom_point(aes(fill = condval), shape = 21, size = 3) +
  scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue") +
  labs(x = "Mean SST (°C)",
       y = "RE of MHW Cum. Int.") +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank()) +
  facet_wrap(~fishery, scales = "free")


coef_lat_plot <- coef_data %>% 
  filter(dep == "landed_weight",
         indep == "mhw_int_cumulative") %>% 
  ggplot(aes(x = lat, y = condval)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_errorbar(aes(ymin = condval -condsd,
                    ymax = condval + condsd),
                width = 0) +
  geom_point(aes(fill = condval), shape = 21, size = 2) +
  scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue") +
  labs(x = "°Latitude (Centroid)",
       y = "RE of MHW Cum. Int.") +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank()) +
  facet_wrap(~fishery, scale = "free")

full_plot <- plot_grid(land_coef_plot, coef_sst_plot, coef_sst_plot,
                       ncol = 1,
                       rel_heights = c(2, 1, 1))

startR::lazy_ggsave(plot = land_coef_plot,
                    filename = "land_coef_plot",
                    width = 8.7,
                    height = 8.7 * 1.2)

# Now revenues
rev_coef_plot <- coef_data %>% 
  filter(measure == "MHW int") %>% 
  mutate(grp = fct_reorder(grp, -condval)) %>% 
  filter(variable == "Revenues") %>% 
  ggplot(aes(y = grp, x = condval)) +
  geom_rect(data = filter(rev_mu_data, measure == "MHW int"), aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = F, fill = "gray80") +
  geom_vline(data = filter(rev_mu_data, measure == "MHW int"), aes(xintercept = x), color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = condval -condsd,
                     xmax = condval + condsd),
                 height = 0) +
  geom_point(aes(fill = condval), shape = 21, size = 2) + 
  scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue") +
  labs(x = "RE of MHW Cum. Int.",
       y = NULL) +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank())


# Mega plot --- needs to be fixed
coef_data %>% 
  # left_join(centroids, by = c("grp"= "eu_name")) %>% 
  # mutate(grp = fct_reorder(grp, lat)) %>% 
  ggplot(aes(y = grp, x = condval)) +
  geom_rect(data = mu_data, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = F, fill = "gray80") +
  geom_vline(data = mu_data, aes(xintercept = x), color = "black") +
  geom_errorbarh(aes(xmin = condval -condsd,
                     xmax = condval + condsd),
                 height = 0) +
  geom_point(aes(fill = condval), shape = 21, size = 2) + 
  facet_wrap(variable~measure, ncol = 4, scales = "free_x") +
  scale_fill_gradient2(low = "red", high = "steelblue", mid = "white") +
  labs(x = "Temperature influence",
       y = NULL) +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank())


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------








