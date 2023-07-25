################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# We next calculate the conditional probabilities P((MHW≥observed duration) | MHW occurs)
# that a given heatwave equals or exceeds the duration (intensity/cumulative intensity) of 
# the observed one, under the condition that a (arbitrary long/intense) heatwave occurs.
# We determine this probability by binomial sampling of events, that is, dividing the
# number of heatwaves exceeding the observed duration (intensity/cumulative intensity) by
# the number of all heatwaves that have occurred, for both present-day and pre-industrial
# climate.
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  cowplot,
  tidyverse
)

# Load data --------------------------------------------------------------------
future_mhw <- readRDS(file = here("data", "processed", "future_mhw_by_turf.rds"))

env_panel <- readRDS(file = here("data", "processed", "annual_environmental_panel.rds"))

centroids <- st_read(here("data", "processed", "centroids.gpkg"))

## PROCESSING ##################################################################

data <- future_mhw %>% 
  mutate(climatology = map(mhw, ~.x$climatology)) %>%
  select(model, ssp, eu_rnpa, fishery, climatology) %>%
  unnest(climatology) %>%
  mutate(year = lubridate::year(t))

# P(MHW Occurs) ----------------------------------------------------------------

p_mhw_occurs_future <- data %>%
  filter(year >= 2022) %>% 
  group_by(model, ssp, eu_rnpa, fishery, year) %>%
  summarize(mhw = any(event)) %>%
  ungroup() %>%
  group_by(model, ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(mhw)) %>%
  ungroup() %>% 
  group_by(ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(p_at_least_one)) %>%
  ungroup() %>% 
  mutate(type = "forecast")

# Hindcast
p_mhw_occurs_hindcast <- data %>%
  filter(year <= 2021) %>% 
  group_by(model, ssp, eu_rnpa, fishery, year) %>%
  summarize(mhw = any(event)) %>%
  ungroup() %>%
  group_by(model, ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(mhw)) %>%
  ungroup() %>% 
  group_by(ssp, eu_rnpa, fishery) %>%
  summarize(p_at_least_one = mean(p_at_least_one)) %>%
  ungroup() %>% 
  mutate(type = "hindcast")

references <- env_panel %>% 
  filter(period == 1) %>% 
  group_by(fishery, eu_rnpa) %>% 
  summarize(mean = mean(mhw_int_cumulative),
            max = max(mhw_int_cumulative))

con_p_mhw_threshold_future <- future_mhw %>% 
  select(model, ssp, eu_rnpa, fishery, summary) %>%
  unnest(summary) %>%
  filter(year >= 2022) %>% 
  left_join(references, by = c("eu_rnpa", "fishery")) %>% 
  mutate(exceeds = 1 * (mhw_int_cumulative >= max)) %>% 
  group_by(fishery, eu_rnpa, ssp, model) %>% 
  summarize(p_exceeds = mean(exceeds), .groups = "drop") %>% 
  ungroup() %>% 
  group_by(fishery, eu_rnpa, ssp) %>% 
  summarize(mean = mean(p_exceeds)) %>% 
  ungroup() %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
         fishery = fct_relevel(fishery, c("Lobster", "Urchin", "Sea cucumber")))

saveRDS(object = p_mhw_occurs_future,
        file = here("data", "output", "p_mhw_occurs_future.rds"))

saveRDS(object = p_mhw_occurs_hindcast,
        file = here("data", "output", "p_mhw_occurs_hindcast.rds"))

saveRDS(object = con_p_mhw_threshold_future,
        file = here("data", "output", "con_p_mhw_threshold_future.rds"))


ggplot(p_mhw_occurs_hindcast %>%
         mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
                fishery = fct_relevel(fishery, c("Lobster", "Urchin", "Sea cucumber"))),
       aes(x = fishery, y = p_at_least_one,
           color = ssp, fill = ssp)) +
  geom_point(aes(group = ssp),
             size = 1,
             alpha = 0.5,
             position = my_jitter) + 
  stat_summary(geom = "linerange",
               fun.data = mean_cl_normal,
               color = "gray10",
               linewidth = 0.5,
               position = my_jitter) +
  stat_summary(geom = "pointrange",
               fun.data = mean_se,
               fatten = 4,
               linewidth = 2,
               position = my_jitter) +
  scale_color_manual(values = c("black", "royalblue3", "orange2", "darkred")) +
  labs(x = "Fishery",
       y = "P(MHW Occurs)") +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0))

# P (MHW >= Threshold | MHW Occurs) --------------------------------------------
# Define thresholds


# Calculate, for each ssp, the probability that mhw_int_cum in the future exceeds the mean of what we saw

# Conditional on there being a heatwave, what is the probability that it will be
# as big as X?

## VISUALIZE ###################################################################



p_future <- plot_grid(p_at_least_one, p_as_big,
                      labels = "AUTO", ncol = 1)

startR::lazy_ggsave(plot = p_future,
                    filename = "p_future_mhw_plot",
                    width = 10,
                    height = 18)

mex <- rnaturalearth::ne_countries(country = c("Mexico", "United States of America"),
                                   scale = "large",
                                   returnclass = "sf") %>% 
  st_crop(st_buffer(centroids, dist = 4e4))

spatial_probs <- centroids %>% 
  left_join(fishery_summary, by = c("fishery", "eu_rnpa")) %>% 
  filter(fishery == "lobster")

ggplot() +
  geom_sf(data = mex) +
  geom_sf(data = spatial_probs,
          mapping = aes(fill = mean),
          shape = 21,
          alpha = 0.8) +
  facet_wrap(~ssp) +
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Cond. P") +
  # theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))

ggplot(data = p_mhw_occurs_future %>% 
         mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
                fishery = fct_relevel(fishery, c("Lobster", "Urchin", "Sea cucumber"))),
       aes(x = fishery, y = p_at_least_one, color = ssp, group = ssp)) +
  geom_hline(yintercept =  0.764) +
  geom_point(size = 1,
             alpha = 0.5,
             position = my_jitter) + 
  stat_summary(geom = "pointrange",
               fun.data = mean_se,
               fatten = 4,
               linewidth = 2,
               position = my_jitter) +
  scale_color_manual(values = c("royalblue3", "orange2", "darkred")) +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0)) +
  labs(x = "Fishery",
       y = "P(MHW occurs)")
