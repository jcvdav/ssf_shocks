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
library(ggimage)
library(cowplot)
library(magrittr)
library(tidyverse)

# Load data --------------------------------------------------------------------
baseline <- readRDS(here("data", "estimation_panels", "env_fish_panel.rds")) %>% 
  select(eu_rnpa, fishery) %>% 
  distinct()

mhw_data <- readRDS(here("data", "processed", "mhw_by_turf.rds"))

env_panel <-
  readRDS(file = here("data", "processed", "annual_environmental_panel.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
centroids <- st_read(here("data", "processed", "centroids.gpkg")) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(lon = X, lat = Y)

vis_data <- env_panel %>% 
  inner_join(baseline, by = c("eu_rnpa", "fishery")) %>% 
  select(eu_rnpa, fishery, year, mhw_int_cumulative) %>% 
  distinct() %>% 
  left_join(centroids, by = c("fishery", "eu_rnpa")) %>% 
  mutate(eu_rnpa = fct_reorder(eu_rnpa, lat),
         fishery = str_to_sentence(str_replace(fishery, "_", " ")))


mhw_by_turf <- mhw_data %>% 
  inner_join(baseline, by = c("eu_rnpa", "fishery")) %>% 
  mutate(mhw = map(mhw, ~.x$event)) %>% 
  select(eu_rnpa, fishery, mhw) %>% 
  unnest(mhw) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

events_by_turf <- mhw_data %>% 
  inner_join(baseline, by = c("eu_rnpa", "fishery")) %>% 
  mutate(climatology = map(mhw, ~.x$climatology)) %>% 
  select(eu_rnpa, fishery, climatology) %>% 
  unnest(climatology) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

plot <- ggplot(data = vis_data,
               mapping = aes(x = year, y = eu_rnpa, fill = mhw_int_cumulative)) +
  geom_tile() +
  facet_wrap(~fishery, scales = "free") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette(name = "Zissou1",
                                                           type = "continuous")) +
  guides(fill = guide_colorbar(title = "MHW Cum. Int.\n(°C x days)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "Year",
       y = NULL) +
  theme(legend.position = "bottom") +
  scale_y_discrete(expand = c(0, 0))

plot2 <- events_by_turf %>% 
  group_by(t, fishery) %>% 
  summarize(n = sum(event) / n()) %>% 
  ggplot(aes(x = t, y = n, color = n)) + 
  geom_line() +
  facet_wrap(~fishery, scales = "free", ncol = 1) +
  scale_color_gradientn(colours = wesanderson::wes_palette(name = "Zissou1",
                                                           type = "continuous")) +
  scale_y_continuous(labels = scales::percent) +
  guides(color = guide_colorbar(title = "% Fisheries\nwith MHW",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "Year",
       y = "% Fisheries experiencing MHW")


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = plot,
                    filename = "hovmoller_mhw_int",
                    width = 24, height = 9)

startR::lazy_ggsave(plot = plot2,
                    filename = "sst_mhw_int",
                    width = 20, height = 9)
