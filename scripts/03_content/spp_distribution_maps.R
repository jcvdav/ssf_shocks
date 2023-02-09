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
library(raster)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load data --------------------------------------------------------------------

lob <- raster(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Panulirus_interruptus.tif")) 
purp <- raster(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Strongylocentrotus_purpuratus.tif")) %>% 
  crop(lob)
pep <- raster(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Parastichopus_parvimensis.tif")) %>% 
  crop(lob)

mex <- ne_countries(scale = "large", country = c("Mexico", "United States of America"), returnclass = "sf") %>% 
  st_crop(lob)

my_df <- function(r){
  as.data.frame(r, xy = T) %>% 
    magrittr::set_colnames(c("x", "y", "layer"))
}

df <- list(lob, purp, pep) %>% 
  set_names(c("P. interruptus", "S. purpuratus", "P. parvimensis")) %>% 
  map_dfr(my_df, .id = "species") %>% 
  drop_na() %>% 
  # mutate(species = case_when(species == 1 ~ "P. interruptus",
                             # species == 2 ~ "S. purpuratus",
                             # species == 3 ~ "P. parvimensis")) %>% 
  filter(#layer > 0.5,
         y > 5)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

maps <- ggplot() +
  geom_tile(data = df %>% filter(between(y, 23, 35.5), between(x, -121, -111)), aes(x = x, y = y, fill = layer)) +
  geom_sf(data = mex) +
  geom_hline(yintercept = 25) +
  facet_wrap(~species) +
  scale_fill_viridis_c(limits = c(0.5, 1), option = "B") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # cowplot::theme_map() +
  theme_bw() +
  theme(legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_blank(),
        strip.background = element_blank(),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(title = "p",
                               frame.colour = "black",
                               ticks.colour = "black"))

dists <- df %>% 
  group_by(species, y) %>% 
  summarize(layer = median(layer, na.rm = T)) %>% 
  ggplot(aes(x = y, y = layer, group = species)) + 
  geom_smooth(color = "red", linetype = "dashed", method = "lm", formula = y ~ poly(x, 2)) +
  geom_smooth(color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_point(shape = 21, color = "black", fill = "navyblue", alpha = 0.5, size = 4) +
  facet_wrap(~species, scales = "free_x") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  labs(x = "Latitude", y = "p")

plot_grid(maps, dists, rel_heights = c(2, 1), ncol = 1, align = "v")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------