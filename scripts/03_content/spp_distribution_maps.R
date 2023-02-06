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

lob <- raster(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Panulirus_interruptus.tif")) %>% 
  disaggregate(fact = 10)
purp <- raster(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Strongylocentrotus_purpuratus.tif")) %>% 
  disaggregate(fact = 10) %>% 
  crop(lob)
pep <- raster(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Parastichopus_parvimensis.tif")) %>% 
  disaggregate(fact = 10) %>% 
  crop(lob)

mex <- ne_countries(scale = "large", country = c("Mexico", "United States of America"), returnclass = "sf") %>% 
  st_crop(lob)

my_df <- function(r){
  as.data.frame(r, xy = T) %>% 
    magrittr::set_colnames(c("x", "y", "layer"))
}

df <- list(lob, purp, pep) %>% 
  map_dfr(my_df, .id = "species") %>% 
  drop_na() %>% 
  mutate(species = case_when(species == 1 ~ "P. interruptus",
                             species == 2 ~ "S. purpuratus",
                             species == 3 ~ "P. parvimensis")) %>% 
  filter(layer > 0.5)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = mex) +
  facet_wrap(~species) +
  scale_fill_viridis_c(limits = c(0.5, 1), option = "B") +
  cowplot::theme_map() +
  theme(legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_blank(),
        strip.background = element_blank()) +
  guides(fill = guide_colorbar(title = "p",
                               frame.colour = "black",
                               ticks.colour = "black"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------