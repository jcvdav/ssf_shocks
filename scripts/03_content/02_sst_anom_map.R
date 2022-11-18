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
library(rnaturalearth)
library(raster)
library(sf)
library(tidyverse)

# Load data --------------------------------------------------------------------
sst_files <- list.files(
  path = here::here("../data_remotes/data/sst/processed_annual"),
  pattern = "tif",
  full.names = T)

sst_hist_stack <- stack(sst_files[1:11]) %>% 
  calc(mean)

sst_2015 <- raster(sst_files[13])

coast <-
  ne_countries(country = c("Mexico", "United States of America"),
               returnclas = "sf", scale = "medium") %>% 
  st_transform(crs = "EPSG:4326") %>% 
  st_crop(sst_hist_stack)

turfs <-
  sf::st_read(dsn = file.path(
    mex_data_path,
    "concesiones",
    "processed",
    "lobster_turf_polygons.gpkg"
  )) %>%
  st_transform(crs = 4326)


## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

dif <- (sst_2015 - sst_hist_stack) %>% 
  raster::aggregate(fact = 10) %>% 
  as.data.frame(xy = T) %>% 
  drop_na()

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

sst_anom <- ggplot() +
  geom_raster(data = dif,
              mapping = aes(x = x, y = y, fill = layer)) +
  geom_sf(data = coast, color = "black", size = 0.5) + 
  scale_fill_gradientn(colors = colorRamps::blue2red(10)) +
  labs(x = "",
       y = "",
       fill = "SST anomaly\n(°C)",
       title = "Difference in SST",
       subtitle = bquote(SST[2015]-SST[2010-2013])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))

sst_anom_turfs <- sst_anom +
  geom_sf(data = turfs, color = "black", fill = "transparent")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = sst_anom,
                    filename = "sst_anom",
                    width = 15,
                    height = 15)

startR::lazy_ggsave(plot = sst_anom_turfs,
                    filename = "sst_anom_turfs",
                    width = 15,
                    height = 15)
