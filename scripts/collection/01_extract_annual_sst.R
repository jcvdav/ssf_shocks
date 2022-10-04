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
library(raster)
library(sf)
library(tidyverse)

fedecoop <- sf::st_read(dsn = file.path(mex_data_path, "fedecoop", "fedecoop_polygons.gpkg")) %>% 
  st_transform(crs = 4326)

sst <- list.files(
  path = here::here("../data_remotes/data/sst/processed_annual"),
  pattern = "tif",
  full.names = T) %>% 
  stack() %>% 
  raster::extract(y = fedecoop, df = T, fun = "mean", na.rm = T)


sst_ts <- fedecoop %>% 
  st_drop_geometry() %>% 
  left_join(sst, by = c("objectid" = "ID")) %>% 
  pivot_longer(cols = contains("erdMWsst"),
               names_to = "year",
               values_to = "mean_sst") %>% 
  mutate(year = as.numeric(str_extract(year, "[:digit:]+"))) %>% 
  select(coop, year, mean_sst)

ggplot(sst_ts, aes(x = year, y = mean_sst, group = coop)) +
  geom_rect(xmin = 2013.5, xmax = 2017.5, ymin = 15, ymax = 25, fill = "gray") +
  geom_line() +
  geom_point(aes(fill = mean_sst), size = 4, shape = 21, color = "black") +
  scale_fill_gradientn(colours = colorRamps::matlab.like(20)) +
  labs(x = "Year",
       y = "Mean annual SST (° C)",
       fill = "Mean annual SST (° C)") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  theme_bw() + 
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank())
    
  

