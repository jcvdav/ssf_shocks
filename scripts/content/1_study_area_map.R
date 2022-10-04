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
library(sf)
library(rnaturalearth)
library(cowplot)
library(tidyverse)


fedecoop <- sf::st_read(dsn = file.path(mex_data_path, "fedecoop", "fedecoop_polygons.gpkg")) %>% 
  st_transform(crs = 4326)

corners <- st_bbox(fedecoop)

mex <- ne_countries(country = "Mexico", returnclass = "sf", scale = "large")

mex_crop <- st_crop(
  x = mex,
  y = st_buffer(fedecoop, dist = 1e5)
)

main <- ggplot() +
  geom_sf(data = fedecoop, fill = "steelblue", alpha = 0.5) +
  geom_sf(data = mex_crop) +
  theme_minimal()

ref <- ggplot() +
  geom_sf(data = mex, fill = "black", color = "black") +
  geom_rect(aes(xmin = corners[1],
                xmax = corners[3],
                ymin = corners[2],
                ymax = corners[4]),
            fill = "transparent",
            color = "red") +
  cowplot::theme_map()


map <- ggdraw() +
  draw_plot(main) +
  cowplot::draw_plot(ref, x = 0.1, y = 0, width = 0.4, height = 0.4)


ggsave(
  plot = map,
  filename = here::here("results", "img", "focus_study_area.pdf"),
  width = 3,
  height = 4)
