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


turfs <-
  sf::st_read(dsn = file.path(
    mex_data_path,
    "concesiones",
    "processed",
    "lobster_turf_polygons.gpkg"
  ))

corners <- st_bbox(turfs)

mex <-
  ne_countries(country = "Mexico",
               returnclass = "sf",
               scale = "large")

mex_crop <- st_crop(x = mex,
                    y = st_buffer(turfs, dist = 1e5))

main <- ggplot() +
  geom_sf(
    data = turfs,
    color = "black",
    fill = "steelblue",
    alpha = 0.5
  ) +
  geom_sf(data = mex_crop,
          color = "black") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

ref <- ggplot() +
  geom_sf(data = mex, fill = "black", color = "black") +
  geom_rect(
    aes(
      xmin = corners[1],
      xmax = corners[3],
      ymin = corners[2],
      ymax = corners[4]
    ),
    fill = "transparent",
    color = "red"
  ) +
  cowplot::theme_map()


map <- ggdraw() +
  draw_plot(main) +
  cowplot::draw_plot(
    ref,
    x = 0.1,
    y = 0,
    width = 0.4,
    height = 0.4
  )


startR::lazy_ggsave(
  plot = map,
  filename = "focus_study_area",
  width = 7,
  height = 8
)

