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
library(sf)
library(here)
library(rnaturalearth)
library(rmapshaper)
library(cowplot)
library(tidyverse)

# Load data --------------------------------------------------------------------
turfs <-
  sf::st_read(dsn = here(
    "data",
    "processed",
    "turf_polygons.gpkg"
  )) %>% 
  ms_simplify(keep_shapes = T)

corners <- st_bbox(turfs)

mex <-
  ne_countries(country = c("Mexico", "United States of America"),
               returnclass = "sf",
               scale = "large") %>% 
  select(sov_a3)

mex_low_res <- ne_countries(country = c("Mexico"),
                            returnclass = "sf",
                            scale = "small")

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
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

ref <- ggplot() +
  geom_sf(data = mex_low_res,
          fill = "black", color = "black") +
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
  theme_void() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 


map <- ggdraw() +
  draw_plot(main) +
  cowplot::draw_plot(
    ref,
    hjust = 0,
    vjust = 0,
    x = 0.15,
    y = -0.025,
    width = 0.4,
    height = 0.4
  )


startR::lazy_ggsave(
  plot = map,
  filename = "focus_study_area",
  width = 8,
  height = 10
)

