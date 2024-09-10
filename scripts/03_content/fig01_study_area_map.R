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
pacman::p_load(
  here,
  sf,
  rnaturalearth,
  rmapshaper,
  cowplot,
  tidyverse
)

# Load data --------------------------------------------------------------------
mhw <- readRDS(file = here("data", "processed", "annual_environmental_panel.rds")) %>% 
  group_by(eu_rnpa, fishery) %>% 
  summarize(mhw = max(mhw_int_cumulative))

turfs <-
  sf::st_read(dsn = here(
    "data",
    "processed",
    "turf_polygons.gpkg"
  )) %>% 
  ms_simplify(keep_shapes = T) %>% 
  st_transform("EPSG:6362") %>% 
  left_join(mhw, by = c("eu_rnpa", "fishery"))

corners <- st_bbox(turfs)

mex <-
  ne_countries(country = c("Mexico", "United States of America"),
               returnclass = "sf",
               scale = "large") %>% 
  select(sov_a3) %>% 
  st_transform("EPSG:6362")

mex_low_res <- ne_countries(country = c("Mexico"),
                            returnclass = "sf",
                            scale = "small") %>% 
  st_transform("EPSG:6362")

mex_crop <- st_crop(x = mex,
                    y = st_buffer(turfs, dist = 1e5))

main <- ggplot() +
  geom_sf(
    data = turfs,
    aes(fill = mhw),
    color = "black"
  ) +
  geom_sf(
    data = mex_crop,
    fill = "gray",
    color = "black") +
  guides(
    fill = guide_colorbar(
      title = "Cumulative Intensity (°C days)",
      title.position = "top",
      barwidth = 10,
      frame.colour = "black",
      ticks.colour = "black")) +
  scale_fill_gradientn(colours = rev(ipcc_temp)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = -117:-112) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.direction = "horizontal")

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


map <- ggdraw(main) +
  cowplot::draw_plot(
    ref,
    hjust = 1,
    vjust = 1,
    x = 0.95,
    y = 1.08,
    width = 0.4,
    height = 0.4
  )


startR::lazy_ggsave(
  plot = map,
  filename = "fig01_study_area_map",
  width = 9,
  height = 14
)

