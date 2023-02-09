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

# Load data --------------------------------------------------------------------
env_panel <-
  readRDS(file = here("data", "processed", "annual_environmental_panel.rds"))

turfs <- sf::st_read(
  dsn = file.path(mex_data_path,
                  "concesiones",
                  "processed",
                  "lobster_turf_polygons.gpkg"))

baja <- rnaturalearth::ne_countries(country = "Mexico",
                                    returnclass = "sf",
                                    scale = "large") %>% 
  sf::st_crop(turfs)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
centroids <- st_read(here("data", "processed", "centroids.gpkg")) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(lat = Y, lon = X)

vis_data <- env_panel %>% 
  left_join(centroids, by = c("fishery", "eu_rnpa")) %>% 
  mutate(coop_name = fct_reorder(eu_rnpa, lat))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

plot <- ggplot(data = vis_data,
               mapping = aes(x = year, y = eu_rnpa, fill = mhw_int_cumulative)) +
  geom_tile() +
  facet_wrap(~fishery, scales = "free") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(option = "B", limits = c(0, 1e3)) +
  guides(fill = guide_colorbar(title = "MHW Cum. Int.\n(°C x days)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "Year",
       y = NULL)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = plot,
                    filename = "hovmoller_mhw_int",
                    width = 24, height = 9)
