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
centroids <- turfs %>% 
  st_centroid() %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  select(coop_name, eu_rnpa, lon = X, lat = Y)

vis_data <- env_panel %>% 
  left_join(centroids, by = c("coop_name", "eu_rnpa")) %>% 
  mutate(coop_name = fct_reorder(coop_name, lat))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

plot <- ggplot(data = vis_data,
               mapping = aes(x = year, y = coop_name, fill = mhw_int_cumulative)) +
  geom_tile() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(option = "B", limits = c(0, 1e3)) +
  guides(fill = guide_colorbar(title = "MHW Cum. Int.\n(°C x days)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "Year",
       y = NULL)

env_panel %>% 
  filter(year == 2015) %>% 
  filter(coop_name %in% coef_data$grp) %>% 
  inner_join(turfs, by = c("coop_name")) %>% 
  ggplot() + 
  geom_sf(aes(fill = mhw_int_cumulative, geometry = geom), color = "black", size = 0.1) +
  geom_sf(data = baja, color = "black", size = 0.1, fill = "gray50") +
  scale_fill_viridis_c(option = "B", limits = c(0, 1e3)) +
  scale_y_continuous(expand = c(0, 0.05)) +
  guides(fill = guide_colorbar(title = "MHW Cum. Int.\n(°C x days)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  theme_bw() +
  theme(legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_blank())

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = plot,
                    filename = "hovmoller_mhw_int",
                    width = 24, height = 9)
