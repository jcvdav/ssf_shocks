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
  tidyverse
)

# Load data --------------------------------------------------------------------

mhw_raw <- readRDS(file = here("data", "processed", "annual_environmental_panel.rds"))
centroids <- st_read(dsn = here("data", "processed", "centroids.gpkg")) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(lon = X,
         lat = Y)

## PROCESSING ##################################################################
# X ----------------------------------------------------------------------------
mhw <- mhw_raw %>% 
  left_join(centroids, by = c("eu_rnpa", "fishery"))

# X ----------------------------------------------------------------------------
lines <- tibble(
  intercept = 0,
  slope = c(1, 1.1, 1.5, 2),
  slope_legend = as.factor(slope)
) 

# X ----------------------------------------------------------------------------
plot_data <- mhw %>% 
  group_by(eu_rnpa, year, period, period_long, lat) %>% 
  summarize(mhw_int_cumulative = sum(mhw_int_cumulative, na.rm = T)) %>% 
  ungroup() %>% 
  filter(between(year, 1982, 1983) |
           between(year, 1991, 1992) |
           between(year, 1997, 1998) |
           period == 1) %>% 
  mutate(event = case_when(between(year, 1982, 1983) ~ "'82-83 El Nino",
                           between(year, 1991, 1992) ~ "'91-92 El Nino",
                           between(year, 1997, 1998) ~ "'97-98 El Nino",
                           T ~ "MHW regime")) %>% 
  group_by(eu_rnpa, period_long, lat) %>% 
  summarize(mhw_int_cumulative = sum(mhw_int_cumulative, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = period_long, values_from = mhw_int_cumulative) %>% 
  janitor::clean_names()

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
mhw_vs_enso <- ggplot(data = plot_data,
                      mapping = aes(x = before_mhw, y = during_mhw, fill = lat)) +
  geom_abline(data = lines,
              aes(intercept = intercept,
                  slope = slope,
                  color = slope_legend),
              linetype = "dashed") +
  geom_point(shape = 21,
             size = 4,
             alpha = 21) + 
  coord_equal() +
  scale_fill_viridis_c() +
  scale_color_manual(values = c("gray", "gray75", "gray25", "black")) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  lims(x = c(750, 4500), y = c(750, 4500)) +
  labs(x = "Total cumulative MHW intensity (°C days) across 3 El Nino events (6 yrs)",
       y = "Total cumulative MHW intensity (°C days) across the MHW regime (3 yrs)",
       fill = "Latitude (°N)",
       color = "Slope") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = mhw_vs_enso,
                    filename = "figx_mhw_vs_enso",
                    width = 15,
                    height = 15)
