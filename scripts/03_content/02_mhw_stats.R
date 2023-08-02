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
  cowplot,
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
mhw <- mhw_raw %>% 
  left_join(centroids, by = c("eu_rnpa", "fishery"))

# X ----------------------------------------------------------------------------
plot_data <- mhw %>% 
  filter(year <= 2016,
         mhw_events > 0)

## VISUALIZE ###################################################################

# Some visualization parameters ------------------------------------------------
out_size <- 0.5 # Size of outliers in boxplots
out_alpha <- 0.5 # Opacity of outliers in boxplots

update_geom_defaults(geom = "text", new = list(color = "gray10",
                                               size = 3))

# Build a pseudo-hovmoller diagram
hovmoller <- mhw %>% 
  group_by(year, eu_rnpa) %>% 
  summarize(mhw_int_cumulative = weighted.mean(x = mhw_int_cumulative,
                                               w = turf_area,
                                               na.rm = T),
            lat = min(lat)) %>% 
  ungroup() %>% 
  mutate(eu_rnpa = fct_reorder(eu_rnpa, lat)) %>% 
  ggplot() +
  geom_tile(aes(x = year, y = eu_rnpa, fill = mhw_int_cumulative)) +
  geom_text(x = 1985,
            y = 25,
            label = "'82-'83 El Niño",
            inherit.aes = F) +
  geom_text(x = 1992,
            y = 30,
            label = "'91-'92 El Niño",
            inherit.aes = F) +
  geom_text(x = 1998,
            y = 35,
            label = "'97-'98 El Niño",
            inherit.aes = F) +
  geom_text(x = 2014,
            y = 39,
            label = "MHW regime",
            inherit.aes = F) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette(name = "Zissou1",
                                                          type = "continuous"), na.value = "white") +
  guides(fill = guide_colorbar(title = "Cumulative Intensity (°C days)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "|___________________________________ Before _______________________________| During |_____ After _|",
       y = NULL)+
  theme(legend.position = "bottom",
        legend.box.spacing = unit(0.1, "mm"),
        axis.title.x = element_text(hjust = 1)) +
  scale_y_discrete(expand = c(0, 0))

hovmoller

# Build boxplots ---------------------------------------------------------------
base <- ggplot(data = plot_data,
       mapping = aes(x = period_long,
                     group = period_long,
                     fill = period_long)) +
  scale_fill_manual(values = period_palette) +
  theme(legend.position = "None") +
  labs(x = NULL,
       fill = "Period")

mhw_events <- base +
  geom_boxplot(aes(y = mhw_events),
               outlier.size = out_size,
               outlier.alpha = out_alpha) +
  labs(y = "Number of MHW")

mhw_days <- base +
  geom_boxplot(aes(y = mhw_days),
               outlier.size = out_size,
               outlier.alpha = out_alpha) +
  labs(y = "MHW Days")

mhw_int <- base +
  geom_boxplot(aes(y = mhw_int_mean),
               outlier.size = out_size,
               outlier.alpha = out_alpha) +
  labs(y = "Mean Intensity (°C)")

mhw_int_cumulative <- base +
  geom_boxplot(aes(y = mhw_int_cumulative),
               outlier.size = out_size,
               outlier.alpha = out_alpha) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) +
  labs(y = "Cumulative Intensity (°C days)")

plot <- plot_grid(
  hovmoller,
    plot_grid(
      mhw_events, mhw_days, mhw_int, mhw_int_cumulative,
      ncol = 2,
      labels = c("b", "c", "d", "e"), 
      label_x = 0.8),
  ncol = 1,
  labels = c("a", ""),
  label_x = 0.92,
  rel_heights = c(2, 1.1)
)

plot

## EXPORT ######################################################################
startR::lazy_ggsave(plot = plot,
                    filename = "02_mhw_stats",
                    width = 16,
                    height = 20)


