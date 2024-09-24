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
            lat = min(lat),
            .groups = "drop") %>% 
  mutate(eu_rnpa = str_replace_all(eu_rnpa, "0", "*"),
         eu_rnpa = str_replace_all(eu_rnpa, "8", "°"),
         eu_rnpa = fct_reorder(eu_rnpa, lat)) %>% 
  ggplot() +
  geom_tile(aes(x = year, y = eu_rnpa, fill = mhw_int_cumulative)) +
  geom_text(x = 1985,
            y = 25,
            label = "'82-'83 El Niño",
            color = "white",
            inherit.aes = F) +
  geom_text(x = 1992,
            y = 30,
            label = "'91-'92 El Niño",
            color = "white",
            inherit.aes = F) +
  geom_text(x = 1998,
            y = 35,
            label = "'97-'98 El Niño",
            color = "white",
            inherit.aes = F) +
  geom_text(x = 2010,
            y = 39,
            label = "2014-2016 Blob +
            El Niño",
            color = "white",
            inherit.aes = F) +
  scale_x_continuous(expand = c(0, 0)) +
  # scale_fill_gradientn(colours = wesanderson::wes_palette(name = "Zissou1",
                                                          # type = "continuous")) +
  scale_fill_gradientn(colours = rev(ipcc_temp)) +
  guides(fill = guide_colorbar(title = "Cumulative Intensity (°C days)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "|___________________________ Before ___________________________| During |__ After _|",
       y = "South _______________________________________________________ North     ")+
  theme(legend.position = "bottom",
        legend.box.spacing = unit(0.1, "mm"),
        axis.title.x = element_text(hjust = 1)) +
  scale_y_discrete(expand = c(0, 0))

# Get ranges to make sure the colorbar matches
period_boxplot <- mhw %>% 
  group_by(year, eu_rnpa) %>% 
  summarize(mhw_int_cumulative = weighted.mean(x = mhw_int_cumulative,
                                               w = turf_area,
                                               na.rm = T),
            .groups = "drop") %>%
  mutate(when = case_when(between(year, 1981, 1983) ~ "''82-'83 El Niño",
                          between(year, 1991, 1992) ~ "'91-'92 El Niño",
                          between(year, 1997, 1998) ~ "'97-'98 El Niño",
                          between(year, 2014, 2016) ~ "'14-'16 Blob + El Niño",
                          T ~ "Other years"),
         when = fct_reorder(when, year, max)) %>% 
  group_by(when) %>% 
  mutate(mean_mhw_int_cumulative = median(mhw_int_cumulative)) %>% 
  ungroup() %>% 
  ggplot(aes(x = when, y = mhw_int_cumulative, fill = mean_mhw_int_cumulative)) +
  geom_boxplot(outlier.size = out_size,
               outlier.alpha = out_alpha) +
  stat_summary(geom = "point",
               fun = "mean",
               size = 2) +
  guides(fill = guide_colorbar(title = "Cumulative Intensity (°C days)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_fill_gradientn(colours = rev(ipcc_temp),
                       limits = c(0, 500)) +
  labs(y = "Cumulative Intensity (°C days)",
       x = "") +
  theme(legend.position = "inside",
        legend.position.inside = c(0, 1),
        legend.justification = c(0, 1))

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
  labs(y = "Cumulative Intensity (°C days)")

p1 <- plot_grid(
  hovmoller,
  period_boxplot,
  ncol = 1,
  labels = c("a", "b"),
  rel_heights = c(2, 1.2),
  align = "hv", axis = "x"
)

p2 <- plot_grid(
  mhw_events,
  mhw_days,
  mhw_int,
  mhw_int_cumulative,
  ncol = 1,
  labels = c("c", "d", "e", "f"), 
  align = "hv")

plot <- plot_grid(p1,
                  p2,
                  ncol = 2,
                  rel_widths = c(1, 0.5))

## EXPORT ######################################################################
startR::lazy_ggsave(plot = plot,
                    filename = "fig02_mhw_stats",
                    width = 21,
                    height = 21)


