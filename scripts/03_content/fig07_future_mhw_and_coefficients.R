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
  rmapshaper,
  rnaturalearth,
  sf,
  tidyverse
)

# Load data --------------------------------------------------------------------
coef_data <- readRDS(file = here("data", "output", "effect_on_fishery_and_biophysical.rds"))
con_p_mhw_threshold_future <- readRDS(file = here("data", "output", "con_p_mhw_threshold_future.rds"))
turfs <- sf::st_read(dsn = here("data","processed","turf_polygons.gpkg")) %>% 
  st_centroid() %>% 
  st_transform("EPSG:6362")

corners <- st_bbox(turfs)

mex <- ne_countries(country = c("Mexico", "United States of America"),
                    returnclass = "sf",
                    scale = "large") %>% 
  select(sov_a3) %>% 
  st_transform("EPSG:6362")

mex_low_res <- ne_countries(country = c("Mexico"),
                            returnclass = "sf",
                            scale = "small") %>% 
  st_transform("EPSG:6362")

mex_crop <- st_crop(x = mex,
                    y = st_buffer(turfs, dist = 52e3))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
plot_data <- con_p_mhw_threshold_future %>% 
  left_join(coef_data, by = c("fishery", "eu_rnpa")) %>% 
  mutate(neg_neg = 1 * ((estimate < 0) & (mean >= 0.025)),
         ssp = str_remove(ssp, "delta_"),
         ssp = case_when(ssp == "ssp126" ~ "SSP1-2.6",
                         ssp == "ssp245" ~ "SSP2-4.5",
                         ssp == "ssp585" ~ "SSP5-8.5"))

# Numbers for text:
plot_data %>%
  group_by(ssp, fishery) %>%
  summarize(pct = sum(neg_neg) / length(neg_neg))

mhw_turfs <- turfs %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " "))) %>% 
  left_join(plot_data, by = c("fishery", "eu_rnpa"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

p1 <- ggplot(data = plot_data,
            aes(x = mean, y = estimate, fill = ssp, shape = fishery)) +
  geom_rect(xmin = 0.025, xmax = Inf, ymin = -Inf, ymax = 0, inherit.aes = F, fill = "gray", alpha = 0.05) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.025, linetype = "dashed") +
  geom_errorbarh(aes(xmin = mean - se, xmax = mean + se),
                 linewidth = 0.2, height = 0) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                linewidth = 0.2, width = 0) +
  geom_point(aes(alpha = neg_neg), size = 2) +
  scale_fill_manual(values = ssp_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_alpha_binned(range = c(0.25, 1)) +
  facet_grid(fishery ~ ssp) +
  labs(x = expression("P((Cum. Int. ">="hist Cum. Int.) | MHW Occurs)"),
       y = expression(hat(beta[i]))) +
  theme(legend.position = "None")

p2 <- plot_data %>%
  ungroup() %>% 
  group_by(fishery, ssp) %>%
  summarize(n = sum(neg_neg) / n()) %>%
  ggplot(aes(x = ssp, y = n, shape = fishery, fill = ssp)) +
  geom_line(aes(group = fishery)) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = ssp_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  theme(legend.position = "None") +
  labs(x = "SSP",
       y = "% of economic units")

# Maps
p3 <- ggplot() +
  geom_sf(data = mex_crop,
          color = "black") +
  geom_sf(
    data = mhw_turfs,
    mapping = aes(alpha = neg_neg,
                  fill = ssp,
                  shape = fishery),
    color = "black") +
  scale_fill_manual(values = ssp_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-117, -111, by = 2)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(24, 32, by = 2)) +
  guides(fill = guide_legend(title = "SSP",
                             override.aes = list(shape = 21,
                                                 size = 4)),
         shape = guide_legend(title = "Fishery",
                              override.aes = list(size = 4)),
         alpha = "none") +
  facet_grid(~ssp)

p <- plot_grid(p1, p2,
               ncol = 2,
               rel_widths = c(2.75, 1),
               labels = "auto")

pp <- plot_grid(p, p3,
                ncol = 1,
                rel_heights = c(2, 1.25),
                labels = c("", "c"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(
  plot = pp,
  filename = "fig07_future_mhw_and_coefficients",
  width = 18,
  height = 20
)

