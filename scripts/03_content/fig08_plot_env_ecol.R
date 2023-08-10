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
  ggimage,
  cowplot,
  heatwaveR,
  tidyverse
)

# Load data --------------------------------------------------------------------
mhw_data <- readRDS(here("data", "processed", "mhw_by_turf.rds")) %>% 
  filter(eu_rnpa == "0301000089",
         fishery == "lobster") 

data <- readRDS(here("data", "estimation_panels", "env_eco_panel.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace_all(fishery, "_", " "))) %>% 
  group_by(period, fishery) %>% 
  mutate(mean_density = mean(density),
         sd_density = sd(density)) %>% 
  ungroup()

img <- tibble(fishery = sort(unique(data$fishery)),
              img = here("data", "img", paste0(fishery, ".png")))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################

mhw_ts_long <- ggplot(data = mhw_data$mhw[[1]]$event,
                      mapping = aes(x = date_start,
                                    y = intensity_cumulative,
                                    fill = intensity_max,
                                    color = intensity_max)) +
  geom_rect(xmin = lubridate::ym("2014-01"), xmax = lubridate::ym("2016-12"),
            ymin = -Inf, ymax = Inf, fill = "gray", color = "transparent") +
  geom_vline(xintercept = c(lubridate::ymd(c("2006-01-01", "2021-12-31"))),
             color = "gray10",
             linetype = "dashed") +
  geom_linerange(aes(ymin = 0, ymax = intensity_cumulative)) +
  geom_point(shape = 21, size = 3, color = "gray10") +
  geom_text(x = lubridate::ym("1992-01"),
            y = 350,
            label = "'91-'92 El Niño",
            color = "gray10",
            inherit.aes = F) +
  geom_text(x = lubridate::ym("1998-01"),
            y = 550,
            label = "'97-'98 El Niño",
            color = "gray10",
            inherit.aes = F) +
  scale_fill_gradientn(colours = wesanderson::wes_palette(name = "Zissou1",
                                                          type = "continuous")) +
  scale_color_gradientn(colours = wesanderson::wes_palette(name = "Zissou1",
                                                           type = "continuous")) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black", barwidth = 0.5)) +
  labs(x = "Year",
       y = expression(paste("Cumulative intensity (", degree, "C days)")),
       fill = expression(paste("Maximum intensity (", degree, "C)")),
       color = expression(paste("Maximum intensity (", degree, "C)"))) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))

eco_ts <- ggplot(data = data, aes(shape = fishery)) +
  geom_rect(xmin= 2013.5, xmax = 2016.5, ymin = -Inf, ymax = Inf, fill = "gray", inherit.aes = F) +
  geom_ribbon(aes(x = year,
                  ymin = mean_density - sd_density,
                  ymax = mean_density + sd_density,
                  fill = period_long,
                  group = period_long),
              alpha = 0.25) +
  geom_line(aes(x = year,
                y = mean_density,
                color = period_long,
                group = period_long),
            linewidth = 0.5,
            linetype = "dashed") +
  stat_summary(mapping = aes(x = year, y = density),
               geom = "line",
               fun = mean,
               color = "gray10") +
  stat_summary(mapping = aes(x = year, y = density,
                             fill = period_long),
               geom = "pointrange",
               fun.data = mean_se,
               color = "gray10") +
  scale_x_continuous(breaks = seq(2006, 2020, by = 2),
                     labels = seq(2006, 2020, by = 2)) +
  scale_fill_manual(values = period_palette) +
  scale_color_manual(values = period_palette,
                     guide = "none") +
  scale_shape_manual(values = c(21, 22, 24)) +
  guides(shape = "none",
         color = "none",
         fill = guide_legend(override.aes = list(shape = 21))) +
  facet_wrap(~fishery, ncol = 1, scales = "free_y") +
  theme(legend.position = c(1, 0.3),
        legend.justification = c(1, 1)) +
  labs(x = "Year",
       y = bquote(Density~(org.~m^-1)),
       fill = "Period")


c_mhw <- ggplot(data = data,
                mapping = aes(x = mhw_int_cumulative, y = density, shape = fishery)) +
  geom_image(data = img,
             mapping = aes(image = img),
             x = 800,
             y = c(0.25, 0.120, 0.17), 
             inherit.aes = F,
             size = 0.2, by = "height") +
  geom_smooth(method = "lm",
              color = "gray10",
              linetype = "dashed",
              fill = "gray",
              alpha = 1,
              linewidth = 0.5) +
  stat_summary(aes(fill = period_long, group = year),
               geom = "pointrange",
               fun.data = mean_se,
               color = "gray10") +
  scale_fill_manual(values = period_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  labs(x = expression(paste("Cumulative intensity (", degree, "C days)")),
       y = bquote(Density~(org.~m^-1))) +
  facet_wrap(~fishery, ncol = 1, scales = "free_y") +
  theme(legend.position = "None",
        strip.text = element_text(hjust = 0))


# X ----------------------------------------------------------------------------
p <- plot_grid(mhw_ts_long,
               plot_grid(eco_ts, c_mhw, ncol = 2, labels = c("b", "c")),
               ncol = 1, rel_heights = c(1, 2.5),
               labels = c("a", ""))

## EXPORT ######################################################################
startR::lazy_ggsave(plot = p,
                    filename = "fig08_env_ecol",
                    width = 15,
                    height = 20)
