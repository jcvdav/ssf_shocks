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
  tidyverse
)

# Load data --------------------------------------------------------------------
data <- readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

total_data <- readRDS(file = here("data", "output", "total_annual_normalized_landigs.rds"))

critter <- c("data/img/Lobster_90.png",
             "data/img/Sea cucumber_90.png",
             "data/img/Urchin.png")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
landings_ts <- ggplot(data = data,
                      mapping = aes(x = year,
                                    y = norm_live_weight)) +
  geom_rect(xmin= 2013.5, xmax = 2016.5,
            ymin = -100, ymax = 500, fill = "gray", alpha = 0.5) +
  geom_line(aes(group = eu_rnpa),
            linewidth = 0.2,
            color = "#016895") +
  labs(x = "Year",
       y = "Standardized\nlandings",
       color = "Cooperative") +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~fishery, ncol = 1, scales = "free_y")

total_landings_ts <- ggplot(data = total_data,
                            aes(x = year,
                                y = norm_live_weight / 1e3,
                                shape = fishery)) +
  geom_rect(xmin= 2013.5, xmax = 2016.5,
            ymin = 0, ymax = Inf, fill = "gray", alpha = 0.5) +
  geom_ribbon(aes(x = year,
                  ymin = (period_mean - period_sd) / 1e3,
                  ymax = (period_mean + period_sd) / 1e3,
                  group = period_long,
                  fill = period_long),
              alpha = 0.25) +
  geom_line(aes(x = year,
                y = period_mean / 1e3,
                group = period_long,
                color = period_long)) +
  geom_line(color = "gray10") +
  geom_point(aes(fill = period_long),
             color = "gray10") +
  scale_fill_manual(values = period_palette) +
  scale_color_manual(values = period_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  facet_wrap(~fishery, ncol = 1, scales = "free_y") +
  labs(x = "Year",
       y = "Landings\n(tons / active economic units)",
       fill = "Period") +
  theme(legend.position = "bottom")

combined <- plot_grid(total_landings_ts +
                         theme(legend.position = "None"),
                       landings_ts,
               ncol = 2,
               labels = "auto",
               label_x = 0.85)

scale <- 0.075
halign <- 0.45

p <- ggdraw() +
  draw_plot(combined) +
  draw_image(critter[1],
             scale = scale,
             halign = halign,
             valign = 0.92) +
  draw_image(critter[2],
             scale = scale,
             halign = halign,
             valign = 0.6) +
  draw_image(critter[3],
             scale = scale,
             halign = halign,
             valign = 0.27)


## EXPORT ######################################################################
# X ----------------------------------------------------------------------------
startR::lazy_ggsave(p,
                    filename = "04_fishery_ts",
                    width = 12,
                    height = 10)











