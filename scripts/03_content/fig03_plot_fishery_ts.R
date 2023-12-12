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
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " "))) %>% 
  group_by(fishery, period_long) %>% 
  mutate(period_mean = mean(norm_live_weight),
         period_sd = sd(norm_live_weight))

total_data <- readRDS(file = here("data", "output", "total_annual_normalized_landigs.rds"))

# Get PCT change by period
pct_changes <- total_data %>%
  ungroup() %>%
  select(fishery, period, period_mean) %>%
  distinct() %>%
  pivot_wider(names_from = period, values_from = period_mean, names_prefix = "p") %>%
  mutate(pct1 = ((p1 - p0) / p0),
         pct2 = ((p2 - p0) / p0))

critter <- c("data/img/Lobster_90.png",
             "data/img/Sea cucumber_90.png",
             "data/img/Urchin.png")

## VISUALIZE ###################################################################

# Aggregate landings (panel a) -------------------------------------------------
total_landings_ts <- ggplot(data = total_data,
                            mapping = aes(x = year,
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
  geom_point(aes(fill = period_long,
                 size = live_weight / 1e3),
             color = "gray10") +
  scale_fill_manual(values = period_palette) +
  scale_color_manual(values = period_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_y_continuous(expand = expansion(mult = 0.2, add = 0)) +
  guides(fill = guide_legend(title = "Period",
                             title.position = "top",
                             title.hjust = 0.5,
                             override.aes = list(alpha = 0.75,
                                                 pch = "")),
         size = guide_legend(title = "Total landings (tons)",
                             title.position = "top",
                             title.hjust = 0.5),
         color = "none",
         shape = "none") +
  labs(x = "Year",
       y = "Landings\n(tons / # active economic units)",
       fill = "Period") +
  facet_wrap(~fishery, ncol = 1, scales = "free_y") +
  theme(legend.position = "bottom") +
  geom_text(data = pct_changes,
            mapping = aes(x = 2015.5,
                          y = 1.2 * (p0 / 1e3),
                          label = scales::percent(pct1)), size = 3)

# Landings by TURF (panel b) ---------------------------------------------------
landings_ts <- ggplot(data = data,
                      mapping = aes(x = year,
                                    y = norm_live_weight)) +
  geom_rect(xmin= 2013.5, xmax = 2016.5,
            ymin = -100, ymax = 500, fill = "gray", alpha = 0.5) +
  geom_ribbon(mapping = aes(x = year,
                            ymin = (period_mean - period_sd),
                            ymax = (period_mean + period_sd),
                            group = period_long,
                            fill = period_long),
              alpha = 0.25) +
  geom_line(aes(x = year,
                y = period_mean,
                group = period_long,
                color = period_long)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line(aes(group = eu_rnpa),
            linewidth = 0.1,
            color = "black") +
  scale_fill_manual(values = period_palette,
                    aesthetics = c("color", "fill")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Year",
       y = "Standardized\nlandings") +
  facet_wrap(~fishery, ncol = 1, scales = "free_y") +
  theme(legend.position = "None")


legend <- get_legend(total_landings_ts)

combined <- plot_grid(total_landings_ts  + theme(legend.position = "None"),
                       landings_ts,
               ncol = 2,
               align = "hv",
               labels = "auto")

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

p2 <- plot_grid(p, legend, ncol = 1, rel_heights = c(1, 0.1))


## EXPORT ######################################################################
# X ----------------------------------------------------------------------------
startR::lazy_ggsave(p2,
                    filename = "fig03_fishery_ts",
                    width = 18,
                    height = 11)











