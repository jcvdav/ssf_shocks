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

# # Define functions -------------------------------------------------------------
# ihs <- function(x){
#   log(x + sqrt((x ^ 2) + 1))
# }

# Load data --------------------------------------------------------------------
data <- readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

critter <- c("data/img/Lobster_90.png",
             "data/img/Sea cucumber_90.png",
             "data/img/Urchin.png")

total_data <- data %>%
  group_by(period, period_long, year, fishery) %>%
  summarize(live_weight = sum(live_weight, na.rm = T),
            n = n_distinct(eu_rnpa),
            norm_live_weight = live_weight / n) %>%
  group_by(period, period_long, fishery) %>% 
  mutate(period_mean = mean(norm_live_weight),
         period_sd = sd(norm_live_weight))

period_data <- data %>%
  group_by(period, period_long, year, fishery) %>%
  summarize(live_weight = sum(live_weight, na.rm = T),
            n = n_distinct(eu_rnpa),
            norm_live_weight = live_weight / n) %>%
  group_by(period, period_long, fishery) %>% 
  summarize(period_mean = mean(norm_live_weight),
            period_sd = sd(norm_live_weight)) 

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
    scale_x_continuous(expand = c(0, 0)) +
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

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(p,
                    filename = "04_fishery_ts",
                    width = 12,
                    height = 10)

period_bars <- ggplot(data = period_data,
                      aes(x = period_long,
                          y = period_mean,
                          fill = period_long)) +
  geom_col(color = "gray10") +
  geom_errorbar(aes(ymin = period_mean - period_sd,
                     ymax = period_mean + period_sd),
                 width = 0) +
  facet_wrap(~fishery, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = period_palette) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Period",
       y = "Standardized landings\n(Landed wieght / active economic units)",
       fill = "Period") +
  theme(legend.position = "None")

library(car)

# bartlett.test(norm_live_weight ~ period_long, data = total_data %>% filter(fishery == "Sea cucumber"))

linear_mod <- lm(norm_live_weight ~ period_long, data = total_data %>% filter(fishery == "Urchin"),
                 contrasts=list(period_long=contr.sum))
Anova(mod = linear_mod,
      type = 3)
TukeyHSD(aov(linear_mod))
## EXPORT ######################################################################










