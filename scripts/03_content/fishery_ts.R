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
library(here)
library(cowplot)
library(tidyverse)

# # Define functions -------------------------------------------------------------
# ihs <- function(x){
#   log(x + sqrt((x ^ 2) + 1))
# }

# Load data --------------------------------------------------------------------
data <- readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds")) %>% 
  filter(balanced)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
landings_ts <- ggplot(data = data) +
  geom_rect(xmin= 2013.5, xmax = 2017.5, ymin = -100, ymax = 500, fill = "gray") +
  geom_line(aes(x = year,
                y = norm_landed_weight,
                group = coop_name)) +
  theme_bw() +
  labs(x = "Year",
       y = "Standardized\nlandings",
       color = "Cooperative") +
  theme(legend.position = "None") +
  guides(color = guide_legend(ncol = 2)) +
  scale_x_continuous(expand = c(0, 0))

value_ts <- ggplot(data = data) +
  geom_rect(xmin= 2013.5, xmax = 2017.5, ymin = -100, ymax = 500, fill = "gray") +
  geom_line(aes(x = year,
                y = norm_value,
                group = coop_name)) +
  theme_bw() +
  labs(x = "Year",
       y = "Standardized\nrevenues",
       color = "Cooperative") +
  theme(legend.position = "None") +
  guides(color = guide_legend(ncol = 2))


fishery_ts <- plot_grid(landings_ts, value_ts, ncol = 1)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(fishery_ts,
                    filename = "fishery_ts",
                    width = 9,
                    height = 7)



panel <- ggplot(data, aes(x = ihs(mhw_int_cumulative), y = landed_weight / 1e3)) + 
  geom_smooth(method = "lm", se = T, fullrange = T, color = "black", linetype = "dashed", size = 0.5) +
  geom_point(aes(fill = period_long), shape = 21, color = "black", size = 2) +
  facet_wrap(~coop_name, ncol = 4, scales = "free_y") +
  labs(x = "Log-transforemed Cum. Int. (°C X days)",
       y = "Landed weight (MT)",
       fill = "Period") +
  scale_fill_manual(values = period_palette) +
  theme_bw() +
  theme(legend.position = c(1, -0.05),
        legend.justification = c(1, 0),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 6)) +
  guides(fill = guide_legend(title.position = "top", ncol = 2))

startR::lazy_ggsave(panel,
                    filename = "panel_figure",
                    width = 8.7 * 1.5,
                    height = 8.7 * 1.5)


