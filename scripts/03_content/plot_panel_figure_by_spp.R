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
library(magrittr)
library(tidyverse)

# Load data --------------------------------------------------------------------
data <- readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds")) %>% 
  filter(balanced) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

# Define a plotting function ---------------------------------------------------
make_panel <- function(data){
  panel <- ggplot(data = data,
                  mapping = aes(x = norm_mhw_int_cumulative, y = norm_landed_weight)) + 
    geom_smooth(method = "lm", se = T, fullrange = T, color = "black", linetype = "dashed", linewidth = 0.5) +
    geom_point(aes(fill = period_long), shape = 21, color = "black", size = 2) +
    facet_wrap(~eu_rnpa, ncol = 4, scales = "free_y") +
    labs(x = "Standard normalized Cum. Int. (°C X days)",
         y = "Standard normalized landings",
         fill = "Period") +
    scale_fill_manual(values = period_palette) +
    theme(legend.position = c(1, -0.05),
          legend.justification = c(1, 0),
          legend.direction = "horizontal") +
    guides(fill = guide_legend(title.position = "top", ncol = 2))
  
  return(panel)
}

## VISUALIZE ###################################################################
# Call the function ------------------------------------------------------------
panel_plots <- data %>% 
  group_by(fishery) %>% 
  nest() %>% 
  mutate(plot = map(data, make_panel))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
panel_plots %$% 
  walk2(.x = fishery,
        .y = plot,
        .f = ~startR::lazy_ggsave(plot = .y,
                                  filename = paste0("panel_figure_",
                                                    str_to_lower(str_replace(.x, " ", "_"))),
                                  width = 8.7 * 1.5,
                                  height = 8.7 * 1.5))





