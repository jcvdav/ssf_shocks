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
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
landings_ts <- ggplot(data = data,
                      mapping = aes(x = year,
                                    y = norm_landed_weight)) +
  geom_rect(xmin= 2013.5, xmax = 2017.5,
            ymin = -100, ymax = 500, fill = "gray", alpha = 0.5) +
  geom_line(aes(group = eu_rnpa),
            linewidth = 0.2,
            color = "cadetblue") +
  labs(x = "Year",
       y = "Standardized\nlandings",
       color = "Cooperative") +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~fishery, ncol = 1, scales = "free_y")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(landings_ts,
                    filename = "fishery_ts",
                    width = 12,
                    height = 10)








