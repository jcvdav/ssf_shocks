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
  corrplot,
  tidyverse
)

# Load data --------------------------------------------------------------------
env_panel <- readRDS(here("data", "processed", "annual_environmental_panel.rds"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
pdf(file = here("results", "img", "corrplot.pdf"), width = 5, height = 5)

env_panel %>% 
  select(temp_max, mhw_int_max, mhw_events, mhw_days, mhw_int_cumulative) %>% 
  cor() %>% 
  corrplot(method = "ellipse", 
           type = "lower",
           diag = F,
           addCoef.col = "black",
           tl.col = "black")

dev.off()
