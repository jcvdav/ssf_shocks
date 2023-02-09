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
library(heatwaveR)
library(magrittr)
library(tidyverse)

# Load data --------------------------------------------------------------------
mhw <- readRDS(file = here("data", "processed", "mhw_by_turf.rds"))


my_plot <- function(data){
  ggplot(data$event, aes(x = date_peak, y = intensity_cumulative, ymax = intensity_cumulative)) +
    geom_linerange(aes(ymin = 0), linewidth = 0.5) +
    geom_point(size = 1) +  
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid = element_blank())
}

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
mhw %>%
  filter(fishery == "lobster") %$% 
  map(mhw, my_plot) %>% 
  plot_grid(plotlist = ., ncol = 5)

# X ----------------------------------------------------------------------------

mhw_plot <- function(data){
  heatwaveR::event_line(data, start_date = "2012-01-01",
                        end_date = "2021-12-31",
                        spread = 365 * 2) +
    guides(color = "none")
}

plot_grid(
  lolli_plot(natividad$mhw[[1]], metric = "intensity_cumulative") + labs(x = "", y = "Cum. Int. [°C x days]"),
  lolli_plot(natividad$mhw[[1]], metric = "duration") + labs(x = ""),
  mhw_plot(natividad$mhw[[1]]),
  ncol = 1
)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------