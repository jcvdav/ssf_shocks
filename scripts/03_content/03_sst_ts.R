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
  tidyverse
)

# Load data --------------------------------------------------------------------

sst_ts <- readRDS(file = here("data", "processed", "annual_mean_sst_by_turf.rds"))


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
sst_ts_plot <- ggplot() +
  geom_rect(aes(xmin = 2013.5, xmax = 2017.5, ymin = 15, ymax = 25), fill = "gray") +
  geom_line(data = sst_ts, aes(x = year, y = mean_sst, color = coop_name)) +
  scale_color_viridis_d() +
  labs(x = "Year",
       y = "Mean annual\nSST (° C)",
       title = "Mean SST for 24 lobster TURFs along the Baja California Peninsula") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  theme_bw() + 
  theme(legend.position = "None")


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

startR::lazy_ggsave(plot = sst_ts_plot,
                    filename = "sst_ts",
                    width = 9,
                    height = 3.5)
