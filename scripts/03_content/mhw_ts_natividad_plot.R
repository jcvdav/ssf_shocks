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

# Load data --------------------------------------------------------------------
mhw <- readRDS(file = here("data", "processed", "mhw_by_turf.rds"))

natividad <- mhw %>% 
  filter(coop_name == "Buzos y Pescadores")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
map(mhw$mhw, lolli_plot, metric = "intensity_cumulative") %>% 
  plot_grid(plotlist = .)

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