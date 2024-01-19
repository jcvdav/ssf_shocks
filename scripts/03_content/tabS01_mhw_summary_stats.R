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
  modelsummary,
  tidyverse
)
# Load data --------------------------------------------------------------------
mhw <- readRDS(file = here("data", "processed", "mhw_by_turf.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
# Median duration
event_info <- mhw %>%
  mutate(events = map(mhw, ~.x$event)) %>%
  select(eu_rnpa, events) %>%
  unnest(events)

# Build table 
datasummary((`Duration` = duration) +
              (`Mean intensity` = intensity_mean) +
              (`Maximun intensity` = intensity_max) +
              (`Cumulative intensity` = intensity_cumulative) ~ Mean + SD + Min + Max + Median,
            event_info,
            out = here("results", "tab", "tabS01_mhw_summary_stats.tex"),
            title = "\\label{tab:mhw_summary_stats}Summary statistics for all marine heatwave events experienced across all TURFs between 1982 and 2021",
            escape = T,
            threeparttable = T)
