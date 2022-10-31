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
library(lubridate)
library(heatwaveR)
library(tidyverse)

# Define processing funciton ---------------------------------------------------
anual_intensity <- function(data) {
  data$climatology %>%
    drop_na() %>%
    dplyr::select(-c(doy, thresh, threshCriterion, durationCriterion, event)) %>%
    mutate(year = year(t)) %>%
    mutate(intensity = temp - seas) %>%
    group_by(year) %>%
    summarise(
      mhw_events = length(unique(event_no)),
      mhw_days = length(t),
      mhw_int_cumulative = sum(intensity),
      mhw_int_mean = mean(intensity),
      mhw_int_max = max(intensity),
      .groups = "drop"
    )
}

# Load data --------------------------------------------------------------------
daily_sst_ts <-
  readRDS(file = here("data", "processed", "daily_mean_sst_by_turf.rds"))

## PROCESSING ##################################################################

# Get HWs ----------------------------------------------------------------------
mhw <- daily_sst_ts %>%
  select(coop_name, t, temp) %>%
  nest(data = c(t, temp)) %>%
  mutate(
    ts = map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2013-12-31")),
    mhw = map(ts, detect_event),
    summary = map(mhw, anual_intensity)
  )

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = mhw,
        file = here("data", "processed", "mhw_by_turf.rds"))
