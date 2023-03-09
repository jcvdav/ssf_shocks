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
CMIP_sst_ts <-
  readRDS(file = here("data", "processed", "ssp245_daily_mean_sst_by_turf.rds"))

## PROCESSING ##################################################################

# Get HWs ----------------------------------------------------------------------
mhw <- CMIP_sst_ts %>% # daily_sst_ts %>%
  select(eu_rnpa, fishery, t, temp) %>%
  nest(data = c(t, temp)) %>%
  head() %>% 
  mutate(
    ts = map(data, ts2clm, climatologyPeriod = c("2017-01-01", "2047-12-31")),
    mhw = map(ts, detect_event),
    summary = map(mhw, anual_intensity)
  )

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = mhw,
        file = here("data", "processed", "ssp245_mhw_by_turf.rds"))

