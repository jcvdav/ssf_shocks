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

read_and_merge <- function(file, hist_sst){
  future <- readRDS(file) %>% 
    filter(year(t) >= 2022)
  
  complete <- bind_rows(hist_sst, future)
  
  return(complete)
}

# Load data --------------------------------------------------------------------
# Historical data to build climatology
climatologies <- readRDS(file = here("data", "processed", "mhw_by_turf.rds")) %>% 
  select(eu_rnpa, fishery, ts)

hist_sst <- readRDS(file = here("data", "processed", "daily_mean_sst_by_turf.rds"))

CMIP_sst_ts <- tibble(file = list.files(here("data", "processed", "CMIP_SST_projections"),
                                        recursive = T,
                                        full.names = T)) %>% 
  mutate(ssp = str_extract(file, "ssp[:digit:]{3}"),
         model = str_remove_all(file, ".+ssp[:digit:]{3}/|\\.rds")) %>% 
  mutate(data = map(file, read_and_merge, hist_sst = hist_sst)) %>% 
  unnest(data) %>% 
  select(model, ssp, eu_rnpa, fishery, t, temp) %>%
  nest(data = c(t, temp)) 

  

## PROCESSING ##################################################################

# Get HWs ----------------------------------------------------------------------
mhw <- CMIP_sst_ts %>% 
  arrange(eu_rnpa, ssp, fishery) %>% 
  mutate(
    ts = map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2012-12-31")),
    mhw = map(ts, detect_event),
    summary = map(mhw, anual_intensity)
  )

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = mhw,
        file = here("data", "processed", "future_mhw_by_turf.rds"))
