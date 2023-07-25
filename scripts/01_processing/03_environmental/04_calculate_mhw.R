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
  lubridate,
  heatwaveR,
  tidyverse
)

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

combine_hw_cs <- function(hw, cs) {
  net <- bind_rows(hw, cs) %>% 
    select(year, mhw_int_cumulative) %>% 
    group_by(year) %>% 
    summarize(net_mhw = sum(mhw_int_cumulative))
  
  full_join(hw, net, by = "year")
}

# Load data --------------------------------------------------------------------
daily_sst_ts <-
  readRDS(file = here("data", "processed", "daily_mean_sst_by_turf.rds"))

## PROCESSING ##################################################################

# Get HWs ----------------------------------------------------------------------
mhw <- daily_sst_ts %>%
  select(eu_rnpa, fishery, t, temp) %>%
  nest(data = c(t, temp)) %>%
  mutate(
    ts = map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2012-12-31")),
    ts2 = map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2012-12-31"), pctile = 10),
    mhw = map(ts, detect_event),
    cld = map(ts2, detect_event, coldSpells = T),
    summary = map(mhw, anual_intensity),
    summary2 = map(cld, anual_intensity),
    summary = map2(summary, summary2, combine_hw_cs)
  )

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = mhw,
        file = here("data", "processed", "mhw_by_turf.rds"))
