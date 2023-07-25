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
  MBC,
  heatwaveR,
  furrr,
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

# read_and_merge <- function(file, hist_sst){
#   predicted <- readRDS(file)
#   
#   rmse <- hist_sst %>% 
#     rename(obs = temp) %>% 
#     inner_join(predicted, by = c("eu_rnpa", "fishery", "t")) %>% 
#     group_by(eu_rnpa, fishery) %>% 
#     summarize(rmse = sqrt(mean((obs - temp)^2, na.rm = TRUE)),
#               .groups = "drop") %>% 
#     ungroup()
#   
#   future <- predicted %>% 
#     filter(year(t) >= 2022)
#   
#   complete <- bind_rows(hist_sst, future) %>% 
#     left_join(rmse, by = c("eu_rnpa", "fishery"))
#   
#   return(complete)
# }


# Load data --------------------------------------------------------------------
# Historical data to build climatology
# climatologies <- readRDS(file = here("data", "processed", "mhw_by_turf.rds")) %>% 
#   select(eu_rnpa, fishery, ts)

hist_sst <- readRDS(file = here("data", "processed", "daily_mean_sst_by_turf.rds")) %>% 
  rename(obs_temp = temp) %>% 
  nest(data = c(t, obs_temp)) %>% 
  rename(obs = data)

plan(multisession, workers = 10)

CMIP_data <- tibble(file = list.files(here("data", "processed", "CMIP_SST_projections"),
                                        recursive = T,
                                        full.names = T)) %>% 
  mutate(ssp = str_extract(file, "ssp[:digit:]{3}|historical"),
         model = str_remove_all(file, ".+ssp[:digit:]{3}/|.+historical/|\\.rds")) %>% 
  mutate(data = future_map(file, readRDS)) %>% 
  unnest(data) %>% 
  select(model, ssp, eu_rnpa, fishery, t, temp) %>%
  nest(data = c(t, temp))

# Split into historical model output and SSP model output
CMIP_historicals <- CMIP_data %>% 
  filter(str_detect(ssp, "historical")) %>% 
  select(-ssp) %>% 
  rename(hist_pred = data)

CMIP_projections <- CMIP_data %>% 
  filter(str_detect(ssp, "ssp")) %>% 
  rename(pred = data)

# Now put side by side
CMIP_and_hist_sst_ts <- CMIP_projections %>% 
  left_join(CMIP_historicals, by = c("model", "eu_rnpa", "fishery")) %>% 
  left_join(hist_sst, by = c("eu_rnpa", "fishery"))

# Free up some memory space
rm(CMIP_data, CMIP_historicals, CMIP_projections)


## PROCESSING ##################################################################

# Perform bias-correction ------------------------------------------------------
# Define a function to wrap around the Quantile Delta Mapping method
qdm_wrap <- function(obs, his_pred, pred) {
  
  model_data <- bind_rows(his_pred, pred)
  
  data <- model_data %>% 
    rename(mod_temp = temp) %>% 
    left_join(obs, by = "t") %>% 
    filter(!(is.na(obs_temp) & t <= ymd("2021-12-31")))
  
  calibration_indices <- between(data$t, ymd("1982-01-01"), ymd("2021-12-31"))
  
  res <- QDM(o.c = data$obs_temp[calibration_indices],
             m.c = data$mod_temp[calibration_indices],
             m.p = data$mod_temp[!calibration_indices])
  
  data <- data %>% 
    mutate(temp = c(res[[1]], res[[2]]))
  
  return(data)
}

bias_corrected_CMIP <- CMIP_and_hist_sst_ts %>% 
  mutate(data = future_pmap(list(obs, hist_pred, pred), qdm_wrap)) %>% 
  select(model, ssp, eu_rnpa, fishery, data) 

# Get HWs ----------------------------------------------------------------------
mhw <- bias_corrected_CMIP %>% 
  mutate(
    ts = future_map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2015-12-31"),
                    .options = furrr_options(seed = 1)),
    mhw = future_map(ts, detect_event,
                     .options = furrr_options(seed = 1)),
    summary = future_map(mhw, anual_intensity)
  )

plan(sequential)
## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = mhw,
        file = here("data", "processed", "future_mhw_by_turf.rds"))



