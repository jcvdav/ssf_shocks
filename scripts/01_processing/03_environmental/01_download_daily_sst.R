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
library(rerddap)
library(lubridate)
library(tidyverse)

# Define a function to download data -------------------------------------------
OISST_sub_dl <- function(time_df) {
  OISST_dat <- griddap(
    datasetx = "ncdcOisst21Agg_LonPM180",
    url = "https://coastwatch.pfeg.noaa.gov/erddap/",
    time = c(time_df$start, time_df$end),
    zlev = c(0, 0),
    latitude = c(23, 32.75),
    longitude = c(-119,-110),
    fields = "sst"
  )$data %>%
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
    dplyr::rename(t = time, temp = sst) %>%
    dplyr::select(longitude, latitude, t, temp) %>%
    na.omit()
}

# Load data --------------------------------------------------------------------
dl_years <- tibble(
  date_index = 1:5,
  start = ymd(c("1982-01-01",
                "1990-01-01", 
                "1998-01-01",
                "2006-01-01",
                "2014-01-01")),
  end = ymd(c("1989-12-31",
              "1997-12-31", 
              "2005-12-31",
              "2013-12-31",
              "2021-12-31")))

OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(longitude, latitude, t, temp)

saveRDS(object = OISST_data,
        file = here("data", "raw", "daily_sst", "ncdcOisst21Agg_LonPM180.rds"))



