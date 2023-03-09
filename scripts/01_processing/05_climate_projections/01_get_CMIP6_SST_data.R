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

# Based on Burgess et al., 202X
# Climate_change_scenarios_in_fisheries_and_aquatic_conservation_research
# The most likely: SSP234 SSP245 / RCP4.5
# Others are RCP26 / RCP60
# Avoid: /SSP5-8.5, SSP3-7.0

# Get data from:
# https://esgf-node.llnl.gov/search/cmip6/

# ssp245 : Future scenario with medium radiative forcing by the end of century.
# Following approximately RCP4.5 global forcing pathway but with new forcing
# based on SSP2. Concentration-driven

# I am using: CMIP6.ScenarioMIP.NOAA-GFDL.GFDL-ESM4.ssp245.r1i1p1f1.Oday.tos.gr

# ssp126 : Future scenario with low radiative forcing by the end of century.
# Following approximately RCP2.6 global forcing pathway but with new forcing
# based on SSP1. Concentration-driven. As a tier 2 option, this simulation
# should be extended to year 2300

# If we need to, we can use: CMIP6.ScenarioMIP.NOAA-GFDL.GFDL-ESM4.ssp126.r1i1p1f1.Oday.tos.gr

# ssp460 : Future scenario with medium radiative forcing by the end of century.
# Following approximately RCP6.0 global forcing pathway but with new forcing
# based on SSP4. Concentration-driven



## SET UP ######################################################################
# Load packages ----------------------------------------------------------------
library(here)
library(raster)
library(ncdf4)
library(lubridate)
library(magrittr)
library(furrr)
library(tidyverse)

# Define a function to call ----------------------------------------------------
get_future_sst <- function(filename,
                           main_dir = here("data", "raw", "std_climate_model_output")) {
  
  # Make sure directory exists
  if(!dir.exists(main_dir)) {
    dir.create(main_dir)
  }
  
  # Get year, month, and maximum number of years in data set -------------------
  # year <- str_sub(str_extract(filename, "-[:digit:]{8}"), start = 2, end = 5)
  # month <- str_sub(str_extract(filename, "-[:digit:]{8}"), start = 6, end = 7)
  # max_day <- as.numeric(str_remove(str_extract(filename, "[:digit:]{2}.nc"), "\\.nc"))
  
  # Get filename info ---------------------------------------------------------------
  # Model name
  model <- str_remove_all(string = tools::file_path_sans_ext(basename(filename)),
                          pattern = "tos_Oday_|_ssp[:digit:]{3}_.+")
  
  # Scenario
  ssp <- str_extract(filename, "ssp[:digit:]{3}")
  
  # Infer date start from filename
  date_start <- ymd(str_sub(str_extract(string = filename,
                                        pattern = "_[:digit:]{8}-"),
                            start = 2,
                            end = 9))
  
  file <- nc_open(filename = filename)
  
  # Deal with coordinates ------------------------------------------------------
  # Get coordinates
  lon <- ncvar_get(file, "longitude")
  lat <- ncvar_get(file, "latitude")
  
  centered <- any(lon < 0)
  mat <- length(dim(lon)) > 1
  
  
  # Ge the indices for my bounding box
  # This allows us to identify -180 to 180 or 0-360
  if(centered) {
    if(mat) {
      lon_indices <- which(between(lon[,1], -119, -110)) %>% range()
      lat_indices <- which(between(lat[1,], 23, 33)) %>% range()  
    } else {
      lon_indices <- which(between(lon, -119, -110)) %>% range()
      lat_indices <- which(between(lat, 23, 33)) %>% range()  
    }
  } else {
    if(mat) {
      lon_indices <- which(between(lon[,1], 241, 250)) %>% range()
      lat_indices <- which(between(lat[1,], 23, 33)) %>% range()
    }  else {
      lon_indices <- which(between(lon, 241, 250)) %>% range()
      lat_indices <- which(between(lat, 23, 33)) %>% range()
    }
  }
  
  # Find starting position and number of indices for each lat long
  lon_start <- lon_indices[1]
  lon_count <- (diff(lon_indices) + 1)
  lat_start <- lat_indices[1]
  lat_count <- (diff(lat_indices) + 1)
  
  # Build start and count vectors
  start <- c(lon_start, lat_start)
  count <- c(lon_count, lat_count)
  
  # Get the latitudes and longitudes for our area
  if(mat) {
    lons <- ncvar_get(nc = file,
                      varid = "longitude",
                      start = start,
                      count = count)
    lats <- ncvar_get(nc = file,
                      varid = "latitude",
                      start = start,
                      count = count)
  } else {
    lons <- ncvar_get(nc = file,
                      varid = "longitude",
                      start = lon_start,
                      count = lon_count)
    lats <- ncvar_get(nc = file,
                      varid = "latitude",
                      start = lat_start,
                      count = lat_count)
  }
  
  # Deal with dates ------------------------------------------------------------
  # Extract all difftimes, they all point to "days since 1850-01-01" and ignore
  # leap years, so we have to fix that
  dates <- ncvar_get(nc = file,
                     varid = "time")
  
  all_dates <- tibble(index = 1:length(dates),
                      days_since_origin = dates) %>% 
    mutate(diftime = (dates - min(dates)),
           date = date_start + days(diftime),
           is_feb_29 = (day(date) == 29 & month(date) == 2),
           adds = cumsum(is_feb_29),
           fixed_date = date + days(adds)) %>% 
    filter(year(fixed_date) <= 2050) # I only want to project out to 2050
  
  # Create a reference raster --------------------------------------------------
  if(mat) {
    res <- lon[,1] %>% sort() %>% diff() %>% unique() %>% head(1) %>% as.numeric()
  } else {
    res <- lon %>% sort() %>% diff() %>% unique() %>% head(1) %>% as.numeric()
  }
  
  if(centered) {
    reference_raster <- raster(xmn = -119, xmx = -110,
                               ymn = 23, ymx = 33,
                               resolution = res,
                               crs = "EPSG:4326")
  } else {
    reference_raster <- raster(xmn = 241, xmx = 250,
                               ymn = 23, ymx = 33,
                               resolution = res,
                               crs = "EPSG:4326")
  }
  
  
  out_dir <- here(main_dir, ssp, model)
  
  # Iterate across days --------------------------------------------------------
  for(day in all_dates$index) {
    
    # Define output file name based on date
    out_file <- here(out_dir,
                     paste0(all_dates$fixed_date[day], ".tif"))
    
    if(!dir.exists(here(out_dir))) {
      
      if(!dir.exists(here(main_dir, ssp))) {
        dir.create(
          here(main_dir, ssp)
        )}
      dir.create(
        out_dir
      )
    }
    
    # Check if file exists already ---------------------------------------------
    if(!file.exists(out_file)) {
      # Extract data
      tos <- ncvar_get(nc = file,
                       varid = "tos",
                       start = c(start, day),
                       count = c(count, 1))
      
      # Build a data.frame and remove land values
      if(mat) {
        data <- tibble(x = as.vector(lons),
                       y  = as.vector(lats),
                       tos  = as.vector(tos)) %>% 
          drop_na() 
      } else {
        data <- expand_grid(lat = as.vector(lats),
                            lon = as.vector(lons)) %>% 
          mutate(tos = as.vector(tos)) %>% 
          dplyr::select(lon, lat, tos) %>% 
          drop_na()
      }
      
      # Rasterize our SST file
      sst <- rasterize(x = data[, 1:2], 
                       y = reference_raster, 
                       field = data[, 3])
      
      if(!res == 0.25) {
        factors <- res / 0.25
        
        sst <- disaggregate(x = sst,
                            fact = factors,
                            method = "bilinear")
      }
      
      if(!centered) {
        sst <- rotate(sst)
      }
      
      # And export it
      writeRaster(x = sst,
                  filename = out_file) 
      
      # END IF
    }
    
    # END FOOR LOOP
  }
  
  nc_close(file)
  
  # Notify user
  print(paste0("Done with: ", filename))
}
# END FUNCTION

# Define URLS, models, and scenarios -------------------------------------------


safe_get_future_sst <- safely(get_future_sst)

# Get data ---------------------------------------------------------------------
files <- list.files(here::here("data", "raw", "climate_model_output"), recursive = T, pattern = "\\.nc$", full.names = T)

# Run inparallel
walk(files, safe_get_future_sst)

# DONE!
