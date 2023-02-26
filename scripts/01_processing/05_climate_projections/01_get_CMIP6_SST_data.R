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
library(furrr)
library(tidyverse)

# Define a function to call ----------------------------------------------------
get_future_sst <- function(filename) {
  # Get year, month, and maximum number of years in data set -------------------
  # year <- str_sub(str_extract(filename, "-[:digit:]{8}"), start = 2, end = 5)
  # month <- str_sub(str_extract(filename, "-[:digit:]{8}"), start = 6, end = 7)
  # max_day <- as.numeric(str_remove(str_extract(filename, "[:digit:]{2}.nc"), "\\.nc"))
  
  # Build URL and filename -----------------------------------------------------
  ssp <- str_extract(filename, "ssp[:digit:]{3}")
  url <- paste0("https://esgf-data1.llnl.gov/thredds/dodsC/css03_data/CMIP6/ScenarioMIP/NOAA-GFDL/GFDL-ESM4/",
                ssp,
                "/r1i1p1f1/Oday/tos/gn/v20180701/",
                filename)
  file <- nc_open(filename = url)
  
  # Deal with coordinates ------------------------------------------------------
  # Get coordinates
  lon <- ncvar_get(file, "lon")
  lat <- ncvar_get(file, "lat")
  
  # Ge the indices for my bounding box
  lon_indices <- which(between(lon[,1], -119, -110)) %>% range()
  lat_indices <- which(between(lat[1,], 23, 33)) %>% range()
  
  # Find starting position and number of indices for each lat long
  lon_start <- lon_indices[1]
  lon_count <- (diff(lon_indices) + 1)
  lat_start <- lat_indices[1]
  lat_count <- (diff(lat_indices) + 1)
  
  # Buidl start and count vectors
  start <- c(lon_start, lat_start)
  count <- c(lon_count, lat_count)
  
  # Get the latitudes and longitudes for our area
  lons <- ncvar_get(nc = file,
                    varid = "lon",
                    start = start,
                    count = count)
  lats <- ncvar_get(nc = file,
                    varid = "lat",
                    start = start,
                    count = count)
  
  # Deal with dates ------------------------------------------------------------
  # Extract all difftimes, they all point to "days since 1850-01-01" and ignore
  # leap years, so we have to fix that
  dates <- ncvar_get(nc = file,
                     varid = "time")
  
  # Infer date start from filename
  date_start <- 
    ymd(
      str_sub(
        str_extract(
          string = filename,
          pattern = "_[:digit:]{8}-"),
        start = 2,
        end = 9)
    ) 
  
  all_dates <- tibble(index = 1:length(dates),
                      days_since_origin = dates) %>% 
    mutate(diftime = (dates - min(dates)),
           date = date_start + days(diftime),
           is_feb_29 = (day(date) == 29 & month(date) == 2),
           adds = cumsum(is_feb_29),
           fixed_date = date + days(adds)) %>% 
    filter(year(fixed_date) <= 2050)
  
  # Create a reference raster ----------------------------------------------------
  r_obj <- raster(xmn = -119, xmx = -110,
                  ymn = 23, ymx = 33,
                  resolution = c(0.5, 0.5),
                  crs = "EPSG:4326")
  
  # Iterate across days --------------------------------------------------------
  for(day in all_dates$index) {
    
    # Define output file name based on date
    out_file <- here("data", "raw", "climate_data", ssp,
                     paste0("tos_Oday_GFDL-ESM4_",
                            ssp,
                            all_dates$fixed_date[day], ".tif"))
    
    if(!dir.exists(here("data", "raw", "climate_data", ssp))) {
      dir.create(
        here("data", "raw", "climate_data", ssp)
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
      data <- tibble(x = as.vector(lons),
                     y  = as.vector(lats),
                     tos  = as.vector(tos)) %>% 
        drop_na()
      
      # Rasterize our SST file
      sst <- rasterize(x = data[, 1:2], 
                       y = r_obj, 
                       field = data[, 3])
      
      # And expor tit
      writeRaster(x = sst,
                  filename = out_file,
                  overwrite = T) 
    } # END IF
  } # END FOOR LOOP
  
  # Notify user
  print(paste0("Done with: ", filename))
  
} # END FUNCTION

file_names <- c("tos_Oday_GFDL-ESM4_ssp245_r1i1p1f1_gn_20150101-20241231.nc",
                "tos_Oday_GFDL-ESM4_ssp245_r1i1p1f1_gn_20250101-20341231.nc",
                "tos_Oday_GFDL-ESM4_ssp245_r1i1p1f1_gn_20350101-20441231.nc",
                "tos_Oday_GFDL-ESM4_ssp245_r1i1p1f1_gn_20450101-20541231.nc")

file_names %>%
  walk(get_future_sst)

# # Define all the file names we want (2025 - 250) -------------------------------
# 
# gr <- nc_open("http://esgdata.gfdl.noaa.gov/thredds/dodsC/gfdl_dataroot4/ScenarioMIP/NOAA-GFDL/GFDL-ESM4/ssp245/r1i1p1f1/Oday/tos/gn/v20180701/tos_Oday_GFDL-ESM4_ssp245_r1i1p1f1_gn_20150101-20241231.nc")
# 
# 
# gr_lon <- ncvar_get(gr, "lon")
# gr_lat <- ncvar_get(gr, "lat")
# tos <- ncvar_get(gr, "tos", start = c(1, 1, 1), count = c(720, 576, 1))
# 
# d <- tibble(gr_lon = as.vector(gr_lon),
#             gr_lat = as.vector(gr_lat),
#             tos = as.vector(tos)) %>% 
#   drop_na()
# 
# d %>% 
#   filter(between(gr_lat, 23, 32.75),
#          between(gr_lon, -119, -110)) %>% 
#   ggplot(aes(x = gr_lon, y = gr_lat, fill = tos)) +
#   geom_raster() +
#   coord_equal()
# 
# 
# r_obj
# 
# 
# r <- rasterize(x = d[ ,1:2],
#           y = r_obj,
#           field = d[ ,3])
# 
# 
# plot(r)
