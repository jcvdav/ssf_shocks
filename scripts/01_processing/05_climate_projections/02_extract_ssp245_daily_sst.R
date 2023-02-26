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
library(terra)
library(sf)
library(exactextractr)
library(lubridate)
library(tidyverse)

# Load data --------------------------------------------------------------------

# Polygons
turfs <- st_read(dsn = here("data",
                            "processed",
                            "turf_polygons.gpkg"))

my_files <- function(decade, path = here::here("data", "raw", "climate_data", "ssp245")){
  list.files(
    path = path,
    pattern = decade,
    full.names = T
  )
}

my_name <- function(stack, names){
  names(stack) <- names
  return(stack)
}

sst <- tibble(decade = c("201")) %>% #, "202", "203", "204", "205")) %>% 
  mutate(files = map(decade, my_files),
         name = map(files,
                    ~str_remove(basename(tools::file_path_sans_ext(.x)), "tos_Oday_GFDL-ESM4_ssp245")),
         s = map(files, terra::rast),
         s = map2(.x = s, .y = name, .f = my_name))


## PROCESSING ##################################################################

# Apply extracting function to each decade's stack -----------------------------
extracted <- sst %>% 
  mutate(sst = map(.x = s,
                   .f = ~exact_extract(x = .x,
                                       y = turfs, 
                                       fun = "mean", 
                                       append_cols = c("eu_rnpa", "fishery"),
                                       progress = T))) %>% 
  select(decade, files, sst)



# Define a pivot function wraper -----------------------------------------------
my_pivot <- function(data) {
  pivot_longer(data = data,
               cols = contains("mean"),
               names_to = "t",
               values_to = "temp") 
}

# Apply the wraper to each decade's df -----------------------------------------
extracted_long <- extracted %>%
  mutate(sst = map(sst, my_pivot)) %>%
  unnest(sst) %>%
  mutate(t = str_remove(t, "mean.X"),
         t = ymd(t)) %>% 
  select(eu_rnpa, fishery, t, temp)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = extracted_long,
        file = here("data", "processed", "ssp245_daily_mean_sst_by_turf.rds"))


