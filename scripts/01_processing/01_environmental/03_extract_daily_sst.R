## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(here)
library(raster)
library(sf)
library(exactextractr)
library(lubridate)
library(tidyverse)

# Load data --------------------------------------------------------------------
turfs <-
  sf::st_read(dsn = file.path(
    mex_data_path,
    "concesiones",
    "processed",
    "lobster_turf_polygons.gpkg"
  )) #%>% 
#   mutate(lat = st_centroid(.) %>%
#            st_coordinates() %>%
#            as.data.frame() %>%
#            pull(Y)) %>%
#   arrange(desc(lat))
# 
# top_turfs <- head(turfs, 10)
# bottom_turfs <- tail(turfs, 14)

# sst_names <- list.files(
#   path = here::here("data", "raw", "daily_sst", "rasters"),
#   pattern = "tif",
#   full.names = T
# )

my_files <- function(decade){
  list.files(
    path = here::here("data", "raw", "daily_sst", "rasters"),
    pattern = decade,
    full.names = T
  )
}

my_name <- function(stack, names){
  names(stack) <- names
  return(stack)
}

sst <- tibble(decade = c("198", "199", "200", "201", "202")) %>% 
  mutate(files = map(decade, my_files),
         name = map(files,
                    ~str_remove(basename(tools::file_path_sans_ext(.x)), "ncdcOisst21Agg_LonPM180_")),
         s = map(files, stack),
         s = map2(.x = s, .y = name, .f = my_name))

print("read all files")

## PROCESSING ##################################################################

# Define an extractinf function wraper -----------------------------------------
my_extract <- function(x, y) {
  exact_extract(x = x,
                y = y, 
                fun = "mean", 
                append_cols = c("coop_name", "eu_rnpa"),
                progress = T)
}

# Apply extracting function wraper to each decade's stack ----------------------
extracted <- sst %>% 
  mutate(sst = map(s, my_extract, y = turfs)) %>% 
  select(decade, files, sst)

print("Extracted")

# names <- map(sst$files,
#              ~str_remove(basename(tools::file_path_sans_ext(.x)), "ncdcOisst21Agg_LonPM180_")) %>% 
#   unlist()

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
  select(coop_name, eu_rnpa, t, temp)

print("unnested")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = extracted_long,
        file = here("data", "processed", "daily_mean_sst_by_turf.rds"))


