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
library(sf)
library(terra)
library(exactextractr)
library(magrittr)
library(lubridate)
library(tidyverse)

# Load data --------------------------------------------------------------------

# Polygons
turfs <- st_read(dsn = here("data",
                            "processed",
                            "turf_polygons.gpkg"))

my_files <- function(ssp, model, decade){
  list.files(
    path = here("data", "raw", "std_climate_model_output", ssp, model),
    pattern = paste0(decade, ".+\\.tif$"),
    full.names = T
  )
}

my_name <- function(stack, names){
  names(stack) <- names
  return(stack)
}


dirs <- list.files(path = here::here("data", "raw", "std_climate_model_output"),
                   recursive = T) %>% 
  dirname() %>% 
  unique()

sst_metadata <- tibble(dir = dirs) %>% 
  mutate(ssp = str_extract(dir, pattern = "ssp[:digit:]{3}"),
         model = str_remove(dir, "ssp[:digit:]{3}/")) %>% 
  select(ssp, model) %>% 
  distinct() %>% 
  expand_grid(decade = c("201", "202", "203", "204", "205")) %>% 
  mutate(files = pmap(list(ssp, model, decade), my_files),
         name = map(files,
                    ~basename(tools::file_path_sans_ext(.x))))

print("Metadata done")

sst <- sst_metadata %>% 
  mutate(s = map(files, rast),
         s = map2(.x = s, .y = name, .f = my_name)) %>% 
  select(ssp, model, decade, s)

print("Loading done")
## PROCESSING ##################################################################
# Apply extracting function to each decade's stack -----------------------------
extracted <- sst %>% 
  mutate(sst = map(.x = s,
                   .f = ~exact_extract(x = .x,
                                       y = turfs, 
                                       fun = "mean", 
                                       append_cols = c("eu_rnpa", "fishery"),
                                       progress = T))) %>% 
  select(ssp, model, decade, sst)

print("Extraction done")

# Define a pivot function wraper -----------------------------------------------
my_pivot <- function(data) {
  pivot_longer(data = data,
               cols = contains("mean"),
               names_to = "t",
               values_to = "temp") %>% 
    mutate(t = str_remove(t, "mean.X"),
           t = ymd(t))
}

# Apply the wraper to each decade's df -----------------------------------------
extracted_long <- extracted %>%
  mutate(sst = map(sst, my_pivot)) %>%
  select(-decade) %>% 
  unnest(sst) %>% 
  group_by(ssp, model) %>% 
  nest()

print("Pivoting done")
## EXPORT ######################################################################
my_write <- function(ssp, model, data) {
  
  main_dir <- here("data", "processed", "CMIP_SST_projections", ssp)
  filename <- paste0(model, ".rds")
  
  path_and_name <- here(main_dir, filename)
  
    saveRDS(object = data,
            file = path_and_name) 
}

# X ----------------------------------------------------------------------------
extracted_long %$%
  pwalk(list(ssp, model, data),
        my_write)

# sst %>% 
#   filter(ssp == "ssp126") %>% 
#   mutate(ext = map(s, as.data.frame, xy = T)) %>% 
#   select(-s) %>% 
#   unnest(ext) %>% 
#   ggplot(aes(x = x, y = y, fill = `2015-01-01`)) +
#   geom_tile() +
#   facet_wrap(~model, ncol = 4, scales = "free")

