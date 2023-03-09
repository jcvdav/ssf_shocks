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
library(rcmip6)
library(magrittr)
library(furrr)
library(tidyverse)

source(here("scripts", "00_collect_externals", "04_rcmip6_helpers.R"))
# Load data --------------------------------------------------------------------

## PROCESSING ##################################################################

# Find models ------------------------------------------------------------------
# Build a query
query <- list(
  type = "Dataset",
  replica = "false",
  latest = "true",
  variable_id = "tos",
  project = "CMIP6",
  grid_label = "gn",
  frequency = "day",                          
  table_id = "Oday",
  experiment_id = c("ssp126",
                    "ssp245",
                    "ssp585"),
  member_id = "r1i1p1f1",
  source_id = c("GFDL-ESM4",
                "ACCESS-CM2",
                "ACCESS-ESM1-5",
                "BCC-CSM2-MR",
                "CanESM5",
                "EC-Earth3",
                "MIROC6",
                "MRI-ESM2-0",
                "NorESM2-LM",
                "NorESM2-MM")
)


results <- cmip_search(query)
# cmip_info(results)

# results |>
#   cmip_simplify() |>   # To keep only the most informative columns
#   subset(, -full_info) |>
#   View()

# Download ---------------------------------------------------------------------
# Set a root directory
cmip_root_set(here("data", "raw", "climate_model_output"))
options(timeout = 360)            # Kind of important for some reason

nested_res <- results %>% 
  arrange(experiment_id, institution_id) %>% 
  mutate(my_id = 1:nrow(.)) %>% 
  group_by(my_id) %>% 
  nest()

nested_res %$%
  walk(data, cmip_download_one)

# files <- cmip_download(results)
# 
# list.files(here("data", "raw", "climate_model_output"), recursive = T, pattern = "\\.nc$")


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

