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
  kableExtra,
  rcmip6,
  magrittr,
  furrr,
  tidyverse
)

# source(here("scripts", "00_collect_externals", "04_rcmip6_helpers.R"))
# Load data --------------------------------------------------------------------

## PROCESSING ##################################################################

# Find models ------------------------------------------------------------------
# Build a query
query <- list(
  type = "Dataset",
  replica = "true",
  latest = "true",
  variable_id = "tos",
  project = "CMIP6",
  grid_label = "gn",
  frequency = "day",
  table_id = "Oday",
  experiment_id = c("historical",
                    "ssp126",
                    "ssp245",
                    "ssp585"
                    ),
  member_id = "r1i1p1f1",
  source_id = c(
                "ACCESS-CM2",
                "ACCESS-ESM1-5",
                "BCC-CSM2-MR",
                "CanESM5",
                "CMCC-ESM2",
                "EC-Earth3",
                "GFDL-ESM4",
                "MIROC6",
                "MRI-ESM2-0",
                "NorESM2-LM",
                "NorESM2-MM"
                ## "IPSL-CM6A-LR",
                ## "NESM3"
  )
)


results <- cmip_search(query)
cmip_info(results)


# Download ---------------------------------------------------------------------
# Set a root directory
cmip_root_set(here("data", "raw", "climate_model_output"))
options(timeout = 360)            # Kind of important for some reason

## EXPORT ######################################################################

# Download ---------------------------------------------------------------------
cmip_download(results, year_range = c(1982, 2050))

results %>%
  select(institution_id, source_id, nominal_resolution) %>%
  distinct() %>%
  arrange(source_id) %>% 
  knitr::kable(col.names = c("Institution id", "Source id", "Nominal resolution")) %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::save_kable(file = "cmip_tos_model_info.png")
