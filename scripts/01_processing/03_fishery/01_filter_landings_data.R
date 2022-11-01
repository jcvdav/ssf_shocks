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
library(tidyverse)

# Load data --------------------------------------------------------------------
landings <- readRDS(
  file = file.path(
    "/Users/juancarlosvillasenorderbez/GitHub/",
    "data_mex_fisheries",
    "data",
    "mex_landings",
    "clean",
    "mex_annual_landings_by_eu.rds"
  )
)

# Define species I want --------------------------------------------------------
spp <- c(
  "LANGOSTA"#,
  #,"ABULON",
  # "PEPINO DE MAR",
  # "ERIZO"
)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
filtered <- landings %>%
  inner_join(coop_eurnpa, by = "eu_rnpa") %>%
  filter(main_species_group %in% spp)
  

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = filtered,
        file = here("data", "processed", "filtered_landings.rds"))


TO DO
- Filter landings data: why am I grouping and summarizing?
- Build fishery estimation panel