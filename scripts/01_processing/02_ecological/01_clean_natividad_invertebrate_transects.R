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
library(readxl)
library(tidyverse)

# Load data --------------------------------------------------------------------
cobi <- read_excel(
  path = here("data",
              "raw",
              "natividad_invert_transects",
              "COBI _Invertebrates_Baja_2006-2021_12jul2021.xlsx"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
cobi_clean <- cobi %>%
  filter(ZONA == "Reserva") %>% 
  select(year = ANIO,
         community = COMUNIDAD,
         site = SITIO,
         depth_m = PROFUNDIDAD_INICIAL,
         transect = Transecto,
         genus = GENERO,
         species = ESPECIE,
         abundance = ABUNDANCIA) %>% 
  filter(species == "Panulirus interruptus",
         community %in% c("Isla Natividad", "El Rosario")) %>% 
  mutate(density = abundance / 60)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = cobi_clean,
        file = here("data", "processed", "reserve_lobster_density_site_year.Rds"))
