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
  filter(species %in% c("Panulirus interruptus",
                        "Parastichopus parvimensis",
                        "Mesocentrotus franciscanus"),
         community %in% c("Isla Natividad", "El Rosario")) %>% 
  mutate(density = abundance / 60,
         coop_name = case_when(community == "Isla Natividad" ~ "Buzos y Pescadores",
                               community == "El Rosario" ~ "Ensenada"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = cobi_clean,
        file = here("data", "processed", "annual_ecological_panel.rds"))
