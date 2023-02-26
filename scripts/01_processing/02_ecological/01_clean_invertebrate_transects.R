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

std_normal <- function(x, year, cutoff_year = 2013) {
  (x - mean(x[year <= cutoff_year], na.rm = T)) / sd(x[year <= cutoff_year], na.rm = T)
}

# Load data --------------------------------------------------------------------
cobi <- read_excel(
  path = here("data",
              "raw",
              "natividad_invert_transects",
              "COBI _Invertebrates_Baja_2006-2021_12jul2021.xlsx"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
cobi_clean <- cobi %>%
  filter(ZONA == "Reserva",
         COMUNIDAD == "Isla Natividad") %>% 
  select(year = ANIO,
         community = COMUNIDAD,
         site = SITIO,
         depth_m = PROFUNDIDAD_INICIAL,
         transect = Transecto,
         species = ESPECIE,
         abundance = ABUNDANCIA) %>% 
  filter(species %in% c("Panulirus interruptus",
                        "Parastichopus parvimensis",
                        "Mesocentrotus franciscanus")) %>% 
  complete(year, community, site, species, fill = list(transect = 0,
                                            abundance = 0)) %>%
  mutate(density = abundance / 60,
         eu_rnpa = "0301000089",
         fishery = case_when(
           species == "Panulirus interruptus" ~ "lobster",
           species == "Parastichopus parvimensis" ~ "sea_cucumber",
           species == "Mesocentrotus franciscanus" ~ "urchin",
         )) %>% 
  left_join(periods, by = "year") %>% 
  group_by(eu_rnpa, fishery, site) %>% 
  mutate(n_periods = n_distinct(period)) %>% 
  ungroup() %>% 
  filter(n_periods == 3) %>% 
  group_by(site, eu_rnpa, fishery, period) %>% 
  mutate(n_years = n_distinct(year)) %>% 
  ungroup() %>% 
  group_by(site, eu_rnpa, fishery) %>% 
  filter(all(n_years >= 3)) %>%
  select(-c(n_periods, n_years)) %>%
  group_by(eu_rnpa, site, fishery) %>% 
  mutate(norm_abundance = std_normal(x = abundance, y = year),
         norm_density = std_normal(x = density, y = year)) %>% 
  ungroup() 

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

saveRDS(object = cobi_clean,
        file = here("data", "processed", "annual_ecological_panel.rds"))

