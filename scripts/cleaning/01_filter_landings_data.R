######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(tidyverse)

landings <- readRDS(
  file = file.path("/Users/juancarlosvillasenorderbez/GitHub/",
                   "data_mex_fisheries",
                   "data",
                   "mex_landings",
                   "clean",
                   "mex_annual_landings_by_eu.rds")
)


filtered <- landings %>% 
  ungroup() %>% 
  inner_join(coop_eurnpa, by = "eu_rnpa") %>% 
  filter(main_species_group %in% c("ABULON", "LANGOSTA", "PEPINO DE MAR")) %>% 
  group_by(year_cut, main_species_group, coop) %>% 
  summarize(landed_weight = sum(landed_weight, na.rm = T),
            value = sum(value, na.rm = T)) %>% 
  mutate(year_cut = as.numeric(year_cut))

saveRDS(object = filtered,
        file = here("data", "processed", "filtered_landings.rds"))
