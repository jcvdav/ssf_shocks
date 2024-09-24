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
  tidyverse
)

# Load data --------------------------------------------------------------------
references <- readRDS(file = here("data", "output", "reference_mhw_by_TURF.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
references %>%
  mutate(max = as.character(round(max, 2))) %>% 
  pivot_wider(names_from = fishery, values_from = max, values_fill = "-") %>%
  mutate(eu_rnpa = str_replace_all(eu_rnpa, "0", "*"),
         eu_rnpa = str_replace_all(eu_rnpa, "8", "°")) %>% 
  kable(col.names = c("RNPA of Economic Unit",
                      "Year",
                      "Lobster",
                      "Sea Cucumber",
                      "Sea Urchin"),
        linesep = "",
        format = "latex",
        booktabs = T,
        label = "hist_max_mhw_ref_points",
        caption = "Largest cumulative marine heatwave intensity (°C Days) observed for each TURF.") %>% 
  cat(file = here("results", "tab", "tabS05_hist_max_mhw_ref_points.tex"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------