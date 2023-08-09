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
pacman::p_pload(
  here,
  fixest,
  tidyverse
)

# Load data --------------------------------------------------------------------
data <- readRDS(here("data", "estimation_panels", "env_eco_panel.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace_all(fishery, "_", " ")))

## ESTIMATION ##################################################################
gof_omits <- "Adj|With|RMS|Std|FE"
top <- feols(norm_density ~ period + depth_m | site, data = data,
             split = ~ fishery, vcov = "DK", panel.id = ~site + year) %>% 
  modelsummary::modelsummary(stars = T,
                             coef_rename = c("norm_mhw_int_cumulative" = "Cumulative intensity",
                                             "period1" = "During",
                                             "period2" = "After",
                                             "depth_m" = "Depth (m)"),
                             # output = "data.frame",
                             gof_omit = gof_omits)

bottom <- feols(norm_density ~ norm_mhw_int_cumulative + depth_m | site, data = data,
             split = ~ fishery,
             vcov = "DK",
             panel.id = ~site + year) %>% 
  modelsummary::modelsummary(stars = T,
                             coef_rename = c("norm_mhw_int_cumulative" = "Cumulative intensity",
                                             "period1" = "During",
                                             "period2" = "After",
                                             "depth_m" = "Depth (m)"),
                             # output = "data.frame",
                             gof_omit = gof_omits)

bind_rows(top, bottom) %>% 
  mutate(term = ifelse(statistic == "std.error", "", term)) %>%
  select(matches("term|fishery")) %>% 
  kableExtra::kbl(booktabs = T,
                  col.names = c("", "Lobster", "Sea cucumber", "Urchin"),
                  caption = "Difference in means during three periods of marine
                  heatwave regime (panel A) and cumulative marine heatwave intensity
                  (panel B) on densities of lobster, sea cucumber, and urchin
                  surveyed in two no-take marine reserves in Isla Natividad.
                  In all cases the outcome variable is standard-normalized density
                  (historical mean removed and scaled by standard deviation).
                  Cummulative MHW intensity in panel A is also standard-normalized, and
                  regressors in panel B are dummies for During MHW and After MHW regimes.") %>% 
  kableExtra::pack_rows("A)", 1, dim(top)[1]) %>% 
  kableExtra::pack_rows("B)", dim(top)[1] + 1, dim(bottom)[1]) %>% 
  kableExtra::add_footnote(notation = "none",
                           "Note: All models include site-level fixed
                           effects and use Driscoll-Kraay standard errors")

