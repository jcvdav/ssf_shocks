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
  mutate(fishery = str_to_sentence(str_replace_all(fishery, "_", " "))) %>% 
  drop_na(depth_m)

## ESTIMATION ##################################################################
lob_mod <- lm(norm_density ~ period_long,
   data = data %>% 
     filter(fishery == "Lobster"))
car::Anova(lob_mod, type = "II", white.adjust = T)
TukeyHSD(aov(lob_mod))

cuc_mod <- lm(norm_density ~ period_long,
              data = data %>% 
                filter(fishery == "Sea cucumber"))
car::Anova(cuc_mod, type = "II", white.adjust = T)
TukeyHSD(aov(cuc_mod))

urc_mod <- lm(norm_density ~ period_long,
              data = data %>% 
                filter(fishery == "Urchin"))
car::Anova(urc_mod, type = "II")
TukeyHSD(aov(urc_mod))

feols(norm_density ~ norm_mhw_int_cumulative + depth_m | site, data = data,
             split = ~ fishery,
             vcov = "DK",
             panel.id = ~site + year) %>% 
  magrittr::set_names(c("Lobster", "Sea cucumber", "Urchin")) %>% 
  modelsummary::modelsummary(stars = T,
                             coef_rename = c("norm_mhw_int_cumulative" = "Cumulative intensity",
                                             "period1" = "During",
                                             "period2" = "After",
                                             "depth_m" = "Depth (m)"),
                             gof_omit = "R|With|RMS|Std|FE",
                             title = "Effect of cumulative marine heatwave intensity
                             on densities of lobster, sea cucumber, and urchin
                             surveyed in two no-take marine reserves in Isla Natividad.
                             In all cases the outcome variable is standard-normalized density
                             (historical mean removed and scaled by standard deviation).
                             Cummulative MHW intensity in panel A is also standard-normalized, and
                             regressors in panel B are dummies for During MHW and After MHW regimes.")

bind_rows(top, bottom) %>% 
  mutate(term = ifelse(statistic == "std.error", "", term)) %>%
  select(matches("term|fishery")) %>% 
  kableExtra::kbl(booktabs = T,
                  col.names = ,
                  caption = ) %>% 
  kableExtra::pack_rows("A)", 1, dim(top)[1]) %>% 
  kableExtra::pack_rows("B)", dim(top)[1] + 1, dim(bottom)[1]) %>% 
  kableExtra::add_footnote(notation = "none",
                           "Note: All models include site-level fixed
                           effects and use Driscoll-Kraay standard errors")

