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
  tidyverse
)

# Load data --------------------------------------------------------------------
p_mhw_occurs_past <- readRDS(file = here("data", "output", "p_mhw_occurs.rds")) %>% 
  mutate(ssp = "Historical",
         type = "Historical")

p_mhw_occurs_future <- readRDS(file = here("data", "output", "p_mhw_occurs_future.rds"))

## PROCESSING ##################################################################
# Some stats for text ----------------------------------------------------------
# Range of future P(MHW)
p_mhw_occurs %>%
  group_by(ssp) %>%
  summarize(p = mean(p_at_least_one),
            sd = sd(p_at_least_one))

# X ----------------------------------------------------------------------------
p_mhw_occurs <- bind_rows(p_mhw_occurs_past,
                          p_mhw_occurs_future) 
# Test whether there are diffrences between SSP and Historical
model <- lm(p_at_least_one ~ fishery + ssp, data = p_mhw_occurs)

car::Anova(model, type = "II", white.adjust = TRUE) %>% 
  stargazer::stargazer(summary = F,
                       label = "tab:future_mhw_anova", 
                       title = "ANOVA results testing for differences in future probability of exposure relative to historical probabilities",
                       out = here("results", "tab", "tabS03_future_MHW_anova.tex"))

TukeyHSD(aov(model)) %>% 
  as.list() %>% 
  map_dfr(~as_tibble(.x, rownames = "group"), .id = "source") %>% 
  janitor::clean_names() %>% 
  select(-c(4, 5)) %>% 
  knitr::kable(format = "latex",
               col.names = c("Source", "Group", "Difference", "adjusted p value"),
               caption = "Tukey’s HSD table for differences in Future P(marine heatwave)",
               label = "future_mhw_hsd",
               booktabs = T) %>%
  cat(file = here("results", "tab", "tabS04_future_MHW_hsd.tex"))


# Test whetehr there are differences between SSPs
model <- lm(mean ~ fishery + ssp, data = con_p_mhw_threshold_future)

car::Anova(model, type = "II", white.adjust = TRUE)

TukeyHSD(aov(model)) %>% 
  as.list() %>% 
  map_dfr(~as_tibble(.x, rownames = "group"), .id = "source") %>% 
  janitor::clean_names() %>% 
  select(-c(4, 5)) %>% 
  knitr::kable(format = "latex",
               col.names = c("Source", "Group", "Difference", "adjusted p value"),
               caption = "Post-hoc testing via Tukey’s HSD for differences in future probability of extreme events across fisheries and SSPs",
               label = "future_extreme_mhw_hsd",
               booktabs = T) %>%
  cat(file = here("results", "tab", "tabS06_future_extreme_MHW_hsd.tex"))

