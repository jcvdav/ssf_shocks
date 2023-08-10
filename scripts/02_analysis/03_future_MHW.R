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

# X ----------------------------------------------------------------------------
p_mhw_occurs <- bind_rows(p_mhw_occurs_past,
                          p_mhw_occurs_future) 
# Test whether there are diffrences between SSP and Historical
model <- lm(p_at_least_one ~ fishery + ssp, data = p_mhw_occurs)

car::Anova(model, type = "II", white.adjust = TRUE)

TukeyHSD(aov(model)) %>% 
  as.list() %>% 
  map_dfr(~as_tibble(.x, rownames = "group"), .id = "source") %>% 
  janitor::clean_names() %>% 
  select(-c(4, 5)) %>% 
  knitr::kable() %>%
  kableExtra::collapse_rows(columns = 1) %>%
  kableExtra::kable_styling()


# Test whetehr there are differences between SSPs
model <- lm(mean ~ fishery + ssp, data = con_p_mhw_threshold_future)

car::Anova(model, type = "II", white.adjust = TRUE)

TukeyHSD(aov(model)) %>% 
  as.list() %>% 
  map_dfr(~as_tibble(.x, rownames = "group"), .id = "source") %>% 
  janitor::clean_names() %>% 
  select(-c(4, 5)) %>% 
  knitr::kable() %>%
  kableExtra::collapse_rows(columns = 1) %>%
  kableExtra::kable_styling()

