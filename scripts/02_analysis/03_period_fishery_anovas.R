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
  car,
  tidyverse
)

# Load data --------------------------------------------------------------------
data <- readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
total_data <- data %>%
  group_by(period, period_long, year, fishery) %>%
  summarize(live_weight = sum(live_weight, na.rm = T), #) %>% 
            n = n_distinct(eu_rnpa),
            norm_live_weight = live_weight / n) %>%
  group_by(period, period_long, fishery) %>% 
  mutate(period_mean = mean(norm_live_weight),
         period_sd = sd(norm_live_weight)) %>%
  ungroup()

# Get pct change by period
total_data %>%
  ungroup() %>%
  select(fishery, period, period_mean) %>%
  distinct() %>%
  pivot_wider(names_from = period, values_from = period_mean, names_prefix = "p") %>%
  mutate(pct1 = ((p1 - p0) / p0) * 100,
         pct2 = ((p2 - p0) / p0) * 100)

total_data %>%
  ungroup() %>%
  select(fishery, period, period_sd) %>%
  distinct() %>%
  pivot_wider(names_from = period, values_from = period_sd, names_prefix = "p") %>%
  mutate(pct1 = ((p1 - p0) / p0) * 100,
         pct2 = ((p2 - p0) / p0) * 100)

# DF's should be Df1 = (3 "periods" - 1) = 2
# and DF2 = 22 years - 3 periods = 19

# X ----------------------------------------------------------------------------
lob_mod <- lm(norm_live_weight ~ period_long, data = total_data %>% filter(fishery == "Lobster"))
Anova(mod = lob_mod, type = "II", white.adjust = TRUE)
TukeyHSD(aov(lob_mod))
# X ----------------------------------------------------------------------------
cuc_mod <- lm(norm_live_weight ~ period_long, data = total_data %>% filter(fishery == "Sea cucumber"))
Anova(mod = cuc_mod, type = "II", white.adjust = TRUE)
TukeyHSD(aov(cuc_mod))

# X ----------------------------------------------------------------------------
urc_mod <- lm(norm_live_weight ~ period_long, data = total_data %>% filter(fishery == "Urchin"))
Anova(mod = urc_mod, type = "II", white.adjust = TRUE)
TukeyHSD(aov(urc_mod))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = total_data,
        file = here("data", "output", "total_annual_normalized_landigs.rds"))

