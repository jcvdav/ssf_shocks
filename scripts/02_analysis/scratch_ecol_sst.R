################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Continued from 01_clean_natividad_invertebrate_transects.R
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------

# Load data --------------------------------------------------------------------

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

p1 <- data %>% 
  ggplot(mapping = aes(x = year, y = density)) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, fill = "cadetblue", color = "gray10") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  facet_wrap(~coop_name, scales = "free_y", ncol = 1) +
  labs(x = "Year",
       y = "Lobster density (N / m2)")

p2 <- data %>% 
  ggplot(mapping = aes(x = mean_sst, y = density)) +
  stat_summary(geom = "pointrange", fun.data = mean_se, shape = 21, fill = "cadetblue", color = "gray10") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  facet_wrap(~coop_name, scales = "free_y", ncol = 1) +
  labs(x = "Mean SST",
       y = "Lobster density (N / m2)")

p <- plot_grid(p1, p2, ncol = 2)

fixest::feols(log(density) ~ mean_sst | site, data = data, cluster = "site", split = ~coop_name) %>% 
  modelsummary::modelsummary(stars = T,
                             gof_omit = "IC",
                             notes = "Using heteroskedastic-robust standard errors clustered by site")

fixest::feols(log(density) ~ hw | site, data = data, cluster = "site", split = ~coop_name) %>% 
  modelsummary::modelsummary(stars = T,
                             gof_omit = "IC",
                             notes = "Using heteroskedastic-robust standard errors clustered by site",
                             coef_rename = c("hw1" = "During HW",
                                             "hw2" = "After HW"))