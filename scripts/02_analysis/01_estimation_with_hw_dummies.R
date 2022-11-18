######################################################
#title#
######################################################
# 
# Purpose
#
######################################################
library(here)
library(cowplot)
library(modelsummary)
library(fixest)
library(lme4)
library(tidyverse)


ihs <- function(x){
  log(x + sqrt((x ^ 2) + 1))
}

# Load data
data <- readRDS(file = here("data", "estimation_panels", "env_fish_panel.rds")) %>% 
  filter(balanced)

landings_ts <- ggplot(data = data) +
  geom_rect(xmin= 2013.5, xmax = 2017.5, ymin = -100, ymax = 500, fill = "gray") +
  geom_line(aes(x = year,
                y = norm_landed_weight,
                group = coop_name)) +
  # geom_line(data = data %>%
  #             filter(coop_name == "Buzos y Pescadores"),
  #           aes(x = year,
  #               y = norm_landed_weight),
  #           color = "steelblue", size = 2) +
  theme_bw() +
  labs(x = "Year",
       y = "Standardized\nlandings",
       color = "Cooperative") +
  theme(legend.position = "None") +
  guides(color = guide_legend(ncol = 2)) +
  scale_x_continuous(expand = c(0, 0))

value_ts <- ggplot(data = data) +
  geom_rect(xmin= 2013.5, xmax = 2017.5, ymin = -100, ymax = 500, fill = "gray") +
  geom_line(aes(x = year,
                y = norm_value,
                group = coop_name)) +
  theme_bw() +
  labs(x = "Year",
       y = "Standardized\nrevenues",
       color = "Cooperative") +
  theme(legend.position = "None") +
  guides(color = guide_legend(ncol = 2))


fishery_ts <- plot_grid(landings_ts, value_ts, ncol = 1)

startR::lazy_ggsave(fishery_ts,
                    filename = "fishery_ts",
                    width = 9,
                    height = 7)

panel <- ggplot(data, aes(x = ihs(mhw_int_cumulative), y = landed_weight / 1e3)) + 
  geom_smooth(method = "lm", se = T, fullrange = T, color = "black", linetype = "dashed", size = 0.5) +
  geom_point(aes(fill = period_long), shape = 21, color = "black", size = 2) +
  facet_wrap(~coop_name, ncol = 5, scales = "free_y") +
  labs(x = "Log-transforemed Cum. Int. (°C X days)",
       y = "Landed weight (MT)",
       fill = "Period") +
  scale_fill_manual(values = period_palette) +
  theme_bw() +
  theme(legend.position = c(1, -0.05),
        legend.justification = c(1, 0),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 6)) +
  guides(fill = guide_legend(title.position = "top", ncol = 2))

startR::lazy_ggsave(panel,
                    filename = "panel_figure",
                    width = 20,
                    height = 12)

## Scatter plot

ggplot(data, aes(x = ihs(mhw_int_cumulative), y = norm_landed_weight)) + 
  geom_point(aes(fill = period_long), shape = 21, color = "black", size = 2) +
  labs(x = "Log-transforemed Cum. Int. (°C X days)",
       y = "Standardized landed weight",
       fill = "Period") +
  scale_fill_manual(values = period_palette) +
  theme_bw() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 6)) 


# Estimate Random Effect models
reg_data <- data %>% 
  mutate_at(.vars = vars(temp_mean, mhw_int_cumulative, mhw_days, mhw_events), .funs = ~(.x - mean(.x)) / sd(.x))
# mutate_at(.vars = vars(temp_mean, mhw_int_cumulative, mhw_days, mhw_events), .funs = log1p)
# mutate_at(.vars = vars(temp_mean, mhw_int_cumulative, mhw_days, mhw_events), .funs = ihs)

extractr <- function(obj, pattern = "mhw|temp"){
  as.data.frame(ranef(obj)) %>%
    filter(str_detect(term, pattern))
}

get_mu <- function(obj){
  coef <- fixef(obj)[2]
  vars <- obj %>% vcov() %>% diag()
  sd <- sqrt(vars[2])
  tibble(x = coef, xmin = coef - sd, xmax = coef + sd)
}

# Landings
land_temp_mod <- lme4::lmer(landed_weight / 1e3 ~ temp_mean + (temp_mean | coop_name), data = reg_data)
land_int_mod <- lme4::lmer(landed_weight / 1e3 ~ mhw_int_cumulative + (mhw_int_cumulative | coop_name), data = reg_data)
land_days_mod <- lme4::lmer(landed_weight / 1e3 ~ mhw_days + (mhw_days | coop_name), data = reg_data)
land_event_mod <- lme4::lmer(landed_weight / 1e3 ~ mhw_events + (mhw_events | coop_name), data = reg_data)

# Revenues
rev_temp_mod <- lme4::lmer(value / 1e6 ~ temp_mean + (temp_mean | coop_name), data = reg_data)
rev_int_mod <- lme4::lmer(value / 1e6 ~ mhw_int_cumulative + (mhw_int_cumulative | coop_name), data = reg_data)
rev_days_mod <- lme4::lmer(value / 1e6 ~ mhw_days + (mhw_days | coop_name), data = reg_data)
rev_event_mod <- lme4::lmer(value / 1e6 ~ mhw_events + (mhw_events | coop_name), data = reg_data)

# Group models
land_mods <- list(land_temp_mod,
                  land_int_mod, 
                  land_days_mod,
                  land_event_mod) %>% 
  set_names(c("SST", "MHW int", "MHW days", "MHW events"))

rev_mods <- list(rev_temp_mod,
                 rev_int_mod, 
                 rev_days_mod,
                 rev_event_mod) %>% 
  set_names(c("SST", "MHW int", "MHW days", "MHW events")) 

# Get coefficients
land_coefs <- land_mods %>% 
  map_df(extractr, .id = "measure") %>%
  mutate(variable = "Landings")


rev_coefs <- rev_mods %>% 
  map_df(extractr, .id = "measure") %>%
  mutate(variable = "Revenues")

coef_data <- 
  bind_rows(land_coefs, rev_coefs) %>% 
  mutate(grp = fct_reorder(grp, -condval)) %>% 
  group_by(measure, variable) %>% 
  mutate(condval_col = condval / max(condval))


# Central 
land_mu_data <- land_mods %>% 
  map_df(get_mu, .id = "measure") %>%
  mutate(variable = "Landings")

rev_mu_data <- rev_mods %>% 
  map_df(get_mu, .id = "measure") %>%
  mutate(variable = "Revenues")

mu_data <- bind_rows(land_mu_data, rev_mu_data)

# Plot landings coefficients
land_coef_plot <- coef_data %>% 
  filter(measure == "MHW int",
         variable == "Landings") %>% 
  mutate(grp = fct_reorder(grp, -condval)) %>% 
  ggplot(aes(y = grp, x = condval)) +
  geom_rect(data = filter(land_mu_data, measure == "MHW int"), aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = F, fill = "gray80") +
  geom_vline(data = filter(land_mu_data, measure == "MHW int"), aes(xintercept = x), color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = condval -condsd,
                     xmax = condval + condsd),
                 height = 0) +
  geom_point(aes(fill = condval), shape = 21, size = 2) + 
  scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue") +
  labs(x = "RE of MHW Cum. Int.",
       y = NULL) +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank())


rev_coef_plot <- coef_data %>% 
  filter(measure == "MHW int") %>% 
  mutate(grp = fct_reorder(grp, -condval)) %>% 
  filter(variable == "Revenues") %>% 
  ggplot(aes(y = grp, x = condval)) +
  geom_rect(data = filter(rev_mu_data, measure == "MHW int"), aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = F, fill = "gray80") +
  geom_vline(data = filter(rev_mu_data, measure == "MHW int"), aes(xintercept = x), color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = condval -condsd,
                     xmax = condval + condsd),
                 height = 0) +
  geom_point(aes(fill = condval), shape = 21, size = 2) + 
  scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue") +
  labs(x = "RE of MHW Cum. Int.",
       y = NULL) +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank())



# Mega plot --- needs to be fixed
coef_data %>% 
  # left_join(centroids, by = c("grp"= "coop_name")) %>% 
  # mutate(grp = fct_reorder(grp, lat)) %>% 
  ggplot(aes(y = grp, x = condval)) +
  geom_rect(data = mu_data, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = F, fill = "gray80") +
  geom_vline(data = mu_data, aes(xintercept = x), color = "black") +
  geom_errorbarh(aes(xmin = condval -condsd,
                     xmax = condval + condsd),
                 height = 0) +
  geom_point(aes(fill = condval_col), shape = 21, size = 2) + 
  facet_wrap(variable~measure, ncol = 4, scales = "free_x") +
  scale_fill_gradient2(low = "red", high = "steelblue", mid = "white") +
  labs(x = "Temperature influence",
       y = NULL) +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank())


coef_lat_plot <- coef_data %>% 
  filter(measure == "MHW int",
         variable == "Landings") %>% 
  ggplot(aes(x = lat, y = condval)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_errorbar(aes(ymin = condval -condsd,
                    ymax = condval + condsd),
                width = 0) +
  geom_point(aes(fill = condval), shape = 21, size = 2) +
  scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue") +
  labs(x = "°Latitude (Centroid)",
       y = "RE of MHW Cum. Int.") +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank())

coef_data %>% 
  filter(measure == "MHW int",
         variable == "Landings") %>% 
  left_join(data %>% 
              group_by(coop_name) %>% 
              summarize(sd = sd(temp_mean),
                        temp_long_term = mean(temp_mean)) %>% 
              ungroup(), by = c("grp" = "coop_name")) %>% 
  ggplot(aes(x = temp_long_term, y = condval)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_errorbar(aes(ymin = condval -condsd,
                    ymax = condval + condsd),
                width = 0) +
  geom_errorbarh(aes(xmin = temp_long_term - sd, xmax = temp_long_term + sd)) +
  geom_point(aes(fill = condval), shape = 21, size = 3) +
  scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue") +
  labs(x = "Mean SST (°C)",
       y = "RE of MHW Cum. Int.") +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank())

reg_data %>% 
  select(coop_name, year, period, period_long, landed_weight, norm_landed_weight) %>% 
  filter(period == "2") %>% 
  mutate(return = between(norm_landed_weight, -0.5, 10)) %>%
  group_by(coop_name) %>%
  summarize(return = sum(return)) %>% 
  ungroup() %>% 
  left_join(coef_data, by = c("coop_name" = "grp")) %>% 
  filter(measure == "MHW int",
         variable == "Landings") %>% 
  ggplot(aes(x = return, y = condval, fill = condval)) +
  geom_pointrange(aes(ymin = condval -condsd,
                      ymax = condval + condsd),
                  shape = 21,
                  position = position_jitter(width = 0.3, height = 0, seed = 1)) +
  scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "steelblue") +
  labs(x = "Recovered",
       y = "RE of MHW Cum. Int.") +
  theme_bw() +
  theme(legend.position = "None",
        strip.background = element_blank())





























# ODLF STUFF


# After exploring, lets etimate
p_m1 <- feols(list(log(landed_weight), log(value)) ~ period, data = data, cluster = "coop_name")
p_m2 <- feols(list(log(landed_weight), log(value)) ~ period + year, data = data, cluster = "coop_name")
p_m3 <- feols(list(log(landed_weight), log(value)) ~ period + year | coop_name, data = data)


p_land <- list(p_m1[[1]],
               p_m2[[1]],
               p_m3[[1]]) %>% 
  set_names("Landings")

p_val <- list(p_m1[[2]],
              p_m2[[2]],
              p_m3[[2]]) %>% 
  set_names("Revenues")

effect_size <- list(p_land[[3]], p_val[[3]]) %>% 
  set_names(c("Landings", "Revenues")) %>% 
  map_df(tidy, .id = "dep.var") %>% 
  filter(term %in% c("period1", "period2")) %>% 
  mutate(term = case_when(term == "period1" ~ "During HW",
                   T ~ "After HW"),
         term = fct_relevel(term, "During HW", "After HW")) %>% 
  ggplot(aes(x = dep.var, y = estimate, fill = term)) +
  geom_pointrange(aes(ymin = estimate-std.error,
                      ymax = estimate+std.error),
                  position = position_dodge(width = 0.2),
                  shape = 21,
                  size = 1) +
  geom_text(aes(label = paste0("B =", round(estimate, 3), "\np = ", round(p.value, 3))),
            nudge_x = c(-0.25, 0.25, -0.25, 0.25), nudge_y = 0.1, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "",
       y = "Estimate",
       fill = "Period")

startR::lazy_ggsave(plot = effect_size,
                    filename = "effect_size",
                    width = 10,
                    height = 5)


modelsummary(models = list(p_land[[3]], p_val[[3]], p_land[[3]], p_val[[3]]) %>% 
               set_names(c("log(Landings)", "log(Revenues)", "% transofrmed", "% transformed")),
             exponentiate = c(F, F, T, T),
             # output = here("results", "tab", "effect_sizes.md"),
             stars = T,
             gof_omit = c("IC|Adj|With|RMSE"),
             coef_map = c("hw1" = "During HW",
                          "hw2" = "Aeftr HW",
                          "year" = "Time trend"))


s_land <- 
  stan_glm(landed_weight ~ period + year + coop_name, data = data)

s_rev <- 
  stan_glm(value ~ period + year + coop_name, data = data)

plot(s_land, plotfun = "areas", pars = c("period1", "period2")) +
  labs(title = "Posterior distributions") +
  scale_y_discrete(labels = c("During MHW", "After MHW"))

plot(s_rev, plotfun = "areas", pars = c("period1", "period2")) +
  labs(title = "Posterior distributions") +
  scale_y_discrete(labels = c("During MHW", "After MHW"))

reg_data <- data %>% 
  mutate_at(.vars = vars(temp_mean, mhw_int_cumulative, mhw_days, mhw_events), .funs = ~(.x - mean(.x)) / sd(.x))

# Effect on landed weight
temp <- stan_glm(landed_weight / 1e3  ~ temp_mean + coop_name, data = reg_data)
mhw1 <- stan_glm(landed_weight / 1e3  ~ mhw_int_cumulative + coop_name, data = reg_data)
mhw2 <- stan_glm(landed_weight / 1e3  ~ mhw_days + coop_name, data = reg_data)
mhw3 <- stan_glm(landed_weight / 1e3  ~ mhw_events + coop_name, data = reg_data)
periods <- stan_glm(landed_weight / 1e3  ~ period + coop_name, data = reg_data)

l_models <- list(temp, mhw1, mhw2, mhw3)%>% 
  set_names(c("SST", "MHW int", "MHW days", "MHW events"))

map_dfr(l_models, get_post, .id = "model") %>%
  filter(!str_detect(term, "Inter"),
         term %in% c("temp_mean", "mhw_int_cumulative", "mhw_events", "mhw_days")) %>%
  mutate(term = case_when(term == "temp_mean" ~ "SST",
                          term == "mhw_int_cumulative" ~ "MHW Cummulative intensity",
                          term == "mhw_events" ~ "MHW events",
                          term == "mhw_days" ~ "MHW days")) %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_errorbar(aes(ymin = X5., ymax = X95.), size = 1, width = 0) +
  geom_errorbar(aes(ymin = X25., ymax = X75.), size = 2, width = 0) +
  geom_point(size = 5, shape = 21, fill = "cadetblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_bw() +
  labs(x = "",
       y = "Effect on landings")
  
plot(periods, plotfun = "areas", pars = c("period1", "period2")) +
  labs(title = "Posterior distributions") +
  scale_y_discrete(labels = c("During MHW", "After MHW"))



# Effect on revenues
temp <- stan_glm(value / 1e3  ~ temp_mean + coop_name, data = reg_data)
mhw1 <- stan_glm(value / 1e3  ~ mhw_int_cumulative + coop_name, data = reg_data)
mhw2 <- stan_glm(value / 1e3  ~ mhw_days + coop_name, data = reg_data)
mhw3 <- stan_glm(value / 1e3  ~ mhw_events + coop_name, data = reg_data)
periods <- stan_glm(value / 1e3  ~ period + coop_name, data = reg_data)

v_models <- list(temp, mhw1, mhw2, mhw3)%>% 
  set_names(c("SST", "MHW int", "MHW days", "MHW events"))

map_dfr(v_models, get_post, .id = "model") %>%
  filter(!str_detect(term, "Inter"),
         term %in% c("temp_mean", "mhw_int_cumulative", "mhw_events", "mhw_days")) %>%
  mutate(term = case_when(term == "temp_mean" ~ "SST",
                          term == "mhw_int_cumulative" ~ "MHW Cummulative intensity",
                          term == "mhw_events" ~ "MHW events",
                          term == "mhw_days" ~ "MHW days")) %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_errorbar(aes(ymin = X5., ymax = X95.), size = 1, width = 0) +
  geom_errorbar(aes(ymin = X25., ymax = X75.), size = 2, width = 0) +
  geom_point(size = 5, shape = 21, fill = "cadetblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_bw() +
  labs(x = "",
       y = "Effect on landings")

plot(periods, plotfun = "areas", pars = c("period1", "period2")) +
  labs(title = "Posterior distributions") +
  scale_y_discrete(labels = c("During MHW", "After MHW"))


re <- stan_gamm4(norm_value  ~ temp_mean, random = ~(1 | coop_name), data = reg_data)

feols(norm_landed_weight  ~ temp_mean, data = reg_data)
feols(norm_landed_weight  ~ temp_mean | coop_name, data = reg_data)
feols(norm_landed_weight  ~ temp_mean * coop_name, data = reg_data)

data %>% 
  filter(balanced) %>% 
  ggplot(aes(y = landed_weight / 1e3, x = temp_mean)) +
  geom_point() +
  theme_classic(base_size = 5) +
  stat_smooth(method = "lm", formula = 'y ~ x', se = F, fullrange = T) +
  facet_wrap(~coop_name, scales = "free_y")



