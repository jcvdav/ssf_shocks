######################################################
#title#
######################################################
# 
# Purpose
#
######################################################
library(modelsummary)
library(fixest)
library(tidyverse)

# Load data
sst <- readRDS(here::here("data", "processed", "monthly_sst.rds"))
filtered <- readRDS(here::here("data", "processed", "filtered_landings.rds"))

# Process
processed <- filtered %>% 
  group_by(coop, main_species_group) %>% 
  mutate(landed_weight = (landed_weight - mean(landed_weight)) / sd(landed_weight),
         n = n()) %>% 
  filter(n > 5) %>% 
  ungroup() %>% 
  mutate(hw = case_when(year_cut <= 2013 ~ "0",
                        year_cut >= 2017 ~ "2",
                        T ~ "1"))


data <- processed #%>% 
  # left_join(sst, by = "coop")

ggplot(data = data) +
  geom_rect(xmin= 2014, xmax = 2017, ymin = -5, ymax = 5, color = "black", fill = "gray80") +
  # geom_smooth(aes(x = year_cut, y = landed_weight, group = hw), method = "lm", color = "black") +
  geom_line(aes(x = year_cut,
                y = landed_weight / 1e3, color = coop)) +
  # geom_line(aes(x = year_cut, y = mean, group = hw)) +
  facet_wrap(~main_species_group, ncol = 2, scales = "free_y") +
  theme_bw() +
  labs(x = "Year",
       y = "Normalized landings (z-score)",
       color = "Cooperative") +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0)) +
  guides(color = guide_legend(ncol = 2)) +
  scale_color_brewer(palette = "Set1")


library(fixest)
library(modelsummary)
m1 <- feols(log(landed_weight) ~ hw + year_cut, data = processed, split = ~main_species_group)
m2 <- feols(log(landed_weight) ~ hw + year_cut| coop, data = processed, split = ~main_species_group)
#(exp(𝛽1)−1)⋅100
modelsummary(m1, stars = T)
modelsummary(m2, stars = T, coef_map = c("hw1" = "During HW",
                                         "hw2" = "After HW",
                                         "year_cut" = "Trend"))

# fedecoop <- sf::st_read(dsn = file.path(data_path, "mex_fisheries", "fedecoop", "fedecoop_polygons.gpkg"))

to_pct <- function(x){
  (exp(x) - 1)
}

map_dfr(m2, broom::tidy, .id = c("species")) %>% 
  filter(term %in% c("hw1", "hw2")) %>% 
  # mutate(estimate = to_pct(estimate),
         # std.error = to_pct(std.error)) %>% 
  ggplot(aes(x = term, y = estimate, color = species)) +
  geom_pointrange(aes(ymin = estimate-std.error,
                      ymax = estimate+std.error),
                  position = position_dodge(width = 0.1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(labels = c("During HW", "After HW")) +
  labs(x = "When", y = "Estimate")

