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
  filter(year_cut >= 2003) %>% 
  group_by(coop, main_species_group) %>% 
  mutate(landed_weight = (landed_weight - mean(landed_weight)) / sd(landed_weight),
         n = n()) %>%
  filter(n > 5) %>%
  ungroup() %>% 
  mutate(hw = case_when(year_cut <= 2013 ~ "Before HW",
                        year_cut >= 2017 ~ "After HW",
                        T ~ "During HW"),
         hw = fct_reorder(hw, year_cut)) %>% 
  filter(main_species_group == "LANGOSTA")


data <- processed %>% 
  left_join(sst_ts, by = c("year_cut" = "year", "coop")) %>% 
  filter(!coop == "Ensenada")

ggplot(data = data) +
  geom_rect(xmin= 2013.5, xmax = 2017.5, ymin = -100, ymax = 500, fill = "gray") +
  geom_line(aes(x = year_cut,
                y = landed_weight, color = coop)) +
  theme_bw() +
  labs(x = "Year",
       y = "Standardized landed weight",
       color = "Cooperative") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2)) +
  scale_color_brewer(palette = "Set1")


library(fixest)
library(modelsummary)

m1 <- feols(landed_weight ~ mean_sst + year_cut, data = data, split = ~coop)
m2 <- feols(landed_weight ~ mean_sst, data = data)
m3 <- feols(landed_weight ~ mean_sst + year_cut, data = data)
m4 <- feols(landed_weight ~ mean_sst + year_cut | coop, data = data)

#(exp(𝛽1)−1)⋅100
modelsummary(m1, stars = T)
modelsummary(list(m2, m3, m4), stars = T)

res <- map_dfr(m1, tidy, .id = "coop") %>% 
  filter(term == "mean_sst") %>% 
  mutate(string = paste(
    "Slope = ",
    formatC(estimate, digits = 3),
    "\np = ",
    formatC(p.value, digits = 2)
  ))

ggplot(data, aes(x = mean_sst, y = landed_weight)) + 
  geom_smooth(method = "lm", se = T, color = "black", linetype = "dashed") +
  geom_point(aes(fill = hw), shape = 21, color = "black", size = 2) +
  geom_text(data = res, x = 21.5, y = 2, aes(label = string), size = 3) +
  facet_wrap(~coop) +
  labs(x = "Mean annual SST (°C)",
       y = "Standardized landed weight",
       fill = "Period") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.position = c(0.9, 0),
        legend.justification = c(1, 0),
        strip.background = element_blank())




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

