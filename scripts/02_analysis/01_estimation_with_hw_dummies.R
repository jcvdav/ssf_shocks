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
library(tidyverse)

# Load data
sst_ts <- readRDS(file = here("data", "processed", "annual_mean_sst_by_turf.rds"))
filtered <- readRDS(here::here("data", "processed", "filtered_landings.rds"))

# Process
processed <- filtered %>% 
  filter(year_cut >= 2003,
         main_species_group == "LANGOSTA") %>% 
  group_by(coop, main_species_group) %>% 
  mutate(norm_landed_weight = (landed_weight - mean(landed_weight)) / sd(landed_weight),
         norm_value = (value - mean(value)) / sd(value),
         n = n()) %>%
  filter(n > 5) %>%
  ungroup() %>% 
  mutate(hw = case_when(year_cut <= 2013 ~ "0",
                        year_cut >= 2017 ~ "2",
                        T ~ "1"),
         hw_long = case_when(year_cut <= 2013 ~ "Before HW",
                             year_cut >= 2017 ~ "After HW",
                             T ~ "During HW"),
         hw_long = fct_reorder(hw_long, year_cut))

data <- processed %>% 
  left_join(sst_ts, by = c("year_cut" = "year", "coop" = "coop_name"))

landings_ts <- ggplot(data = data) +
  geom_rect(xmin= 2013.5, xmax = 2017.5, ymin = -100, ymax = 500, fill = "gray") +
  geom_line(aes(x = year_cut,
                y = norm_landed_weight,
                color = coop)) +
  theme_bw() +
  labs(x = "Year",
       y = "Standardized\nlandings",
       color = "Cooperative") +
  theme(legend.position = "None") +
  guides(color = guide_legend(ncol = 2)) +
  scale_color_viridis_d()

value_ts <- ggplot(data = data) +
  geom_rect(xmin= 2013.5, xmax = 2017.5, ymin = -100, ymax = 500, fill = "gray") +
  geom_line(aes(x = year_cut,
                y = norm_value,
                color = coop)) +
  theme_bw() +
  labs(x = "Year",
       y = "Standardized\nrevenues",
       color = "Cooperative") +
  theme(legend.position = "None") +
  guides(color = guide_legend(ncol = 2)) +
  scale_color_viridis_d()


fishery_ts <- plot_grid(landings_ts, value_ts, ncol = 1)

startR::lazy_ggsave(fishery_ts,
                    filename = "fishery_ts",
                    width = 9,
                    height = 7)










m1 <- feols(norm_landed_weight ~ mean_sst + year_cut, data = data, split = ~coop)

#(exp(𝛽1)−1)⋅100
modelsummary(m1, stars = T)

res <- map_dfr(m1, tidy, .id = "coop") %>% 
  filter(term == "mean_sst") %>% 
  mutate(string = paste(
    "Slope = ",
    formatC(estimate, digits = 3),
    "\np = ",
    formatC(p.value, digits = 2)
  )) %>% 
  mutate(coop = str_remove(coop, "sample.var: coop; sample: "),
         coop = fct_reorder(coop, estimate))

data2 <- data %>% 
  left_join(res, by = "coop") %>% 
  mutate(coop = fct_reorder(coop, estimate)) %>% 
  select(mean_sst, norm_landed_weight, landed_weight, hw, hw_long, coop)

panel <- ggplot(data2, aes(x = mean_sst, y = norm_landed_weight)) + 
  geom_smooth(method = "lm", se = T, color = "black", linetype = "dashed", size = 0.5) +
  geom_point(aes(fill = hw_long), shape = 21, color = "black", size = 2) +
  geom_text(data = res, x = 21.5, y = 2, aes(label = string), size = 2) +
  facet_wrap(~coop, ncol = 4) +
  labs(x = "Mean annual SST (°C)",
       y = "Standardized landed weight",
       fill = "Period") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(legend.position = c(1, 0.05),
        legend.justification = c(1, 0),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 6)) +
  guides(fill = guide_legend(title.position = "top"))

startR::lazy_ggsave(panel,
                    filename = "panel_figure",
                    width = 16,
                    height = 12)



# fedecoop <- sf::st_read(dsn = file.path(data_path, "mex_fisheries", "fedecoop", "fedecoop_polygons.gpkg"))

# SST regressions
sst_m1 <- feols(list(norm_landed_weight, log(landed_weight)) ~ mean_sst, data = data, cluster = "coop")
sst_m2 <- feols(list(norm_landed_weight, log(landed_weight)) ~ mean_sst + year_cut, data = data, cluster = "coop")
sst_m3 <- feols(list(norm_landed_weight, log(landed_weight)) ~ mean_sst + year_cut | coop, data = data)

sst_norm <- list(sst_m1[[1]],
             sst_m2[[1]],
             sst_m3[[1]]) %>% 
  set_names("Normalized landings")

sst_log <- list(sst_m1[[2]],
            sst_m2[[2]],
            sst_m3[[2]]) %>% 
  set_names("log(landings)")

modelsummary(sst_norm,
             stars = T)
             
modelsummary(sst_log,
             stars = T, exponentiate = T)

# Dummy regressions

dmy_m1 <- feols(list(norm_landed_weight, log(landed_weight)) ~ hw, data = data, cluster = "coop")
dmy_m2 <- feols(list(norm_landed_weight, log(landed_weight)) ~ hw + year_cut, data = data, cluster = "coop")
dmy_m3 <- feols(list(norm_landed_weight, log(landed_weight)) ~ hw + year_cut | coop, data = data)

dmy_norm <- list(dmy_m1[[1]],
                 dmy_m2[[1]],
                 dmy_m3[[1]]) %>% 
  set_names("Normalized landings")

dmy_log <- list(dmy_m1[[2]],
                dmy_m2[[2]],
                dmy_m3[[2]]) %>% 
  set_names("log(landings)")

# Preferred
modelsummary(dmy_norm,
             stars = T)

modelsummary(dmy_log,
             stars = T)


# After exploring, lets etimate
p_m1 <- feols(list(log(landed_weight), log(value)) ~ hw, data = data, cluster = "coop")
p_m2 <- feols(list(log(landed_weight), log(value)) ~ hw + year_cut, data = data, cluster = "coop")
p_m3 <- feols(list(log(landed_weight), log(value)) ~ hw + year_cut | coop, data = data)


p_land <- list(p_m1[[1]],
               p_m2[[1]],
               p_m3[[1]]) %>% 
  set_names("Landings")

p_val <- list(p_m1[[2]],
              p_m2[[2]],
              p_m3[[2]]) %>% 
  set_names("Revenues")

modelsummary(p_land, stars = T, exponentiate = T)
modelsummary(p_val, stars = T, exponentiate = T)


effect_size <- list(p_land[[3]], p_val[[3]]) %>% 
  set_names(c("Landings", "Revenues")) %>% 
  map_df(tidy, .id = "dep.var") %>% 
  filter(term %in% c("hw1", "hw2")) %>% 
  mutate(term = case_when(term == "hw1" ~ "During HW",
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
                          "year_cut" = "Time trend"))
