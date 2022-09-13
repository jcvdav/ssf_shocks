######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

processed <- filtered %>% 
  group_by(coop, main_species_group) %>% 
  mutate(landed_weight = (landed_weight - mean(landed_weight)) / sd(landed_weight),
         n = n()) %>% 
  filter(n > 5) %>% 
  ungroup() %>% 
  mutate(hw = case_when(year_cut <= 2013 ~ "0",
                        year_cut >= 2017 ~ "2",
                        T ~ "1")) %>% 
  group_by(main_species_group, hw) %>% 
  mutate(mean = mean(landed_weight)) %>% 
  ungroup()

ggplot(data = processed) +
  geom_rect(xmin= 2014, xmax = 2017, ymin = -3, ymax = 3, color = "black", fill = "gray80") +
  # geom_smooth(aes(x = year_cut, y = landed_weight, group = hw), method = "lm", color = "black") +
  geom_line(aes(x = year_cut, y = landed_weight, color = coop)) +
  geom_line(aes(x = year_cut, y = mean, group = hw)) +
  facet_wrap(~main_species_group, ncol = 2) +
  theme_bw() +
  labs(x = "Year",
       y = "Normalized landings (z-score)",
       color = "Cooperative")



m1 <- feols(landed_weight ~ hw, data = processed, split = ~main_species_group)
m2 <- feols(landed_weight ~ hw | coop, data = processed, split = ~main_species_group)

modelsummary(m1)
modelsummary(m2)

# fedecoop <- sf::st_read(dsn = file.path(data_path, "mex_fisheries", "fedecoop", "fedecoop_polygons.gpkg"))
