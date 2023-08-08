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
  cowplot,
  sf,
  tidyverse
)

# Load data --------------------------------------------------------------------
# Probability of MHW
p_mhw_occurs_past <- readRDS(file = here("data", "output", "p_mhw_occurs.rds")) %>% 
  mutate(ssp = "Historical",
         type = "Historical")

p_mhw_occurs_future <- readRDS(file = here("data", "output", "p_mhw_occurs_future.rds"))

con_p_mhw_threshold_past <- readRDS(file = here("data", "output", "con_p_mhw_threshold.rds"))

con_p_mhw_threshold_future <- readRDS(file = here("data", "output", "con_p_mhw_threshold_future.rds"))

centroids <- st_read(dsn = here("data", "processed", "centroids.gpkg")) %>% 
  bind_cols(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(lon = X,
         lat = Y)

## PROCESSING ##################################################################
# X ----------------------------------------------------------------------------
p_mhw_occurs <- bind_rows(p_mhw_occurs_past,
                          p_mhw_occurs_future) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
         fishery = fct_relevel(fishery, c("Lobster", "Urchin", "Sea cucumber")),
         ssp = case_when(ssp == "ssp126" ~ "SSP1-2.6",
                         ssp == "ssp245" ~ "SSP2-4.5",
                         ssp == "ssp585" ~ "SSP5-8.5",
                         T ~ "Historical"))

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

# X ----------------------------------------------------------------------------
change_in_p <- bind_rows(p_mhw_occurs_past,
                         p_mhw_occurs_future) %>% 
  left_join(centroids, by = c("eu_rnpa", "fishery")) %>% 
  select(eu_rnpa, fishery, p_at_least_one, ssp, lat) %>% 
  pivot_wider(names_from = ssp, values_from = p_at_least_one) %>%
  mutate(delta_ssp126 = ssp126 - Historical,
         delta_ssp245 = ssp245 - Historical,
         delta_ssp585 = ssp585 - Historical) %>% 
  select(eu_rnpa, fishery, lat, contains("delta")) %>% 
  pivot_longer(cols = contains("delta"),
               names_to = "ssp",
               values_to = "delta") %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
         fishery = fct_relevel(fishery, c("Lobster", "Urchin", "Sea cucumber")),
         ssp = str_remove(ssp, "delta_"),
         ssp = case_when(ssp == "ssp126" ~ "SSP1-2.6",
                         ssp == "ssp245" ~ "SSP2-4.5",
                         ssp == "ssp585" ~ "SSP5-8.5"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
my_jitter <- position_jitterdodge(jitter.width = 0.2,
                                  jitter.height = 0,
                                  dodge.width = 0.75,
                                  seed = 1)

# P at least one
p_at_least_one <- ggplot(
  data = p_mhw_occurs,
  mapping = aes(x = fishery,
                y = p_at_least_one,
                color = ssp,
                fill = ssp,
                shape = fishery)) +
  geom_point(aes(group = ssp),
             size = 1,
             alpha = 0.5,
             position = my_jitter) + 
  stat_summary(geom = "linerange",
               fun.data = mean_cl_normal,
               color = "gray10",
               linewidth = 0.5,
               position = my_jitter) +
  stat_summary(geom = "pointrange",
               fun.data = mean_se,
               fatten = 4,
               linewidth = 2,
               position = my_jitter,
               color = "gray10") +
  scale_color_manual(values = c("black", ssp_palette)) +
  scale_fill_manual(values = c("black", ssp_palette)) +
  scale_shape_manual(values = c(21, 22, 24)) +
  guides(color = guide_legend(ncol = 2,
                              override.aes=list(shape = 21,
                                                size = 0.9)),
         shape = "none") +
  labs(x = "Fishery",
       y = "P(MHW Occurs)",
       fill = "Scenario",
       color = "Scenario") +
  lims(y = c(0.6, 1)) +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0))

# P as big
p_as_big <- ggplot(data = con_p_mhw_threshold_future,
                   mapping = aes(x = fishery,
                                 y = mean,
                                 color = ssp,
                                 fill = ssp,
                                 shape = fishery)) +
  geom_hline(yintercept = 1/40,
             linetype = "dashed") +
  geom_point(aes(group = ssp),
             size = 1,
             alpha = 0.5,
             position = my_jitter) + 
  stat_summary(geom = "linerange",
               fun.data = mean_cl_normal,
               color = "gray10",
               linewidth = 0.5,
               position = my_jitter) +
  stat_summary(geom = "pointrange",
               fun.data = mean_se,
               fatten = 4,
               linewidth = 2,
               position = my_jitter,
               color = "gray10") +
  scale_color_manual(values = ssp_palette) +
  scale_fill_manual(values = ssp_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  guides(color = guide_legend(ncol = 2),
         shape = NULL) +
  labs(x = "Fishery",
       y = expression("P((Cum. Int. ">="hist Cum. Int.) | MHW Occurs)"),
       color = "Scenario",
       fill = "Scenario") +
  theme(legend.position = "None")

delta_p <- ggplot(change_in_p,
                  mapping = aes(x = lat,
                                y = delta,
                                fill = ssp,
                                group = ssp)) +
  geom_point(aes(shape = fishery)) +
  scale_fill_manual(values = ssp_palette) +
  scale_shape_manual(values = c(21, 22, 24)) +
  facet_grid(fishery ~ ssp) +
  geom_smooth(aes(group = ssp),
              method = "lm",
              color = "black",
              linetype = "dashed") +
  guides(shape = guide_legend(title = "Fishery"),
         fill = "none") +
  labs(x = "Latitude (Centroid)",
       y = "Change in P(MHW Occurs)") +
  theme(legend.position = "None")

p1 <- plot_grid(p_at_least_one,
                p_as_big,
                ncol = 1,
                labels = c("a", "c"),
                label_x = 0.9)

p2 <- plot_grid(p1,
          delta_p,
          ncol = 2,
          rel_widths = c(1, 1.5),
          labels = c("", "b"),
          label_x = 0.9)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p2,
                    filename = "03_future_mhw_plot",
                    width = 19,
                    height = 12)

# In case we want to add the d/MHW(PDF)
cond_p_past <- ggplot(data = con_p_mhw_threshold_past, 
                      mapping = aes(x = threshold, y = cond_p)) +
  geom_line(aes(group = eu_rnpa),
            linewidth = 0.1,
            color = "cadetblue") +
  stat_summary(geom = "ribbon",
               fill = "#E41A1C",
               alpha = 0.5,
               fun.data = mean_cl_normal) +
  stat_summary(geom = "line",
               fun = "mean",
               linewidth = 0.5) +
  facet_wrap(~fishery, ncol = 1) +
  labs(x = "MHW Cum. Int. (°C days)",
       y = expression("P((Cum. Int. ">=~" X) | MHW Occurs)"))

p_mhw_plot <- cowplot::plot_grid(p1, cond_p_past,
                                 ncol = 2,
                                 labels = c("", "c"),
                                 label_x = 0.9)

