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
  tidyverse
)

# Load data --------------------------------------------------------------------
# Probability of MHW
p_mhw_occurs_past <- readRDS(file = here("data", "output", "p_mhw_occurs.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
         ssp = "Historical",
         type = "Historical")

# p_mhw_occurs_hindcast <- readRDS(file = here("data", "output", "p_mhw_occurs_hindcast.rds")) %>% 
#   mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

p_mhw_occurs_future <- readRDS(file = here("data", "output", "p_mhw_occurs_future.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

con_p_mhw_threshold_past <- readRDS(file = here("data", "output", "con_p_mhw_threshold.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

con_p_mhw_threshold_future <- readRDS(file = here("data", "output", "con_p_mhw_threshold_future.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
         fishery = fct_relevel(fishery, c("Lobster", "Urchin", "Sea cucumber")))

img <- tibble(fishery = unique(p_mhw_occurs_past$fishery)) %>% 
  mutate(img = ifelse(fishery == "Urchin",
                      here("data", "img", paste0(fishery, ".png")),
                      here("data", "img", paste0(fishery, "_90.png"))))
  

## PROCESSING ##################################################################
p_mhw_occurs_future %>% 
  select(ssp, p_at_least_one) %>% 
  group_by(ssp) %>% 
  summarize_all(function(x){paste0(round(mean(x, na.rm = T), 3), "; ", round(sd(x, na.rm = T), 3))})

con_p_mhw_threshold_future %>% 
  select(ssp, mean) %>% 
  group_by(ssp) %>% 
  summarize_all(function(x){paste0(round(mean(x, na.rm = T), 3), "; ", round(sd(x, na.rm = T), 3))})


# X ----------------------------------------------------------------------------
p_mhw_occurs <- bind_rows(p_mhw_occurs_past,
                          # p_mhw_occurs_hindcast,
                          p_mhw_occurs_future) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")),
         fishery = fct_relevel(fishery, c("Lobster", "Urchin", "Sea cucumber")))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
my_jitter <- position_jitterdodge(jitter.width = 0.1,
                                  jitter.height = 0,
                                  dodge.width = 0.75,
                                  seed = 1)

# P at least one
p_at_least_one <- ggplot(
  data = p_mhw_occurs,
  mapping = aes(x = fishery, y = p_at_least_one,
                color = ssp, fill = ssp)) +
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
               position = my_jitter) +
  scale_color_manual(values = c("black", "royalblue3",
                                "orange2", "darkred")) +
  guides(color = guide_legend(ncol = 2)) +
  labs(x = "Fishery",
       y = "P(MHW Occurs)") +
  lims(y = c(0, 1)) +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0))

# P as big
p_as_big <- ggplot(data = con_p_mhw_threshold_future ,
                   mapping = aes(x = fishery, y = mean, color = ssp, fill = ssp)) +
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
               position = my_jitter) +
  scale_color_manual(values = c("royalblue3", "orange2", "darkred")) +
  labs(x = "Fishery",
       y = expression("P((Cum. Int. ">=MHW~Cum.~Int.[i]~") | MHW Occurs)"),
       color = "Scenario",
       fill = "Scenario") +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1))

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
  labs(x = "MHW Cum. Int.\n(°C x days)",
       y = expression("P((Cum. Int. ">=~" X) | MHW Occurs)"))


p1 <- plot_grid(p_at_least_one,
                p_as_big,
                ncol = 1,
                labels = "auto")



p_mhw_plot <- cowplot::plot_grid(p1, cond_p_past,
                                 ncol = 2,
                                 labels = c("", "c"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p_mhw_plot,
                    filename = "p_mhw_plot",
                    width = 18,
                    height = 12)



daily_sst_ts %>%
  filter(eu_rnpa == "0305000101") %>%
  mutate(day = lubridate::yday(t),
         year = lubridate::year(t),
         colored = ifelse(year %in% c(2014:2016),
                          as.character(year),
                          NA)) %>%
  ggplot(aes(x = day, y = temp)) +
  geom_line(aes(linewidth = year %in% c(2014:2016),
                group = year,
                color = colored),
            linewidth = 0.1) +
  stat_summary(geom = "ribbon",
               fun.data = mean_cl_normal,
               color = "red", alpha = 0.5)
