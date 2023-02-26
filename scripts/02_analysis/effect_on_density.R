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
library(here)
library(fixest)
library(ggimage)
library(cowplot)
library(heatwaveR)
library(tidyverse)

# Load data --------------------------------------------------------------------
mhw_data <- readRDS(here("data", "processed", "mhw_by_turf.rds")) %>% 
  filter(eu_rnpa == "0301000089",
         fishery == "lobster") 

data <- readRDS(here("data", "estimation_panels", "env_eco_panel.rds")) %>% 
  filter(eu_rnpa == "0301000089") %>% 
  mutate(fishery = str_to_sentence(str_replace_all(fishery, "_", " ")))

img <- tibble(fishery = sort(unique(data$fishery)),
              img = here("data", "img", paste0(fishery, ".png")))

## ESTIMATION ##################################################################

gof_omits <- "Adj|With|RMS|Std|FE"
top <- feols(norm_density ~ norm_mhw_int_cumulative | site, data = data,
             split = ~ fishery, vcov = "DK", panel.id = ~site + year) %>% 
  modelsummary::modelsummary(stars = T,
                             coef_rename = c("norm_mhw_int_cumulative" = "Cumulative intensity",
                                             "period1" = "During",
                                             "period2" = "After"),
                             output = "data.frame",
                             gof_omit = gof_omits)

bottom <- feols(norm_density ~ period | site, data = data,
                split = ~ fishery, vcov = "DK", panel.id = ~site + year) %>% 
  modelsummary::modelsummary(stars = T,
                             coef_rename = c("norm_mhw_int_cumulative" = "Cumulative intensity",
                                             "period1" = "During",
                                             "period2" = "After"),
                             output = "data.frame",
                             gof_omit = gof_omits)

bind_rows(top, bottom) %>% 
  mutate(term = ifelse(statistic == "std.error", "", term)) %>%
  select(matches("term|fishery")) %>% 
  kableExtra::kbl(booktabs = T,
                  col.names = c("", "Lobster", "Sea cucumber", "Urchin"),
                  caption = "Effect of cumulative marine heatwave intensity
                  (panel A)and marine heatwave regime (panel B) on densities of
                  lobster, sea cucumber, and urchin surveyed in two no-take
                  marine reserves in Isla Natividad. In all cases the outcome
                  variable is standard-normalized density (historical mena
                  removed and scaled by standard deviation). Cummulative MHW
                  intensity in panel A is also standard-normalized, and
                  regressors in panel B are dummies for During MHW and
                  After MHW regimes.") %>% 
  kableExtra::pack_rows("A)", 1, dim(top)[1]) %>% 
  kableExtra::pack_rows("B)", dim(top)[1] + 1, dim(bottom)[1]) %>% 
  kableExtra::add_footnote(notation = "none",
                           "Note: All models include site-level fixed
                           effects and use Driscoll-Kraay standard errors")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

mhw_ts_long <- ggplot(data = mhw_data$mhw[[1]]$event,
                      mapping = aes(x = date_start,
                                    y = intensity_cumulative,
                                    fill = intensity_max,
                                    color = intensity_max)) +
  geom_rect(xmin = lubridate::ym("2014-01"), xmax = lubridate::ym("2017-12"),
            ymin = -Inf, ymax = Inf, fill = "gray", color = "transparent") +
  geom_vline(xintercept = c(lubridate::ymd(c("2006-01-01", "2021-12-31"))),
             color = "black",
             linetype = "dashed") +
  geom_linerange(aes(ymin = 0, ymax = intensity_cumulative)) +
  geom_point(shape = 21, size = 3, color = "black") +
  geom_text(x = lubridate::ym("1992-01"),
            y = 350,
            label = "'91-'92 El Niño",
            color = "black",
            inherit.aes = F) +
  geom_text(x = lubridate::ym("1998-01"),
            y = 550,
            label = "'97-'98 El Niño",
            color = "black",
            inherit.aes = F) +
  scale_fill_gradientn(colours = wesanderson::wes_palette(name = "Zissou1",
                                                          type = "continuous")) +
  scale_color_gradientn(colours = wesanderson::wes_palette(name = "Zissou1",
                                                          type = "continuous")) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "Year",
       y = expression(paste("Cumulative intensity (", degree, "C days)")),
       fill = expression(paste("Maximum intensity (", degree, "C)")),
       color = expression(paste("Maximum intensity (", degree, "C)"))) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))
  
baseline_rec <- function(data){
  data %>% 
    filter(!period == 1) %>% 
    group_by(year, period_long) %>% 
    summarize(mean_density = mean(density)) %>% 
    ungroup() %>% 
    filter(mean_density > 0) %>% 
    mutate(hist_min = min(mean_density[period_long == "Before MHW"])) %>% 
    filter((mean_density > hist_min & period_long == "After MHW") |
             (mean_density == hist_min & period_long == "Before MHW")) %>% 
    group_by(period_long) %>% 
    slice_min(year) %>% 
    ungroup()  
}

rec_data <- data %>% 
  group_by(fishery) %>% 
  nest() %>% 
  mutate(recs = map(data, baseline_rec)) %>% 
  select(-data) %>% 
  unnest(recs)

eco_ts <- ggplot(data = data, aes(group = fishery)) +
  geom_rect(xmin= 2013.5, xmax = 2017.5, ymin = -Inf, ymax = Inf, fill = "gray") +
  stat_summary(mapping = aes(x = year, y = density),
               geom = "line", fun = mean, color = "gray10") +
  stat_summary(mapping = aes(x = year, y = density,
                             fill = period_long,
                             shape = fishery),
               geom = "pointrange", fun.data = mean_se, color = "gray10") +
  geom_point(data = rec_data, aes(x = year, y = mean_density, color = period_long),
             shape = 21, size = 5) +
  geom_point(data = rec_data, aes(x = year, y = mean_density, color = period_long),
             shape = 21, size = 4) +
  scale_x_continuous(breaks = seq(2006, 2020, by = 2), labels = seq(2006, 2020, by = 2)) +
  scale_fill_manual(values = period_palette) +
  scale_color_manual(values = period_palette[c(1, 3)], guide = "none") +
  scale_shape_manual(values = c(21, 22, 23)) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.direction = "horizontal") +
  labs(x = "Year",
       y = bquote(Density~(org.~m^-1)),
       fill = "Period",
       shape = "Species")


c_mhw <- ggplot(data = data,
                mapping = aes(x = norm_mhw_int_cumulative, y = norm_density, shape = fishery)) +
  geom_image(data = img,
             mapping = aes(image = img),
             x = 5,
             y = c(0.35, 0.5, 1), 
             inherit.aes = F,
             size = 0.2, by = "height") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  stat_summary(aes(fill = period_long, group = year),
               geom = "pointrange", fun.data = mean_se, color = "gray10") +
  scale_fill_manual(values = period_palette) +
  scale_shape_manual(values = c(21, 22, 23)) +
  labs(x = expression(paste("Standard-normalized cumulative intensity")),
       y = "Standard-normalized density") +
  facet_wrap(~fishery, scales = "free_y") +
  theme(legend.position = "None",
        strip.text = element_text(hjust = 0))
  

p <- plot_grid(mhw_ts_long, eco_ts, c_mhw, ncol = 1, rel_heights = c(1, 1, 1), labels = "AUTO")

startR::lazy_ggsave(plot = p,
                    filename = "env_ecol",
                    width = 20,
                    height = 16)

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

