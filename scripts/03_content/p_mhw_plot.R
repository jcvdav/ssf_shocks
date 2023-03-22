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

# Load data --------------------------------------------------------------------
p_mhw_occurs <- readRDS(file = here("data", "output", "p_mhw_occurs.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

con_p_mhw_threshold <- readRDS(file = here("data", "output", "con_p_mhw_threshold.rds")) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " ")))

img <- tibble(fishery = unique(p_mhw_occurs$fishery)) %>% 
  mutate(img = ifelse(fishery == "Urchin",
                      here("data", "img", paste0(fishery, ".png")),
                      here("data", "img", paste0(fishery, "_90.png"))))
  
## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p1 <- ggplot(data = p_mhw_occurs,
             mapping = aes(x = fishery,
                           y = p_at_least_one)) +
  geom_jitter(color = "cadetblue",
              size = 0.75,
              alpha = 0.75,
              width = 0.2,
              height = 0) +
  stat_summary(geom = "linerange",
               fun.data = mean_cl_normal,
               color = "#E41A1C",
               linewidth = 0.5) +
  stat_summary(geom = "pointrange",
               fun.data = mean_se,
               fatten = 4,
               linewidth = 2) +
  labs(y = "P(MHW Occurs)") +
  theme(axis.title.x = element_blank())


p2 <- ggplot(data = con_p_mhw_threshold, 
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
  geom_image(data = img,
             mapping = aes(image = img),
             x = 900,
             y = 0.9,
             inherit.aes = F,
             size = 0.2) +
  facet_wrap(~fishery, ncol = 3) +
  labs(x = "MHW Cum. Int.\n(°C x days)",
       y = expression("P((Cum. Int. ">=~" X) | MHW Occurs)"))

p_mhw_plot <- cowplot::plot_grid(p1, p2,
                                 ncol = 1,
                                 labels = "AUTO", rel_heights = c(0.8, 1))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p_mhw_plot,
                    filename = "p_mhw_plot",
                    width = 18,
                    height = 12)
