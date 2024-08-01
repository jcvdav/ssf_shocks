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
  sf,
  ggimage,
  cowplot,
  magrittr,
  tidyverse
)

# Load data --------------------------------------------------------------------
coef_data <- readRDS(file = here("data", "output", "effect_on_fishery_and_biophysical.rds"))


scatter_plot <- function(data, variable, lab, img = F) {
  p <- data %>% 
    ggplot(aes(x = {{variable}},
               y = estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.5) + 
    geom_smooth(method = "lm", linetype = "dashed", linewidth = 0.5, color = "black") +
    geom_errorbar(aes(ymin = estimate + std.error,
                      ymax = estimate - std.error),
                  width = 0) +
    geom_point(aes(fill = p_fill,
                   size = live_weight / 1e3,
                   shape = fishery)) +
    scale_fill_gradientn(colours = ipcc_temp) +
    scale_shape_manual(values = c(21, 22, 23)) +
    labs(x = lab,
         y = expression(hat(beta[i]))) +
    guides(fill = guide_colorbar(title = expression(hat(beta[i])),
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 direction = "horizontal"),
           size = guide_legend(title = "Mean landings before MHW regime",
                               title.position = "top",
                               direction = "horizontal"),
           shape = guide_legend(title = "Fishery",
                                title.position = "top",
                                direction = "horizontal", override.aes = list(size = 3))) +
    theme(legend.box = "horizontal",
          legend.justification="center",
          legend.box.background = element_rect(fill = "white", color = "transparent"))
  
  if(img) {
    p <- p + 
      geom_image(data = select(data, fishery, img) %>% distinct(),
                 mapping = aes(image = img), 
                 x = c(32, 32, 31.5),
                 y = c(0.65, 0.95, 0.4),
                 inherit.aes = F,
                 size = 0.15)
  }
  
  return(p)
}

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
# Need to fiddle with eh coordinates for the images
lat <- scatter_plot(coef_data, lat_dist, "Distance from 25°N ('00 Km)")
leg <- get_legend(lat)
lat <- lat + 
  theme(legend.position = "None") +
  annotate(geom = "text",
           x = 1.5, y = -1.1,
           label = "Southern TURFs",
           size = 2) +
  annotate(geom = "text",
           x = 7, y = -1.1,
           label = "Northern TURFs",
           size = 2)
temp_cv <- scatter_plot(coef_data, temp_cv, expression("Variation in SST"~(CV[i]))) +
  theme(legend.position = "None") +
  annotate(geom = "text",
           x = 0.03, y = -1.1,
           label = "Less variable SST",
           size = 2) +
  annotate(geom = "text",
           x = 0.042, y = -1.1,
           label = "More variable SST",
           size = 2)
land_cv <- scatter_plot(coef_data, live_weight_cv, expression("Variation in landings"~(CV[i]))) +
  theme(legend.position = "None") +
  annotate(geom = "text",
           x = 0.4, y = -1.1,
           label = "Less variable landings",
           size = 2) +
  annotate(geom = "text",
           x = 1, y = -1.1,
           label = "More variable landings",
           size = 2)

plots <- plot_grid(lat,
                   temp_cv,
                   land_cv,
                   ncol = 3,
                   align = "hv",
                   labels = c("auto"))

p <- plot_grid(plots, leg,
               rel_heights = c(1, 0.2),
               align = "hv",
               axis = "l",
               ncol = 1)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "fig05_biophysical_vs_effect",
                    width = 20,
                    height = 9)

