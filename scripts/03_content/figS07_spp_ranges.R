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
  terra,
  tidyverse
)

# Load data --------------------------------------------------------------------

lob <- read_csv(here("data/raw/AquaMaps/lobster.csv"), skip = 12) %>% 
  janitor::clean_names() %>%
  select(lon = center_long, lat = center_lat, p = overall_probability)

cuc <- read_csv(here("data/raw/AquaMaps/cucumber.csv"), skip = 12) %>% 
  janitor::clean_names() %>%
  select(lon = center_long, lat = center_lat, p = overall_probability) 

red <- read_csv(here("data/raw/AquaMaps/red.csv"), skip = 12) %>% 
  janitor::clean_names() %>%
  select(lon = center_long, lat = center_lat, p = overall_probability) 

pur <- read_csv(here("data/raw/AquaMaps/purple.csv"), skip = 12) %>% 
  janitor::clean_names() %>%
  select(lon = center_long, lat = center_lat, p = overall_probability) 

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
mex <- rnaturalearth::ne_countries(country = c("Mexico", "United States of America", "Canada"), returnclass = "sf") %>% 
  sf::st_crop(xmin = -130,
              xmax = -100,
              ymin = 0,
              ymax = 60)

meow <- sf::st_read("../dissertation/transferable_conservation/clean_data/clean_meow.gpkg") %>%
  filter(ecoregion %in% c("Magdalena Transition", "Southern California Bight"))

plot_aquamaps <- function(data, lims = 25) {
  df <- as.data.frame(data, xy = T) %>% 
    set_names(c("x", "y", "p")) %>% 
    filter(between(x, -130, -100),
           between(y, 0, 60))
  
  min <- df %>% 
    filter(p >= 0.5) %>% 
    pull(y) %>% 
    min()
  
  print(min)
  
  ggplot() +
    geom_sf(data = meow, fill = "transparent") +
    geom_sf(data = sf::st_centroid(meow), color = "black") +
    geom_sf(data = mex) +
    geom_hline(yintercept = 25, color = "black") +
    geom_hline(yintercept = lims, color = "darkred", linetype = "dashed") +
    geom_hline(yintercept = min, color = "steelblue", linetype = "dashed") +
    geom_tile(data = df, 
              mapping = aes(x = x, y = y, fill = p >= 0.5)) +
    scale_fill_manual(values = c("darkred", "steelblue")) +
    theme_minimal(base_size = 7) +
    theme(legend.position = "None") +
    labs(x = NULL,
         y = NULL)
}

lob_plot <- plot_aquamaps(lob,
                         lims = c(23))
cuc_plot <- plot_aquamaps(cuc,
                          lims = c(27))
rur_plot <- plot_aquamaps(rur,
                          lims = c(32))
pur_plot <- plot_aquamaps(pur,
                          lims = c(NULL)) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 0.9),
        legend.justification = c(1, 1))


cowplot::plot_grid(lob_plot, cuc_plot, rur_plot, pur_plot, ncol = 4,
                   labels = c("Lobster", "Cucumber", "Red urchin", "Purple urchin"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------