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
  rnaturalearth,
  sf,
  rmapshaper,
  tidyverse
)

# Load data --------------------------------------------------------------------
lob <- rast(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Panulirus_interruptus.tif")) 
# red <- raster(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Mesocentrotus_franciscanus.tif")) 
pur <- rast(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Strongylocentrotus_purpuratus.tif")) %>% 
  crop(lob)
pep <- rast(here("../dissertation/transferable_conservation/raw_data/thredds_aquamaps_rasters/Parastichopus_parvimensis.tif")) %>% 
  crop(lob)

turfs <- st_read(here("data", "processed", "turf_polygons.gpkg")) %>% 
  rmapshaper::ms_simplify(keep_shapes = T) %>% 
  mutate(fishery = str_to_sentence(str_replace(fishery, "_", " "))) %>% 
  rename(species = fishery)
  

mex <- ne_countries(scale = "large", country = c("Mexico", "United States of America"), returnclass = "sf") %>% 
  st_crop(lob)

my_df <- function(r){
  as.data.frame(r, xy = T) %>% 
    magrittr::set_colnames(c("x", "y", "layer"))
}

df <- list(lob, pur, pep) %>% 
  set_names(c("Lobster", "Urchin", "Sea cucumber")) %>% 
  map_dfr(my_df, .id = "species") %>% 
  drop_na() %>% 
  # mutate(species = case_when(species == 1 ~ "P. interruptus",
                             # species == 2 ~ "S. purpuratus",
                             # species == 3 ~ "P. parvimensis")) %>% 
  filter(layer > 0.5) %>% 
  filter(between(y, 23, 35.5), between(x, -121, -111))

img <- tibble(species = sort(unique(df$species)),
              img = here("data", "img", paste0(species, ".png")))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

maps <- ggplot() +
  geom_tile(data = df,
            mapping = aes(x = x, y = y),
            fill = "cadetblue") +
  geom_sf(data = mex) +
  geom_sf(data = turfs) +
  # geom_image(data = img,
             # mapping = aes(x = -113, y = 34, image = img),
             # inherit.aes = F,
             # size = 0.2, by = "height") +
  geom_hline(yintercept = 25) +
  facet_wrap(~species) +
  # scale_fill_viridis_c(limits = c(0.5, 1), option = "B") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # cowplot::theme_map() +
  theme(legend.justification = c(0, 0),
        legend.position = c(0, 0),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(title = "p",
                               frame.colour = "black",
                               ticks.colour = "black"))

dists <- df %>% 
  group_by(species, y) %>% 
  summarize(layer = median(layer, na.rm = T)) %>% 
  ggplot(aes(x = y, y = layer, group = species)) + 
  geom_smooth(color = "red", linetype = "dashed", method = "lm", formula = y ~ poly(x, 2)) +
  geom_smooth(color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_point(shape = 21, color = "black", fill = "navyblue", alpha = 0.5, size = 4) +
  facet_wrap(~species, scales = "free_x") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  theme(strip.text = element_blank()) +
  labs(x = "Latitude", y = "p")

p <- plot_grid(maps, dists, rel_heights = c(2, 1), ncol = 1, align = "v", labels = "AUTO")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "spp_dists",
                    width = 20,
                    height = 15)