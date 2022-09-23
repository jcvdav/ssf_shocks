######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(sf)
library(raster)
library(tidyverse)

r <- tibble(
  file = list.files(here::here("data", "raw", "sst", "erdMWsst_monthly"),
                    pattern = "*.nc",
                    full.names = T),
  year = str_extract(string = file,
                     pattern = "[:digit:]+")) %>% 
  mutate(sst = map(file, brick),
         sst = map(sst, rotate))

my_extract <- function(b, poly) {
  # browser()
  raster::extract(b, poly, fun = mean, na.rm = T, df = T) %>% 
    pivot_longer(cols = c(2:ncol(.)),
                 names_to = "date",
                 values_to = "mean_sst")
}

a <- r %>% 
  mutate(data = map(sst, .f = ~my_extract(b = .x, poly = fedecoop))) %>% 
  select(year, data) %>% 
  unnest(data) %>% 
  mutate(date = str_extract(date, "[:digit:]{4}.[:digit:]{2}.[:digit:]{2}"),
         date = lubridate::ymd(str_replace_all(date, "\\.", "-"))) %>% 
  left_join(st_drop_geometry(fedecoop), by = c("ID" = "objectid")) %>% 
  select(date, coop, mean_sst)


saveRDS(object = a,
        file = here::here("data", "processed", "monthly_sst.rds"))


ggplot(data = a) +
  geom_rect(xmin = ymd("2014-01-01"), xmax = ymd("2017-12-12"), ymin = 10, ymax = 30, color = "black", fill = "gray80") +
  geom_line(mapping = aes(x = date, y = mean_sst, group = coop)) +
  theme_bw()


# download.file(url = "https://opendap.earthdata.nasa.gov/providers/POCLOUD/collections/GHRSST%20Level%204%20MUR%20Global%20Foundation%20Sea%20Surface%20Temperature%20Analysis%20(v4.1)/granules/20020602090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1",
#               destfile = "MUR_JPL_L4_GLOB_rstudio.nc.nc4")
# 
# download.file("http://podaac-opendap.jpl.nasa.gov/opendap/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/2009/009/20090109090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc.nc4?lat[0:1:17998],lon[0:1:35999],analysed_sst[0:1:0][0:1:17998][0:1:35999]",
#               "MUR_JPL_L4_GLOB_rstudio.nc.nc4")






fedecoop <- sf::st_read(dsn = file.path(data_path, "mex_fisheries", "fedecoop", "fedecoop_polygons.gpkg")) %>% 
  st_transform(crs = 4326)

raster(list.files(here::here("data", "sst", "requested_files"), full.names = T)[1]) %>% 
  as.data.frame(xy = T) %>% 
  drop_na(Sea.Surface.Temperature) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Sea.Surface.Temperature)) +
  geom_sf(data = fedecoop)

  


rr <- raster(list.files(here::here("data", "sst", "test"), pattern = ".nc", full.names = T)[1], varname = "sst") %>% 
  crop(extent(-118, -109, 21, 35))

plot(rr)
plot(fedecoop[,1], add = T)






# 1) Convertir datos de mensual a estacional (cuales son los grupo )
# 2) Generar el grid estandar de SST
# 3) Anomalia de datos
# 4) Link del kelp


# Y_i = Kelp
# T = antes, durante, o despues
# Status del pixel: full, partial, no protection, concesion, permisionario
# Controles: SST, Wave exposure, slope / gradiente (distancia a la linea de 200 m)
# Ano
# Estacion