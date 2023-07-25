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
pacman::p_load(
  here
)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
mex_data_path <- "/Users/juancarlosvillasenorderbez/GitHub/data_mex_fisheries/data"
origin <- file.path(mex_data_path, "concesiones", "processed", "all_spp_permit_and_concessions_polygons.gpkg")
destiny <- here("data", "raw", "turf_polygons.gpkg")

file.copy(
  from = origin,
  to = destiny,
  overwrite = T
)
