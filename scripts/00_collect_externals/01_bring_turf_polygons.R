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

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
origin <- file.path(mex_data_path, "concesiones", "processed", "all_spp_permit_and_concessions_polygons.gpkg")
destiny <- here("data", "processed", "turf_polygons.gpkg")

file.copy(
  from = origin,
  to = destiny,
  overwrite = T
)
