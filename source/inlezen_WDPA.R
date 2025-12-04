library(sf)
library(qgisprocess)
library(tidyverse)
library(here)
library(mapview)

polygons <- st_read(here("data", "WDPA_WDOECM_May2025_Public_BEL_shp-polygons.shp"))

mapview(polygons)
qgisprocess::qgis_algorithms() %>% View

