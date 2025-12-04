
pa_ccda_nat2000_2024 <- st_read("C:/Users/emiel_delombaerde/Documents/R/ProtConn/data/ccda_nat2000/clip_evolutie_2000_2024.shp") %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>%
  st_make_valid()

vlaanderen_crs <- st_read("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp") %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>%
  st_make_valid()
sf_use_s2(FALSE) # Voor robuustheid

### area check

pa_ccda_nat2000_2001_vl <- st_intersection(pa_ccda_nat2000_2001, vlaanderen_crs)
# Bereken oppervlakte per polygon (in m², want CRS is 31370)
pa_ccda_nat2000_2024_vl$area_m2 <- st_area(pa_ccda_nat2000_2024_vl)
vlaanderen_crs$area_m2 <- st_area(vlaanderen_crs)

# Sommeer alles
tot_area_pa <- sum(pa_ccda_nat2000_2024_vl$area_m2)

tot_area_vl <- sum(vlaanderen_crs$area_m2)

tot_area_pa / tot_area_vl

#### protconn bereken

protconn_24 <- MK_ProtConn(
  nodes = pa_ccda_nat2000_2024,
  region = vlaanderen_crs,
  area_unit = "ha",
  distance = list(type = "edge"),
  distance_thresholds = c(1000, 10000, 30000, 100000),
  probability = 0.5,
  transboundary = 50000,
  plot = FALSE, write = NULL,
  parallel = NULL, intern = TRUE
)


sum(!st_is_valid(pa_ccda_nat2000_2024))
sum(!st_is_valid(vlaanderen_crs))

