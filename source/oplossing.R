
pa_ccda_nat2000_2012_simp <- st_read("C:/Users/emiel_delombaerde/Documents/R/ProtConn/data/ccda_nat2000/clip_evolutie_2000_2012.shp") %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid() %>%
  ms_simplify() %>%
  st_make_valid()

pa_ccda_nat2000_2024_simp <- st_read("C:/Users/emiel_delombaerde/Documents/R/ProtConn/data/ccda_nat2000/clip_evolutie_2000_2024.shp") %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid() %>%
  ms_simplify() %>%
  st_make_valid()

vlaanderen_crs <- st_read("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp") %>%
  dplyr::select(geometry) %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>%
  st_make_valid()
sf_use_s2(FALSE) # Voor robuustheid

### area check

intersect <- st_intersection(pa_ccda_nat2000_2024_simp, vlaanderen_crs)
# Bereken oppervlakte per polygon (in m², want CRS is 31370)
intersect$area_m2 <- st_area(intersect)
vlaanderen_crs$area_m2 <- st_area(vlaanderen_crs)

# Sommeer alles
tot_area_pa <- sum(intersect$area_m2)

tot_area_vl <- sum(vlaanderen_crs$area_m2)

tot_area_pa / tot_area_vl



pa_ccda_nat2000_2012_simp_vect <- vect("C:/Users/emiel_delombaerde/Documents/R/ProtConn/data/ccda_nat2000/clip_evolutie_2000_2012.shp") %>%
  st_as_sf() %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid() %>%
  ms_simplify() %>%
  st_make_valid()

pa_ccda_nat2000_2012_simp_vect$area <- st_area(pa_ccda_nat2000_2012_simp_vect)
sum(pa_ccda_nat2000_2012_simp_vect$area)

pa_ccda_nat2000_2012_complex <- st_read("C:/Users/emiel_delombaerde/Documents/R/ProtConn/data/ccda_nat2000/clip_evolutie_2000_2012.shp") %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  # ms_simplify() %>%
  st_make_valid()




#### protconn bereken
start_time <- Sys.time()

protconn_12_simp <- MK_ProtConn(
  nodes = pa_ccda_nat2000_2012_simp,
  region = vlaanderen_crs,
  area_unit = "ha",
  distance = list(type = "edge"),
  distance_thresholds = c(1000, 10000, 30000, 100000),
  probability = 0.5,
  transboundary = 50000,
  plot = FALSE, write = NULL,
  parallel = NULL, intern = TRUE
)

end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()

protconn_24_simp_vect <- MK_ProtConn(
  nodes = pa_ccda_nat2000_2012_simp_vect,
  region = vlaanderen_crs,
  area_unit = "ha",
  distance = list(type = "edge"),
  distance_thresholds = c(1000, 10000, 30000, 100000),
  probability = 0.5,
  transboundary = 50000,
  plot = FALSE, write = NULL,
  parallel = NULL, intern = TRUE
)

end_time <- Sys.time()
end_time - start_time


sum(!st_is_valid(pa_ccda_nat2000_2012_simp))
sum(!st_is_valid(vlaanderen_crs))

