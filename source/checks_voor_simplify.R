beschermde_gebieden_toon <- st_read(shapefile_path, quiet = TRUE) %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid() %>%
  ms_simplify() %>%
  st_cast("POLYGON") %>%
  st_make_valid() %>%
  # st_collection_extract("POLYGON") %>%
  st_cast("POLYGON")

mapview(beschermde_gebieden_toon)


simp <- st_read("data/data_toon/shapes_per_jaar/patches_2021.shp", quiet = TRUE) %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid() %>%
  ms_simplify()
full <- st_read("data/data_toon/shapes_per_jaar/patches_2021.shp", quiet = TRUE) %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid()

mapview(full) + mapview(simp, col.region = "red")

sumsimp <- as.numeric(sum(st_area(simp)))
sumfull <- as.numeric(sum(st_area(full)))
(sumfull - sumsimp)/10000

cdda <- st_read("data/CCDA/evolutie_2000_2024.shx", quiet = TRUE) %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid()

cdda_vlaanderen_clip <- st_intersection(cdda, vlaanderen_crs) %>%
  # Soms ontstaan er lijnen of punten op de grens, die wil je weg
  st_collection_extract("POLYGON") %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid()

cdda_vlaanderen_clip_simp <- cdda_vlaanderen_clip %>%
  st_simplify(preserveTopology = T, dTolerance = 50)

sumsimp <- as.numeric(sum(st_area(cdda_vlaanderen_clip_simp)))/10000
sumfull <- as.numeric(sum(st_area(cdda_vlaanderen_clip)))/10000
area_vlaanderen <- as.numeric(st_area(vlaanderen_crs))/10000
(sumfull - sumsimp)
sumfull/area_vlaanderen
sumsimp/area_vlaanderen

library(mapview)
mapview(cdda_vlaanderen_clip) + mapview(cdda_vlaanderen_clip_simp, col.region = "red")
