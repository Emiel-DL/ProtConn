#### Packages ####
library(tidyverse)
library(raster)
library(sf)
library(terra)
library(rmapshaper)
library(INBOtheme)
library(Makurhini)
conflicted::conflicts_prefer(dplyr::filter)
library(plotly)
library(here)
library(mapview)

# Vlaanderen shapefile voorbereiden
vlaanderen <- vect("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp")
vlaanderen <- sf::st_as_sf(vlaanderen)[, "geometry"]
vlaanderen_crs <- st_transform(vlaanderen, crs = 31370)
vlaanderen_crs <- st_make_valid(vlaanderen_crs)
sf_use_s2(FALSE) # Voor robuustheid

#kijken ccda
ccda_2000 <- st_read(dsn = here("data", "ccda", "evolutie_2000_2000.shp"))
ccda_2021 <- st_read(dsn = here("data", "ccda", "evolutie_2000_2021.shp"))
ccda_2024 <- st_read(dsn = here("data", "ccda", "evolutie_2000_2024.shp"))
wdpa_2024 <- st_read(here("data", "WDPA_WDOECM_May2025_Public_BEL_shp-polygons.shp"))

mapview(ccda_2024) + mapview(ccda_2021)
mapview(wdpa_2024) + mapview(ccda_2024)

# Directory met shapefiles
shapefile_dir <- "C:/Users/emiel_delombaerde/Documents/R/ProtConn/data/CCDA/"
shapefiles <- list.files(path = shapefile_dir, pattern = "\\.shp$", full.names = TRUE)[23]

# Resultaat-dataframe initialiseren
resultaten_gegevens <- data.frame()

# Itereer over shapefiles
for (shapefile_path in shapefiles) {
  # Jaartal extraheren uit bestandsnaam (bv. "evolutie_2000.shp" → "2000")
  jaar <- str_extract(basename(shapefile_path), "\\d{4}(?=\\.shp)")

  message(paste("Verwerken van jaar:", jaar))

  beschermde_gebieden <- vect(shapefile_path) %>%
    sf::st_as_sf() %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    ms_simplify()


  # ProtConn berekenen
  protconn <- MK_ProtConn(
    nodes = beschermde_gebieden,
    region = vlaanderen_crs,
    area_unit = "ha",
    distance = list(type = "edge"),
    distance_thresholds = c(1000, 10000, 30000, 100000),
    probability = 0.5,
    transboundary = 50000,
    plot = FALSE, write = NULL,
    parallel = NULL, intern = TRUE
  )

  # Extract ProtConn info
  dist_names <- names(protconn)
  dist_df_list <- lapply(dist_names, function(dist_name) {
    df <- protconn[[dist_name]] %>%
      dplyr::select(-Index, -Value) %>%
      dplyr::filter(`ProtConn indicator` %in% c("Unprotected", "Prot", "ProtConn")) %>%
      mutate(dist = dist_name) %>%
      as.data.frame()
    return(df)
  })

  gegevens <- bind_rows(dist_df_list) %>%
    group_by(`ProtConn indicator`) %>%
    summarise(
      SD = sd(Percentage / 100),
      Percentage = mean(Percentage / 100),
      .groups = "drop"
    ) %>%
    mutate(
      ProtConn = case_when(
        `ProtConn indicator` == "Prot" ~ "Beschermd",
        `ProtConn indicator` == "Unprotected" ~ "Onbeschermd",
        TRUE ~ "Geconnecteerd en beschermd"
      ),
      Jaar = jaar
    ) %>%
    dplyr::select(-`ProtConn indicator`)

  # Toevoegen aan eindresultaat
  resultaten_gegevens <- bind_rows(resultaten_gegevens, gegevens)

  gc() # Opschonen geheugen
}

# Optioneel: wegschrijven naar CSV
write.csv(resultaten_gegevens, file = "C:/Users/emiel_delombaerde/Documents/R/ProtConn/protconn_resultaten_2000_2021.csv", row.names = FALSE)

resultaten_gegevens <- read.csv(file = "C:/Users/emiel_delombaerde/Documents/R/ProtConn/protconn_resultaten_2000_2021.csv")

ggplot(resultaten_gegevens, aes(x = Jaar)) +
  geom_point(aes(y = Percentage, col = ProtConn)) +
  geom_line(aes(y = Percentage, col = ProtConn)) +
  geom_hline(yintercept = 0.17, col = "green", linetype = "dashed") +
  geom_hline(yintercept = 0.30, col = "red", linetype = "dashed")
