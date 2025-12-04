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
vlaanderen_crs <- st_read("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp") %>%
  dplyr::select(geometry) %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>%
  st_make_valid()
sf_use_s2(FALSE) # Voor robuustheid

# #kijken ccda
# besch_gebied_2000 <- st_read(dsn = here("data", "ccda_nat2000", "clip_evolutie_2000_2000.shp"))
# besch_gebied_2024 <- st_read(dsn = here("data", "ccda_nat2000", "clip_evolutie_2000_2024.shp"))
#
# beschermde_gebieden_indicator <- vect("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/WDPA_wgs84_BE_100km.shp") # Spatvector

# ccda_2021 <- st_read(dsn = here("data", "ccda", "evolutie_2000_2021.shp"))
# ccda_2024 <- st_read(dsn = here("data", "ccda", "evolutie_2000_2024.shp"))
# wdpa_2024 <- st_read(here("data", "WDPA_WDOECM_May2025_Public_BEL_shp-polygons.shp"))

# mapview(ccda_2024) + mapview(ccda_2021)
# mapview(wdpa_2024) + mapview(ccda_2024)


# Directory met shapefiles
shapefile_dir <- "C:/Users/emiel_delombaerde/Documents/R/ProtConn/data/ccda_nat2000/"
shapefiles <- list.files(path = shapefile_dir, pattern = "\\.shp$", full.names = TRUE)[2]

# Resultaat-dataframe initialiseren
resultaten_gegevens <- data.frame()

# Itereer over shapefiles
for (shapefile_path in shapefiles) {
  # shapefile_path <- shapefiles
  # Jaartal extraheren uit bestandsnaam (bv. "evolutie_2000.shp" → "2000")
  jaar <- str_extract(basename(shapefile_path), "\\d{4}(?=\\.shp)")

  message(paste("Verwerken van jaar:", jaar))

  beschermde_gebieden <- st_read(shapefile_path, quiet = T) %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    ms_simplify() %>%
    st_make_valid()


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
        `ProtConn indicator` == "Unprotected" ~ "Niet beschermd",
        TRUE ~ "Beschermd en geconnecteerd"
      ),
      Jaar = jaar
    ) %>%
    dplyr::select(-`ProtConn indicator`)

  # Toevoegen aan eindresultaat
  resultaten_gegevens <- bind_rows(resultaten_gegevens, gegevens)

  gc() # Opschonen geheugen
}

# Optioneel: wegschrijven naar CSV
write.csv(resultaten_gegevens, file = "C:/Users/emiel_delombaerde/Documents/R/ProtConn/protconn_resultaten_totaal_new.csv", row.names = FALSE)

resultaten_gegevens_plot <- readxl::read_excel("C:/Users/emiel_delombaerde/Documents/R/ProtConn/protconn_resultaten_totaal.xlsx") %>%
  mutate(low = Percentage - SD,
         high = Percentage + SD)


ggplot(resultaten_gegevens_plot, aes(x = Jaar, y = Percentage * 100)) +
  geom_point(aes(col = ProtConn)) +
  geom_line(aes(col = ProtConn)) +
  geom_ribbon(aes(ymin = low * 100, ymax = high * 100, fill = ProtConn), alpha = 0.2) +
  geom_hline(yintercept = 17, col = "green", linetype = "dashed") +
  geom_hline(yintercept = 30, col = "red", linetype = "dashed") +
  annotate("text", y = 17, x = 2010, label = "2020 Achidoel 11", vjust = -1, col = "green") +
  annotate("text", y = 30, x = 2010, label = "2030 GBF-doel 3", vjust = -1, col = "red") + ylab("Percentage (%)") +
  labs(
    caption = "Figuur 1: aandeel (%) niet-beschermde, beschermde en geconnecteerde beschermde natuur in de totale oppervlakte van Vlaanderen. 'Beschermd en geconnecteerd' is een deelverzameling binnen de categorie ‘beschermd’. De stippellijnen geven de doelen van de biodiversiteitsverdragen van de Verenigde Naties voor 2020 (17%) en 2030 (30%) weer. De gekleurde band rond de lijn voor 'beschermd en Geconnecteerd' geeft de spreiding (± 1 standaardafwijking) van dit aandeel weer, gebaseerd op vier dispersieafstanden (zie metadata)."
  )
