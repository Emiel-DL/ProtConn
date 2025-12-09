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

# --- CONFIGURATIE ---
sf_use_s2(FALSE) # Voor robuustheid

# 1. Vlaanderen shapefile voorbereiden
# CHECK HET PAD HIERONDER
vlaanderen_crs <- st_read("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp") %>%
  dplyr::select(geometry) %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>% # ms_simplify toegevoegd om rekentijd te beperken
  st_make_valid()

# 2. Directory met de nieuwe GeoPackages
shapefile_dir <- "data/data_toon/shapes_per_jaar"

# Lijst van bestanden (nu .gpkg en GEEN [25] limitatie meer)
shapefiles_toon <- list.files(path = shapefile_dir, pattern = "\\.shp$", full.names = TRUE)

# Resultaat-dataframe initialiseren
resultaten_gegevens <- data.frame()

# 3. Itereer over de jaren
for (shapefile_path in shapefiles_toon) {

  # Jaartal extraheren (zoekt naar 4 cijfers in bestandsnaam)
  # shapefile_path <- shapefiles_toon[1] # testen
  jaar <- str_extract(basename(shapefile_path), "\\d{4}")

  message(paste("Verwerken van jaar:", jaar, "-", basename(shapefile_path)))

  # Inlezen en voorbereiden (ms_simplify is belangrijk voor ProtConn snelheid)
  beschermde_gebieden_toon <- st_read(shapefile_path, quiet = TRUE) %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    ms_simplify() %>%
    st_make_valid() %>%
    # st_collection_extract("POLYGON") %>%
    st_cast("POLYGON")

  # ProtConn berekenen
  protconn <- Custom_MK_ProtConn_MultiScale(
    nodes = beschermde_gebieden_toon,
    region = vlaanderen_crs,
    distance_threshold = c(1000, 5000, 30000, 100000),
    probability = 0.5,
    transboundary = 0,
    plot = FALSE
  )

  gegevens <- as.data.frame(protconn$Results) %>%
    mutate(jaar = jaar)

  # Toevoegen aan eindresultaat
  resultaten_gegevens <- bind_rows(resultaten_gegevens, gegevens)

  gc() # Opschonen geheugen
}

# 4. Wegschrijven naar CSV
output_csv <- "resultaten/protconn_resultaten_2021_2024_test_custom.csv"
if (!dir.exists("resultaten")) dir.create("resultaten")
write.csv(resultaten_gegevens, file = output_csv, row.names = FALSE)

# 5. Plotten (Aangepast om direct de berekende data te gebruiken)
resultaten_gegevens_plot <- resultaten_gegevens %>%
  mutate(low = (Percentage * 100) - (SD * 100),
         high = (Percentage * 100) + (SD * 100))

ggplot(resultaten_gegevens_plot, aes(x = Jaar, y = Percentage * 100)) +
  geom_point(aes(col = ProtConn)) +
  geom_line(aes(col = ProtConn)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = ProtConn), alpha = 0.2) +
  geom_hline(yintercept = 17, col = "green", linetype = "dashed") +
  geom_hline(yintercept = 30, col = "red", linetype = "dashed") +
  # Labels iets verschoven omdat x-as nu slechts 4 jaar is
  annotate("text", y = 17, x = min(resultaten_gegevens_plot$Jaar), label = "2020 Achidoel 11", vjust = -1, col = "green", hjust = 0) +
  annotate("text", y = 30, x = min(resultaten_gegevens_plot$Jaar), label = "2030 GBF-doel 3", vjust = -1, col = "red", hjust = 0) +
  ylab("Percentage (%)") +
  labs(
    title = "ProtConn Indicator (2021-2024)",
    caption = "Figuur: Aandeel beschermde en geconnecteerde natuur."
  ) +
  theme_inbo() # Of theme_minimal() als INBOtheme niet werkt
