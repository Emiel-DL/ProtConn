#### Packages ####
library(tidyverse)
library(raster)
library(sf)
library(terra)
library(rmapshaper)
library(INBOtheme)
library(Makurhini)
library(plotly)
library(here)
library(mapview)
library(furrr) # Nodig voor parallel

conflicted::conflicts_prefer(dplyr::filter)

# --- CONFIGURATIE ---
sf_use_s2(FALSE)

# 1. PARALLEL SETUP
# Zet workers op detectCores() - 1 om je PC bruikbaar te houden.
# Als je RAM volloopt, verlaag dit getal handmatig naar bvb 4.
plan(multisession, workers = availableCores() - 2)

message(paste("Parallel processing gestart met", availableCores() - 2, "cores."))


# 2. VLAANDEREN PREPAREREN (Eenmalig buiten de loop)
# Let op: zorg dat dit pad klopt op jouw pc
vlaanderen_crs <- st_read("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp", quiet = TRUE) %>%
  dplyr::select(geometry) %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>%
  st_make_valid()


# 3. BESTANDEN LIJST
shapefile_dir <- "data/data_toon/shapes_per_jaar"
shapefiles_toon <- list.files(path = shapefile_dir, pattern = "\\.shp$", full.names = TRUE)


# 4. DE FUNCTIE (Exact jouw werkende logica)
verwerk_jaar_parallel <- function(shapefile_path, regio_map) {

  # Packages laden binnen de worker
  library(sf)
  library(tidyverse)
  library(Makurhini)
  library(rmapshaper)

  # Jaartal extraheren
  jaar <- str_extract(basename(shapefile_path), "\\d{4}")

  # --- START JOUW EXACTE CODE ---
  beschermde_gebieden_toon <- st_read(shapefile_path, quiet = TRUE) %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    ms_simplify() %>%           # Geen parameters, zoals gevraagd
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON")
  # --- EINDE JOUW CODE ---

  # ProtConn berekenen
  # Let op: 'transboundary' stond op 0 in jouw laatste snippet
  protconn <- MK_ProtConn(
    nodes = beschermde_gebieden_toon,
    region = regio_map,
    area_unit = "ha",
    distance = list(type = "edge"),
    distance_thresholds = c(1000, 10000, 30000, 100000),
    probability = 0.5,
    transboundary = 0,
    plot = FALSE, write = NULL,
    parallel = NULL, intern = TRUE
  )

  # Extract ProtConn info & Data Processing
  dist_names <- names(protconn)
  dist_df_list <- lapply(dist_names, function(dist_name) {
    protconn[[dist_name]] %>%
      dplyr::select(-Index, -Value) %>%
      dplyr::filter(`ProtConn indicator` %in% c("Unprotected", "Prot", "ProtConn")) %>%
      mutate(dist = dist_name) %>%
      as.data.frame()
  })

  resultaat_jaar <- bind_rows(dist_df_list) %>%
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
      Jaar = as.numeric(jaar)
    ) %>%
    dplyr::select(-`ProtConn indicator`)

  return(resultaat_jaar)
}


# 5. UITVOEREN (Met progress bar)
# We geven 'vlaanderen_crs' mee als argument 'regio_map'
resultaten_gegevens <- future_map_dfr(
  .x = shapefiles_toon,
  .f = verwerk_jaar_parallel,
  regio_map = vlaanderen_crs,
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

# Netjes afsluiten
plan(sequential)


# 6. OPSLAAN & PLOTTEN
output_csv <- "resultaten/protconn_resultaten_2021_2024_no_boundary.csv"
if (!dir.exists("resultaten")) dir.create("resultaten")
write.csv(resultaten_gegevens, file = output_csv, row.names = FALSE)

resultaten_gegevens_plot <- resultaten_gegevens %>%
  mutate(low = (Percentage * 100) - (SD * 100),
         high = (Percentage * 100) + (SD * 100))

ggplot(resultaten_gegevens_plot, aes(x = Jaar, y = Percentage * 100)) +
  geom_point(aes(col = ProtConn)) +
  geom_line(aes(col = ProtConn)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = ProtConn), alpha = 0.2) +
  geom_hline(yintercept = 17, col = "green", linetype = "dashed") +
  geom_hline(yintercept = 30, col = "red", linetype = "dashed") +
  annotate("text", y = 17, x = min(resultaten_gegevens_plot$Jaar), label = "2020 Achidoel 11", vjust = -1, col = "green", hjust = 0) +
  annotate("text", y = 30, x = min(resultaten_gegevens_plot$Jaar), label = "2030 GBF-doel 3", vjust = -1, col = "red", hjust = 0) +
  ylab("Percentage (%)") +
  labs(
    title = "ProtConn Indicator (2021-2024)",
    caption = "Figuur: Aandeel beschermde en geconnecteerde natuur."
  ) +
  theme_inbo()
