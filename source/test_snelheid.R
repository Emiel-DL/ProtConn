#### Packages ####
library(tidyverse)
library(sf)
library(rmapshaper)
library(Makurhini)
library(parallel) # Nodig om cores te tellen
library(mapview)

# --- CONFIGURATIE ---
sf_use_s2(FALSE)

# ---------------------------------------------------------
# STAP 1: DATA VOORBEREIDING (Buiten de tijdsmeting)
# ---------------------------------------------------------
message("--- Stap 1: Data inladen en voorbereiden... ---")

# A. Vlaanderen
vlaanderen_crs <- st_read("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp", quiet = TRUE) %>%
  dplyr::select(geometry) %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>%
  st_make_valid()

# B. Beschermde gebieden 2021 (Specifiek bestand zoeken)
shapefile_dir <- "data/data_toon/shapes_per_jaar"
file_2021 <- list.files(path = shapefile_dir, pattern = "2021.*\\.shp$", full.names = TRUE)[1]

test_masker <- vlaanderen_crs %>%
  st_union() %>%                 # Zeker zijn dat het 1 vlak is
  st_centroid() %>%              # Pak het middelpunt
  st_buffer(30000) %>%           # Cirkel van 15km straal (pas aan indien nodig)
  st_as_sf()

shape_2021 <- st_read(file_2021, quiet = T) %>%
  st_intersection(., test_masker)
mapview(shape_2021)


if(is.na(file_2021)) stop("Geen shapefile voor 2021 gevonden in de map!")

message(paste("Bestand gevonden:", basename(file_2021)))

# C. Schoonmaken (Jouw exacte pipeline)
nodes_2021_full <- shape_2021 %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid()

nodes_2021_mssimp <- shape_2021 %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid() %>%
  ms_simplify() %>%
  st_make_valid() %>%
  st_cast("POLYGON")

nodes_2021_stsimp <- shape_2021 %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid() %>%
  st_simplify(preserveTopology = T, dTolerance = 50)

vlaanderen_crs_clip <- vlaanderen_crs %>%
  st_intersection(., test_masker)

message("Data is klaar. Start benchmark...")

# ---------------------------------------------------------
# STAP 2: SEQUENTIËLE TEST (1 Core)
# ---------------------------------------------------------
message("\n--- Test 1: Sequentieel (Parallel = NULL) ---")
message("Dit kan even duren...")

tijd_seq <- system.time({
  protconn_full <- MK_ProtConn(
    nodes = nodes_2021_full,
    region = vlaanderen_crs_clip,
    area_unit = "ha",
    distance = list(type = "edge"),
    distance_thresholds = c(1000, 10000, 30000, 100000),
    probability = 0.5,
    transboundary = 0,
    plot = FALSE, write = NULL,
    intern = TRUE,

    parallel = NULL # <--- HIER TESTEN WE DE STANDAARD
  )
})

print(paste("Tijd Sequentieel:", round(tijd_seq["elapsed"], 2), "seconden"))

tijd_seq <- system.time({
  protconn_mssimp <- MK_ProtConn(
    nodes = nodes_2021_mssimp,
    region = vlaanderen_crs_clip,
    area_unit = "ha",
    distance = list(type = "edge"),
    distance_thresholds = c(1000, 10000, 30000, 100000),
    probability = 0.5,
    transboundary = 0,
    plot = FALSE, write = NULL,
    intern = TRUE,

    parallel = NULL # <--- HIER TESTEN WE DE STANDAARD
  )
})

print(paste("Tijd Sequentieel:", round(tijd_seq["elapsed"], 2), "seconden"))

tijd_seq <- system.time({
  protconn_stsimp <- MK_ProtConn(
    nodes = nodes_2021_stsimp,
    region = vlaanderen_crs_clip,
    area_unit = "ha",
    distance = list(type = "edge"),
    distance_thresholds = c(1000, 10000, 30000, 100000),
    probability = 0.5,
    transboundary = 0,
    plot = FALSE, write = NULL,
    intern = TRUE,

    parallel = NULL # <--- HIER TESTEN WE DE STANDAARD
  )
})

print(paste("Tijd Sequentieel:", round(tijd_seq["elapsed"], 2), "seconden"))


# berekenen aandeel protcon/tot prot

protconn_stsimp$d10000$Percentage[3] / protconn_stsimp$d10000$Percentage[1]
