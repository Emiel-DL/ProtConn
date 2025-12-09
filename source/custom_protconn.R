library(sf)
library(dplyr)
library(units)

library(sf)
library(dplyr)
library(units)
library(igraph)

Custom_MK_ProtConn_Final <- function(nodes,
                                     region,
                                     distance_threshold = 10000,
                                     probability = 0.5,
                                     transboundary = 0,
                                     plot = FALSE) {

  message("--- Start Custom ProtConn (Graph-Based / Stepping Stones) ---")

  # ------------------------------------------------
  # Stap 1: Input & Schoonmaak
  # ------------------------------------------------
  if(!inherits(nodes, "sf")) stop("Nodes moet sf zijn")
  if(!inherits(region, "sf")) stop("Region moet sf zijn")

  # CRS gelijk trekken
  if(st_crs(nodes) != st_crs(region)) {
    message("Warning: CRS getransformeerd naar Region CRS.")
    nodes <- st_transform(nodes, st_crs(region))
  }

  region <- st_make_valid(region)
  nodes  <- st_make_valid(nodes)

  # AL is ALTIJD oppervlakte Vlaanderen (zonder buffer)
  AL <- sum(as.numeric(st_area(region)))

  # ------------------------------------------------
  # Stap 2: Fysieke Patches Definiëren (Cruciaal voor Overlap)
  # ------------------------------------------------
  message("Step 2: Defining physical patches...")

  # A. Maak het totale studiegebied (Vlaanderen + Buffer)
  if(transboundary > 0){
    study_mask <- st_buffer(region, dist = transboundary) %>% st_union() %>% st_make_valid()
  } else {
    study_mask <- st_union(region) %>% st_make_valid()
  }

  # B. Selecteer & Dissolve alles binnen dit gebied
  # Dit lost het "versnippering door overlap" probleem op
  nodes_cut <- st_intersection(nodes, study_mask)
  nodes_cut <- nodes_cut[st_is(nodes_cut, c("POLYGON", "MULTIPOLYGON")), ]

  if(nrow(nodes_cut) == 0) stop("Geen nodes in studiegebied.")

  message("   -> Merging overlaps (Dissolve)...")
  # Alles samensmelten tot fysieke blokken
  physical_patches <- st_union(nodes_cut) %>%
    st_cast("POLYGON") %>%
    st_as_sf()

  # ID geven
  physical_patches$patch_id <- 1:nrow(physical_patches)

  # ------------------------------------------------
  # Stap 3: Target Area Berekenen (De "a_i" vector)
  # ------------------------------------------------
  message("Step 3: Calculating target areas...")

  # We moeten weten hoeveel m2 van elke patch BINNEN Vlaanderen ligt.
  # Als een patch volledig in Wallonie ligt, is dit 0.

  # Intersection met enkel Vlaanderen
  overlap_vl <- st_intersection(physical_patches, region)

  # Bereken oppervlakte per stukje en sommeer per patch_id
  # (Sommige patches kunnen in 2 stukken geknipt worden door de grens)
  overlap_vl$area_vl <- as.numeric(st_area(overlap_vl))

  target_areas_df <- overlap_vl %>%
    st_drop_geometry() %>%
    group_by(patch_id) %>%
    summarise(target_area = sum(area_vl))

  # Voeg samen met de fysieke patches (zet 0 waar geen match is)
  physical_patches <- left_join(physical_patches, target_areas_df, by = "patch_id")
  physical_patches$target_area[is.na(physical_patches$target_area)] <- 0

  message(paste0("   -> ", nrow(physical_patches), " unique physical patches identified."))

  # ------------------------------------------------
  # Stap 4: Netwerk Bouwen (Graph Theory)
  # ------------------------------------------------
  message("Step 4: Building connectivity network...")

  # Bereken afstanden tussen alle fysieke patches
  # (Zelfs als target_area 0 is, doet de patch mee als brug!)
  d_mat <- st_distance(physical_patches)
  d_mat <- matrix(as.numeric(d_mat), nrow = nrow(d_mat))

  # Maak Adjacency Matrix
  adj_mat <- d_mat
  # Zet verbindingen > threshold op 0 (geen link)
  adj_mat[adj_mat > distance_threshold] <- 0

  # Bouw Graaf
  g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE)

  # ------------------------------------------------
  # Stap 5: Shortest Paths Berekenen (Stepping Stones)
  # ------------------------------------------------
  message("Step 5: Calculating shortest paths...")

  # We berekenen de effectieve afstand via het netwerk.
  # Dit staat gelijk aan de 'phast' stap in Makurhini.
  # We gebruiken gewogen afstanden.

  # Formule voor gewicht: w = distance * (-ln(p) / d)
  # Dit zorgt dat optellen van gewichten = vermenigvuldigen van probabilities
  k <- -log(probability) / distance_threshold
  E(g)$weight <- E(g)$weight * k

  # Dijkstra berekent de som van gewichten (minimale weerstand)
  shortest_path_dist <- distances(g, algorithm = "dijkstra")

  # Terugrekenen naar Probabilities: P = exp(-Som_Gewichten)
  prob_mat <- exp(-shortest_path_dist)

  # ------------------------------------------------
  # Stap 6: Sommatie (Alleen Vlaamse Oppervlakte telt)
  # ------------------------------------------------
  message("Step 6: Calculating final score...")

  # Vector a (bevat nullen voor pure stepping stones)
  a <- physical_patches$target_area

  # ProtConn Teller: a^T * P * a
  weighted_sum <- as.numeric(t(a) %*% prob_mat %*% a)

  ECA <- sqrt(weighted_sum)
  ProtConn <- 100 * (ECA / AL)

  message("Done!")

  return(list(
    ProtConn = round(ProtConn, 4),
    ECA_m2 = ECA,
    Total_Region_Area_m2 = AL,
    Protected_Area_m2 = sum(a), # Dit moet matchen met je eerdere tests
    Patch_Count = nrow(physical_patches)
  ))
}

library(sf)
library(dplyr)
library(units)
library(igraph)

Custom_MK_ProtConn_MultiScale <- function(nodes,
                                          region,
                                          distance_thresholds = c(1000, 10000), # Nu een vector!
                                          probability = 0.5,
                                          transboundary = 0,
                                          plot = FALSE) {

  message("--- Start Custom ProtConn (Multi-Scale Graph-Based) ---")

  # ------------------------------------------------
  # Stap 1: Input & Schoonmaak (Eenmalig)
  # ------------------------------------------------
  if(!inherits(nodes, "sf")) stop("Nodes moet sf zijn")
  if(!inherits(region, "sf")) stop("Region moet sf zijn")

  if(st_crs(nodes) != st_crs(region)) {
    message("Warning: CRS getransformeerd naar Region CRS.")
    nodes <- st_transform(nodes, st_crs(region))
  }

  region <- st_make_valid(region)
  nodes  <- st_make_valid(nodes)

  AL <- sum(as.numeric(st_area(region)))

  # ------------------------------------------------
  # Stap 2: Fysieke Patches (Eenmalig)
  # ------------------------------------------------
  message("Step 2: Defining physical patches...")

  if(transboundary > 0){
    study_mask <- st_buffer(region, dist = transboundary) %>% st_union() %>% st_make_valid()
  } else {
    study_mask <- st_union(region) %>% st_make_valid()
  }

  nodes_cut <- st_intersection(nodes, study_mask)
  nodes_cut <- nodes_cut[st_is(nodes_cut, c("POLYGON", "MULTIPOLYGON")), ]

  if(nrow(nodes_cut) == 0) stop("Geen nodes in studiegebied.")

  message("   -> Merging overlaps (Dissolve)...")
  physical_patches <- st_union(nodes_cut) %>%
    st_cast("POLYGON") %>%
    st_as_sf()

  physical_patches$patch_id <- 1:nrow(physical_patches)

  # ------------------------------------------------
  # Stap 3: Target Area Berekenen (Eenmalig)
  # ------------------------------------------------
  message("Step 3: Calculating target areas...")
  overlap_vl <- st_intersection(physical_patches, region)
  overlap_vl$area_vl <- as.numeric(st_area(overlap_vl))

  target_areas_df <- overlap_vl %>%
    st_drop_geometry() %>%
    group_by(patch_id) %>%
    summarise(target_area = sum(area_vl))

  physical_patches <- left_join(physical_patches, target_areas_df, by = "patch_id")
  physical_patches$target_area[is.na(physical_patches$target_area)] <- 0

  # Vector 'a' (Target area) blijft constant
  a <- physical_patches$target_area

  # ------------------------------------------------
  # Stap 4: Basis Afstandsmatrix (Eenmalig & Zwaarst)
  # ------------------------------------------------
  message("Step 4: Calculating base distance matrix...")
  d_mat_base <- st_distance(physical_patches)
  d_mat_base <- matrix(as.numeric(d_mat_base), nrow = nrow(d_mat_base))

  # ------------------------------------------------
  # Stap 5: Loop over alle Thresholds
  # ------------------------------------------------
  results_list <- list()

  message(paste0("Step 5: Iterating over ", length(distance_thresholds), " thresholds..."))

  for(d in distance_thresholds) {
    message(paste0("   -> Processing d = ", d, " m..."))

    # A. Maak specifieke graaf voor deze threshold
    # Kopieer basis matrix
    adj_mat <- d_mat_base
    # Knip verbindingen door die te lang zijn voor DEZE threshold
    adj_mat[adj_mat > d] <- 0

    # Bouw netwerk
    g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE)

    # B. Shortest Paths (Stepping Stones)
    # Gewicht aanpassen op basis van d
    k <- -log(probability) / d

    # Als adj_mat 0 is, is er geen edge. Waar wel edge is, is het gewicht de afstand.
    E(g)$weight <- E(g)$weight * k

    # Dijkstra
    shortest_path_dist <- distances(g, algorithm = "dijkstra")

    # C. Probabilities & Sommatie
    prob_mat <- exp(-shortest_path_dist)

    weighted_sum <- as.numeric(t(a) %*% prob_mat %*% a)
    ECA <- sqrt(weighted_sum)
    ProtConn <- 100 * (ECA / AL)

    # Resultaat opslaan in lijst
    results_list[[paste0("d_", d)]] <- data.frame(
      Threshold_m = d,
      ProtConn = round(ProtConn, 4),
      ECA_m2 = ECA,
      Protected_Area_m2 = sum(a),
      Percentage_Protected = (sum(a)/AL)*100
    )
  }

  message("Done!")

  # Alles samenvoegen tot 1 tabel
  final_table <- do.call(rbind, results_list)
  rownames(final_table) <- NULL

  return(list(
    Results = final_table,
    Total_Region_Area_m2 = AL,
    Patch_Count = nrow(physical_patches)
  ))
}

#### Packages ####
library(tidyverse)
library(raster)
library(sf)
# library(terra)
library(rmapshaper)
# library(INBOtheme)
library(Makurhini)
conflicted::conflicts_prefer(dplyr::filter)
# library(plotly)
library(here)
# library(mapview)

# --- CONFIGURATIE ---
sf_use_s2(FALSE) # Voor robuustheid

# 1. Vlaanderen shapefile voorbereiden
# CHECK HET PAD HIERONDER
vlaanderen_crs <- st_read("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp") %>%
  dplyr::select(geometry) %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>% # ms_simplify toegevoegd om rekentijd te beperken
  st_make_valid()

beschermde_gebieden_2021_mssimp <- st_read(here("data", "data_toon", "shapes_transboundary", "patches_transboundary_2021.shp"), quiet = TRUE) %>%
  dplyr::select(geometry) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 31370) %>%
  st_make_valid() %>%
  ms_simplify() %>%
  st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON")


  beschermde_gebieden_2021_stsimp <- st_read(here("data", "data_toon", "shapes_transboundary", "patches_transboundary_2021.shp"), quiet = TRUE) %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    st_simplify(preserveTopology = T, dTolerance = 50)

  test_masker <- vlaanderen_crs %>%
    st_union() %>%                 # Zeker zijn dat het 1 vlak is
    st_centroid() %>%              # Pak het middelpunt
    st_buffer(30000) %>%           # Cirkel van 15km straal (pas aan indien nodig)
    st_as_sf()

  test_region <- vlaanderen_crs %>%
    st_union() %>%                 # Zeker zijn dat het 1 vlak is
    st_centroid() %>%              # Pak het middelpunt
    st_buffer(10000) %>%           # Cirkel van 15km straal (pas aan indien nodig)
    st_as_sf()

  clip_2021 <- st_read(here("data", "data_toon", "shapes_transboundary", "patches_transboundary_2021.shp"), quiet = TRUE) %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    st_intersection(., test_masker)
  mapview(clip_2021) + mapview(test_region)


protconn_test <- Custom_MK_ProtConn_Final(nodes = clip_2021,
                   region = test_region,
                   distance_threshold = 5000,
                   probability = 0.5,
                   transboundary = 0,
                   plot = FALSE)
protconn_test

protconn_test_2 <- MK_ProtConn(
  nodes = clip_2021,
  region = test_region,
  area_unit = "ha",
  distance = list(type = "edge"),
  distance_thresholds = c(5000),
  probability = 0.5,
  transboundary = 0,
  plot = FALSE, write = NULL,
  parallel = NULL, intern = TRUE
)
protconn_test_2


protconn_test <- Custom_MK_ProtConn_Final(nodes = beschermde_gebieden_2021_mssimp,
                                          region = vlaanderen_crs,
                                          distance_threshold = 5000,
                                          probability = 0.5,
                                          transboundary = 2000,
                                          plot = FALSE)
protconn_test

protconn_test_2 <- MK_ProtConn(
  nodes = beschermde_gebieden_2021_mssimp,
  region = vlaanderen_crs,
  area_unit = "ha",
  distance = list(type = "edge"),
  distance_thresholds = c(5000),
  probability = 0.5,
  transboundary = 2000,
  plot = FALSE, write = NULL,
  parallel = NULL, intern = TRUE
)
protconn_test_2


protconn_test <- Custom_MK_ProtConn_MultiScale(nodes = beschermde_gebieden_2021_mssimp,
                                          region = vlaanderen_crs,
                                          distance_threshold = c(1000, 5000, 30000, 100000),
                                          probability = 0.5,
                                          transboundary = 50000,
                                          plot = FALSE)
protconn_test

protconn_test_2 <- MK_ProtConn(
  nodes = beschermde_gebieden_2021_mssimp,
  region = vlaanderen_crs,
  area_unit = "ha",
  distance = list(type = "edge"),
  distance_thresholds = c(1000, 5000, 30000, 100000),
  probability = 0.5,
  transboundary = 50000,
  plot = FALSE, write = NULL,
  parallel = NULL, intern = TRUE
)
protconn_test_2
