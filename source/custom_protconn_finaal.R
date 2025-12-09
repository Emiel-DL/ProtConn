library(sf)
library(dplyr)
library(units)
library(igraph)
library(purrr)
library(furrr) # Voor parallel processing
library(here)

Custom_MK_ProtConn_Atomic <- function(nodes,
                                      region,
                                      distance_threshold,
                                      transboundary,
                                      probability = 0.5) {

  # Geen message output om je console niet te overspoelen tijdens parallel runs
  # Errors worden wel gevangen door de parallel handler

  # ------------------------------------------------
  # Stap 1: Input Check
  # ------------------------------------------------
  if(!inherits(nodes, "sf")) stop("Nodes moet sf zijn")
  if(!inherits(region, "sf")) stop("Region moet sf zijn")

  # Zorg voor gelijk CRS (neem CRS van regio)
  current_crs <- st_crs(region)
  if(st_crs(nodes) != current_crs) {
    nodes <- st_transform(nodes, current_crs)
  }

  # Maak valide
  region <- st_make_valid(region)
  nodes  <- st_make_valid(nodes)

  # Totale Oppervlakte (Noemer) = Vlaanderen
  AL <- sum(as.numeric(st_area(region)))

  # ------------------------------------------------
  # Stap 2: Studiegebied & Fysieke Patches
  # ------------------------------------------------
  # Buffer aanmaken op basis van DEZE specifieke transboundary waarde
  if(transboundary > 0){
    study_mask <- st_buffer(region, dist = transboundary) %>% st_union() %>% st_make_valid()
  } else {
    study_mask <- st_union(region) %>% st_make_valid()
  }

  # Selecteer & Dissolve
  nodes_cut <- st_intersection(nodes, study_mask)
  # Filter lege geometrieën (punten/lijnen)
  nodes_cut <- nodes_cut[st_is(nodes_cut, c("POLYGON", "MULTIPOLYGON")), ]

  # Als er niks is, return 0 (voorkomt crash)
  if(nrow(nodes_cut) == 0) {
    return(data.frame(
      Threshold_m = distance_threshold,
      Transboundary_m = transboundary,
      ProtConn = 0,
      ECA_m2 = 0,
      Protected_Area_m2 = 0,
      Percentage_Protected = 0
    ))
  }

  # Samenvoegen tot fysieke blokken
  physical_patches <- st_union(nodes_cut) %>%
    st_cast("POLYGON") %>%
    st_as_sf()

  physical_patches$patch_id <- 1:nrow(physical_patches)

  # ------------------------------------------------
  # Stap 3: Target Area (Vlaams deel)
  # ------------------------------------------------
  overlap_vl <- st_intersection(physical_patches, region)
  overlap_vl$area_vl <- as.numeric(st_area(overlap_vl))

  target_areas_df <- overlap_vl %>%
    st_drop_geometry() %>%
    group_by(patch_id) %>%
    summarise(target_area = sum(area_vl))

  physical_patches <- left_join(physical_patches, target_areas_df, by = "patch_id")
  physical_patches$target_area[is.na(physical_patches$target_area)] <- 0

  a <- physical_patches$target_area

  # ------------------------------------------------
  # Stap 4: Afstanden & Netwerk
  # ------------------------------------------------
  d_mat <- st_distance(physical_patches)
  adj_mat <- matrix(as.numeric(d_mat), nrow = nrow(d_mat))

  # Harde cut-off op de threshold
  adj_mat[adj_mat > distance_threshold] <- 0

  # Graaf bouwen
  g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE)

  # Gewichten voor Dijkstra (Shortest Path)
  k <- -log(probability) / distance_threshold
  E(g)$weight <- E(g)$weight * k

  shortest_path_dist <- distances(g, algorithm = "dijkstra")

  # ------------------------------------------------
  # Stap 5: Score
  # ------------------------------------------------
  prob_mat <- exp(-shortest_path_dist)

  weighted_sum <- as.numeric(t(a) %*% prob_mat %*% a)
  ECA <- sqrt(weighted_sum)
  ProtConn <- 100 * (ECA / AL)

  # Output als 1 nette rij
  return(data.frame(
    Threshold_m = distance_threshold,
    Transboundary_m = transboundary,
    ProtConn = round(ProtConn, 4),
    ECA_m2 = ECA,
    Protected_Area_m2 = sum(a),
    Percentage_Protected = (sum(a)/AL)*100
  ))
}


# 1. Instellingen
# ---------------------
# Definieer je scenario's
thresholds <- c(1000, 10000, 30000, 10000)
years <- c(2021, 2022, 2023, 2024)

# Maak een tabel met alle combinaties die je wilt berekenen
# Hier zeggen we: transboundary IS gelijk aan threshold
scenarios <- expand.grid(year = years, threshold = thresholds) %>%
  mutate(transboundary = 50000) # <--- JOUW EIS

print(head(scenarios))

# 2. Data Inladen (Voorbereiding)
# ---------------------
# Laad je regio één keer in (die is statisch)
vlaanderen_crs <- st_read("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp", quiet = T) %>%
  dplyr::select(geometry) %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>% # ms_simplify toegevoegd om rekentijd te beperken
  st_make_valid()

# Laad AL je nodes in een lijst (zodat de workers ze niet telkens van schijf hoeven te lezen)
# Pas de paden aan naar jouw bestanden
nodes_list <- list(
  "2021" = st_read("data/data_toon/shapes_transboundary/patches_transboundary_2021.shp", quiet = T) %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    st_simplify(preserveTopology = T, dTolerance = 50),
  "2022" = st_read("data/data_toon/shapes_transboundary/patches_transboundary_2022.shp", quiet = T) %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    st_simplify(preserveTopology = T, dTolerance = 50),
  "2023" = st_read("data/data_toon/shapes_transboundary/patches_transboundary_2023.shp", quiet = T) %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    st_simplify(preserveTopology = T, dTolerance = 50),
  "2024" = st_read("data/data_toon/shapes_transboundary/patches_transboundary_2024.shp", quiet = T) %>%
    dplyr::select(geometry) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    st_simplify(preserveTopology = T, dTolerance = 50)
)

# 3. Parallel Uitvoeren
# ---------------------
# Zet parallel aan (gebruik aantal fysieke cores - 1)
plan(multisession, workers = availableCores() - 1)

message("Start parallelle berekening...")

# We mappen over de rijen van je scenario tabel
results_df <- future_pmap_dfr(list(scenarios$year, scenarios$threshold, scenarios$transboundary),
                              function(y, t, tb) {

                                # Pak de juiste nodes uit de lijst op basis van het jaar
                                current_nodes <- nodes_list[[as.character(y)]]

                                # Roep de ATOMIC functie aan
                                res <- Custom_MK_ProtConn_Atomic(
                                  nodes = current_nodes,
                                  region = vlaanderen_crs,
                                  distance_threshold = t,
                                  transboundary = tb,
                                  probability = 0.5
                                )

                                # Voeg jaar kolom toe aan het resultaat
                                res$Year <- y
                                return(res)

                              }, .options = furrr_options(seed = TRUE)) # Seed true voor reproduceerbaarheid

# 4. Resultaat opslaan
# ---------------------
print(results_df)
output_csv <- "resultaten/protconn_resultaten_2021_2024_finaal.csv"

write.csv(resultaten_df, file = output_csv, row.names = FALSE)
