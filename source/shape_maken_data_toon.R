library(sf)
library(tidyverse)
library(mapview)

shape <- st_read("data/data_toon/opp_data_GIS.shp")

##### parallel

library(furrr) # Voor parallel processing

# 1. Zet parallel aan
# Gebruik het aantal beschikbare cores min 1 (zodat je pc niet vastloopt)
plan(multisession, workers = availableCores() - 2)

# 2. Maak een functie van je berekening (wat vroeger in je loop stond)
verwerk_jaar <- function(huidig_jaar, input_shape) {

  # Jouw logica hier:
  patches <- input_shape %>%
    filter(jaar == huidig_jaar, !is.na(Categr3)) %>%
    st_make_valid() %>%
    st_union() %>%         # De zware stap
    st_cast("POLYGON") %>%
    st_sf() %>%
    mutate(patch_id = row_number(),
           jaar = huidig_jaar) # Voeg jaar terug toe

  return(patches)
}

# 3. Voer uit in parallel
# Dit splitst de jaren op, stuurt elk jaar naar een andere CPU-kern
# en plakt alles op het einde weer aan elkaar.
resultaat_lijst <- future_map(unique(shape$jaar),
                              ~verwerk_jaar(.x, shape),
                              .options = furrr_options(seed = TRUE))

# 4. Bind alles samen tot 1 object
patches_alle_jaren <- bind_rows(resultaat_lijst) %>%
  mutate(jaar = factor(jaar))

patches_2021 <- patches_alle_jaren %>%
  filter(jaar == 2021)

vlaanderen_crs <- st_read("C:/Users/emiel_delombaerde/Documents/R/Versnippering/Data/Data/protconn/vlaanderen_excl_bru_wgs84.shp") %>%
  dplyr::select(geometry) %>%
  st_transform(crs = 31370) %>%
  ms_simplify() %>%
  st_make_valid()
sf_use_s2(FALSE) # Voor robuustheid
opp_vlaanderen_ha <- as.numeric(st_area(vlaanderen_crs)) / 10000

mapview(vlaanderen_crs, col.region = "red") + mapview(patches_alle_jaren, zcol = "jaar")


controle_totaal <- patches_alle_jaren %>%
  # 1. Bereken de oppervlakte opnieuw op basis van de huidige geometrie
  # st_area geeft m², delen door 10.000 geeft hectare
  mutate(opp_ha_check = as.numeric(st_area(geometry)) / 10000) %>%

  # 2. Verwijder de geometrie (maakt de tabel lichter en leesbaarder)
  st_drop_geometry() %>%

  # 3. Groepeer per jaar en sommeer
  group_by(jaar) %>%
  summarise(
    Totaal_Opp_Ha = sum(opp_ha_check, na.rm = TRUE),
    Aantal_Patches = n()  # Ook nuttig: hoeveel losse polygoontjes zijn er?
  ) %>%
  ungroup() %>%
  mutate(perc_vl = Totaal_Opp_Ha/opp_vlaanderen_ha)

# Print de tabel
print(controle_totaal)


## wegschrijven gpkg per jaar

# 1. Definieer de output map
output_map <- "data/data_toon/shapes_per_jaar"

# Maak de map aan als deze nog niet bestaat (recursive = TRUE maakt ook bovenliggende mappen aan indien nodig)
if (!dir.exists(output_map)) {
  dir.create(output_map, recursive = TRUE)
}

# 2. Haal de unieke jaren uit je dataset
jaren <- unique(patches_alle_jaren$jaar)

# 3. Loop door elk jaar en sla op
for (huidig_jaar in jaren) {

  # Filter de data voor dit specifieke jaar
  temp_layer <- patches_alle_jaren %>%
    filter(jaar == huidig_jaar)

  # Maak de bestandsnaam (bv: "patches_2021.gpkg")
  bestandsnaam <- file.path(output_map, paste0("patches_", huidig_jaar, ".shp"))

  # Schrijf weg (delete_dsn = TRUE overschrijft het bestand als het al bestaat)
  st_write(temp_layer, dsn = bestandsnaam, delete_dsn = TRUE, quiet = TRUE)

  print(paste("Opgeslagen:", bestandsnaam))
}

#### boundary data toevoegen ####

# --- CONFIGURATIE ---
sf_use_s2(FALSE)

# Mappen definiëren
dir_binnen <- "data/data_toon/shapes_per_jaar"
dir_buiten <- "data/ccda_nat2000/"
dir_output <- "data/data_toon/shapes_transboundary"

# Output map maken indien nodig
if (!dir.exists(dir_output)) dir.create(dir_output)


# 2. Bestanden lijsten ophalen
# Lijst A: De patches die we al hadden (Binnen Vlaanderen)
# Zorg dat deze gesorteerd zijn (2021, 2022, 2023, 2024)
files_binnen <- list.files(path = dir_binnen, pattern = "\\.shp$", full.names = TRUE) %>% sort()

# Lijst B: De ruwe CCDA data (Buiten Vlaanderen + Binnen)
# Jij gaf aan indices 22 tot 25
files_buiten <- list.files(path = dir_buiten, pattern = "\\.shp$", full.names = TRUE) %>% sort()
files_buiten_selectie <- files_buiten[22:25]

# Check of we evenveel bestanden hebben
if (length(files_binnen) != length(files_buiten_selectie)) {
  stop("Aantal bestanden komt niet overeen! Check je indices of mappen.")
}

# 3. Loop door de 4 jaren
jaren <- c(2021, 2022, 2023, 2024)

for (i in 1:4) {

  huidig_jaar <- jaren[i]
  file_in <- files_binnen[i]
  file_out <- files_buiten_selectie[i]

  message(paste("--- Verwerken jaar:", huidig_jaar, "---"))
  message(paste("Binnen file:", basename(file_in)))
  message(paste("Buiten file:", basename(file_out)))

  # A. Inlezen Binnen-laag (deze is al schoon, maar voor zekerheid select geometry)
  layer_binnen <- st_read(file_in, quiet = TRUE) %>%
    dplyr::select(geometry) %>%
    mutate(bron = "binnen_vlaanderen")

  # B. Inlezen & Schoonmaken Buiten-laag
  # We moeten hier dezelfde strenge cleaning toepassen als eerder!
  layer_buiten_raw <- st_read(file_out, quiet = TRUE) %>%
    dplyr::select(geometry) %>%
    st_transform(crs = 31370) %>%
    st_make_valid() %>%
    ms_simplify() %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON")

  # C. De 'Knip': Verwijder alles wat IN Vlaanderen ligt uit de buiten-laag
  message("   ...Vlaanderen wegknippen uit buiten-laag (st_difference)...")

  layer_buiten_clean <- layer_buiten_raw %>%
    st_difference(vlaanderen_crs) %>%
    # st_difference kan lijnen/punten achterlaten op de grens -> opschonen
    st_collection_extract("POLYGON") %>%
    mutate(bron = "buiten_vlaanderen")

  # D. Samenvoegen
  layer_totaal <- bind_rows(layer_binnen, layer_buiten_clean) %>%
    mutate(jaar = huidig_jaar) %>%
    st_cast("POLYGON") # Zorg dat alles strikt polygon is

  # E. Opslaan
  output_naam <- file.path(dir_output, paste0("patches_transboundary_", huidig_jaar, ".shp"))
  st_write(layer_totaal, output_naam, delete_layer = TRUE, quiet = TRUE)

  message(paste("   Opgeslagen:", output_naam))
}
