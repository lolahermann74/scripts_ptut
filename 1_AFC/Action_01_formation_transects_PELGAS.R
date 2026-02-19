# Récupération des transects pour PELGAS 
# Auteur : Philippine Bacquey

# SARDINE ####
### Préparation des données ####
## Importation du jeu de données 
data <- read_delim("C:/Users/Bacquey/Desktop/M2/Ptut/WP1_indiv_trie.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Selection de l'espèce et de la campagne 
data <- data %>%
  filter(
    Nom_Scientifique == "Sardina pilchardus", 
    Campagne == "PELGAS"
  ) %>%
  select(Annee, LongDeb, LatDeb, LatFin, LongFin, Nbr, Strate, longueur_trait, Taille)

#Transforme en shapefile 
# Transformation en lignes (transects)
data_sf <- data %>%
  rowwise() %>%
  # On crée une ligne qui part de (LongDeb, LatDeb) vers (LongFin, LatFin)
  mutate(geometry = st_sfc(st_linestring(matrix(c(LongDeb, LongFin, LatDeb, LatFin), ncol = 2)))) %>%
  st_as_sf(crs = 4326) %>%
  ungroup()

# Exportation en Shapefile
st_write(data_sf, "C:/Users/Bacquey/Desktop/M2/Ptut/traits_PELGAS_lignes_sardine.shp", delete_layer = TRUE)
#exportation csv 
write_csv(data_sf, "C:/Users/Bacquey/Desktop/M2/Ptut/traits_PELGAS_lignes_sardine.csv")

coords_unique_sardine <- read.csv("C:/Users/Bacquey/Desktop/M2/Ptut/traits_PELGAS_lignes_sardine.csv") # ou ton data frame existant
head(coords_unique_sardine)

## Formation des transects ####
# Chargement du transect théorique (Route PELGAS)
zone_maq <- st_read("C:/Users/Bacquey/Desktop/M2/Ptut/transect maquereau/route_previ3.shp")
zone_maq_l93 <- st_transform(zone_maq, 2154)

# Préparation des données de capture
df_sardine <- coords_unique_sardine

# On calcule le point milieu pour chaque trait 
df_sardine <- df_sardine %>%
  mutate(
    LongMid = (LongDeb + LongFin) / 2,
    LatMid  = (LatDeb + LatFin) / 2
  )

coords_sardine_sf <- st_as_sf(
  df_sardine,
  coords = c("LongMid", "LatMid"),
  crs = 4326) %>%
  st_transform(2154)

# Rattachage au transect le plus proche
idx_transect <- st_nearest_feature(coords_sardine_sf, zone_maq_l93)

# Ajout des infos du transect et calcul de la distance d'erreur (en mètres)
coords_sardine_sf <- coords_sardine_sf %>%
  mutate(
    transect_index = idx_transect,
    transect_id    = zone_maq_l93$id[idx_transect], 
    dist_transect_m = as.numeric(st_distance(., zone_maq_l93[idx_transect, ], by_element = TRUE)))

# Nettoyage pour l'exportation
capture_transect_table_wgs84 <- coords_sardine_sf %>%
  st_transform(4326) %>%
  st_drop_geometry() %>%
  as.data.frame()

# Export
write.csv(capture_transect_table_wgs84, "C:/Users/Bacquey/Desktop/M2/Ptut/captures_transects_sardine_final.csv", row.names = FALSE)

# MAQUEREAU ####
### Préparation du shapefile ####
data <- read_delim("C:/Users/Bacquey/Desktop/M2/Ptut/WP1_indiv_trie_strandarise.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

data <- data %>%
  filter(
    Nom_Scientifique == "Scomber scombrus", 
    Campagne == "PELGAS") %>%
  # On garde les colonnes nécessaires
  select(Annee, LongDeb, LatDeb, LatFin, LongFin, Nbr, Strate, longueur_trait, Taille)

# Transformation en lignes (transects)
data_sf <- data %>%
  rowwise() %>%
  # On crée une ligne qui part de (LongDeb, LatDeb) vers (LongFin, LatFin)
  mutate(geometry = st_sfc(st_linestring(matrix(c(LongDeb, LongFin, LatDeb, LatFin), ncol = 2)))) %>%
  st_as_sf(crs = 4326) %>%
  ungroup()

# Exportation en Shapefile
st_write(data_sf, "C:/Users/Bacquey/Desktop/M2/Ptut/traits_PELGAS_lignes.shp", delete_layer = TRUE)
#exportation csv 
write_csv(data_sf, "C:/Users/Bacquey/Desktop/M2/Ptut/traits_PELGAS_lignes.csv")

coords_unique_maquereau <- read.csv("C:/Users/Bacquey/Desktop/M2/Ptut/traits_PELGAS_lignes.csv") # ou ton data frame existant
head(coords_unique_maquereau)

## Formation des transects ####
# Chargement du transect théorique (Route PELGAS)
zone_maq <- st_read("C:/Users/Bacquey/Desktop/M2/Ptut/transect maquereau/route_previ3.shp")
zone_maq_l93 <- st_transform(zone_maq, 2154)

# Préparation de tes données de capture
df_maquereau <- coords_unique_maquereau 

# Transformation en objet spatial (SF) basé sur le point milieu
coords_maquereau_sf <- st_as_sf(
  df_maquereau,
  coords = c("LongMid", "LatMid"),
  crs = 4326) %>%
  st_transform(2154)

# Nettoyage pour l'exportation
# On repasse en WGS84 pour avoir des coordonnées GPS classiques dans le CSV
capture_transect_table_wgs84 <- coords_maquereau_sf %>%
  st_transform(4326) %>%
  st_drop_geometry() %>%
  as.data.frame()

# Export
write.csv(capture_transect_table_wgs84, "C:/Users/Bacquey/Desktop/M2/Ptut/captures_transects_maquereau_final.csv", row.names = FALSE)

