# Récupération des transects pour PELGAS 
# Auteur : Philippine Bacquey

# SARDINE ####

# Packages
library(dplyr)
library(readr)
library(sf)

### Préparation des données ####
## Importation du jeu de données 

# Importation des données ####
# Chemin vers les datasets (à adapter)
setwd("G:/PTUT_AVIZONS/wetransfer") # mettre le bon chemin
getwd()

data <- read.csv("WP1_indiv_trie.csv")

# Sélection de l'espèce et de la campagne (+ des variables d'intérêts)
sardine <- data %>%
  filter(Nom_Scientifique == "Sardina pilchardus", Campagne == "PELGAS") %>%
  select(Annee, LongDeb, LatDeb, LatFin, LongFin, Nbr, Strate, longueur_trait, Taille)

#Transforme en shapefile 
# Transformation en lignes (transects)
sardine_sf <- sardine %>%
  rowwise() %>%
  # On crée une ligne qui part de (LongDeb, LatDeb) vers (LongFin, LatFin)
  mutate(geometry = st_sfc(st_linestring(matrix(c(LongDeb, LongFin, LatDeb, LatFin), ncol = 2)))) %>%
  st_as_sf(crs = 4326) %>%
  ungroup()

# Exportation en Shapefile
st_write(sardine_sf, "traits_PELGAS_lignes_sardine.shp", delete_layer = TRUE)
#exportation csv 
write_csv(sardine_sf, "traits_PELGAS_lignes_sardine.csv")

coords_unique_sardine <- read.csv("traits_PELGAS_lignes_sardine.csv")
head(coords_unique_sardine)

## Formation des transects ####
# Chargement du transect théorique (Route PELGAS)
zone_maq <- st_read("route_previ3.shp")
zone_maq_l93 <- st_transform(zone_maq, 2154)

# Préparation des données de capture
df_sardine <- coords_unique_sardine

# On calcule le point milieu pour chaque trait 
df_sardine <- df_sardine %>%
  mutate(LongMid = (LongDeb + LongFin) / 2,
         LatMid  = (LatDeb + LatFin) / 2)

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
capture_transect_table_wgs84_sar <- coords_sardine_sf %>%
  st_transform(4326) %>%
  st_drop_geometry() %>%
  as.data.frame()

# Export
write.csv(capture_transect_table_wgs84_sar, "captures_transects_sardine_final2.csv", row.names = FALSE)

# MAQUEREAU ####
### Préparation du shapefile ####
maquereau <- data %>%
  filter(Nom_Scientifique == "Scomber scombrus", 
         Campagne == "PELGAS") %>%
  # On garde les colonnes nécessaires
  select(Annee, LongDeb, LatDeb, LatFin, LongFin, Nbr, Strate, longueur_trait, Taille)

# Transformation en lignes (transects)
maquereau_sf <- maquereau %>%
  rowwise() %>%
  # On crée une ligne qui part de (LongDeb, LatDeb) vers (LongFin, LatFin)
  mutate(geometry = st_sfc(st_linestring(matrix(c(LongDeb, LongFin, LatDeb, LatFin), ncol = 2)))) %>%
  st_as_sf(crs = 4326) %>%
  ungroup()

# Exportation en Shapefile
st_write(maquereau_sf, "traits_PELGAS_lignes_maquereau.shp", delete_layer = TRUE)
#exportation csv 
write_csv(maquereau_sf, "traits_PELGAS_lignes_maquereau.csv")

coords_unique_maquereau <- read.csv("traits_PELGAS_lignes_maquereau.csv")
head(coords_unique_maquereau)

## Formation des transects ####
# Il s'agit des mêmes transects théoriques (Route PELGAS) que pour la sardine : "route_previ3.shp"
# Préparation de tes données de capture
df_maquereau <- coords_unique_maquereau 

# On calcule le point milieu pour chaque trait 
df_maquereau <- df_maquereau %>%
  mutate(LongMid = (LongDeb + LongFin) / 2,
         LatMid  = (LatDeb + LatFin) / 2)

# Transformation en objet spatial (SF) basé sur le point milieu
coords_maquereau_sf <- st_as_sf(
  df_maquereau,
  coords = c("LongMid", "LatMid"),
  crs = 4326) %>%
  st_transform(2154)

# Nettoyage pour l'exportation
# On repasse en WGS84 pour avoir des coordonnées GPS classiques dans le CSV
capture_transect_table_wgs84_maq <- coords_maquereau_sf %>%
  st_transform(4326) %>%
  st_drop_geometry() %>%
  as.data.frame()

# Export
write.csv(capture_transect_table_wgs84_maq, "captures_transects_maquereau_final.csv", row.names = FALSE)

