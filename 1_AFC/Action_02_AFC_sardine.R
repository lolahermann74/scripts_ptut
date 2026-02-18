# AFC Sardine 
# Auteur : Philippine Bacquey

#Packages 
library(readr)      
library(tidyverse)  
library(gtools)    
library(ade4)
library(FactoMineR)
library(factoextra)
library(explor)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(RColorBrewer)

# Préparation des données ####
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

## Formation des transects ####
# Préparation des données de capture
coords_unique_sardine <- read.csv("C:/Users/Bacquey/Desktop/M2/Ptut/traits_PELGAS_lignes_sardine.csv") # ou ton data frame existant
head(coords_unique_sardine)
df_sardine <- coords_unique_sardine

# Calcul du point au milieu pour chaque trait 
df_sardine <- df_sardine %>%
  mutate(
    LongMid = (LongDeb + LongFin) / 2,
    LatMid  = (LatDeb + LatFin) / 2
  )

# Chargement du transect théorique (Route PELGAS)
zone_maq <- st_read("C:/Users/Bacquey/Desktop/M2/Ptut/transect maquereau/route_previ3.shp")
zone_maq_l93 <- st_transform(zone_maq, 2154)

# Rattachage au transect le plus proche
idx_transect <- st_nearest_feature(coords_sardine_sf, zone_maq_l93)

# Ajout des infos du transect et calcul de la distance d'erreur (en mètres)
coords_sardine_sf <- coords_sardine_sf %>%
  mutate(
    transect_index = idx_transect,
    transect_id    = zone_maq_l93$id[idx_transect], 
    dist_transect_m = as.numeric(st_distance(., zone_maq_l93[idx_transect, ], by_element = TRUE))
  )

#  Visualisation
nb_transects <- length(unique(capture_transect_table_wgs84$transect_id))
palette_transect <- scales::hue_pal()(nb_transects) 

ggplot() +
  geom_sf(data = zone_maq_l93, color = "black", linewidth = 0.8, alpha = 0.5) +
  geom_sf(data = coords_sardine_sf, aes(color = as.factor(transect_id)), size = 2) +
  scale_color_manual(values = palette_transect) +
  theme_minimal() +
  labs(
    color = "Transect ID",
    title = "Rattachage des traits PELGAS aux transects théoriques",
    subtitle = "Position basée sur le milieu du trait de chalut"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Export
write.csv(capture_transect_table_wgs84, "C:/Users/Bacquey/Desktop/M2/Ptut/captures_transects_sardine_final.csv", row.names = FALSE)

## Préparation du data avec les transects  ####
df <- read.csv("C:/Users/Bacquey/Desktop/M2/Ptut/captures_transects_sardine_final.csv")
head(df)
head(data)

# Retire les doublons 
correspondance_transect <- df %>%
  select(LongDeb, LatDeb, LongFin, LatFin, transect_id) %>%
  distinct()

# Fusion avec data
data_avec_transect <- data %>%
  left_join(correspondance_transect, 
            by = c("LongDeb", "LatDeb", "LongFin", "LatFin"))

# Vérification
head(data_avec_transect)


## Préparation des Classes ####
df_classe <- data_avec_transect %>%
  mutate(Classe_Tri = if_else(Taille > 15, "Grand", "Petit")) %>%
  select(Annee, LongDeb, LatDeb, LatFin, LongFin, Nbr, Classe_Tri, transect_id, longueur_trait)

length(unique(df_classe$Annee))
names(df_classe)

# On vérifie les NA 
df_classe %>%
  summarise(across(everything(), ~ sum(is.na(.))))
df_classe <- df_classe %>%
  drop_na()

# Compte le nombre total d'individus capturés par classe
df_classe %>% 
  group_by(Classe_Tri) %>% 
  summarise(Total_Nbr = sum(Nbr))

# Liste des noms des strates (sans doublons)
unique(df_classe$transect_id)
length(unique(df$transect_id))
unique(df_classe$Annee)

# Standarise ####
df_prep_somme <- df_classe %>%
  group_by(transect_id, Annee, Classe_Tri) %>%
  summarise(
    Somme_Nbr = sum(Nbr, na.rm = TRUE),
    Somme_Dist = sum(longueur_trait, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Abondance_Std = (Somme_Nbr / Somme_Dist) * 1000)

# Vérification division par zéro 
df_prep_somme <- df_prep_somme %>% filter(!is.infinite(Abondance_Std))

# Aggrégation ####
# Transformation au format large
df_matrice <- df_prep_somme %>%
  complete(transect_id, Annee, Classe_Tri, fill = list(Abondance_Std = 0)) %>%
  pivot_wider(
    id_cols = transect_id,
    names_from = c(Annee, Classe_Tri),
    names_sep = "_",
    values_from = Abondance_Std
  )

# Passage en rownames pour l'AFC
df_final <- df_matrice %>% column_to_rownames(var = "transect_id")


### Diagnostics ##
any(is.na(df_final))
sum(is.na(df_final)) 
prop_zeros <- mean(df_final == 0) * 100
cat("Proportion de zéros dans la matrice :", round(prop_zeros, 2), "%\n")

df_afc <- df_final[rowSums(df_final) > 0, colSums(df_final) > 0]

# AFC ####
afc <- dudi.coa(df_afc, scannf = FALSE, nf = 2)
explor(afc)

## AFC plus belle ####
res.afc <- CA(df_afc, graph = FALSE)

#Graphiquer inertie 
fviz_eig(res.afc, 
         addlabels = TRUE, 
         barfill = "#2c3e50", 
         barcolor = "white",
         linecolor = "#e74c3c",
         main = "Inertie expliquée par axe (%)") +
  theme_minimal()


#Graphique année + classe 
# On crée un vecteur de groupes basé sur le nom des colonnes
groupes_taille <- ifelse(grepl("Grand", colnames(df_afc)), "Grand", "Petit")

fviz_ca_col(res.afc,
            repel = TRUE,
            habillage = as.factor(groupes_taille), 
            palette = c("#E74C3C", "#3498DB"),    
            title = "AFC — Séparation spatio-temporelle des tailles",
            pointsize = 2,      
            labelsize = 5       
) +
  theme_minimal() +
  labs(
    color = "Groupes de taille", 
    shape = "Groupes de taille"  
  ) +
  theme(
    axis.title = element_text(face = "bold", size = 16, color = "black"), 
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(face = "bold", size = 14)
  )

#Graphique site 
fviz_ca_row(res.afc,
            repel = TRUE,                  
            geom = c("point", "text"),     
            pointsize = 4,                 
            labelsize = 5,                 
            col.row = "#2c3e50") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14, color = "black"), 
    axis.text = element_text(face = "bold", size = 12, color = "black")
  )

## Fin du script 