# AFC  Merlu
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

# Préparation des données ####
data <- read_delim("C:/Users/Bacquey/Desktop/M2/Ptut/WP1_indiv_trie.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Préparation des Classes ####
#Taille = 57 cm
df <- data %>%
  filter(
    Nom_Scientifique == "Merluccius merluccius",
    Campagne == "EVHOE"
  ) %>%
  mutate(Classe_Tri = if_else(Taille > 57, "Grand", "Petit")) %>%
  select(Annee, LongDeb, LatDeb, LatFin, LongFin, Nbr, Classe_Tri, Strate, longueur_trait)

# On vérifie les NA 
df %>%
  summarise(across(everything(), ~ sum(is.na(.))))
df <- df %>%
  drop_na()

# Compte le nombre total d'individus capturés par classe
df %>% 
  group_by(Classe_Tri) %>% 
  summarise(Total_Nbr = sum(Nbr))

# Liste des noms des strates (sans doublons)
unique(df$Strate)
length(unique(df$Strate))
unique(df$Annee)

# On prend à partie de 1995
df <- df %>%
  filter(Annee >= 1995) 

# Compte le nombre de strates uniques
n_distinct(df$Strate)

verif_strates <- df %>%
  group_by(Strate) %>%
  summarise(
    Nb_Points = n(),
    Nb_Annees = n_distinct(Annee),
    Annee_Debut = min(Annee),
    Annee_Fin = max(Annee)
  ) %>%
  arrange(desc(Nb_Points)) 

print(verif_strates)


# Standarise ####
df_prep_somme <- df %>%
  group_by(Strate, Annee, Classe_Tri) %>%
  summarise(
    Somme_Nbr = sum(Nbr, na.rm = TRUE),
    Somme_Dist = sum(longueur_trait, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Abondance_Std = (Somme_Nbr / Somme_Dist) * 1000)

df_prep_somme <- df_prep_somme %>% filter(!is.infinite(Abondance_Std))

# Transformation au format large
df_matrice <- df_prep_somme %>%
  complete(Strate, Annee, Classe_Tri, fill = list(Abondance_Std = 0)) %>%
  pivot_wider(
    id_cols = Strate,
    names_from = c(Annee, Classe_Tri),
    names_sep = "_",
    values_from = Abondance_Std
  )

# Passage en rownames pour l'AFC
df_final <- df_matrice %>% column_to_rownames(var = "Strate")

### Diagnostics ##
any(is.na(df_final))
sum(is.na(df_final)) 
# Calcul de la proportion de zéros 
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
groupes_taille <- ifelse(grepl("Grand", colnames(df_afc)), "Grand", "Petit")

# Graphique avec couleurs par classe
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

# Fin du script 