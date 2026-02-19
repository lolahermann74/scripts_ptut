# Série temporelle de la température (thetao_m3_q3) par transect pour le maquereau
# Auteur : Mathis Damestoy

# Packages
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)
library(viridis)
library(readr)
library(forecast)

# Importation des données ####
# Chemin vers les datasets (à adapter)
setwd("G:/PTUT_AVIZONS/wetransfer") # mettre le bon chemin
getwd()

# datasets
maquereau_enviro_10fev26_350 <- read.csv("maquereau_enviro_10fev26_350.csv")
maquereau_with_transects_final <- read.csv("maquereau_with_transects_final.csv")

# Les renommer pour plus de clarté
env_maquereau <- maquereau_enviro_10fev26_350
transect_maquereau <- maquereau_with_transects_final

# Vérification rapide des données
head(env_maquereau)
head(transect_maquereau)

# Rajouter la colonne transect id au jeu de donnée env_maquereau ####
# On nettoie d’abord transect_maquereau pour n’avoir qu’une ligne par station
ref_transect <- transect_maquereau %>%
  select(Code_Station, Long, Lat, transect_id) %>%
  distinct()

# On rajoute transect_id à env_maquereau
env_maquereau_transect <- env_maquereau %>%
  left_join(
    ref_transect,
    by = c("Code_Station", "Long", "Lat"))

env_maquereau_transect$transect_id

env_transect_annee <- env_maquereau_transect %>%
  filter(!is.na(transect_id)) %>% # garder seulement les points matchés
  group_by(transect_id, Annee = year) %>% # Transect × Année
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)),
    .groups = "drop")
head(env_transect_annee)

# Garder la variable ressortant du random forest thetao_m3_q3####
# + les colonne qui nous intéressent
env_transect_var <- env_transect_annee %>%
  select(transect_id, Annee, thetao_m3_q3)

head(env_transect_var)

# Une ligne par année et une colonne par strate
env_transect_wide <- env_transect_var %>%
  pivot_wider(
    names_from  = transect_id,
    values_from = thetao_m3_q3) %>%
  arrange(Annee)

head(env_transect_wide)

# Transformer en série temporelle avec ts
env_transect_ts <- env_transect_wide %>%
  column_to_rownames("Annee") %>%
  ts(start = min(env_transect_var$Annee), frequency = 1)

autoplot(env_transect_ts) +
  ylab("température (thetao_m3_q3)") +
  xlab("Année") +
  ggtitle("")

# Supprimer les colonnes si il y a plus de 2 NA 

head(env_transect_wide)

env_transect_wide_1 <- env_transect_wide %>%
  select(
    Annee,
    where(~ sum(is.na(.)) <= 2))

head(env_transect_wide_1)

# matrice transects
mat <- env_transect_wide_1 %>%
  arrange(Annee) %>%
  select(-Annee) %>%
  as.matrix()

# série temporelle annuelle
ts_transect <- ts(
  mat,
  start = min(env_transect_wide_1$Annee),
  frequency = 1)

# visualisation multi-séries
autoplot(ts_transect) +
  ylab("température (thetao_m3_q3)") +
  xlab("Année") +
  ggtitle("")

## Amélioration graphique
env_long <- env_transect_wide_1 %>%
  pivot_longer(
    -Annee,
    names_to = "transect_id",
    values_to = "thetao_m3_q3") %>%
  drop_na(thetao_m3_q3)

labels_start <- env_long %>%
  group_by(transect_id) %>%
  slice_min(Annee)

labels_end <- env_long %>%
  group_by(transect_id) %>%
  slice_max(Annee)

ggplot(env_long,
       aes(x = Annee,
           y = thetao_m3_q3,
           group = transect_id,
           colour = transect_id)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_text(data = labels_start, aes(label = transect_id), hjust = 1.1,  # labels debut
            size = 3, show.legend = FALSE) +
  
  
  geom_text(data = labels_end, aes(label = transect_id), hjust = -0.1,# labels fin
            size = 3,
            show.legend = FALSE) +
  labs(title = "Évolution interannuelle de la température par transect",
       x = "Année",
       y = "température (thetao_m3_q3)") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"))


# 1 Graphique par zone géographique ####
# 1) Définir un ordre Sud -> Nord + une "famille" de transects (4 groupes)
env_long2 <- env_long %>%
  mutate(
    base = as.numeric(str_extract(transect_id, "^\\d+")),
    lat_score = case_when(
      base %in% c(28, 29) ~ 340 + base,  # 28/29 encore + nord
      TRUE ~ base),
    groupe = case_when(
      transect_id %in% c("28","28_2","29","29_2") ~ "28 à 29_2 (Nord++)",
      base >= 300 & base <= 310 ~ "300 à 310_2 (Sud)",
      base >= 311 & base <= 320 ~ "310 à 320_2 (Centre)",
      base >= 321 & base <= 330 ~ "320_2 à 330_2 (Nord)",
      TRUE ~ "Autres"))

# ordre Sud -> Nord (sert à fixer les couleurs de façon stable)
ordre_transects <- env_long2 %>%
  distinct(transect_id, lat_score) %>%
  arrange(lat_score) %>%
  pull(transect_id)

env_long2 <- env_long2 %>%
  mutate(
    transect_id = factor(transect_id, levels = ordre_transects),
    groupe = factor(groupe, levels = c(
      "28 à 29_2 (Nord++)",
      "300 à 310_2 (Sud)",
      "310 à 320_2 (Centre)",
      "320_2 à 330_2 (Nord)",
      "Autres")))

# 2) Palette DISCRÈTE mais ordonnée (mêmes couleurs dans tous les panneaux)
pal <- setNames(viridis(nlevels(env_long2$transect_id), option = "C", begin = 0.1, end = 0.95),
                levels(env_long2$transect_id))

# 3) Labels début/fin (par transect) -> et on les garde dans le bon groupe
labels_start2 <- env_long2 %>%
  group_by(transect_id) %>%
  slice_min(Annee, with_ties = FALSE) %>%
  ungroup()

labels_end2 <- env_long2 %>%
  group_by(transect_id) %>%
  slice_max(Annee, with_ties = FALSE) %>%
  ungroup()

# 4) Seuil (à remplacer)
seuil_sal <- 12.891   # <-- mets ta valeur ici

# 5) Plot en 4 panneaux empilés, mêmes échelles
ggplot(env_long2 %>% filter(groupe != "Autres"),
       aes(x = Annee, y = thetao_m3_q3, group = transect_id, colour = transect_id)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_hline(yintercept = seuil_sal, linetype = "dashed", linewidth = 0.6, colour = "black") +
  geom_text(
    data = labels_start2 %>% filter(groupe != "Autres"),
    aes(label = transect_id),
    hjust = 1.1, size = 2.8, show.legend = FALSE) +
  geom_text(data = labels_end2 %>% filter(groupe != "Autres"),
            aes(label = transect_id), 
            hjust = -0.1, size = 2.8, show.legend = FALSE) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = sort(unique(env_long2$Annee))) +
  facet_wrap(~ groupe, ncol = 1, scales = "fixed") +
  labs(title = "Évolution interannuelle de la salinité par transect",
       x = "Année", y = "température (thetao_m3_q3)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

# Graphique pour le rapport : 

# 4) Seuil (à remplacer)
seuil_sal <- 12.891   # <-- mets ta valeur ici

ggplot(env_long2,
       aes(x = Annee, y = thetao_m3_q3, group = transect_id, colour = transect_id)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  geom_hline(yintercept = seuil_sal,
             linetype = "dashed",
             linewidth = 0.9,
             colour = "black") +
  geom_text(data = labels_start2, aes(label = transect_id),
            hjust = 1.1, size = 4, show.legend = FALSE) +
  geom_text(data = labels_end2, aes(label = transect_id),
            hjust = -0.1, size = 4, show.legend = FALSE) +
  scale_colour_viridis_d(option = "C", end = 0.95, begin = 0.1) +
  scale_x_continuous(breaks = sort(unique(env_long2$Annee))) +
  coord_cartesian(ylim = c(11, 15)) +   # Échelle fixée
  labs(x = "Année",
       y = "Thetao (thetao_m3_q3)") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14))

# Fin de script
