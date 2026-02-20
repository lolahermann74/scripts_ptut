# Série temporelle de la salinité calculé sur les 2 années précédant chaque capture
# (sal_y2_sd) par transect pour la sole
# Auteur : Mathis Damestoy

# Packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(tibble)
library(tidyr)
library(forecast)

# Importation des données ####
# Chemin vers les datasets (à adapter)
setwd("G:/PTUT_AVIZONS/wetransfer") # mettre le bon chemin
getwd()

sole_enviro_11fev26_880 <- read.csv("sole_enviro_11fev26_880.csv", na = "NA")
WP1_indiv_trie <- read.csv("WP1_indiv_trie.csv", na = "NA")

# Les renommer pour plus de clarté
WP1 <- WP1_indiv_trie
env_sole <- sole_enviro_11fev26_880

# Filtrer la bonne espèce de la bonne campagne
WP1_sole <- WP1 %>%
  filter(
    Nom_Scientifique == "Solea solea",
    Campagne == "ORHAGO" )

# Visualisation rapide des données
head(WP1_sole)
head(env_sole)


# série temporelle pour sol avec une courbe par strate

# Association strate du WP1 à l'environnement####

# WP1 : on prend la position (Deb) et la date (DateDeb)
wp1_strate <- WP1_sole %>%
  mutate(date  = as.Date(DateDeb),
         Annee = as.integer(format(date, "%Y"))) %>%
  transmute(station_code = Code_Station,
            Strate,
            Annee,
            Long = LongDeb,
            Lat  = LatDeb) %>%
  distinct()   

# On change le nom de la colonne pour avoir la même 
head(env_sole)
env_sole <- env_sole %>%
  rename(station_code = Code_Station)

# ENV : on convertit date POSIXct -> Année 
env_day <- env_sole %>%
  mutate(date  = as.Date(date),
         Annee = as.integer(format(date, "%Y"))) %>%
  select(station_code, Long, Lat, Annee, everything())

#même station et même année
env_wp1_strate <- wp1_strate %>%
  left_join(env_day, by = c("station_code", "Long", "Lat", "Annee"))

#on supprime les ligne où il y a des NA
env_wp1_strate1 <- env_wp1_strate %>%
  filter(if_all(everything(), ~ !is.na(.x) & !is.nan(.x)))

sum(!complete.cases(env_wp1_strate1))  # doit être 0
any(is.nan(as.matrix(env_wp1_strate1))) # doit être FALSE

#Moyenner chaque strate par année 
#Une ligne pour une strate à une année

head(env_wp1_strate1)

env_strate_annee <- env_wp1_strate1 %>%
  mutate(Annee = year(date)) %>%
  group_by(Strate, Annee) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Strate = gsub("^Strate\\s+", "", Strate),
    Strate_Annee = paste0(Strate, "_", Annee))

head(env_strate_annee)


### Série temporelle 

# Garder la variable ressortant du random forest sal_y2_sd ####
env_strate_1 <- env_strate_annee %>%
  select(Strate, Annee, sal_y2_sd)

head(env_strate_1)
env_strate_1

ggplot(env_strate_1, aes(x = Annee, y = sal_y2_sd, color = Strate)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Évolution interannuelle de la salinité par strate",
       x = "Année",
       y = "salinité",
       color = "Strate") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"))


# Une ligne par année et une colonne par strate
env_strate_wide <- env_strate_1 %>%
  pivot_wider(
    names_from  = Strate,
    values_from = sal_y2_sd) %>%
  arrange(Annee)
head(env_strate_wide)


# Transformer en série temporelle avec ts
env_strate_ts <- env_strate_wide %>%
  column_to_rownames("Annee") %>%
  ts(start = min(env_strate_1$Annee), frequency = 1)

autoplot(env_strate_ts) +
  ylab("salinité (sal_y2_sd)") +
  xlab("Année") +
  ggtitle("")

# Supprimer les colonnes si il y a plus de 2 NA ####
head(env_strate_wide)

env_strate_wide_1 <- env_strate_wide %>%
  select(Annee, where(~ sum(is.na(.)) <= 2))

head(env_strate_wide_1)

# matrice transects
mat <- env_strate_wide_1 %>%
  arrange(Annee) %>%
  select(-Annee) %>%
  as.matrix()

# série temporelle annuelle
ts_strate <- ts(
  mat,
  start = min(env_strate_wide_1$Annee),
  frequency = 1)

# visualisation multi-séries
autoplot(ts_strate) +
  ylab("salinité (sal_y2_sd)") +
  xlab("Année") +
  ggtitle("")

####Amélioration graphique
env_long <- env_strate_wide_1 %>%
  pivot_longer(-Annee, names_to = "strate_id",values_to = "sal_y2_sd") %>%
  drop_na(sal_y2_sd)

labels_start <- env_long %>%
  group_by(strate_id) %>%
  slice_min(Annee)

labels_end <- env_long %>%
  group_by(strate_id) %>%
  slice_max(Annee)


# Graphique rapport :

# Seuil 
seuil_sal <- 1.093

#Bon graphique avec les bonnes couleurs pour les strates

  # 1) Long format (comme toi)
env_long <- env_strate_wide_1 %>%
  pivot_longer(-Annee, names_to = "strate_id", values_to = "sal_y2_sd") %>%
  drop_na(sal_y2_sd)

  # 2) Ordre géographique Sud -> Nord
ordre_strates <- c("Sud", "Centre Large", "Centre Côte", "Nord")

env_long <- env_long %>%
  mutate(strate_id = factor(strate_id, levels = ordre_strates))

labels_start <- env_long %>%
  group_by(strate_id) %>%
  slice_min(Annee, with_ties = FALSE) %>%
  ungroup()

labels_end <- env_long %>%
  group_by(strate_id) %>%
  slice_max(Annee, with_ties = FALSE) %>%
  ungroup()

seuil_sal <- 1.093

ggplot(env_long, aes(x = Annee, y = sal_y2_sd, group = strate_id, colour = strate_id)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_hline(yintercept = seuil_sal, linetype = "dashed", linewidth = 0.9, colour = "black") +
  
  # plus de place à gauche/droite pour les étiquettes
  geom_text(data = labels_start, aes(label = strate_id),
            hjust = 1.1, size = 4, show.legend = FALSE) +
  geom_text(data = labels_end, aes(label = strate_id),
            hjust = -0.1, size = 4, show.legend = FALSE) +
  scale_colour_viridis_d(option = "C", begin = 0.05, end = 0.98) +
  scale_x_continuous(breaks = sort(unique(env_long$Annee)),
                     expand = expansion(mult = c(0.08, 0.12))) +  
  coord_cartesian(ylim = c(0, 3), clip = "off") +
  labs(x = "Année", y = "Salinité (sal_y2_sd) en PSU") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 40, 10, 40),
    axis.text.x = element_text(size = 12))

