setwd("C:/Users/mathi/Desktop/PTUT_M2/PTUT_AVIZONS")

#Série temporelle Merlu

merlu_enviro_11fev26_3752 <- read_csv("merlu_enviro_11fev26_3752.csv", 
                                      +     na = "NA")

WP1_indiv_trie1 <- read_csv("WP1_indiv_trie1.csv", 
                            +     na = "NA")

WP1 <- WP1_indiv_trie1
env_merlu <- merlu_enviro_11fev26_3752

WP1_merlu <- WP1 %>%
  filter(
    Nom_Scientifique == "Merluccius merluccius",
    Campagne == "EVHOE" )

#série temporelle pour sol avec une courbe par strate
WP1_merlu
env_merlu

head(WP1_merlu)
head(env_merlu)

library(dplyr)
library(lubridate)

###############Association strate du WP1 à l'environnement##################

#---- WP1 : on prend la position (Deb) et la date (DateDeb) ----
wp1_strate <- WP1_merlu %>%
  mutate(
    date  = as.Date(DateDeb),
    Annee = as.integer(format(date, "%Y"))
  ) %>%
  transmute(
    station_code = Code_Station,
    Strate,
    Annee,
    Long = LongDeb,
    Lat  = LatDeb
  ) %>%
  distinct()   # évite les doublons (WP1 a souvent plusieurs lignes par station/jour)

#on change le nom de la colonne pour avoir la même 
head(env_merlu)
env_merlu <- env_merlu %>%
  rename(station_code = Code_Station)

# ---- ENV : on convertit date POSIXct -> Année ----
env_day <- env_merlu %>%
  mutate(
    date  = as.Date(date),
    Annee = as.integer(format(date, "%Y"))
  ) %>%
  select(station_code, Long, Lat, Annee, everything())

#même station et même année
env_wp1_strate <- wp1_strate %>%
  left_join(
    env_day,
    by = c("station_code", "Long", "Lat", "Annee")
  )

#on supprime les ligne où il y a des NA
env_wp1_strate1 <- env_wp1_strate %>%
  filter(
    if_all(
      everything(),
      ~ !is.na(.x) & !is.nan(.x)
    ))
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

#####################Série temporelle#######################
#on choisi la colonne qu'on veut : 

env_strate_1 <- env_strate_annee %>%
  select(Strate, Annee, spco2_y2_sd)

head(env_strate_1)
env_strate_1

library(ggplot2)

ggplot(env_strate_1, aes(x = Annee, y = spco2_y2_sd, color = Strate)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Évolution interannuelle du spco2 par strate",
    x = "Année",
    y = "spco2",
    color = "Strate"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


#une ligne par année et une colonne par strate
env_strate_wide <- env_strate_1 %>%
  pivot_wider(
    names_from  = Strate,
    values_from = spco2_y2_sd
  ) %>%
  arrange(Annee)
head(env_strate_wide)

library(tibble)

#serie temporelle avec ts
env_strate_ts <- env_strate_wide %>%
  column_to_rownames("Annee") %>%
  ts(start = min(env_strate_1$Annee), frequency = 1)

autoplot(env_strate_ts) +
  ylab("Pression partielle en Co2 (spco2_y2_sd)") +
  xlab("Année") +
  ggtitle("")

##########supprimer les colonnes si il y a plus de 2 NA ####################################

head(env_strate_wide)

env_strate_wide_1 <- env_strate_wide %>%
  select(
    Annee,
    where(~ sum(is.na(.)) <= 2)
  )

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
  frequency = 1
)

# visualisation multi-séries
autoplot(ts_strate) +
  ylab("pression partielle en CO2 (spco2_y2_sd)") +
  xlab("Année") +
  ggtitle("")

####Amélioration graphique

library(tidyr)
library(dplyr)

env_long <- env_strate_wide_1 %>%
  pivot_longer(
    -Annee,
    names_to = "strate_id",
    values_to = "spco2_y2_sd"
  ) %>%
  drop_na(spco2_y2_sd)

labels_start <- env_long %>%
  group_by(strate_id) %>%
  slice_min(Annee)

labels_end <- env_long %>%
  group_by(strate_id) %>%
  slice_max(Annee)


#bon graphique : 

# 4) Seuil (à remplacer)
seuil_sal <- 2.697   # <-- mets ta valeur ici

#Bon graphique avec les bonnes couleurs pour les strates

# 1) Long format (comme toi)
env_long <- env_strate_wide_1 %>%
  pivot_longer(-Annee, names_to = "strate_id", values_to = "spco2_y2_sd") %>%
  drop_na(spco2_y2_sd)

# 2) Ordre géographique Sud -> Nord (à adapter si tes noms exacts diffèrent)
ordre_strates <- c("Gs7","Gs6","Gs5", "Gs4", "Gs3", "Gs2", "Gs1", "Gn7", "Gn6", "Gn5", "Gn4", "Gn3", "Gn2", "Gn1", "Cs7", "Cs6", "Cs5", "Cs4", "Cs3", "Cc7", "Cc6", "Cc5", "Cc4", "Cc3", "Cn3", "Cn2")

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

seuil_sal <- 2.697

ggplot(env_long, aes(x = Annee, y = spco2_y2_sd, group = strate_id, colour = strate_id)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_hline(yintercept = seuil_sal, linetype = "dashed", linewidth = 0.9, colour = "black") +
  
  # plus de place à gauche/droite pour les étiquettes
  geom_text(data = labels_start, aes(label = strate_id),
            hjust = 1.1, size = 4, show.legend = FALSE) +
  geom_text(data = labels_end, aes(label = strate_id),
            hjust = -0.1, size = 4, show.legend = FALSE) +
  
  # IMPORTANT : viridis option C va du violet -> jaune dans l'ordre des niveaux du facteur
  scale_colour_viridis_d(option = "C", begin = 0.05, end = 0.98) +
  
  scale_x_continuous(
    breaks = sort(unique(env_long$Annee))[seq(1, length(unique(env_long$Annee)), 2)],
    expand = expansion(mult = c(0.08, 0.12))  # <- réduit l’échelle utile et ajoute marge pour les labels
  ) +
  
  coord_cartesian(ylim = c(0, 10), clip = "off") +  # <- autorise les labels à dépasser
  labs(x = "Année", y = "Pression partielle en CO2 (spco2_y2_sd)") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 40, 10, 40),      # <- marges pour voir les noms à gauche/droite
    axis.text.x = element_text(size = 12)
  )


#Graphique en radar

# 1) Préparer les années comme facteur (ordre chronologique)
env_radar <- env_long %>%
  mutate(Annee = factor(Annee, levels = sort(unique(Annee)))) %>%
  arrange(strate_id, Annee)

# 2) Fermer la boucle (répéter le 1er point à la fin pour chaque strate)
env_radar_closed <- env_radar %>%
  group_by(strate_id) %>%
  summarise(
    Annee = c(as.character(Annee), as.character(first(Annee))),
    ph_y2_sd = c(spco2_y2_sd, first(spco2_y2_sd)),
    .groups = "drop"
  ) %>%
  mutate(Annee = factor(Annee, levels = levels(env_radar$Annee)))

# 3) Radar (polar)
ggplot(env_radar_closed,
       aes(x = Annee, y = spco2_y2_sd, group = strate_id, colour = strate_id)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_hline(yintercept = seuil_sal, linetype = "dashed", linewidth = 0.9, colour = "black") +
  geom_point(size = 0.1, alpha = 0.9) +
  coord_polar(start = 0) +
  scale_colour_viridis_d(option = "C", begin = 0.05, end = 0.98) +
  labs(
    title = "",
    x = NULL,
    y = "CO2 (spco2_y2_sd)",
    colour = "Strate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 9),
    legend.position = "right"
  )
