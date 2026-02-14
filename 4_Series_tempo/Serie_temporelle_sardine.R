
#Série temporelle moyenne de tous les sites pour chaque jour pour une variable envi


### Packages
library(dplyr)
library(tidyr)
library(sf)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(readr)

###Importation des données
setwd("C:/Users/mathi/Desktop/PTUT_M2/PTUT_AVIZONS")

#sardine

WP1_indiv_trie1 <- read_csv("WP1_indiv_trie1.csv", 
                            +     na = "NA")

sardine_enviro_10fev26_398 <- read_csv("sardine_enviro_10fev26_398.csv", 
                                       +     na = "NA")

sardine_with_transects_final_1_ <- read_delim("sardine_with_transects_final (1).csv", 
                                              +     delim = ";", escape_double = FALSE, na = "NA", 
                                              +     trim_ws = TRUE)

WP1 <- WP1_indiv_trie1
env_sardine <- sardine_enviro_10fev26_398

WP1_sardine <- WP1 %>%
  filter(
    Nom_Scientifique == "Sardina pilchardus",
    Campagne == "PELGAS" )

transect_sardine <- sardine_with_transects_final_1_

head(env_sardine)
head(transect_sardine)

#rajouter la colonne transect id au jeu de donnée env_sardine

library(dplyr)

#On nettoie d’abord transect_sardine pour n’avoir qu’une ligne par station
ref_transect <- transect_sardine %>%
  select(Code_Station, Long, Lat, transect_id) %>%
  distinct()

#on rajoute transect_id à env_sardine
env_sardine_transect <- env_sardine %>%
  left_join(
    ref_transect,
    by = c("Code_Station", "Long", "Lat"))

head(env_sardine_transect)
env_sardine_transect$transect_id
sum(is.na(env_sardine_transect$transect_id))

env_transect_annee <- env_sardine_transect %>%
  filter(!is.na(transect_id)) %>%                         # garder seulement les points matchés
  group_by(transect_id, Annee = year) %>%                 # Transect × Année
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
head(env_transect_annee)

###################################on garde les colonne qui nous interesse###################################################
env_transect_var <- env_transect_annee %>%
  select(transect_id, Annee, sal_m6_q3)

head(env_transect_var)

library(dplyr)
library(tidyr)

#une ligne par année et une colonne par strate
env_transect_wide <- env_transect_var %>%
  pivot_wider(
    names_from  = transect_id,
    values_from = sal_m6_q3
  ) %>%
  arrange(Annee)
head(env_transect_wide)

library(tibble)

#serie temporelle avec ts
env_transect_ts <- env_transect_wide %>%
  column_to_rownames("Annee") %>%
  ts(start = min(env_transect_var$Annee), frequency = 1)

autoplot(env_transect_ts) +
  ylab("salinité (sal_m6_q3)") +
  xlab("Année") +
  ggtitle("")

################supprimer les colonnes ayant des na#######################################
env_transect_wide_clean <- env_transect_wide %>%
  select(
    Annee,
    where(~ !any(is.na(.)))
  )

head(env_transect_wide_clean)

library(fpp2)

# matrice transects
mat <- env_transect_wide_clean %>%
  arrange(Annee) %>%
  select(-Annee) %>%
  as.matrix()

# série temporelle annuelle
ts_transect <- ts(
  mat,
  start = min(env_transect_wide_clean$Annee),
  frequency = 1
)

# plot multi-séries
autoplot(ts_transect) +
  ylab("Température (thetao_t0)") +
  xlab("Année") +
  ggtitle("Évolution annuelle de la température par transect")


##########supprimer les colonnes si il y a plus de 2 NA ####################################

head(env_transect_wide)

env_transect_wide_1 <- env_transect_wide %>%
  select(
    Annee,
    where(~ sum(is.na(.)) <= 2)
  )

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
  frequency = 1
)

# visualisation multi-séries
autoplot(ts_transect) +
  ylab("Salinité (sal_m6_q3)") +
  xlab("Année") +
  ggtitle("")

####Amélioration graphique

library(tidyr)
library(dplyr)

env_long <- env_transect_wide_1 %>%
  pivot_longer(
    -Annee,
    names_to = "transect_id",
    values_to = "sal_m6_q3"
  ) %>%
  drop_na(sal_m6_q3)

labels_start <- env_long %>%
  group_by(transect_id) %>%
  slice_min(Annee)

labels_end <- env_long %>%
  group_by(transect_id) %>%
  slice_max(Annee)

ggplot(env_long,
       aes(x = Annee,
           y = sal_m6_q3,
           group = transect_id,
           colour = transect_id)) +
  
  geom_line(linewidth = 0.7, alpha = 0.8) +
  
  # labels début
  geom_text(
    data = labels_start,
    aes(label = transect_id),
    hjust = 1.1,
    size = 3,
    show.legend = FALSE
  ) +
  
  # labels fin
  geom_text(
    data = labels_end,
    aes(label = transect_id),
    hjust = -0.1,
    size = 3,
    show.legend = FALSE
  ) +
  
  labs(
    title = "Évolution interannuelle de la température par transect",
    x = "Année",
    y = "salinité (sal_m6_q3"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

#Amélioration

library(dplyr)
library(stringr)
library(ggplot2)
library(viridis)

env_long2 <- env_long %>%
  mutate(
    base = as.numeric(str_extract(transect_id, "^\\d+")),
    lat_score = case_when(
      base %in% c(28, 29) ~ 400 + base,
      TRUE ~ base
    )
  ) %>%
  group_by(transect_id) %>%
  mutate(lat_score = first(lat_score)) %>%
  ungroup()

# ordre Sud -> Nord
ordre_transects <- env_long2 %>%
  distinct(transect_id, lat_score) %>%
  arrange(lat_score) %>%
  pull(transect_id)

env_long2 <- env_long2 %>%
  mutate(transect_id = factor(transect_id, levels = ordre_transects))

labels_start2 <- env_long2 %>%
  group_by(transect_id) %>%
  slice_min(Annee, with_ties = FALSE) %>%
  ungroup()

labels_end2 <- env_long2 %>%
  group_by(transect_id) %>%
  slice_max(Annee, with_ties = FALSE) %>%
  ungroup()

seuil_sal <- 34.84

#bon graphique : 
ggplot(env_long2,
       aes(x = Annee, y = sal_m6_q3, group = transect_id, colour = transect_id)) +
  
  geom_line(linewidth = 0.7, alpha = 0.85) +
  
  geom_hline(yintercept = seuil_sal,
             linetype = "dashed",
             linewidth = 0.8,
             colour = "black") +
  
  geom_text(data = labels_start2, aes(label = transect_id),
            hjust = 1.1, size = 3, show.legend = FALSE) +
  
  geom_text(data = labels_end2, aes(label = transect_id),
            hjust = -0.1, size = 3, show.legend = FALSE) +
  
  scale_colour_viridis_d(option = "C", end = 0.95, begin = 0.1) +
  
  scale_x_continuous(breaks = sort(unique(env_long2$Annee))) +
  
  coord_cartesian(ylim = c(31.5, 36)) +   # 🔹 Échelle fixée
  
  labs(
    x = "Année",
    y = "Salinité (sal_m6_q3)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_text(size=12), axis.title = element_text(size=12))

library(ggplot2)
library(reshape2)
library(viridis)

#pour avoir l'échelle
ggplot(melt(x)) +
  geom_line(aes(Var2, value, group = Var1, color = Var1)) +
  scale_color_viridis_c(option = "C", begin = 0.1, end = 0.95)

