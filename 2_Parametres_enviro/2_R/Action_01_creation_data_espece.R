# Packages
library(dplyr)
library(readr)
library(readxl)
library(writexl)

#importation des données
WP1_indiv_trie <- read.csv("G:/PTUT_AVIZONS/R/data/WP1_indiv_trie.csv")

WP1_filtered_4esp <- WP1_indiv_trie %>%
  filter(
    (Nom_Scientifique == "Merluccius merluccius" & Campagne == "EVHOE") |
      (Nom_Scientifique == "Solea solea"           & Campagne == "ORHAGO") |
      (Nom_Scientifique %in% c("Scomber scombrus", "Sardina pilchardus") & 
         Campagne == "PELGAS"))

names(WP1_filtered_4esp)

# Nettoyage des données
WP1_filtered_4esp <- WP1_filtered_4esp %>%
  select(-c(DateFin, Tot_V_HV, ICESNAME, Engin, Class_Tri, Taille, Coef_Elev_Espece_Capture, Poids_Reference, LongFin, LatFin))

WP1_filtered_4esp <- WP1_filtered_4esp %>%
  rename(Long = LongDeb,
         Lat  = LatDeb)

WP1_filtered_4esp <- WP1_filtered_4esp %>%
  rename(date = DateDeb) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d", tz = "UTC"))


# Création d'une liste de dataframes par espèce
data_par_espece <- WP1_filtered_4esp %>%
  group_by(Nom_Scientifique) %>%
  group_split()

# Donner un nom à chaque dataframe dans la liste
names(data_par_espece) <- unique(WP1_filtered_4esp$Nom_Scientifique)

merlu <- data_par_espece[["Merluccius merluccius"]]
sardine <- data_par_espece[["Sardina pilchardus"]]
maquereau <- data_par_espece[["Scomber scombrus"]]
sole   <- data_par_espece[["Solea solea"]]

# regroupement des données
#Merlu à partir de 1995
merlu_1995 <- merlu %>%
  filter(date >= as.POSIXct("1995-01-01"))

merlu_summarized <- merlu_1995 %>%
  group_by(Annee, date, Campagne, Code_Station, Long, Lat, Nom_Scientifique) %>%
  summarise(Nbr = sum(Nbr), .groups = "drop")

# Vérifier le résultat
head(merlu_summarized)
nrow(merlu_summarized)

maquereau_summarized <- maquereau %>%
  group_by(Annee, date, Campagne, Code_Station, Long, Lat, Nom_Scientifique) %>%
  summarise(Nbr = sum(Nbr), .groups = "drop")

# Vérifier le résultat
head(maquereau_summarized)
nrow(maquereau_summarized)

sardine_summarized <- sardine %>%
  group_by(Annee, date, Campagne, Code_Station, Long, Lat, Nom_Scientifique) %>%
  summarise(Nbr = sum(Nbr), .groups = "drop")

# Vérifier le résultat
head(sardine_summarized)
nrow(sardine_summarized)

# Vérifier les lignes dupliquées
duplicated_rows <- merlu_summarized[duplicated(merlu_summarized), ]

# Combien de duplicata ?
nrow(duplicated_rows)

# Voir les premières lignes dupliquées
head(duplicated_rows)

sole_summarized <- sole %>%
  group_by(Annee, date, Campagne, Code_Station, Long, Lat, Nom_Scientifique) %>%
  summarise(Nbr = sum(Nbr), .groups = "drop")

# Vérifier le résultat
head(sole_summarized)
nrow(sole_summarized)


# Enregistrer au format CSV 
write.csv(merlu_summarized, "G:/PTUT_AVIZONS/R/data/merlu.csv", row.names = FALSE)
write.csv(sardine_summarized, "G:/PTUT_AVIZONS/R/data/sardine.csv", row.names = FALSE)
write.csv(maquereau_summarized, "G:/PTUT_AVIZONS/R/data/maquereau.csv", row.names = FALSE)
write.csv(sole_summarized, "G:/PTUT_AVIZONS/R/data/sole.csv", row.names = FALSE)
