# aggrégation des paramètres environnementaux du data espèce
# Auteur : Lola Hermann


#Packages
library(dplyr)
library(readr)

# SALINITE maquereau ####
sal_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/salinite_maquereau_2019.csv")
sal_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/salinite_maquereau_2021.csv")
sal_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/salinite_maquereau_2022.csv")
sal_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/salinite_maquereau_2023.csv")
sal_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/salinite_maquereau_2024.csv")
sal_2025 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/salinite_maquereau_2025.csv")
head(sal_2019)


sal_all <- bind_rows(
  sal_2019,
  sal_2021,
  sal_2022,
  sal_2023,
  sal_2024,
  sal_2025)

names(sal_all)
names(sal_all) <- gsub("^(m[136]|y[12])_", "sal_\\1_", names(sal_all))
names(sal_all)
# t0
names(sal_all) <- gsub("^sal_day_t0$", "sal_t0", names(sal_all))

# t-1 → t1
names(sal_all) <- gsub("^sal_day_tminus1$", "sal_t1", names(sal_all))

# week → t7
names(sal_all) <- gsub("^sal_week_", "sal_t7_", names(sal_all))

names(sal_all)

# TEMPERATURE maquereau ####
temp_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/temperature_maquereau_2019.csv")
temp_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/temperature_maquereau_2021.csv")
temp_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/temperature_maquereau_2022.csv")
temp_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/temperature_maquereau_2023.csv")
temp_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/temperature_maquereau_2024.csv")
temp_2025 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/temperature_maquereau_2025.csv")
head(temp_2019)


temp_all <- bind_rows(
  temp_2019,
  temp_2021,
  temp_2022,
  temp_2023,
  temp_2024,
  temp_2025)

names(temp_all)
# day → t0 / t1
names(temp_all) <- gsub("^sal_day_t0$", "thetao_t0", names(temp_all))
names(temp_all) <- gsub("^sal_day_tminus1$", "thetao_t1", names(temp_all))

# week → t7
names(temp_all) <- gsub("^sal_week_", "thetao_t7_", names(temp_all))

# m1 m3 m6 y1 y2 → ajouter thetao_
names(temp_all) <- gsub("^(m[136]|y[12])_", "thetao_\\1_", names(temp_all))
names(temp_all)


# OXYGENE maquereau ####
oxy_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/oxygene_maquereau_2019.csv")
oxy_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/oxygene_maquereau_2021.csv")
oxy_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/oxygene_maquereau_2022.csv")
oxy_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/oxygene_maquereau_2023.csv")
oxy_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/oxygene_maquereau_2024.csv")
oxy_2025 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/oxygene_maquereau_2025.csv")
head(oxy_2019)


oxy_all <- bind_rows(
  oxy_2019,
  oxy_2021,
  oxy_2022,
  oxy_2023,
  oxy_2024,
  oxy_2025)

names(oxy_all)
# day → t0 / t1
names(oxy_all) <- gsub("^sal_day_t0$", "o2_t0", names(oxy_all))
names(oxy_all) <- gsub("^sal_day_tminus1$", "o2_t1", names(oxy_all))

# week → t7
names(oxy_all) <- gsub("^sal_week_", "o2_t7_", names(oxy_all))

# m1 m3 m6 y1 y2 → ajouter o2_
names(oxy_all) <- gsub("^(m[136]|y[12])_", "o2_\\1_", names(oxy_all))
names(oxy_all)

# NITRATE maquereau ####
nit_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/nitrate_maquereau_2019.csv")
nit_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/nitrate_maquereau_2021.csv")
nit_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/nitrate_maquereau_2022.csv")
nit_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/nitrate_maquereau_2023.csv")
nit_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/nitrate_maquereau_2024.csv")
nit_2025 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/nitrate_maquereau_2025.csv")
head(nit_2019)


nit_all <- bind_rows(
  nit_2019,
  nit_2021,
  nit_2022,
  nit_2023,
  nit_2024,
  nit_2025)

names(nit_all)
# day → t0 / t1
names(nit_all) <- gsub("^sal_day_t0$", "no3_t0", names(nit_all))
names(nit_all) <- gsub("^sal_day_tminus1$", "no3_t1", names(nit_all))

# week → t7
names(nit_all) <- gsub("^sal_week_", "no3_t7_", names(nit_all))

# m1 m3 m6 y1 y2 → ajouter no3_
names(nit_all) <- gsub("^(m[136]|y[12])_", "no3_\\1_", names(nit_all))
names(nit_all)




# pH maquereau ####
ph_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/ph_maquereau_2019.csv")
ph_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/ph_maquereau_2021.csv")
ph_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/ph_maquereau_2022.csv")
ph_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/ph_maquereau_2023.csv")
ph_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/ph_maquereau_2024.csv")
ph_2025 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/ph_maquereau_2025.csv")

head(ph_2019)


ph_all <- bind_rows(
  ph_2019,
  ph_2021,
  ph_2022,
  ph_2023,
  ph_2024,
  ph_2025)

names(ph_all)
# day → t0 / t1
names(ph_all) <- gsub("^sal_day_t0$", "ph_t0", names(ph_all))
names(ph_all) <- gsub("^sal_day_tminus1$", "ph_t1", names(ph_all))

# week → t7
names(ph_all) <- gsub("^sal_week_", "ph_t7_", names(ph_all))

# m1 m3 m6 y1 y2 → ajouter ph_
names(ph_all) <- gsub("^(m[136]|y[12])_", "ph_\\1_", names(ph_all))
names(ph_all)


# spCO2 maquereau ####
spco2_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/spco2_maquereau_2019.csv")
spco2_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/spco2_maquereau_2021.csv")
spco2_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/spco2_maquereau_2022.csv")
spco2_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/spco2_maquereau_2023.csv")
spco2_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/spco2_maquereau_2024.csv")
spco2_2025 <- read.csv("G:/PTUT_AVIZONS/R/output/maquereau2/spco2_maquereau_2025.csv")

head(spco2_2019)


spco2_all <- bind_rows(
  spco2_2019,
  spco2_2021,
  spco2_2022,
  spco2_2023,
  spco2_2024,
  spco2_2025)

names(spco2_all)
# day → t0 / t1
names(spco2_all) <- gsub("^sal_day_t0$", "spco2_t0", names(spco2_all))
names(spco2_all) <- gsub("^sal_day_tminus1$", "spco2_t1", names(spco2_all))

# week → t7
names(spco2_all) <- gsub("^sal_week_", "spco2_t7_", names(spco2_all))

# m1 m3 m6 y1 y2 → ajouter spco2_
names(spco2_all) <- gsub("^(m[136]|y[12])_", "spco2_\\1_", names(spco2_all))
names(spco2_all)


# NETTOYAGE ####
names(sal_all)
names(temp_all)
names(nit_all)
names(oxy_all)
names(ph_all)
names(spco2_all)

clean_env_data <- function(df) {
  df <- df[ , !names(df) %in% c("depth_used_m", "n_days_available")]
  df <- df[ , !grepl("_n$", names(df))]
  return(df)
}

sal_all   <- clean_env_data(sal_all)
temp_all  <- clean_env_data(temp_all)
nit_all   <- clean_env_data(nit_all)
oxy_all   <- clean_env_data(oxy_all)
ph_all    <- clean_env_data(ph_all)
spco2_all <- clean_env_data(spco2_all)

head(sal_all)
head(temp_all)
head(nit_all)
head(oxy_all)
head(ph_all)
head(spco2_all)
names(sal_all)

# FUSIONNER les datas
id_cols <- c("Id_Operation", "date", "Serie", "Code_Station",
             "Long", "Lat", "Nom_Scientifique", "Nbr", "date_posix", "year", "cell",
             "long_cop", "lat_cop", "dist_m")



keep_env <- function(df, prefix) {
  env_cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  df[, c(id_cols, env_cols)]
}

sal2   <- keep_env(sal_all, "sal_")
temp2  <- keep_env(temp_all, "thetao_")
nit2   <- keep_env(nit_all, "no3_")
oxy2   <- keep_env(oxy_all, "o2_")
ph2    <- keep_env(ph_all, "ph_")
spco22 <- keep_env(spco2_all, "spco2_")



env_all <- Reduce(function(x, y) {
  merge(x, y, by = id_cols, all = FALSE)
},
list(sal2, temp2, nit2, oxy2, ph2, spco22))

head(env_all)

nrow(sal_all)
nrow(oxy_all)
nrow(temp_all)
nrow(nit_all)
nrow(ph_all)
nrow(spco2_all)
nrow(env_all)

any(duplicated(sal2[, id_cols]))
any(duplicated(temp2[, id_cols]))
any(duplicated(nit2[, id_cols]))
any(duplicated(oxy2[, id_cols]))
any(duplicated(ph2[, id_cols]))
any(duplicated(spco22[, id_cols]))


sal2   <- sal2[!duplicated(sal2[, id_cols]), ]
temp2  <- temp2[!duplicated(temp2[, id_cols]), ]
nit2   <- nit2[!duplicated(nit2[, id_cols]), ]
oxy2   <- oxy2[!duplicated(oxy2[, id_cols]), ]
ph2    <- ph2[!duplicated(ph2[, id_cols]), ]
spco22 <- spco22[!duplicated(spco22[, id_cols]), ]

env_all <- Reduce(function(x, y) merge(x, y, by = id_cols, all = FALSE),
                  list(sal2, temp2, nit2, oxy2, ph2, spco22))


# EXPORT
write.csv(env_all, "G:/PTUT_AVIZONS/R/output/maquereau_enviro_10fev26_350.csv", row.names = FALSE)
