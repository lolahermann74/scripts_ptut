# Random forest pour le merlu
# Auteur : Mathis Dubois

# Packages
library(randomForest)
library(rpart)
library(rpart.plot)
library(party)
library(ggplot2)
library(dplyr)
library(pROC)

# Chargement des données
donnees <- merlu_enviro_11fev26_3752

# Sélection des variables
df_model <- donnees[, c(7, 14:193)]

# Suppression des lignes avec des valeurs manquantes (NA)
df_model <- na.omit(df_model)

# Exécution du Random Forest 
set.seed(123)
rf_test <- cforest(Nbr ~ ., data = df_model, 
                   controls = cforest_unbiased(ntree = 500, mtry = 5))
print(rf_test)
importance_cf <- varimp(rf_test, conditional = FALSE)

# On trie et on prend les 10 dernières valeurs (les plus hautes)
dotchart(tail(sort(importance_cf), 10), 
         main = "Top 10 Importance (cforest)", 
         xlab = "Score d'importance",
         pch = 19)

# Conversion en tableau pour ggplot
df_imp_cf <- data.frame(Variable = names(importance_cf), 
                        Importance = as.numeric(importance_cf))

# Sélection du Top 10
top_10_cf <- df_imp_cf %>%
  arrange(desc(Importance)) %>%
  head(10)

# Le graphique en couleurs
ggplot(top_10_cf, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "#56B4E9", high = "#004D40") +
  theme_minimal() +
  labs(title = "Top 10 Importance des variables",
       x = NULL,
       y = "Score d'Importance") +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12, face = "bold", color = "black"))


# Récupération de la meilleure variable
scores_importance <- sort(varimp(rf_test, conditional = FALSE), decreasing = TRUE)
ma_variable <- names(scores_importance)[1]
print(ma_variable)

# --- Binarisation de l'Abondance (Étape Obligatoire pour ROC) ---
# La courbe ROC a besoin de 2 classes (0 ou 1).
# Cas A : On teste la PRÉSENCE (Nbr > 0) vs ABSENCE (Nbr = 0)
# Cas B : Si vous n'avez que des présences, on teste FORTE ( > Médiane) vs FAIBLE abondance

seuil_coupure_poisson <- 0 
if(min(df_model$Nbr) > 0) {
  seuil_coupure_poisson <- mean(df_model$Nbr)
  print("Aucune absence détectée. Coupure basée sur la médiane (Forte vs Faible abondance).")
}

# Création de la colonne binaire (0 ou 1)
df_model$classe_abondance <- ifelse(df_model$Nbr > seuil_coupure_poisson, 1, 0)

# Calcul de la Courbe ROC
roc_obj <- roc(df_model$classe_abondance, df_model[[ma_variable]], 
               quiet = TRUE) 

# Détermination du Seuil Optimal (Youden)
# La méthode de Youden cherche le point qui maximise (Sensibilité + Spécificité)
resultat_seuil <- coords(roc_obj, "best", best.method = "youden", transpose = FALSE)
print(resultat_seuil)

# Visualisation Graphique
plot(roc_obj, 
     main = paste("Seuil optimal pour", ma_variable),
     col = "#004D40", lwd = 3)

# Ajout du point seuil sur le graphique
points(resultat_seuil$specificity, resultat_seuil$sensitivity, 
       pch = 19, col = "red", cex = 1.5)

# Affichage de la valeur du seuil sur le graphique
text(resultat_seuil$specificity, resultat_seuil$sensitivity - 0.1, 
     labels = paste("Seuil =", round(resultat_seuil$threshold, 3)), 
     col = "red", font = 2)

# Affichage de l'AUC (Qualité du seuil)
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 2)), box.col = NA)

# Récupération du seuil calculé précédemment
valeur_seuil <- resultat_seuil$threshold

# Création des groupes (En-dessous vs Au-dessus)
df_model$Groupe_Seuil <- ifelse(df_model[[ma_variable]] > valeur_seuil, 
                               paste("Au-dessus (> ", round(valeur_seuil, 3), ")", sep=""), 
                               paste("En-dessous (<= ", round(valeur_seuil, 3), ")", sep=""))

# Calcul des statistiques par groupe
synthese_effet <- df_model %>%
  group_by(Groupe_Seuil) %>%
  summarise(
    Moyenne_Abondance = mean(Nbr, na.rm = TRUE),
    Mediane_Abondance = median(Nbr, na.rm = TRUE),
    Nombre_Stations = n()
  )
print(synthese_effet)

# Interprétation Automatique (Augmentation ou Diminution ?)
# On récupère les médianes pour les comparer
moy_basse <- synthese_effet$Moyenne_Abondance[grep("En-dessous", synthese_effet$Groupe_Seuil)]
moy_haute <- synthese_effet$Moyenne_Abondance[grep("Au-dessus", synthese_effet$Groupe_Seuil)]

diff_pourcentage <- ((moy_haute - moy_basse) / moy_basse) * 100

cat("\n--- CONCLUSION BIOLOGIQUE ---\n")
if(moy_haute > moy_basse) {
  cat("RELATION POSITIVE : Quand", ma_variable, "dépasse", round(valeur_seuil, 3), 
      ", l'abondance moyenne AUGMENTE de", round(diff_pourcentage, 1), "%.\n")
} else {
  cat("RELATION NÉGATIVE : Quand", ma_variable, "dépasse", round(valeur_seuil, 3), 
      ", l'abondance moyenne DIMINUE de", round(abs(diff_pourcentage), 1), "%.\n")
}

# Graphique final
ggplot(df_model, aes(x = Groupe_Seuil, y = Nbr, fill = Groupe_Seuil)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "darkred") +
  coord_cartesian(ylim = c(0, quantile(df_model$Nbr, 0.95) * 1.5)) + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00")) +
  theme_minimal() +
  labs(title = paste("Effet de", ma_variable, "sur l'abondance de merlus"),
       subtitle = paste("Seuil :", round(valeur_seuil, 3), "| Point rouge = Moyenne d'abondance"),
       x = NULL, y = "Abondance (Nbr)") +
  theme(legend.position = "none",axis.text = element_text(size=13,color="black"),axis.title = element_text(size=14,color="black")) +
  annotate("text", x = 1.5, y = Inf, vjust = 2, 
           label = paste0("Variation : ", ifelse(diff_pourcentage > 0, "+", ""), round(diff_pourcentage, 1), "%"), 
           size = 5, fontface = "bold", color = "black") 

# Fin de script