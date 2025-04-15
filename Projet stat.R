##Analyse descriptive:

View(phishing_attacks_sample_company_7)
donnees=phishing_attacks_sample_company_7
# Determination des Classes de la variable detection_time_hours
E=max(donnees$detection_time_hours)-min(donnees$detection_time_hours) # etendu de la serie statistique
n=length(donnees$detection_time_hours) # taille de la serie
k=floor(1+3.22*log10(n)) # Formule de Sturges
e=E/k #etendu de chaque classe
bornes_classes=seq(min(donnees$detection_time_hours),max(donnees$detection_time_hours),by=e) # les bornes des classes

# Découpage en classes
classes = cut(donnees$detection_time_hours, breaks = bornes_classes, include.lowest = TRUE, right = FALSE)

# Calcul des effectifs par classe
ni_detection_time_hours = table(classes)

# Calcul des fréquences par classe
fi_detection_time_hours = ni_detection_time_hours / n

# Frequences accumulees croissantes
Fi_detection_time_hours=cumsum(fi_detection_time_hours/sum(fi_detection_time_hours))

#La moyenne 
Moy_detection_time_hours=mean(donnees$detection_time_hours)

#Afficher la moyenne 
cat("La moyenne de temps de detection en heures est : ",Moy_detection_time_hours,"\n")

#La mediane 
# Etapes pour le calcul de médiane
classe_mediane = which(Fi_detection_time_hours >= 0.5)[1]

# Borne inférieure de la classe médiane
x_i_1 = bornes_classes[classe_mediane]

# Fréquence cumulée avant la classe médiane
F_i_1 = Fi_detection_time_hours[classe_mediane-1]

# frequence de la classe médiane
f_i = fi_detection_time_hours[classe_mediane]

# Calcul de la médiane
mediane_classe = x_i_1 + ((0.5 - F_i_1) / f_i) * e

# Afficher la médiane
cat("La mediane de temps de detection en heures",mediane_classe,"\n")

#Le mode 
# Etapes pour le calcul de mode
classe_modale = which.max(fi_detection_time_hours)

# Borne inférieure de la classe modale
x_i_1 = bornes_classes[classe_modale]

# Borne superieure de la classe modale
x_iplus1 = bornes_classes[classe_modale+1]

# Calcul de k1 et k2
# Traitement du cas classe_modale n'est pas la première classe
k1= if (classe_modale > 1) {
  ni_detection_time_hours[classe_modale] - ni_detection_time_hours[classe_modale - 1]
}else {
  ni_detection_time_hours[classe_modale]
}
# Traitement du cas classe_modale n'est pas la dernière classe
k2= if (classe_modale < length(ni_detection_time_hours)) {
  ni_detection_time_hours[classe_modale] - ni_detection_time_hours[classe_modale + 1]
}else{ 
  ni_detection_time_hours[classe_modale]
}
# Calcul du mode
mode_classe = x_i_1 + (k1 / (k1 + k2)) * e

# Afficher le mode
cat("Le mode de temps de detection en heures est:", mode_classe, "\n")

#Le diagramme circulaire représentant la répartition des attaques par canal de communication
freq_channel=table(donnees$channel)
pie(freq_channel, 
    main = "Répartition des attaques par canal de communication", 
    col = c("pink", "skyblue", "lightcoral", "lightgreen"))

#Le diagramme en barre représentant la fréquence des attaques par département 
freq_departement=table(donnees$target_type)
barplot(freq_departement, 
        main = "Fréquence des attaques par département",
        xlab = "Départements", 
        ylab = "Nombre d'attaques",
        col = "lightblue")

#le diagramme en boite pour visualiser le temps de détection des attaques de phishing
boxplot(donnees$detection_time_hours, 
        main = "Temps de détection des attaques de phishing", 
        ylab = "Temps de détection (heures)", 
        col = "darkorange", 
        horizontal = TRUE)

#Le taux de reussite des attaques 
freq_success=table(donnees$success)
View(freq_success)
barplot(freq_success, 
        main = "Taux de réussite des attaques",
        xlab = "Résultat", 
        ylab = "Nombre d'attaques", 
        col = c("lightgreen", "lightcoral"),
        names.arg = c("Échec", "Succès"))

##Analyse bivariée:

# bibliothèques nécessaires
library(kableExtra)
library(knitr)
library(ggplot2)
library(plotly)

# Vérifier et convertir les colonnes 'channel' et 'success' en facteurs
donnees$channel = as.factor(donnees$channel)
donnees$success = as.factor(donnees$success)

# Créer le tableau de contingence
table_contingence0=table(donnees$channel, donnees$success)
# Ajouter les marges X et Y
table_contingence = addmargins(table_contingence0)
# Ajouter les titres Marge_X et Marge_Y
colnames(table_contingence)[ncol(table_contingence)] ="Marge_X"
rownames(table_contingence)[nrow(table_contingence)] ="Marge_Y"
# Convertir la table en format kable
col_names=append("X\\Y", colnames(table_contingence))
print(table_contingence)
table_kable = kable(table_contingence, caption = 'Table de contingence', "html", col.names = NULL, digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered")) %>%
  add_header_above(col_names) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(1:nrow(table_contingence), align = "c")
# Afficher la table
print(table_kable)

# Tableau des fréquences relatives
table_contingence_freq = table_contingence / table_contingence[nrow(table_contingence), ncol(table_contingence)] * 100
table_freq_kable = kable(table_contingence_freq, caption = 'Table des fréquences', "html", col.names = NULL, digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered")) %>%
  add_header_above(col_names) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(1:nrow(table_contingence), align = "c")
# Afficher la table
print(table_freq_kable)

# Tableau des distributions conditionnelles X|Y
table_proba_cond_X_Y = table_contingence # Copier la table de contingence
for (i in 1:ncol(table_proba_cond_X_Y)) {
  table_proba_cond_X_Y[, i] = table_proba_cond_X_Y[, i] / table_proba_cond_X_Y[nrow(table_proba_cond_X_Y), i] * 100
}
colnames(table_proba_cond_X_Y)[ncol(table_proba_cond_X_Y)] = "Marge_X"
rownames(table_proba_cond_X_Y)[nrow(table_proba_cond_X_Y)] = "Total"
col_names = append("X|Y=y_i", colnames(table_proba_cond_X_Y))
table_proba_cond_X_Y_kable = kable(table_proba_cond_X_Y, caption = 'Distribution conditionnelle X|Y', "html", col.names = NULL, digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered")) %>%
  add_header_above(col_names) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(1:nrow(table_contingence), align = "c")
# Afficher la table
print(table_proba_cond_X_Y_kable)

# Tableau des distributions conditionnelles Y|X
table_proba_cond_Y_X = table_contingence # Copier la table de contingence
for (i in 1:nrow(table_proba_cond_Y_X)) {
  table_proba_cond_Y_X[i, ] = table_proba_cond_Y_X[i, ] / table_proba_cond_Y_X[i, ncol(table_proba_cond_Y_X)] * 100
}
colnames(table_proba_cond_Y_X)[ncol(table_proba_cond_Y_X)] = "Total"
rownames(table_proba_cond_Y_X)[nrow(table_proba_cond_Y_X)] = "Marge_Y"
col_names = append("Y|X=x_i", colnames(table_proba_cond_Y_X))
table_proba_cond_Y_X_kable = kable(table_proba_cond_Y_X, caption = 'Distribution conditionnelle Y|X', "html", col.names = NULL, digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered")) %>%
  add_header_above(col_names) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(1:nrow(table_contingence), align = "c")
# Afficher la table
print(table_proba_cond_Y_X_kable)
d1 <- table_proba_cond_Y_X
d2 <- table_proba_cond_X_Y
print(kable(list(d1, d2),
            caption = 'Distributions Conditionnelles', "html", digits = 3) %>%
        kable_styling(bootstrap_options = c("striped", "condensed", "bordered")))


# Afficher la table de contingence de base
#################################
table_kable0 = kable(table_contingence0, caption = 'Table de contingence', "html", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered")) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(1:nrow(table_contingence0), align = "c")
# Afficher la table
print(table_kable0)

##Estimation de la proportion d'attaques reussies 

# Données de l'échantillon
n = 1000  # Taille de l'échantillon
x =278   # Nombre de succes 

# Niveau de confiance (par exemple, 95%)
alpha = 0.05

# Proportion observée
f_n = x / n

# Écart type estimé pour la proportion
s_est = sqrt(f_n * (1 - f_n) / n)

# Calcul de l'intervalle de confiance
marge_erreur = qnorm(1 - alpha / 2) * s_est

# Limite inférieure de l'intervalle
limite_inferieure = f_n - marge_erreur

# Limite supérieure de l'intervalle
limite_superieure = f_n + marge_erreur

# Affichage de l'intervalle de confiance
cat("Intervalle de confiance au seuil de", 100 * (1 - alpha), "%  : [", limite_inferieure, ", ", limite_superieure, "]\n")

##Les tests d'hypotheses:

#Test de L’hypothese 50 % des attaques par email sont réussies :

# Filtrer les données pour les attaques par email
email_data = subset(donnees, channel == "email")
# Taille de l'échantillon
taille_echantillon = nrow(email_data)
# Proportion théorique sous H0 (50%)
p0 = 0.5
# Calcul de la proportion observée de succès
proportion_stat = sum(email_data$success == "success") / taille_echantillon
print(paste("Proportion observée de succès :", proportion_stat))
# Tester les conditions de normalité pour la proportion
# Vérification : n >= 30, n * p0 * (1 - p0) >= 5
if(taille_echantillon < 30 | taille_echantillon * proportion_stat * (1 - proportion_stat) < 5 |
   taille_echantillon * proportion_stat < 5 | taille_echantillon * (1 - proportion_stat) < 5){
  stop("Votre échantillon ne satisfait pas l'une des conditions : n >= 30 et n * p * (1 - p) >= 5")
}
# Niveau de signification (alpha)
alpha = 0.05
# Calcul de la statistique de test z
z_stat = (proportion_stat - p0) / sqrt(p0 * (1 - p0) / taille_echantillon)
print(paste("Statistique de test z :", z_stat))
# Valeur critique pour le test unilatéral à droite
z_alpha = qnorm(1 - alpha)
print(paste("Valeur critique pour alpha =", alpha, ":", z_alpha))
# Test d'hypothèses
if (z_stat > z_alpha) {
  cat("Rejet de H0 : La proportion est supérieure à", p0, "au niveau de signification de", alpha, "\n")
} else {
  cat("Non-rejet de H0 : Les données ne fournissent pas suffisamment de preuves pour
soutenir H1\n")
}

#Test de l’hypothese : 40 % des attaques par SMS sont réussies :

# Filtrer les données pour les attaques par SMS
sms_data = subset(donnees, channel == "sms")
# Taille de l'échantillon
taille_echantillon = nrow(sms_data)
# Calculer la proportion de succès (1 pour succès et 0 pour échec)
p0 = 0.40 # Proportion théorique sous H0 (40 %)
proportion_stat = sum(sms_data$success == "success") / taille_echantillon
print(paste("Proportion observée:", proportion_stat))
# Tester les conditions de normalité pour proportion
if(taille_echantillon < 30 | taille_echantillon * proportion_stat * (1 - proportion_stat) < 5 |
   taille_echantillon * proportion_stat < 5 | taille_echantillon * (1 - proportion_stat) < 5){
  stop("L'échantillon ne satisfait pas les conditions: n>=30 et n*p*(1-p)>=5")
}
# Niveau de signification (alpha)
alpha = 0.05
# Valeur critique pour le test unilatéral (à gauche)
u_alpha = qnorm(alpha)
print(paste("Valeur critique pour alpha=", alpha, ": ", u_alpha))
# Calcul de la statistique de test u
u_stat = (proportion_stat - p0) / sqrt(p0 * (1 - p0) / taille_echantillon)
# Affichage de la statistique de test
print(paste("Statistique de test:", u_stat))
# Test d'hypothèses
if (u_stat < u_alpha) {
  cat("Rejet de H0 : La proportion de succès est inférieure à", p0, "au niveau de signification
de", alpha, "\n")
} else {
  cat("Non-rejet de H0 : Les données ne fournissent pas suffisamment de preuves pour
soutenir H1\n")
}

#Test de l’hypothese : la proportion de succès des attaques par sites web est inférieure ou égale à 35 %

# Filtrer les données pour les attaques par sites web
website_data = subset(donnees, channel == "website")
# Taille de l'échantillon
taille_echantillon = nrow(website_data)
# Calculer la proportion de succès (1 pour succès et 0 pour échec)
p0 = 0.35 # Proportion théorique sous H0 (35 %)
proportion_stat = sum(website_data$success == "success") / taille_echantillon
print(paste("Proportion observée:", proportion_stat))
# Tester les conditions de normalité pour proportion
if(taille_echantillon < 30 | taille_echantillon * proportion_stat * (1 - proportion_stat) < 5 |
   taille_echantillon * proportion_stat < 5 | taille_echantillon * (1 - proportion_stat) < 5){
  stop("L'échantillon ne satisfait pas les conditions: n>=30 et n*p*(1-p)>=5")
}
# Niveau de signification (alpha)
alpha = 0.05
# Valeur critique pour le test unilatéral (à gauche)
u_alpha = qnorm(alpha)
print(paste("Valeur critique pour alpha=", alpha, ": ", u_alpha))
# Calcul de la statistique de test u
u_stat = (proportion_stat - p0) / sqrt(p0 * (1 - p0) / taille_echantillon)
# Affichage de la statistique de test
print(paste("Statistique de test:", u_stat))
# Test d'hypothèses
if (u_stat < u_alpha) {
  cat("Rejet de H0 : La proportion de succès est inférieure à", p0, "au niveau de signification
de", alpha, "\n")
} else {
  cat("Non-rejet de H0 : Les données ne fournissent pas suffisamment de preuves pour
soutenir H1\n")
} 


##Régression linéaire:

# Calcul des coefficients de la régression linéaire
donnees$success_numeric=ifelse(donnees$success == "success", 1, 0)
print(head(donnees$success_numeric))

# Calculer le taux de succès
nombre_de_succes = sum(donnees$success_numeric)
nombre_total_observations = length(donnees$success_numeric)
taux_de_succes =nombre_de_succes / nombre_total_observations
print(taux_de_succes)

# Créer le nuage de points
plot(donnees$detection_time_hours, donnees$success_numeric, 
     main="Régression linéaire: Taux de succès vs Durée de l'attaque", 
     xlab="Durée de l'attaque (heures)", 
     ylab="Taux de succès", 
     pch=19, col="blue")

# Moyenne de detection_time_hours et le taux de succès
mean_y=mean(donnees$detection_time_hours)
print(mean_y)
mean_x=mean(donnees$success_numeric)
print(mean_x)

# Calcul de la covariance 
cov_xy=cov(donnees$success_numeric, donnees$detection_time_hours)
print(cov_xy)

# Calcul de la corrélation sans NA
Data_clean=na.omit(donnees[, c("success_numeric", "detection_time_hours")])
r=cor(donnees$success_numeric,donnees$detection_time_hours)
print(r)

# Calcul des coefficients de régression
var_success=var(donnees$success_numeric)
b1=cov_xy / var_success
print(b1)
b0=mean_y- b1*mean_x
print(b0)

# Affichage des résultats
cat("Ordonnée à l'origine (b0):", b0, "\nPente (b1):", b1, "\n")
cat("Coefficient de corrélation (r):", r, "\n")

# Ajustement du modèle de régression linéaire
modele=lm(donnees$success_numeric ~ detection_time_hours, data = donnees) 
print(modele)
summary_model=summary(modele)


# Extraction des coefficients
b0=coef(modele)[1]  # intercept
b1=coef(modele)[2]  # Pente

# Affichage des résultats
cat("Coefficient de corrélation r:", r, r^2,"\n")
cat("Pente de la droite (b1):", b1, "\n")
cat("Ordonnée à l'origine (b0):", b0, "\n")

# Tracé de la droite de régression
abline(a=b0, b=b1, col="orange", lwd=2,xlim=c(min(donnees$success_numeric),max(donnees$success_numeric)))
# Affichage de l'équation de régression
cat('Équation de régression estimée de Y sur X : y =', round(b1, 4), 'x +', round(b0, 4), "\n")

# Interprétation de l'effet de la variable X (detection_time_hours) sur Y (success_numeric)
cat('Interprétation : Pour chaque heure supplémentaire de durée de l\'attaque, le taux de succès augmente de', round(b1, 4), "\n")
cat('Equation de regression estimée de Y sur X : y=',b1,'x+',b0,"\n")

# Ajuster le modèle linéaire
modele = lm(success_numeric ~ detection_time_hours, data = donnees)

# Obtenir le résumé du modèle
summary_model=summary(modele)

# Extraire le coefficient de détermination R^2
R2=summary_model$r.squared

# Afficher le coefficient de détermination R^2
cat('Coefficient de détermination R^2 :', round(R2, 4), '\n')

# Obtenir les résidus du modèle
residuals_values=residuals(modele)

# Calculer la somme des carrés des résidus (SCR)
SCR = sum(residuals_values^2)

# Afficher le résultat
cat("La somme des carrés des résidus (SCR) est :", SCR, "\n")

# Nombre d'observations
n = length(donnees$success_numeric)  # Nombre d'observations de la variable dépendante (success_numeric)

# Estimation de la variance (σ^2)
sigma2_hat =SCR / (n - 2) 
# Affichage du résultat
cat("L'estimation de la variance (σ^2) est :", sigma2_hat, "\n")

# Calcul du modèle de régression 
modele = lm(success_numeric ~ detection_time_hours, data = donnees)

# Calcul des intervalles de confiance pour b0 et b1 (niveau 95%)
confint_b0_b1 = confint(modele, level = 0.95)

# Affichage des intervalles de confiance pour b0 et b1
cat("Intervalle de confiance à 95 % pour b0 : [", confint_b0_b1[1, 1], ",", confint_b0_b1[1, 2], "]\n")
cat("Intervalle de confiance à 95 % pour b1 : [", confint_b0_b1[2, 1], ",", confint_b0_b1[2, 2], "]\n")

# Ajuster le modèle de régression linéaire
modele = lm(success_numeric ~ detection_time_hours, data = donnees)

# Créer un nouveau jeu de données pour les prédictions
# Ici, on génère des valeurs de detection_time_hours entre les minimum et maximum de la variable
detection_time_hours_seq = seq(min(donnees$detection_time_hours), 
                               max(donnees$detection_time_hours), 
                               length.out = 100)

# Créer un data frame contenant ces nouvelles valeurs
new_data = data.frame(detection_time_hours = detection_time_hours_seq)

# Générer les prédictions avec un intervalle de prédiction à 95 %
predictions = predict(modele, newdata = new_data, interval = "prediction", level = 0.95)

# Générer les prédictions avec un intervalle de confiance à 95 %
confidence = predict(modele, newdata = new_data, interval = "confidence", level = 0.95)

# Afficher les premières lignes des résultats de prédictions et des intervalles
cat("Prédictions avec intervalle de prédiction:\n")
head(predictions)

cat("Prédictions avec intervalle de confiance:\n")
head(confidence)

# Générer des prédictions et des intervalles pour chaque observation
predictions = predict(modele, newdata = donnees, interval = "prediction", level = 0.95)
confidence = predict(modele, newdata = donnees, interval = "confidence", level = 0.95)

# Ajouter les colonnes de prédiction, intervalles de prédiction et intervalles de confiance au dataframe
donnees$predicted = predictions[, "fit"]  # Prédictions
donnees$lwr_prediction = predictions[, "lwr"]  # Borne inférieure de l'intervalle de prédiction
donnees$upr_prediction = predictions[, "upr"]  # Borne supérieure de l'intervalle de prédiction

donnees$lwr_confidence = confidence[, "lwr"]  # Borne inférieure de l'intervalle de confiance
donnees$upr_confidence = confidence[, "upr"]  # Borne supérieure de l'intervalle de confiance

# Afficher les premières lignes du dataframe avec les nouvelles colonnes
head(new_data)

# Créer un jeu de données avec les valeurs de prédiction et des intervalles
# Utilisation de l'objet `predictions` et `confidence` calculés plus tôt
predictions_df = data.frame(
  detection_time_hours = donnees$detection_time_hours,  # Variables indépendantes
  predicted = donnees$predicted,  # Prédictions du modèle
  lwr_prediction = donnees$lwr_prediction,  # Borne inférieure de l'intervalle de prédiction
  upr_prediction = donnees$upr_prediction,  # Borne supérieure de l'intervalle de prédiction
  lwr_confidence = donnees$lwr_confidence,  # Borne inférieure de l'intervalle de confiance
  upr_confidence = donnees$upr_confidence   # Borne supérieure de l'intervalle de confiance
)

#Creer une fenetre graphique
plot(donnees$detection_time_hours, donnees$success_numeric, 
     main = "Régression linéaire avec IC et IP à 95 %", 
     xlab = "Durée de l'attaque (heures)", 
     ylab = "Taux de succès",
     col = "blue", pch = 16,  # Points bleus
     xlim = c(min(donnees$detection_time_hours), max(donnees$detection_time_hours)),
     ylim = c(min(donnees$success_numeric), max(donnees$success_numeric)))



# Ajouter la droite de régression (prédictions du modèle)
lines(donnees$detection_time_hours, predictions_df$predicted, col = "red", lwd = 2)

# Ajouter l'intervalle de confiance (région rouge)
polygon(c(predictions_df$detection_time_hours, rev(predictions_df$detection_time_hours)), 
        c(predictions_df$lwr_confidence, rev(predictions_df$upr_confidence)), 
        col = rgb(1, 0, 0, alpha = 0.3), border = NA)

# Ajouter l'intervalle de prédiction (région jaune)
polygon(c(predictions_df$detection_time_hours, rev(predictions_df$detection_time_hours)), 
        c(predictions_df$lwr_prediction, rev(predictions_df$upr_prediction)), 
        col = rgb(1, 1, 0, alpha = 0.2), border = NA)

# Ajouter une légende
legend("topleft", legend = c("Données observées", "Régression", "Intervalle de confiance", "Intervalle de prédiction"),
       col = c("blue", "red", "red", "yellow"), lwd = c(1, 2, NA, NA), pch = c(16, NA, NA, NA), 
       fill = c(NA, NA, rgb(1, 0, 0, alpha = 0.3), rgb(1, 1, 0, alpha = 0.2)))
