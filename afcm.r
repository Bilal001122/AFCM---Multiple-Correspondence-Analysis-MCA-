

##############################################################################
######################## Importer d'abord mon dataset ######################## 
##############################################################################


dataframe <- read.csv("shopping_behavior_updated.csv")
# Je vais supprimer les colonnes non qualitatives ( quantitatives )
delete_numeric_columns <- function(df) {
  numeric_columns <- sapply(df, is.numeric)  # trouver les colonnes quantitatives
  df <- df[, !numeric_columns]  # supprimer ses derniers
  return(df)
}

dataframe <- delete_numeric_columns(dataframe)


####################################################################
######################## générer le barplot ########################
####################################################################


library(ggplot2)
library(reshape2)
library(dplyr)


data_melt <- melt(dataframe, id.vars = NULL)

# calculez les tops 8 modalités pour chaque colonne
top_values <- data_melt %>%
  count(variable, value) %>%
  arrange(variable, desc(n)) %>%
  group_by(variable) %>%
  top_n(8)

# filtrer mes données de tels sorte que juste les 8 meilleures modalités vont
# etre affiches dans chaque facette.
data_melt_filtered <- data_melt %>%
  semi_join(top_values, by = c("variable", "value"))

# créez le barplot avec les données filtrées montrant les 8 meilleures
# modalités dans chaque facette.

ggplot(data_melt_filtered, aes(x = value, fill = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")


#############################################################################
########## transformer le tableau de données en tableau disjnoctif ########## 
#############################################################################


library(FactoMineR)

# créer le tableau disjonctif
table_disjonctive <- tab.disjonctif(dataframe)

# calculer le nombre total des réponses ( la somme des 0 et de 1 )
N <- sum(as.matrix(table_disjonctive))

# calculer k.j
k_j <- colSums(table_disjonctive)

# calculer f.j
f_j <- k_j / N

# calculer ki.
ki_ <- rowSums(table_disjonctive)

# calculer fi.
fi_ <- ki_ / N


#############################################################################
############################ Effectuer L'4'AFCM #############################
#############################################################################


afcm <- MCA(dataframe,graph = FALSE)


#############################################################################
########################### les différents plots ############################
#############################################################################


library(factoextra)

# le diagramme de 10 premiers valeurs propres 
# (qui contient le plus grand taux de'information )
fviz_eig(afcm, addlabels=TRUE,barfill = "skyblue", ncp = 10, main = "Diagramme de 10 premiers valeurs propres")
barplot(afcm$eig[,1], col = "skyblue", main="Diagramme de tous les valeurs propres")

# La contibution absolues des variables dans la construction des deux axes fact
fviz_mca_var(afcm, repel=TRUE, col.var = "contrib")

# La contibution absolues des individus dans la construction des deux axes fact
fviz_mca_ind(afcm, repel = TRUE, col.ind = "contrib")

# Biplot individus variables AFCM
fviz_mca_biplot(afcm, repel = TRUE, label = 'var')

#############################################################################
############ Table de contribution et la signification des axes #############
#############################################################################
#############################################################################


library(kableExtra)

# créer la table des valeurs propres
kbl(afcm$eig) %>%
  kable_styling(bootstrap_options = c("bordered")) %>%
  footnote(general = "Table des valeurs de propres")

# 1) modalités

# trouver les modalités qui ont contribuer le plus à la construction de l'axe 1 
# et dans quel sens
dim_one_var_contribs <- (afcm$var$contrib[,1]) / 100  
dim_one_most_contrib_vars <- dim_one_var_contribs[dim_one_var_contribs >= f_j]  
dim_one_most_contrib_vars <- sort(dim_one_most_contrib_vars, decreasing = TRUE)

dim_one_pos_vars <- vector(mode="list")
dim_one_neg_vars <- vector(mode="list")
dim_one_vars_coords <- afcm$var$coord[,1]  

for(catn in names(dim_one_most_contrib_vars)){
  if(dim_one_vars_coords[catn] > 0){
    dim_one_pos_vars[catn] = dim_one_most_contrib_vars[catn]
  } else {
    dim_one_neg_vars[catn] = dim_one_most_contrib_vars[catn]
  }
}

# trouver les modalités qui ont contribuer le plus à la construction de l'axe 2 
# et dans quel sens
dim_two_var_contribs <- (afcm$var$contrib[,2]) / 100 
dim_two_most_contrib_vars <- dim_two_var_contribs[dim_two_var_contribs >= f_j]  
dim_two_most_contrib_vars <- sort(dim_two_most_contrib_vars, decreasing = TRUE) 

dim_two_pos_vars <- vector(mode = "list")  
dim_two_neg_vars <- vector(mode = "list")  
dim_two_vars_coords <- afcm$var$coord[,2]  

for (catn in names(dim_two_most_contrib_vars)) {
  if (dim_two_vars_coords[catn] > 0) {
    dim_two_pos_vars[catn] = dim_two_most_contrib_vars[catn] 
  } else {
    dim_two_neg_vars[catn] = dim_two_most_contrib_vars[catn]
  }
}

# 2) individus

# trouver les individus qui ont contribuer le plus à la construction de l'axe 1 
# et dans quel sens
dim_one_ind_contribs <- (afcm$ind$contrib[,1]) / 100  
dim_one_most_contrib_inds <- dim_one_ind_contribs[dim_one_ind_contribs >= fi_]  
dim_one_most_contrib_inds <- sort(dim_one_most_contrib_inds, decreasing = TRUE) 

dim_one_pos_inds <- vector(mode = "list")
dim_one_neg_inds <- vector(mode = "list")
dim_one_inds_coords <- afcm$ind$coord[,1]

for (catn in names(dim_one_most_contrib_inds)) {
  if (dim_one_inds_coords[catn] > 0) {
    dim_one_pos_inds[catn] = dim_one_most_contrib_inds[catn]  
  } else {
    dim_one_neg_inds[catn] = dim_one_most_contrib_inds[catn]  
  }
}

# trouver les individus qui ont contribuer le plus à la construction de l'axe 2 
# et dans quel sens
dim_two_ind_contribs <- (afcm$ind$contrib[,2]) / 100 
dim_two_most_contrib_inds <- dim_two_ind_contribs[dim_two_ind_contribs >= fi_]  
dim_two_most_contrib_inds <- sort(dim_two_most_contrib_inds, decreasing = TRUE) 

dim_two_pos_inds <- vector(mode = "list")  
dim_two_neg_inds <- vector(mode = "list") 
dim_two_inds_coords <- afcm$ind$coord[,2]  

for (catn in names(dim_two_most_contrib_inds)) {
  if (dim_two_inds_coords[catn] > 0) {
    dim_two_pos_inds[catn] = dim_two_most_contrib_inds[catn] 
  } else {
    dim_two_neg_inds[catn] = dim_two_most_contrib_inds[catn]
  }
}


library(kableExtra)

ddd <- data.frame(
  PL.POS = toString(names(dim_one_pos_inds)),
  PL.NEG = toString(names(dim_one_neg_inds)),
  PC.POS = toString(names(dim_one_pos_vars)),
  PC.NEG = toString(names(dim_one_neg_vars))
)

# générer la table d'analyse pour l'axe 1
kbl(ddd) %>%
  kable_styling(bootstrap_options = c("bordered")) %>%
  add_header_above(c("(PL)" = 2, "(PC)" = 2)) %>%
  footnote(general = "Table d'analyse pour l'axe 1")

ddd <- data.frame(
  PL.POS = toString(names(dim_two_pos_inds)),
  PL.NEG = toString(names(dim_two_neg_inds)),
  PC.POS = toString(names(dim_two_pos_vars)),
  PC.NEG = toString(names(dim_two_neg_vars))
)

# générer la table d'analyse pour l'axe 2
kbl(ddd) %>%
  kable_styling(bootstrap_options = c("bordered")) %>%
  add_header_above(c("(PL)" = 2, "(PC)" = 2)) %>%
  footnote(general = "Table d'analyse pour l'axe 2")


#############################################################################
############ Questions les mieux représentées par AFCM  #####################
#############################################################################
#############################################################################

vars_cos2 <- afcm$var$cos2
first_plan_cos2 <- vars_cos2[,1] + vars_cos2[,2]
first_plan_cos2 <- sort(first_plan_cos2, decreasing=TRUE)
kbl(first_plan_cos2) %>%
  kable_styling(bootstrap_options = c("bordered")) %>%
  footnote(general="Conributions relatives des modalité")





