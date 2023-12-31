

##############################################################################
######################## Importer d'abord mon dataset ######################## 
##############################################################################


dataframe <- read.csv("../shopping_behavior_updated.csv")
# Je vais supprimer les colonnes non qualitatives ( quantitatives )
delete_numeric_columns <- function(df) {
  numeric_columns <- sapply(df, is.numeric)  # trouver les colonnes quantitatives
  df <- df[, !numeric_columns]  # supprimer ses derniers
  return(df)
}

dataframe <- delete_numeric_columns(dataframe)


#############################################################################
############################ Effectuer L'AFC ################################
#############################################################################

library(FactoMineR)
library(Factoshiny)

dataframe<- table(dataframe$Item.Purchased,dataframe$Season)
afc<-CA(dataframe)

N <- sum(as.matrix(dataframe))

k_j <- colSums(dataframe)
f_j <- k_j / N

ki_ <- rowSums(dataframe)
fi_ <- ki_ / N


#############################################################################
############################ PLOT ###########################################
#############################################################################


barplot(afc$eig[,1], col = "skyblue", main="Diagramme de tous les valeurs propres")


#############################################################################
############ Table de contribution et la signification des axes #############
#############################################################################
#############################################################################


kbl(afc$eig) %>%
  kable_styling(bootstrap_options = c("bordered"))%>%
  footnote(general="Table des valeurs propres")

# 1) AXE 1

# trouver les profils lignes qui ont contribuer le plus à la construction de l'axe 1 
# et dans quel sens
dim_one_row_contribs<-(afc$row$contrib[,1])/100
dim_one_most_contrib_rows<-dim_one_row_contribs[dim_one_row_contribs>=fi_]
dim_one_most_contrib_rows<-sort(dim_one_most_contrib_rows, decreasing=TRUE)

dim_one_pos_rows <- vector(mode="list")
dim_one_neg_rows <- vector(mode="list")
dim_one_rows_coords <- afc$row$coord[,1]

for(catn in names(dim_one_most_contrib_rows)){
  if(dim_one_rows_coords[catn]>0){
    dim_one_pos_rows[catn] = dim_one_most_contrib_rows[catn]
  }
  else{
    dim_one_neg_rows[catn] = dim_one_most_contrib_rows[catn] 
  }
} 

# trouver les profils colonnes qui ont contribuer le plus à la construction de l'axe 1 
# et dans quel sens

dim_one_col_contribs<-(afc$col$contrib[,1])/100
dim_one_most_contrib_cols<-dim_one_col_contribs[dim_one_col_contribs>=f_j]
dim_one_most_contrib_cols<-sort(dim_one_most_contrib_cols, decreasing=TRUE)

dim_one_pos_cols <- vector(mode="list")
dim_one_neg_cols <- vector(mode="list")
dim_one_cols_coords <- afc$col$coord[,1]

for(catn in names(dim_one_most_contrib_cols)){
  if(dim_one_cols_coords[catn]>0){
    dim_one_pos_cols[catn] = dim_one_most_contrib_cols[catn]
  }
  else{
    dim_one_neg_cols[catn] = dim_one_most_contrib_cols[catn] 
  }
}

# 2) AXE 2

# trouver les profils lignes qui ont contribuer le plus à la construction de l'axe 2
# et dans quel sens
dim_two_row_contribs<-(afc$row$contrib[,2])/100
dim_two_most_contrib_rows<-dim_two_row_contribs[dim_two_row_contribs>=fi_]
dim_two_most_contrib_rows<-sort(dim_two_most_contrib_rows, decreasing=TRUE)

dim_two_pos_rows <- vector(mode="list")
dim_two_neg_rows <- vector(mode="list")
dim_two_rows_coords <- afc$row$coord[,2]

for(catn in names(dim_two_most_contrib_rows)){
  if(dim_two_rows_coords[catn]>0){
    dim_two_pos_rows[catn] = dim_two_most_contrib_rows[catn]
  }
  else{
    dim_two_neg_rows[catn] = dim_two_most_contrib_rows[catn] 
  }
}  

# trouver les profils colonnes qui ont contribuer le plus à la construction de l'axe 2
# et dans quel sens


dim_two_col_contribs<-(afc$col$contrib[,2])/100
dim_two_most_contrib_cols<-dim_two_col_contribs[dim_two_col_contribs>=f_j]
dim_two_most_contrib_cols<-sort(dim_two_most_contrib_cols, decreasing=TRUE)

dim_two_pos_cols <- vector(mode="list")
dim_two_neg_cols <- vector(mode="list")
dim_two_cols_coords <- afc$col$coord[,2]

for(catn in names(dim_two_most_contrib_cols)){
  if(dim_two_cols_coords[catn]>0){
    dim_two_pos_cols[catn] = dim_two_most_contrib_cols[catn]
  }
  else{
    dim_two_neg_cols[catn] = dim_two_most_contrib_cols[catn] 
  }
}  

# générer la table d'analyse pour l'axe 1


ddd<-data.frame(
  PL.POS = toString(names(dim_one_pos_rows)),
  PL.NEG = toString(names(dim_one_neg_rows)),
  PC.POS = toString(names(dim_one_pos_cols)),
  PC.NEG = toString(names(dim_one_neg_cols))
)


kbl(ddd) %>%
  kable_styling(bootstrap_options = c("bordered"))%>%
  add_header_above(c("(PL)" = 2, "(PC)" = 2))%>%
  footnote(general="Table d'analyse pour l'axe 1")



# générer la table d'analyse pour l'axe 2


ddd<-data.frame(
  PL.POS = toString(names(dim_two_pos_rows)),
  PL.NEG = toString(names(dim_two_neg_rows)),
  PC.POS = toString(names(dim_two_pos_cols)),
  PC.NEG = toString(names(dim_two_neg_cols))
)

kbl(ddd) %>%
  kable_styling(bootstrap_options = c("bordered"))%>%
  add_header_above(c("(PL)" = 2, "(PC)" = 2))%>%
  footnote(general="Table d'analyse pour l'axe 2")




