# 1- Importer the data set

dataframe <- read.csv("shopping_behavior_updated.csv")

# 2- transform the data by deleting numeric data

delete_numeric_columns <- function(df) {
  numeric_columns <- sapply(df, is.numeric)  # Find numeric columns
  df <- df[, !numeric_columns]  # Remove numeric columns
  return(df)
}

dataframe <- delete_numeric_columns(dataframe)
#View(dataframe)

# Calculate top 8 variables for each facet
top_values <- data_melt %>%
  count(variable, value) %>%
  arrange(variable, desc(n)) %>%
  group_by(variable) %>%
  top_n(8)

# Filter the data to retain only the top 8 variables for each facet
data_melt_filtered <- data_melt %>%
  semi_join(top_values, by = c("variable", "value"))

# Create the bar plot with the filtered data showing the top 8 variables in each facet
ggplot(data_melt_filtered, aes(x = value, fill = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")

# 4- transform the data table to a disjonctive table

library(FactoMineR)

# Creating a disjunctive table
table_disjonctive <- tab.disjonctif(dataframe)

# Calculating the total number of responses
N <- sum(as.matrix(table_disjonctive))

# Calculating counts for each modality of each question
k_j <- colSums(table_disjonctive)

# Calculating the relative frequencies for each modality of each question
f_j <- k_j / N

# Calculating counts for each question
ki_ <- rowSums(table_disjonctive)

# Calculating the relative frequencies for each question
fi_ <- ki_ / N

# 5- AFCM

afcm <- MCA(dataframe,graph = FALSE)

# 6- PLOTTING
library(factoextra)

#eigenvalues
fviz_eig(afcm, addlabels=TRUE,barfill = "skyblue", ncp = 10, main = "(AFCm) Diagram of the first 10 eigenvalues")
barplot(afcm$eig[,1], col = "skyblue", main="(AFCm) Scree plot of eigenvalues")

#modalities 
fviz_mca_var(afcm, repel=TRUE, col.var = "contrib")

#individuals
fviz_mca_ind(afcm, repel = TRUE, col.ind = "contrib")
plot.MCA(afcm, title="individuals without labels",col.ind = "skyblue", invisible='var', label='none')

#bi plot
fviz_mca_biplot(afcm, repel = TRUE, label = 'var')
groupe <- as.factor(dataframe[, "Gender"])
fviz_mca_ind(afcm, habillage = groupe, addEllipses = TRUE, repel = TRUE)


# 7- Contribution table and interpret the axes

library(kableExtra)

# Create a table of eigenvalues from the MCA analysis
kbl(afcm$eig) %>%
  kable_styling(bootstrap_options = c("bordered")) %>%
  footnote(general = "Table of Eigenvalues")

# 1) modalities

# Identification of modalities that contributed the most to the construction of axis 1 and on which side
dim_one_var_contribs <- (afcm$var$contrib[,1]) / 100  # Contribution of variables to axis 1, divided by 100
dim_one_most_contrib_vars <- dim_one_var_contribs[dim_one_var_contribs >= f_j]  # Selecting contributions greater than or equal to a certain threshold (f_j)
dim_one_most_contrib_vars <- sort(dim_one_most_contrib_vars, decreasing = TRUE)  # Sorting the contributions in decreasing order

# Initializing empty lists and retrieving variable coordinates
dim_one_pos_vars <- vector(mode="list")
dim_one_neg_vars <- vector(mode="list")
dim_one_vars_coords <- afcm$var$coord[,1]  # Retrieving coordinates of variables on the first axis

# Loop through the significant contributing modalities
for(catn in names(dim_one_most_contrib_vars)){
  # Checking if the modality contributes positively to the first axis
  if(dim_one_vars_coords[catn] > 0){
    dim_one_pos_vars[catn] = dim_one_most_contrib_vars[catn]  # Store positive contributions
  } else {
    dim_one_neg_vars[catn] = dim_one_most_contrib_vars[catn]  # Store negative contributions
  }
}

# Identification of modalities that contributed the most to the construction of axis 2 and on which side
dim_two_var_contribs <- (afcm$var$contrib[,2]) / 100  # Contribution of variables to axis 2, divided by 100
dim_two_most_contrib_vars <- dim_two_var_contribs[dim_two_var_contribs >= f_j]  # Selecting contributions greater than or equal to a certain threshold (f_j)
dim_two_most_contrib_vars <- sort(dim_two_most_contrib_vars, decreasing = TRUE)  # Sorting the contributions in decreasing order

dim_two_pos_vars <- vector(mode = "list")  # Initializing a list for variables contributing positively to axis 2
dim_two_neg_vars <- vector(mode = "list")  # Initializing a list for variables contributing negatively to axis 2
dim_two_vars_coords <- afcm$var$coord[,2]  # Extracting coordinates of variables on axis 2

for (catn in names(dim_two_most_contrib_vars)) {
  if (dim_two_vars_coords[catn] > 0) {
    dim_two_pos_vars[catn] = dim_two_most_contrib_vars[catn]  # Storing positive contributors to axis 2
  } else {
    dim_two_neg_vars[catn] = dim_two_most_contrib_vars[catn]  # Storing negative contributors to axis 2
  }
}

# 2) individus

# Identification of individuals who contributed the most to the construction of axis 1 and on which side
dim_one_ind_contribs <- (afcm$ind$contrib[,1]) / 100  # Contribution of individuals to axis 1, divided by 100
dim_one_most_contrib_inds <- dim_one_ind_contribs[dim_one_ind_contribs >= fi_]  # Selecting contributions greater than or equal to a certain threshold (fi_)
dim_one_most_contrib_inds <- sort(dim_one_most_contrib_inds, decreasing = TRUE)  # Sorting the contributions in decreasing order

dim_one_pos_inds <- vector(mode = "list")  # Initializing a list for individuals contributing positively to axis 1
dim_one_neg_inds <- vector(mode = "list")  # Initializing a list for individuals contributing negatively to axis 1
dim_one_inds_coords <- afcm$ind$coord[,1]  # Extracting coordinates of individuals on axis 1

for (catn in names(dim_one_most_contrib_inds)) {
  if (dim_one_inds_coords[catn] > 0) {
    dim_one_pos_inds[catn] = dim_one_most_contrib_inds[catn]  # Storing positive contributors to axis 1
  } else {
    dim_one_neg_inds[catn] = dim_one_most_contrib_inds[catn]  # Storing negative contributors to axis 1
  }
}

# Identification of individuals who contributed the most to the construction of axis 2 and on which side
dim_two_ind_contribs <- (afcm$ind$contrib[,2]) / 100  # Contribution of individuals to axis 2, divided by 100
dim_two_most_contrib_inds <- dim_two_ind_contribs[dim_two_ind_contribs >= fi_]  # Selecting contributions greater than or equal to a certain threshold (fi_)
dim_two_most_contrib_inds <- sort(dim_two_most_contrib_inds, decreasing = TRUE)  # Sorting the contributions in decreasing order

dim_two_pos_inds <- vector(mode = "list")  # Initializing a list for individuals contributing positively to axis 2
dim_two_neg_inds <- vector(mode = "list")  # Initializing a list for individuals contributing negatively to axis 2
dim_two_inds_coords <- afcm$ind$coord[,2]  # Extracting coordinates of individuals on axis 2

for (catn in names(dim_two_most_contrib_inds)) {
  if (dim_two_inds_coords[catn] > 0) {
    dim_two_pos_inds[catn] = dim_two_most_contrib_inds[catn]  # Storing positive contributors to axis 2
  } else {
    dim_two_neg_inds[catn] = dim_two_most_contrib_inds[catn]  # Storing negative contributors to axis 2
  }
}


library(kableExtra)

# Creating a data frame for Axis 1 analysis table
ddd <- data.frame(
  PL.POS = toString(names(dim_one_pos_inds)),
  PL.NEG = toString(names(dim_one_neg_inds)),
  PC.POS = toString(names(dim_one_pos_vars)),
  PC.NEG = toString(names(dim_one_neg_vars))
)

# Generating a table for Axis 1 analysis
kbl(ddd) %>%
  kable_styling(bootstrap_options = c("bordered")) %>%
  add_header_above(c("Profiles Lines (PL)" = 2, "Profiles Columns (PC)" = 2)) %>%
  footnote(general = "Table of analysis for Axis 1")


ddd <- data.frame(
  PL.POS = toString(names(dim_two_pos_inds)),
  PL.NEG = toString(names(dim_two_neg_inds)),
  PC.POS = toString(names(dim_two_pos_vars)),
  PC.NEG = toString(names(dim_two_neg_vars))
)

kbl(ddd) %>%
  kable_styling(bootstrap_options = c("bordered")) %>%
  add_header_above(c("Profiles Lines (PL)" = 2, "Profiles Columns (PC)" = 2)) %>%
  footnote(general = "Table of analysis for Axis 2")







