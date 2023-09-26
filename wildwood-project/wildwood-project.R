count_missing = function(df) {
  sapply(df, FUN=function(col) sum(is.na(col)) )
}

colnames(survey1)
survey1_deleted <- survey1[-c(2:27, 28, 36)]
colnames(survey1_deleted)
count_missing(survey1_deleted)
sapply(clean_base_1, class)

column_types <- sapply(survey1_deleted, class)

non_numeric_columns <- names(column_types[column_types != "numeric"])

for (col in non_numeric_columns) {
  survey1_deleted[[col]] <- as.numeric(survey1_deleted[[col]])
}

clean_base_1 <- survey1_deleted[complete.cases(survey1_deleted), , drop = FALSE]

library(factoextra)
res.pca <- prcomp(clean_base_1, scale = TRUE)

fviz_pca_var(res.pca, col.var = "black")

# varlist <- setdiff(colnames(survey1), c("Variable.Field.Name"))

# library(vtreat)
# treatment_plan <-
#   design_missingness_treatment(survey1, varlist = varlist)
# training_prepared <- prepare(treatment_plan, survey1)
