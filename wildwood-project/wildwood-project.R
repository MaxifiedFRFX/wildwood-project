library(VIM)

count_missing = function(df) {
  sapply(df, FUN=function(col) sum(is.na(col)) )
}
colnames(survey1)
survey1_deleted <- survey1[-c(1:60, 83:87, 89:95, 99:105, 107)]
survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed <- format(strptime(survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed, "%I%p"), format="%H")
colnames(survey1_deleted)

column_types <- sapply(survey1_deleted, class)

non_numeric_columns <- names(column_types[column_types != "numeric"])

for (col in non_numeric_columns) {
  survey1_deleted[[col]] <- as.numeric(survey1_deleted[[col]])
}

#survey1_deleted[survey1_deleted == "n/a"] <- NA

#imputed_data <- kNN(survey1_deleted)

clean_base_1 <- survey1_deleted[complete.cases(survey1_deleted), , drop = FALSE]
names(clean_base_1) <- substring(names(clean_base_1), 2, 4)
names(clean_base_1) <- make.unique(names(clean_base_1))

library(factoextra)
res.pca <- prcomp(clean_base_1, scale = TRUE)

fviz_pca_var(res.pca,
             col.var = "cos2", # Color by the quality of representation cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# varlist <- setdiff(colnames(survey1), c("Variable.Field.Name"))

# library(vtreat)
# treatment_plan <-
#   design_missingness_treatment(survey1, varlist = varlist)
# training_prepared <- prepare(treatment_plan, survey1)
