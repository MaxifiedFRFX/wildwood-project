library(VIM)
library("dplyr")

count_missing = function(df) {
  sapply(df, FUN=function(col) sum(is.na(col)) )
}
colnames(survey1)
survey1_deleted <- survey1[-c(1:37, 79, 67, 80, 50, 81, 59, 70, 47, 48, 60, 82, 
                              60, 51, 77, 61, 71, 65, 75, 62, 68, 74, 78, 66, 
                              49, 63, 64, 72, 52, 58, 54, 44, 45, 46, 53, 43, 
                              57, 38, 39, 55, 56, 83:108)]
colnames(survey1_deleted)
#survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed <- format(strptime(survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed, "%I%p"), format="%H")
colnames(survey2)
survey2_deleted <- survey2[-c(1:38, 80, 68, 81, 51, 82, 60, 71, 48, 49, 61, 83, 
                              61, 52, 78, 62, 72, 66, 76, 63, 69, 75, 79, 67, 
                              50, 64, 65, 73, 53, 59, 55, 45, 46, 47, 54, 44, 
                              58, 39, 40, 56, 57, 84:109)]
colnames(survey2_deleted)
#survey2_deleted$X31_Over_last_two_weeks_when_gone_to_bed <- format(strptime(survey2_deleted$X31_Over_last_two_weeks_when_gone_to_bed, "%I%p"), format="%H")

column_types1 <- sapply(survey1_deleted, class)
column_types2 <- sapply(survey2_deleted, class)

non_numeric_columns1 <- names(column_types1[column_types1 != "numeric"])
non_numeric_columns2 <- names(column_types2[column_types2 != "numeric"])

for (col in non_numeric_columns1) {
  survey1_deleted[[col]] <- as.numeric(survey1_deleted[[col]])
}
for (col in non_numeric_columns1) {
  survey2_deleted[[col]] <- as.numeric(survey2_deleted[[col]])
}

count_missing(survey1_deleted)
count_missing(survey2_deleted)

clean_base_1 <- survey1_deleted[complete.cases(survey1_deleted), , drop = FALSE]
clean_base_2 <- survey2_deleted[complete.cases(survey2_deleted), , drop = FALSE]
#names(clean_base_1) <- substring(names(clean_base_1), 2, )
#names(clean_base_2) <- substring(names(clean_base_2), 2, )
names(clean_base_1) <- make.unique(names(clean_base_1))
names(clean_base_2) <- make.unique(names(clean_base_2))

matching_row_names1 <- rownames(clean_base_1) %in% rownames(clean_base_2)
only_matching1 <- clean_base_1[matching_row_names1, ]
matching_row_names2 <- rownames(clean_base_2) %in% rownames(clean_base_1)
only_matching2 <- clean_base_2[matching_row_names2, ]

final <- only_matching2 - only_matching1
final <- final %>% 
  rename("36_God_Presence" = "X36_I_experience_the_God_presence",
         "37_religion_guide" = "X37_My_religious_beliefs.guide_my_approach_to_life",
         "38_carry_religion_into_all_dealings" = "X38_I_try_to_carry_my_religion_into_all_dealings_in_life",
         "65_pep" = "X65_Did_you_feel_full_of_pep",
         "69_energy" = "X69_Did_you_have_a_lot_of_energy",
         "72_happy_person" = "X72_Have_you_been_happy_person")
summary(final)

library(factoextra)
res.pca <- prcomp(final, scale = TRUE)
fviz_eig(res.pca, addlabels = TRUE) 

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

