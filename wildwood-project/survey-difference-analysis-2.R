library(VIM)
library("dplyr")

count_missing = function(df) {
  sapply(df, FUN=function(col) sum(is.na(col)) )
}
colnames(survey1)
survey1_deleted <- survey1[-c(1:36, 38:42, 45, 47:62, 65:73, 75:85, 87:102, 104:108 #67, 50, 81, 59, 70, 47, 48, 60, 79, 78,
                              #60, 51, 77, 61, 71, 65, 75, 62, 68, 74, 66, 
                              #49, 63, 64, 72, 52, 58, 54, 44, 45, 46, 53, 43, 
                              #57, 38, 39, 35, 34, 33, 32, 29, 36, 37,
                              #30, 31, 55, 56, 28, 83:108
                          )]
colnames(survey1_deleted)
survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed <- format(strptime(survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed, "%I%p"), format="%H")
colnames(survey2)
survey2_deleted <- survey2[-c(1:37, 39:43, 46, 48:63, 66:74, 76:86, 88:103, 105:109 #68, 51, 82, 60, 71, 48, 49, 61, 80, 79,
                              #61, 52, 78, 62, 72, 66, 76, 63, 69, 75, 67, 
                              #50, 64, 65, 73, 53, 59, 55, 45, 46, 47, 54, 44, 
                              #58, 39, 40, 36, 35, 34, 33, 30, 37, 38,
                              #31, 32, 56, 57, 29, 84:109
                          )]
colnames(survey2_deleted)
survey2_deleted$X31_Over_last_two_weeks_when_gone_to_bed <- format(strptime(survey2_deleted$X31_Over_last_two_weeks_when_gone_to_bed, "%I%p"), format="%H")

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
#names(clean_base_1) <- substring(names(clean_base_1), 2, 4 )
#names(clean_base_2) <- substring(names(clean_base_2), 2, 4 )
names(clean_base_1) <- make.unique(names(clean_base_1))
names(clean_base_2) <- make.unique(names(clean_base_2))

matching_row_names1 <- rownames(clean_base_1) %in% rownames(clean_base_2)
only_matching1 <- clean_base_1[matching_row_names1, ]
matching_row_names2 <- rownames(clean_base_2) %in% rownames(clean_base_1)
only_matching2 <- clean_base_2[matching_row_names2, ]

final <- only_matching2 - only_matching1
final <- final %>% 
  rename(#"32_use_cigarettes" = "X32_Use_cigarettes_e.cigarettes_marijuana_cigars_chew_snuff",
         "33_cigarette_daily_amount" = "X33_How_many_cigarettes_smoke_daily",
         "39_little_interest" = "X39_Little_interest_or_pleasure_in_doing_things",
         "40_feeling_depressed" = "X40_Feeling_down_depressed_.or.hopeless",
         "42_cant_stop_worrying" = "X42_Not_being_able_to_stop_or_control_worrying",
         "59_cut_down_work" = "X59_Cut_down_the_amount_of_time_you_spent_on_work",
         "60_accomplished_less" = "X60_Accomplished_less_than_you_would_like",
         "70_downhearted" = "X70_Have_you_felt_downhearted",
         "3B_fried_starchy_vegetables" = "X3B_perday_fried_starchy_vegetables",
         "18_preday_deserts" = "X18_perday_pastries_cookies_cakes_ice.cream"
         )
summary(final)

library(factoextra)
res.pca <- prcomp(final, scale = TRUE)
fviz_eig(res.pca, addlabels = TRUE) 
fviz_cos2(res.pca, choice = "var", axes = 1:2)

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
