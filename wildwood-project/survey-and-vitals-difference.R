library(VIM)
library("dplyr")

count_missing = function(df) {
  sapply(df, FUN=function(col) sum(is.na(col)) )
}
colnames(survey1)
survey1_deleted <- survey1[c(56, 55, 104, 96, 103 #67, 50, 81, 59, 70, 47, 48, 60, 79, 78,
                              #60, 51, 77, 61, 71, 65, 75, 62, 68, 74, 66, 
                              #49, 63, 64, 72, 52, 58, 54, 44, 45, 46, 53, 43, 
                              #57, 38, 39, 35, 34, 33, 32, 29, 36, 37,
                              #30, 31, 55, 56, 28, 83:108
)]
colnames(survey1_deleted)
survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed <- format(strptime(survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed, "%I%p"), format="%H")
colnames(survey2)
survey2_deleted <- survey2[c(57, 56, 105, 97, 104 #68, 51, 82, 60, 71, 48, 49, 61, 80, 79,
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

surveys <- only_matching2 - only_matching1

# data features cut out are: BMI1, BMI2, BMI3, Days btwn baseline and F/U Wt
# SBP3, DBP3, Days btwn baseline and F/U BP, Wt (lbs)3
new_vitals_df1 <- vital.signs[, !(names(vital.signs) %in% 
                                    c("BMI1", "BMI2", "BMI3", 
                                      "Days.btwn.baseline.and.F.U.Wt", "SBP3",
                                      "DBP3", "Days.btwn.baseline.and.F.U.BP", 
                                      "Wt..lbs.3", "Session.type" ))]
new_vitals_df1[new_vitals_df1 == "n/a"] <- NA
new_vitals_df1$`Session.type` <- as.numeric(as.factor(new_vitals_df1$`Session.type`))

column_types1 <- sapply(new_vitals_df1, class)

non_numeric_columns1 <- names(column_types1[column_types1 != "numeric"])

for (col in non_numeric_columns1) {
  new_vitals_df1 [[col]] <- as.numeric(new_vitals_df1[[col]])
}

cleaned_vitals <- new_vitals_df1[complete.cases(new_vitals_df1), , drop = FALSE]
cleaned_vitals <- mutate(cleaned_vitals, BMI1 = Wt..lbs.1 / Intake_Height^2 * 703)
cleaned_vitals <- mutate(cleaned_vitals, BMI2 = Wt..lbs.2 / Intake_Height^2 * 703)
cleaned_vitals <- mutate(cleaned_vitals, BMI_diff = BMI2 - BMI1)
#cleaned_vitals <- mutate(cleaned_vitals, SBP_diff = SBP2 - SBP1)
#cleaned_vitals <- mutate(cleaned_vitals, DBP_diff = DBP2 - DBP1)
#cleaned_vitals <- mutate(cleaned_vitals, Wt_diff =  Wt..lbs.2 - Wt..lbs.1)
#cleaned_vitals <- mutate(cleaned_vitals, WC_diff =  WC.2 - WC1)
summary(cleaned_vitals)

cleaned_vitals <- cleaned_vitals[, !(names(cleaned_vitals) %in% 
                     c("SBP1", "SBP2", "DBP1", "DBP2", "BMI1", "BMI2", 
                       "Wt..lbs.1", "Wt..lbs.2", "WC1", "WC.2",
                       "Intake_Height"
                     ))]

final <- merge(surveys, cleaned_vitals, by=0, all=TRUE)
summary(final)
final_cleaned <- final[complete.cases(final), , drop = FALSE][-1]

final_cleaned <- final_cleaned %>% 
  rename("52_Walking_several_blocks" = "X52_Walking_several_blocks",
    "51_Walking_more_than_a_mile" = "X51_Walking_more_than_a_mile",
    "19_Fast_food" = "X19_perday_times_eat_in_fast.food_restaurant",
    "11_Red_meat" = "X11_perday_red_meat",
    "18_Desserts" = "X18_perday_pastries_cookies_cakes_ice.cream",
  )
final_cleaned <- final_cleaned[, !(names(final_cleaned) %in% 
                                       c("Program.length"
                                       ))]
summary(final_cleaned)

library(factoextra)
res.pca <- prcomp(final_cleaned, scale = TRUE)
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
