library(VIM)
library(tidyverse)
# data features cut out are: BMI1, BMI2, BMI3, Days btwn baseline and F/U Wt
# SBP3, DBP3, Days btwn baseline and F/U BP, Wt (lbs)3
new_vitals_df1 <- vital.signs[, !(names(vital.signs) %in% 
                      c("BMI1", "BMI2", "BMI3", "Days.btwn.baseline.and.F.U.Wt", "SBP3",
                        "DBP3", "Days.btwn.baseline.and.F.U.BP", "Wt..lbs.3"))]
summary(new_vitals_df1)

# first we need to replace the n/a values with NA
new_vitals_df1[new_vitals_df1 == "n/a"] <- NA

# create and print out version with categorical data as integers as well as turn 
new_vitals_df1$`Session.type` <- as.numeric(as.factor(new_vitals_df1$`Session.type`))
summary(new_vitals_df1)

# we can impute missing data on the remaining columns
#new_vitals_df2 <- kNN(new_vitals_df1)
#summary(new_vitals_df2)
#new_vitals_df2

# new_vitals_df2$`WC1` <- as.numeric(as.factor(new_vitals_df2$`WC1`))
# new_vitals_df2$`SBP2` <- as.numeric(as.factor(new_vitals_df2$`SBP2`))
# new_vitals_df2$`DBP2` <- as.numeric(as.factor(new_vitals_df2$`DBP2`))
# new_vitals_df2$`Wt (lbs)2` <- as.numeric(as.factor(new_vitals_df2$`Wt (lbs)2`))
# new_vitals_df2$`WC 2` <- as.numeric(as.factor(new_vitals_df2$`WC 2`))

column_types1 <- sapply(new_vitals_df1, class)

non_numeric_columns1 <- names(column_types1[column_types1 != "numeric"])

for (col in non_numeric_columns1) {
  new_vitals_df1 [[col]] <- as.numeric(new_vitals_df1[[col]])
}

summary(new_vitals_df1)

cleaned_vitals <- new_vitals_df1[complete.cases(new_vitals_df1), , drop = FALSE]
summary(cleaned_vitals)
cleaned_vitals <- mutate(cleaned_vitals, BMI1 = Wt..lbs.1 / Intake_Height^2 * 703)
cleaned_vitals <- mutate(cleaned_vitals, BMI2 = Wt..lbs.2 / Intake_Height^2 * 703)
cleaned_vitals <- mutate(cleaned_vitals, BMI_diff = BMI2 - BMI1)
cleaned_vitals <- mutate(cleaned_vitals, SBP_diff = SBP2 - SBP1)
cleaned_vitals <- mutate(cleaned_vitals, DBP_diff = DBP2 - DBP1)
cleaned_vitals <- mutate(cleaned_vitals, Wt_diff =  Wt..lbs.2 - Wt..lbs.1)
cleaned_vitals <- mutate(cleaned_vitals, WC_diff =  WC.2 - WC1)
summary(cleaned_vitals)

# we can impute missing data on the remaining columns
#new_vitals_df2 <- kNN(new_vitals_df2)

#cleaned_vitals <- new_vitals_df2[0:12]
#summary(cleaned_vitals) 

# applying PCA to the dataset
library(factoextra)
res.pca <- prcomp(cleaned_vitals, scale = TRUE)

fviz_pca_var(res.pca,
             col.var = "cos2", # Color by the quality of representation cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

