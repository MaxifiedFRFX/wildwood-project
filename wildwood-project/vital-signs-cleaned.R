library(VIM)
library(tidyverse)

new_vitals_df <- read_csv("../vital-signs.csv", show_col_types = FALSE)
# data features cut out are: BMI1, BMI2, BMI3, Days btwn baseline and F/U Wt
# SBP3, DBP3, Days btwn baseline and F/U BP, Wt (lbs)3
new_vitals_df1 <- new_vitals_df[, !(names(new_vitals_df) %in% 
                      c("BMI1", "BMI2", "BMI3", "Days btwn baseline and F/U Wt", "SBP3",
                        "DBP3", "Days btwn baseline and F/U BP", "Wt (lbs)3"))]

# first we need to replace the n/a values with NA
new_vitals_df1[new_vitals_df1 == "n/a"] <- NA

# create and print out version with categorical data as integers as well as turn 
new_vitals_df2$`Session type` <- as.numeric(as.factor(new_vitals_df2$`Session type`))

# we can impute missing data on the remaining columns
new_vitals_df2 <- kNN(new_vitals_df1)
summary(new_vitals_df2)
new_vitals_df2

# new_vitals_df2$`WC1` <- as.numeric(as.factor(new_vitals_df2$`WC1`))
# new_vitals_df2$`SBP2` <- as.numeric(as.factor(new_vitals_df2$`SBP2`))
# new_vitals_df2$`DBP2` <- as.numeric(as.factor(new_vitals_df2$`DBP2`))
# new_vitals_df2$`Wt (lbs)2` <- as.numeric(as.factor(new_vitals_df2$`Wt (lbs)2`))
# new_vitals_df2$`WC 2` <- as.numeric(as.factor(new_vitals_df2$`WC 2`))

summary(new_vitals_df2)
new_vitals_df2

# we can impute missing data on the remaining columns
new_vitals_df2 <- kNN(new_vitals_df2)

cleaned_vitals <- new_vitals_df2[0:12]
summary(cleaned_vitals) 

# applying PCA to the dataset
library(factoextra)
res.pca <- prcomp(new_vitals_df2, scale = TRUE)

fviz_pca_var(res.pca,
             col.var = "cos2", # Color by the quality of representation cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
