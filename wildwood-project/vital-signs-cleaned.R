library(knitr)
opts_chunk$set(fig.width=9, fig.height=7)
opts_chunk$set(comment="", fig.align="center", tidy=TRUE, fig.retina=2, cache = TRUE)

library(tidyverse)
library(broom)
library(naniar)
library(simputation)
library(VIM)

library(tidyverse)

new_vitals_df1 <- read_csv("../vital-signs.csv", show_col_types = FALSE)
# data features to cut out are: BMI1, BMI2, BMI3, Days btwn baseline and F/U Wt
new_vitals_df1 <- new_vitals_df[, !(names(new_vitals_df) %in% 
                      c("BMI1", "BMI2", "BMI3", "Days btwn baseline and F/U Wt"))]

# how to use kNN
?kNN

# looking at data
head(new_vitals_df1)
summary(new_vitals_df1)

# first we need to replace the n/a values with NA
new_vitals_df1[new_vitals_df1 == "n/a"] <- NA

# we can impute missing data on WC1, SBP2, DBP2, WC 2, Wt (lbs)2
new_vitals_df2 <- kNN(new_vitals_df1)
new_vitals_df2

# create and print out version with categorical data as integers
new_vitals_df2$`Session type` <- as.numeric(as.factor(new_vitals_df2$`Session type`))
new_vitals_df2
