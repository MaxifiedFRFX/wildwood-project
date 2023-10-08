library(VIM)
library("dplyr")

count_missing = function(df) {
  sapply(df, FUN=function(col) sum(is.na(col)) )
}

survey1_deleted <- survey2[-c(1:26)]

survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed <- 
  format(strptime(
    survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed, "%I%p"
  ), format="%H")


survey1_deleted <- survey1_deleted %>%
  mutate(breakfast = ifelse(
    X24_Which_meals_eat_everyday %in% c('1', '1_2', '1_3', '1_2_3'),
    1, ifelse(X24_Which_meals_eat_everyday %in% 
                c('1', '2', '3', '1_2', '1_3', '2_3', '1_2_3'), 0, NA)
  ))

survey1_deleted <- survey1_deleted %>%
  mutate(lunch = ifelse(
    X24_Which_meals_eat_everyday %in% c('2', '1_2', '2_3', '1_2_3'),
    1, ifelse(X24_Which_meals_eat_everyday %in% 
                c('1', '2', '3', '1_2', '1_3', '2_3', '1_2_3'), 0, NA)
  ))

survey1_deleted <- survey1_deleted %>%
  mutate(dinner = ifelse(
    X24_Which_meals_eat_everyday %in% c('3', '1_3', '2_3', '1_2_3'),
    1, ifelse(X24_Which_meals_eat_everyday %in% 
                c('1', '2', '3', '1_2', '1_3', '2_3', '1_2_3'), 0, NA)
  ))


survey1_deleted <- survey1_deleted[, !(names(survey1_deleted) %in% 
                                         c("X24_Which_meals_eat_everyday"))]

survey1_deleted <- survey1_deleted %>%
  select(dinner, everything())
survey1_deleted <- survey1_deleted %>%
  select(lunch, everything())
survey1_deleted <- survey1_deleted %>%
  select(breakfast, everything())


column_types1 <- sapply(survey1_deleted, class)

non_numeric_columns1 <- names(column_types1[column_types1 != "numeric"])

for (col in non_numeric_columns1) {
  survey1_deleted[[col]] <- as.numeric(survey1_deleted[[col]])
}

survey1_deleted <- rbind(survey1_deleted, colMeans = colMeans(survey1_deleted, na.rm = TRUE))

#numeric_means <- colMeans(final, na.rm = TRUE)
#final_with_means <- rbind(final, colMeans = numeric_means)

final <- cbind(ID = rownames(survey1_deleted), survey1_deleted)
rownames(final) <- 1:nrow(final)

write.csv(final, file='../survey2-means.csv', row.names=FALSE)

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

