library(VIM)
library("dplyr")

count_missing = function(df) {
  sapply(df, FUN=function(col) sum(is.na(col)) )
}

survey1_deleted <- survey1[-c(1:26)]
survey2_deleted <- survey2[-c(1:27)]

survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed <- 
  format(strptime(
    survey1_deleted$X31_Over_last_two_weeks_when_gone_to_bed, "%I%p"
    ), format="%H")
survey2_deleted$X31_Over_last_two_weeks_when_gone_to_bed <- 
  format(strptime(
    survey2_deleted$X31_Over_last_two_weeks_when_gone_to_bed, "%I%p"
    ), format="%H")


survey1_deleted <- survey1_deleted %>%
  mutate(breakfast = ifelse(
    X24_Which_meals_eat_everyday %in% c('1', '1_2', '1_3', '1_2_3'),
    1, ifelse(X24_Which_meals_eat_everyday %in% 
                c('1', '2', '3', '1_2', '1_3', '2_3', '1_2_3'), 0, NA)
  ))
survey2_deleted <- survey2_deleted %>%
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
survey2_deleted <- survey2_deleted %>%
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
survey2_deleted <- survey2_deleted %>%
  mutate(dinner = ifelse(
    X24_Which_meals_eat_everyday %in% c('3', '1_3', '2_3', '1_2_3'),
    1, ifelse(X24_Which_meals_eat_everyday %in% 
                c('1', '2', '3', '1_2', '1_3', '2_3', '1_2_3'), 0, NA)
  ))


survey1_deleted <- survey1_deleted[, !(names(survey1_deleted) %in% 
                                       c("X24_Which_meals_eat_everyday"))]
survey2_deleted <- survey2_deleted[, !(names(survey2_deleted) %in% 
                                         c("X24_Which_meals_eat_everyday"))]

survey1_deleted <- survey1_deleted %>%
  select(dinner, everything())
survey1_deleted <- survey1_deleted %>%
  select(lunch, everything())
survey1_deleted <- survey1_deleted %>%
  select(breakfast, everything())

survey2_deleted <- survey2_deleted %>%
  select(dinner, everything())
survey2_deleted <- survey2_deleted %>%
  select(lunch, everything())
survey2_deleted <- survey2_deleted %>%
  select(breakfast, everything())


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

matching_row_names1 <- rownames(survey1_deleted) %in% rownames(survey2_deleted)
only_matching1 <- survey1_deleted[matching_row_names1, ]
matching_row_names2 <- rownames(survey2_deleted) %in% rownames(survey1_deleted)
only_matching2 <- survey2_deleted[matching_row_names2, ]

final <- only_matching2 - only_matching1
summary(final)

final <- rbind(final, colMeans = colMeans(final, na.rm = TRUE))

#numeric_means <- colMeans(final, na.rm = TRUE)
#final_with_means <- rbind(final, colMeans = numeric_means)

final <- cbind(ID = rownames(final), final)
rownames(final) <- 1:nrow(final)

write.csv(final, file='../surveys1-and-2-differences.csv', row.names=FALSE)

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

