library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(marginaleffects)

housing_data <-readRDS("data_0511_clean.rds")

# Lazy models ----
## Probit/Intensity given participation ----
# Done due to skew in the data
housing_data$volunteer_particip <- as.integer(housing_data$volunteer_mins >0)
housing_data$civic_particip <- as.integer(housing_data$civic_mins >0)

# Regressions
# particip y/n
mdl_volunteer_probit <- housing_data %>% glm(
  volunteer_particip ~ owner_cat + age + sex_cat + education_cat +
    income_cat + married_cat, data = ., family = binomial(link = "probit"))

## Pre/Post 2008 regressions ----
for(yr in 0:1){
  cat("\n--- Period:", yr, " ---\n")
  # participation y/n
  
  # intensity given y
    
}