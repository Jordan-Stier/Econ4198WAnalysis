library(haven)
library(rlang)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(marginaleffects)
library(survey)
library(knitr)
housing_data <-readRDS("data_0511_clean.rds")

summary_vars <- c(
  "volunteer_mins",
  "civic_mins",
  "age",
  "children"
)

summary_table <- data.frame(
  Variable = summary_vars,
  N = sapply(housing_data[summary_vars], function(x) sum(!is.na(x))),
  Mean = sapply(housing_data[summary_vars], function(x) mean(x, na.rm = TRUE)),
  SD = sapply(housing_data[summary_vars], function(x) sd(x, na.rm = TRUE)),
  Minimum = sapply(housing_data[summary_vars], function(x) min(x, na.rm = TRUE)),
  Maximum = sapply(housing_data[summary_vars], function(x) max(x, na.rm = TRUE))
)

write.table(
  summary_table,
  "summary_statistics.tsv",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

categorical_vars <- c(
  "homeowner_cat", "education_cat", "race_cat", "sex_cat",
  "married_cat", "citizen_cat", "income_cat",
  "employed_cat", "workhours_cat", "year_cat"
)

var_labels <- c(
  homeowner_cat = "Homeownership",
  education_cat = "Education",
  race_cat = "Race",
  sex_cat = "Sex",
  married_cat = "Marital Status",
  citizen_cat = "Citizenship",
  income_cat = "Household Income",
  employed_cat = "Employment Status",
  workhours_cat = "Work Hours",
  year_cat = "Survey Period"
)

output <- character()

for (v in categorical_vars) {
  x <- housing_data[[v]]
  
  if (v == "race_cat") {
    x <- factor(
      x,
      levels = c("White", "Black", "Multiracial", "Other single race")
    )
  }
  
  counts <- table(x, useNA = "no")
  counts <- counts[counts > 0]
  
  total_n <- sum(!is.na(x))
  percentages <- 100 * as.integer(counts) / total_n
  
  output <- c(
    output,
    unname(var_labels[v]),
    paste0("Total N = ", total_n),
    paste(names(counts), collapse = "\t"),
    paste(as.integer(counts), collapse = "\t"),
    paste0(sprintf("%.1f%%", percentages), collapse = "\t"),
    ""
  )
}

writeLines(output, "categorical_statistics.txt")