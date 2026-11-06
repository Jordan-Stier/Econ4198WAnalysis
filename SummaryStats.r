library(haven)
library(rlang)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(marginaleffects)
library(survey)
library(knitr)
library(flextable)
housing_data <-readRDS("data_0511_clean.rds")

# Tables ----
summary_vars <- c(
  "volunteer_mins",
  "civic_mins",
  "social_mins",
  "sports_mins",
  "age",
  "children"
)

summary_table <- data.frame(
  Variable = summary_vars,
  N = sapply(housing_data[summary_vars], function(x) sum(!is.na(x))),
  Mean = sapply(housing_data[summary_vars], function(x) mean(x, na.rm = TRUE)),
  SD = sapply(housing_data[summary_vars], function(x) sd(x, na.rm = TRUE)),
  Minimum = sapply(housing_data[summary_vars], function(x) min(x, na.rm = TRUE)),
  Median = sapply(housing_data[summary_vars], function(x) median(x, na.rm = TRUE)),
  Maximum = sapply(housing_data[summary_vars], function(x) max(x, na.rm = TRUE)),
  Nonzero_N = sapply(housing_data[summary_vars], function(x) sum(x > 0, na.rm = TRUE)),
  Median_Positive = sapply(housing_data[summary_vars], function(x) {
    if (any(x > 0, na.rm = TRUE)) median(x[x > 0], na.rm = TRUE) else NA_real_
  })
)
summary_table$Variable <- c(
  "Volunteer Minutes",
  "Civic Engagement Minutes",
  "Socializing Minutes",
  "Sports and Exercise Minutes",
  "Age",
  "Number of Children"
)
summary_table[] <- lapply(summary_table, function(x) {
  if (is.numeric(x)) sprintf("%.2f", x) else x
})
ft <- flextable(summary_table) |>
  autofit() |>
  set_table_properties(layout = "autofit", width = 1)|>
  set_header_labels(
    Variable = "Activity / Characteristic",
    N = "N",
    Mean = "Mean",
    SD = "Std. Dev.",
    Minimum = "Minimum",
    Median = "Median",
    Maximum = "Maximum",
    Nonzero_N = "Nonzero Observations",
    Median_Positive = "Median (Positive Values)"
  )
# Note: Make Landscape
save_as_docx(ft, path = "summary_statistics.docx")

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

categorical_table <- data.frame(
  Variable = character(),
  Type = character(),
  N = integer(),
  Percent = numeric()
)

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
  
  temp <- data.frame(
    Variable = rep(unname(var_labels[v]), length(counts)),
    Type = names(counts),
    N = as.integer(counts),
    Percent = round(100 * as.integer(counts) / total_n, 1)
  )
  
  categorical_table <- rbind(categorical_table, temp)
}

categorical_table <- rbind(
  categorical_table,
  data.frame(
    Variable = "Total Observations",
    Type = "",
    N = nrow(housing_data),
    Percent = 100
  )
)

write.table(
  categorical_table,
  "categorical_statistics.tsv",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

# Plotting ----
p <- ggplot(
  housing_data %>%
    mutate(
      Homeownership = homeowner_cat,
      Year = year_cat
    ),
  aes(x = Homeownership, y = volunteer_mins, color = Year)
) +
  geom_jitter(
    width = 0.15,
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  scale_y_continuous(trans = "log1p") +
  labs(
    title = "Homeownership and Volunteering by Survey Year",
    x = "Homeownership Status",
    y = "log(1+Volunteer Minutes)",
    color = "Survey Year"
  ) +
  theme_minimal(base_size = 12)

print(p)

ggsave(
  "homeownership_volunteering.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)