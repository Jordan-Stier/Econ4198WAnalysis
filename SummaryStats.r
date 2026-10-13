library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)

housing_data <-readRDS("data_0511_clean.rds")

# Summary Statistics Table. AI Assisted ----
make_summary <- function(data, var, label = var, percent_value = NULL) {
  
  x <- data[[var]]
  
  if (!is.null(percent_value)) {
    # Percentage of observations equal to the specified category
    pct <- mean(x == percent_value, na.rm = TRUE) * 100
    
    data.frame(
      Variable = label,
      N = sum(!is.na(x)),
      Mean = pct,
      SD = NA_real_
    )
    
  } else {
    # Regular continuous-variable summary
    data.frame(
      Variable = label,
      N = sum(!is.na(x)),
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE)
    )
  }
}

table1 <- bind_rows(
  
  make_summary(data_0511, "volunteer_mins", "Volunteer minutes"),
  make_summary(data_0511, "civic_mins", "Civic Participation minutes"),
  make_summary(data_0511, "age", "Age"),
  make_summary(data_0511, "income", "Income"),
  make_summary(data_0511, "education", "Education"),
  make_summary(data_0511, "children", "Children"),
  make_summary(data_0511, "workhours", "Work hours"),
  
  make_summary(
    data_0511, "sex", "Female (%)",
    percent_value = 2
  ),
  make_summary(
    data_0511, "race", "White (%)",
    percent_value = 1
  ),
  make_summary(
    data_0511, "owner", "Homeowner (%)",
    percent_value = 1
  ),
  
  make_summary(
    data_0511, "married", "Married (%)",
    percent_value = 1
  ),
  
  make_summary(
    data_0511, "employed", "Employed (%)",
    percent_value = 1
  ),
  
  make_summary(
    data_0511, "post2008", "Post 2008 (%)",
    percent_value = 1
  )
) %>%
  mutate(
    Mean = round(Mean, 2),
    SD = round(SD, 2)
  )

table1
write.csv(
  table1,
  "table1_descriptive_statistics.csv",
  row.names = FALSE
)