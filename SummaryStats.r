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
housing_data <-readRDS("data_0413_clean.rds")

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
summary_table <- summary_table %>%
  mutate(
    N = format(N, big.mark = ",", scientific = FALSE),
    Nonzero_N = format(Nonzero_N, big.mark = ",", scientific = FALSE),
    across(c(Mean, SD, Minimum, Median, Maximum, Median_Positive),
           ~ sprintf("%.2f", .x))
  )
ft <- flextable(summary_table) |>
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
  ) |>
  theme_booktabs() |>
  bold(part = "header") |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |>
  align(align = "left", j = "Variable", part = "all") |>
  align(align = "center", j = setdiff(names(summary_table), "Variable"), part = "all") |>
  autofit() |>
  set_table_properties(layout = "autofit", width = 1)
# Note: Make Landscape
save_as_docx(ft, path = "summary_statistics.docx")

categorical_vars <- c(
  "homeowner_cat", "education_cat", "race_cat", "sex_cat",
  "relig_cat",
  "married_cat", "citizen_cat", "income_cat",
  "employed_cat", "workhours_cat", "year_cat"
)

var_labels <- c(
  homeowner_cat = "Homeownership",
  education_cat = "Education",
  race_cat = "Race",
  sex_cat = "Sex",
  relig_cat = "Religious",
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
      levels = c("White", "Black", "Other / Multiracial")
    )
  }
  
  counts <- table(x, useNA = "ifany")
  counts <- counts[counts > 0]
  
  total_n <- nrow(housing_data)
  
  temp <- data.frame(
    Variable = rep(unname(var_labels[v]), length(counts)),
    Type = ifelse(is.na(names(counts)) | names(counts) == "<NA>",
                  "Missing", names(counts)),
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

format_categorical_table <- function(data) {
  
  data <- data %>%
    mutate(
      Variable = ifelse(duplicated(Variable), "", Variable),
      N = format(N, big.mark = ",", scientific = FALSE),
      Percent = sprintf("%.1f%%", Percent)
    )
  
  flextable(data) |>
    set_header_labels(
      Variable = "Variable",
      Type = "Category",
      N = "N",
      Percent = "Percent"
    ) |>
    theme_booktabs() |>
    bold(part = "header") |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 10, part = "all") |>
    align(align = "left", j = c("Variable", "Type"), part = "all") |>
    align(align = "center", j = c("N", "Percent"), part = "all") |>
    autofit() |>
    set_table_properties(layout = "autofit", width = 1)
}

# 1. Full appendix table
ft_categorical_all <- format_categorical_table(
  categorical_table
)

save_as_docx(
  ft_categorical_all,
  path = "categorical_full.docx"
)

# 2. Selected data section table
selected_vars <- c("Homeownership", "Survey Period", "Total Observations")

selected_table <- categorical_table %>%
  filter(Variable %in% selected_vars)

ft_categorical_selected <- format_categorical_table(
  selected_table
)

ft_categorical_selected <- ft_categorical_selected |>
  add_footer_lines(
    values = "Note: The complete set of categorical-variable summary statistics is reported in Appendix C, Table C1."
  ) |>
  font(fontname = "Times New Roman", part = "footer") |>
  italic(part = "footer") |>
  fontsize(size = 9, part = "footer")

save_as_docx(
  ft_categorical_selected,
  path = "categorical_select.docx"
)

# Plotting ----
p <- ggplot(
  housing_data %>%
    mutate(
      Homeownership = homeowner_cat,
      Year = year_cat
    ),
  aes(x = Year, y = volunteer_mins, fill = Homeownership, weight = wt_pooled)
) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(width = 0.9),
    color = "black",
    linewidth = 0.3
  ) +
  scale_fill_grey(
    start = 0.85,
    end = 0.35
  ) +
  labs(
    title = "Homeownership and Volunteering by Survey Year",
    x = "Survey Year",
    y = "Weighted Mean Volunteer Minutes",
    fill = "Homeownership Status"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  "homeownership_volunteering.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)