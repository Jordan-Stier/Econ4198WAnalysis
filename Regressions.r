library(haven)
library(rlang)
library(dplyr)
library(tidyr)
library(AER)
library(censReg)
library(ggplot2)
library(lmtest)
library(marginaleffects)
library(survey)
library(pandoc)
library(flextable)
library(modelsummary)

housing_data <-readRDS("data_0511_clean.rds")

# Done due to skew in the data
# Doing these separately to remember what my target vars are
housing_data$volunteer_particip <- as.integer(housing_data$volunteer_mins >0)
housing_data$civic_particip <- as.integer(housing_data$civic_mins >0)
# These 2 are placebos
housing_data$sports_particip <- as.integer(housing_data$sports_mins >0)
housing_data$social_particip <- as.integer(housing_data$social_mins >0)

# Dif ids by year
housing_data <- housing_data %>%
  mutate(strat_yr = interaction(year_cat, GEPSEUST, drop = TRUE),
         clust_yr = interaction(year_cat, GEPSEUCL, drop = TRUE))

# need to use this, i was getting insane results cause I wasn't.
# haven't decided if I'll use svyglm yet
# svdes <- svydesign(
#   ids = ~clust_yr, strata = ~strat_yr,
#   weights = ~TUFINLWGT, nest = TRUE, data = housing_data
# )

# Lazy models ----
## Probit/Intensity given participation ----

# Regressions
# Using a suite to suggest original was unbiased

# Runs a suite of models on participation, with more
# controls in each.
run_models <- function(
    outcome, data = housing_data, subset = NULL, survey_design = svdes) {
  
  subset_expr <- enquo(subset)
  
  if (!rlang::quo_is_null(subset_expr) && !missing(subset)) {
    data <- data %>% filter(!!subset_expr)
  }
  
  # Model pieces
  base  <- "homeowner_cat*year_cat"
  demog <- "age + sex_cat + race_cat + relig_cat"
  socio <- "education_cat + income_cat + employed_cat"
  hh    <- "married_cat + children"
  
  rhs <- c(
    "Baseline"               = base,
    "Demographic Controls"   = paste(base, demog, sep = " + "),
    "Socioeconomic Controls" = paste(base, demog, socio, sep = " + "),
    "Full Model"             = paste(base, demog, socio, hh, sep = " + ")
  )
  
  # Weighted Tobit with robust SEs
  lapply(rhs, function(r) {
    survival::survreg(
      as.formula(paste(
        "survival::Surv(", outcome, ", ",
        outcome, " > 0, type = 'left') ~ ", r
      )),
      data = data,
      weights = TUFINLWGT,
      dist = "gaussian",
      robust = TRUE
    )
  })
}

### Volunteer ----
mdl_volunteer <- run_models("volunteer_mins")
### Civic ----
mdl_civic <- run_models("civic_mins")
### Sports ----
mdl_sports <- run_models("sports_mins")
### Socializing ----
mdl_social <- run_models("social_mins")

# tables ----
# May need to change these since they're tobit now.
# Coef map
coef_names <- c(
  "(Intercept)" = "Constant",
  "homeowner_catHomeowner" = "Homeowner",
  "year_cat2011" = "Year: 2011",
  "homeowner_catHomeowner:year_cat2011" = "Homeowner × 2011"
)
# defining control rows
control_rows <- data.frame(
  term = c("Demographic Controls", "Socioeconomic Controls",
           "Household Controls"),
  `1` = c("", "", ""),
  `2` = c("Yes", "", ""),
  `3` = c("Yes", "Yes",""),
  `4` = c("Yes", "Yes", "Yes"),
  check.names = FALSE
)
attr(control_rows, "position")<-"coef_end"
# formatted tables
format_reg_table <- function(models, filename,
                             coef_mapping,
                             rows_controls,
                             model_names = c(
                               "Baseline",
                               "Demographic Controls",
                               "Socioeconomic Controls",
                               "Full Model"
                             )) {
  
  ft <- modelsummary(
    models,
    coef_map = coef_mapping,
    model_names = model_names,
    stars = TRUE,
    add_rows = rows_controls,
    output = "flextable"
  )
  
  ft <- ft |>
    theme_booktabs() |>
    autofit() |>
    fontsize(size = 9, part = "all") |>
    align(align = "center", part = "all")
  
  save_as_docx(ft, path = filename)
  
  return(ft)
}
# Files for model tables ----
model_tables <- list(
  "reg1-vol_table.docx" = mdl_volunteer,
  "reg2-civ_table.docx" = mdl_civic,
  "reg3-sport_table.docx" = mdl_sports,
  "reg4-soc_table.docx" = mdl_social
)

ft_list <- lapply(names(model_tables), function(filename) {
  format_reg_table(
    models = model_tables[[filename]],
    filename = filename, coef_mapping = coef_names,
    rows_controls = control_rows
  )
})

names(ft_list) <- names(model_tables)