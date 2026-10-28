library(haven)
library(rlang)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(marginaleffects)
library(survey)
library(pandoc)
library(flextable)
library(modelsummary)

housing_data <-readRDS("data_0511_clean.rds")

# haven't decided if I'll use svyglm yet
design <- svydesign(
  ids = ~GEPSEUCL,
  strata = ~GEPSEUST,
  weights = ~TUFINLWGT,
  data = housing_data,
  nest = TRUE
)

# Lazy models ----
## Probit/Intensity given participation ----
# Done due to skew in the data
housing_data$volunteer_particip <- as.integer(housing_data$volunteer_mins >0)
housing_data$civic_particip <- as.integer(housing_data$civic_mins >0)

# Regressions
# Originally used owner_cat + age + sex_cat +
# education_cat + income_cat + race_cat + married_cat +
# employed_cat + workhours_cat + children + metro_cat
# But that lowers statistical confidence

# Runs a suite of models on participation, with more
# controls in each.
run_models <- function(
    outcome, data = housing_data,
    subset = NULL, intensity = FALSE){
  
  subset_expr <- enquo(subset)
  # only uses subset if its given:
  if(!rlang::quo_is_null(subset_expr) &&
     !missing(subset)){
    data <- data %>% filter(!!subset_expr)
  }
  
  if (intensity) {
    # Only people who participated
    data <- data %>% filter(.data[[outcome]] > 0)
    # Model 1: Unadjusted
    m1 <- lm(
      as.formula(paste(outcome, "~ homeowner_cat*year_cat")),
      data = data,
      weights = TUFINLWGT
    )
    # Model 2: Demographic
    m2 <- lm(
      as.formula(paste(
        outcome,
        "~ homeowner_cat*year_cat + age + sex_cat + race_cat"
      )),
      data = data,
      weights = TUFINLWGT
    )
    # Model 3: SocioEcon
    m3 <- lm(
      as.formula(paste(
        outcome,
        "~ homeowner_cat*year_cat + age + sex_cat + race_cat +",
        "education_cat + income_cat + employed_cat"
      )),
      data = data,
      weights = TUFINLWGT
    )
    # Model 4: Household
    m4 <- lm(
      as.formula(paste(
        outcome,
        "~ homeowner_cat*year_cat + age + sex_cat + race_cat +",
        "education_cat + income_cat + employed_cat +",
        "married_cat + children"
      )),
      data = data,
      weights = TUFINLWGT
    )
  } else {
    # Model 1: Unadjusted
    m1 <- glm(
      as.formula(paste(
        outcome, "~homeowner_cat*year_cat"
      )),
      data = data,
      family = binomial(link = "probit"),
      weights = TUFINLWGT
    )
    # Model 2: Demographic
    m2 <- glm(
      as.formula(paste(
        outcome, "~homeowner_cat*year_cat + age + sex_cat + race_cat"
      )),
      data = data,
      family = binomial(link = "probit"),
      weights = TUFINLWGT
    )
    # Model 3: SocioEcon
    m3 <- glm(
      as.formula(paste(
        outcome, "~homeowner_cat*year_cat + age + sex_cat + race_cat +",
        "education_cat + income_cat + employed_cat"
      )),
      data = data,
      family = binomial(link = "probit"),
      weights = TUFINLWGT
    )
    # Model 4: Household
    m4 <- glm(
      as.formula(paste(
        outcome, "~homeowner_cat*year_cat + age + sex_cat + race_cat +",
        "education_cat + income_cat + employed_cat +",
        "married_cat + children"
      )),
      data = data,
      family = binomial(link = "probit"),
      weights = TUFINLWGT
    )
  }
  # Return models for inspection
  list(
    "Baseline" = m1,
    "Demographic Controls" = m2,
    "Socioeconomic Controls" = m3,
    "Full Model" = m4
  )
}

### Volunteer ----
mdl_vol_partic_aggr <- run_models("volunteer_particip")
mdl_volunteer_inten_aggr <- run_models("volunteer_mins",
                                  intensity = TRUE)
### Civic ----
mdl_civic_partic_aggr <- run_models("civic_particip")
mdl_civic_inten_aggr <- run_models("civic_mins",
                              intensity = TRUE)

# tables ----
lapply(mdl_volunteer_inten_aggr, function(m) {
  if (inherits(m, "lm") || inherits(m, "glm")) {
    names(coef(m))
  } else {
    class(m)
  }
})
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
ft1 <- modelsummary(
  mdl_vol_partic_aggr,
  coef_map = coef_names,
  model_names = c(
    "Baseline",
    "Demographic Controls",
    "Socioeconomic Controls",
    "Full Model"
  ),
  stars = TRUE,
  add_rows = control_rows,
  output = "flextable"
)

ft1 <- theme_booktabs(ft1)
ft1 <- autofit(ft1)
ft1 <- fontsize(ft1, size = 9, part = "all")
ft1 <- align(ft1, align = "center", part = "all")
save_as_docx(ft1, path = "reg1-volpart_table.docx")

ft2 <- modelsummary(
  mdl_volunteer_inten_aggr,
  coef_map = coef_names,
  model_names = c(
    "Baseline",
    "Demographic Controls",
    "Socioeconomic Controls",
    "Full Model"
  ),
  stars = TRUE,
  add_rows = control_rows,
  output = "flextable"
)

ft2 <- theme_booktabs(ft2)
ft2 <- autofit(ft2)
ft2 <- fontsize(ft2, size = 9, part = "all")
ft2 <- align(ft2, align = "center", part = "all")
save_as_docx(ft2, path = "reg2-volint_table.docx")

ft3 <- modelsummary(
  mdl_civic_partic_aggr,
  coef_map = coef_names,
  model_names = c(
    "Baseline",
    "Demographic Controls",
    "Socioeconomic Controls",
    "Full Model"
  ),
  stars = TRUE,
  add_rows = control_rows,
  output = "flextable"
)

ft3 <- theme_booktabs(ft3)
ft3 <- autofit(ft3)
ft3 <- fontsize(ft3, size = 9, part = "all")
ft3 <- align(ft3, align = "center", part = "all")
save_as_docx(ft3, path = "reg3-civpart_table.docx")

ft4 <- modelsummary(
  mdl_civic_inten_aggr,
  coef_map = coef_names,
  model_names = c(
    "Baseline",
    "Demographic Controls",
    "Socioeconomic Controls",
    "Full Model"
  ),
  stars = TRUE,
  add_rows = control_rows,
  output = "flextable"
)

ft4 <- theme_booktabs(ft4)
ft4 <- autofit(ft4)
ft4 <- fontsize(ft4, size = 9, part = "all")
ft4 <- align(ft4, align = "center", part = "all")
save_as_docx(ft4, path = "reg4-civint_table.docx")