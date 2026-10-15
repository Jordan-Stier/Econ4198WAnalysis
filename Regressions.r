library(haven)
library(rlang)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(marginaleffects)
library(survey)

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
      as.formula(paste(outcome, "~ owner")),
      data = data,
      weights = TUFINLWGT
    )
    # Model 2: Demographic
    m2 <- lm(
      as.formula(paste(
        outcome,
        "~ owner + age + sex_cat + race_cat"
      )),
      data = data,
      weights = TUFINLWGT
    )
    # Model 3: SocioEcon
    m3 <- lm(
      as.formula(paste(
        outcome,
        "~ owner + age + sex_cat + race_cat +",
        "education_cat + income_cat + employed_cat"
      )),
      data = data,
      weights = TUFINLWGT
    )
    # Model 4: Household
    m4 <- lm(
      as.formula(paste(
        outcome,
        "~ owner + age + sex_cat + race_cat +",
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
        outcome, "~owner"
      )),
      data = data,
      family = binomial(link = "probit"),
      weights = TUFINLWGT
    )
    # Model 2: Demographic
    m2 <- glm(
      as.formula(paste(
        outcome, "~ owner + age + sex_cat + race_cat"
      )),
      data = data,
      family = binomial(link = "probit"),
      weights = TUFINLWGT
    )
    # Model 3: SocioEcon
    m3 <- glm(
      as.formula(paste(
        outcome, "~ owner + age + sex_cat + race_cat +",
        "education_cat + income_cat + employed_cat"
      )),
      data = data,
      family = binomial(link = "probit"),
      weights = TUFINLWGT
    )
    # Model 4: Household
    m4 <- glm(
      as.formula(paste(
        outcome, "~ owner + age + sex_cat + race_cat +",
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
    unadjust = m1,
    demog = m2,
    socio = m3,
    household = m4
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

## Pre/Post 2008 regressions ----
### Volunteer Pre----
mdl_vol_partic_pre <- run_models("volunteer_particip",
                                  subset = post2008 == 0)
mdl_volunteer_inten_pre <- run_models("volunteer_mins",
                          subset = post2008 == 0,
                          intensity = TRUE)
### Civic Pre----
mdl_civic_partic_pre <- run_models("civic_particip",
                                   subset = post2008 == 0)
mdl_civic_inten_pre <- run_models("civic_mins",
                           subset = post2008 == 0,
                           intensity = TRUE)

### Volunteer Post----
mdl_vol_partic_post <- run_models("volunteer_particip",
                                  subset = post2008 == 1)
mdl_volunteer_inten_post <- run_models("volunteer_mins",
                               subset = post2008 == 1,
                               intensity = TRUE)
### Civic Post----
mdl_civic_partic_post <- run_models("civic_particip",
                                    subset = post2008 == 1)
mdl_civic_inten_post <- run_models("civic_mins",
                            subset = post2008 == 1,
                            intensity = TRUE)