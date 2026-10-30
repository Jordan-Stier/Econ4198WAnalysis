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

# Done due to skew in the data
housing_data$volunteer_particip <- as.integer(housing_data$volunteer_mins >0)
housing_data$civic_particip <- as.integer(housing_data$civic_mins >0)
# These 3 are placebos
housing_data$sports_particip <- as.integer(housing_data$sports_mins >0)
housing_data$socgen_particip <- as.integer(housing_data$socgen_mins >0)
housing_data$socevent_particip <- as.integer(housing_data$socevent_mins >0)

# Dif ids by year
housing_data <- housing_data %>%
  mutate(strat_yr = interaction(year_cat, GEPSEUST, drop = TRUE),
         clust_yr = interaction(year_cat, GEPSEUCL, drop = TRUE))

# need to use this, i was getting insane results cause I wasn't.
# haven't decided if I'll use svyglm yet
svdes <- svydesign(
  ids = ~clust_yr, strata = ~strat_yr,
  weights = ~TUFINLWGT, nest = TRUE, data = housing_data
)

# Lazy models ----
## Probit/Intensity given participation ----

# Regressions
# Originally used owner_cat + age + sex_cat +
# education_cat + income_cat + race_cat + married_cat +
# employed_cat + workhours_cat + children + metro_cat
# But that lowers statistical confidence

# Runs a suite of models on participation, with more
# controls in each.
run_models <- function(
    outcome, data = housing_data,
    subset = NULL, intensity = FALSE, survey_design = svdes){
  
  subset_expr <- enquo(subset)
  # only uses subset if its given:
  if(!rlang::quo_is_null(subset_expr) &&
     !missing(subset)){
    data <- data %>% filter(!!subset_expr)
  }
  # Intensity models: only people who participated
  if (intensity) {
    data <- data %>% filter(.data[[outcome]] > 0)
  }

  # Survey design, built once per call
  des <- survey_design
  if (!outcome %in% names(des$variables))
    stop("Outcome '", outcome, "' is not in the survey design data")
  
  keep <- rep(TRUE, nrow(des$variables))
  subset_expr <- enquo(subset)
  if (!rlang::quo_is_null(subset_expr)) {
    keep <- keep & rlang::eval_tidy(subset_expr, data = des$variables)
  }
  if (intensity) keep <- keep & des$variables[[outcome]] > 0
  keep[is.na(keep)] <- FALSE
  des <- des[which(keep), ]   # subsets the design, keeping the full variance structure
  options(survey.lonely.psu = "adjust")
  
  # Right-hand sides, with more controls in each
  rhs <- c(
    "Baseline"               = "homeowner_cat*year_cat",
    "Demographic Controls"   = "homeowner_cat*year_cat + age + sex_cat + race_cat",
    "Socioeconomic Controls" = paste("homeowner_cat*year_cat + age + sex_cat + race_cat +",
                                     "education_cat + income_cat + employed_cat"),
    "Full Model"             = paste("homeowner_cat*year_cat + age + sex_cat + race_cat +",
                                     "education_cat + income_cat + employed_cat +",
                                     "married_cat + children")
  )
  # probit for participation, weighted OLS for minutes
  fam <- if (intensity) gaussian() else quasibinomial(link = "probit")
  
  # returns a named list, so sport_mdl$`Full Model` still works
  lapply(rhs, function(r) {
    svyglm(as.formula(paste(outcome, "~", r)), design = des, family = fam)
  })
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

model_tables <- list(
  "reg1-volpart_table.docx" = mdl_vol_partic_aggr,
  "reg2-volint_table.docx"  = mdl_volunteer_inten_aggr,
  "reg3-civpart_table.docx" = mdl_civic_partic_aggr,
  "reg4-civint_table.docx"  = mdl_civic_inten_aggr
)

ft_list <- lapply(names(model_tables), function(filename) {
  format_reg_table(
    models = model_tables[[filename]],
    filename = filename, coef_mapping = coef_names,
    rows_controls = control_rows
  )
})

names(ft_list) <- names(model_tables)