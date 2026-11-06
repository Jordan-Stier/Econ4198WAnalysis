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
  data <- droplevels(data)
  
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
# Avg marginal effects. Currently only for homeowner:year interaction.
ame_inference <- function(m) {
  
  V <- vcov(m)
  b <- coef(m)
  
  # Check whether the covariance matrix includes log(scale)
  extra <- setdiff(colnames(V), names(b))
  
  if (length(extra) == 1 &&
      grepl("scale", extra, ignore.case = TRUE)) {
    scale_name <- extra
    include_scale <- TRUE
  } else if (length(extra) == 0) {
    include_scale <- FALSE
  } else {
    stop("Unexpected variance-covariance matrix structure.")
  }
  
  # Parameter vector
  pars <- b
  
  if (include_scale) {
    pars <- c(pars, setNames(log(m$scale), scale_name))
  }
  
  V <- V[names(pars), names(pars), drop = FALSE]
  
  # Recalculate AMEs at a given parameter vector
  calculate <- function(p) {
    mm <- m
    mm$coefficients <- p[names(b)]
    
    if (include_scale) {
      mm$scale <- exp(p[scale_name])
    }
    
    as.vector(t(effects_ame(mm)))
  }
  
  estimates <- calculate(pars)
  
  # Numerical gradient (central differences)
  G <- matrix(
    NA_real_,
    nrow = length(estimates),
    ncol = length(pars)
  )
  
  for (j in seq_along(pars)) {
    
    step <- 1e-5 * max(1, abs(pars[j]))
    
    p_plus <- pars
    p_minus <- pars
    
    p_plus[j] <- p_plus[j] + step
    p_minus[j] <- p_minus[j] - step
    
    G[, j] <- (
      calculate(p_plus) - calculate(p_minus)
    ) / (2 * step)
  }
  
  # Delta-method variance
  ame_var <- G %*% V %*% t(G)
  ame_se <- sqrt(pmax(diag(ame_var), 0))
  
  # Organize results
  original <- effects_ame(m)
  
  est <- matrix(
    estimates,
    nrow = nrow(original),
    byrow = TRUE,
    dimnames = dimnames(original)
  )
  
  se <- matrix(
    ame_se,
    nrow = nrow(original),
    byrow = TRUE,
    dimnames = dimnames(original)
  )
  
  p <- 2 * pnorm(-abs(est / se))
  
  list(
    estimate = est,
    se = se,
    p = p
  )
}

# Format marginal effects as flextable
format_ame_table <- function(models, filename = NULL) {
  
  stars <- function(p) {
    ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**",
                  ifelse(p < 0.10, "*", "")))
  }
  
  results <- do.call(rbind, lapply(names(models), function(model_name) {
    
    x <- ame_inference(models[[model_name]])
    
    data.frame(
      Model = model_name,
      Comparison = rownames(x$estimate),
      
      Ey_est = x$estimate[, "Ey"],
      Ey_se = x$se[, "Ey"],
      Ey_p = x$p[, "Ey"],
      
      Pr_est = x$estimate[, "Pr"] * 100,
      Pr_se = x$se[, "Pr"] * 100,
      Pr_p = x$p[, "Pr"],
      
      Eypos_est = x$estimate[, "Eypos"],
      Eypos_se = x$se[, "Eypos"],
      Eypos_p = x$p[, "Eypos"],
      
      row.names = NULL
    )
  }))
  
  results$Comparison <- c(
    pooled = "Pooled",
    gap_2005 = "2005",
    gap_2011 = "2011",
    did = "Change (2011 - 2005)"
  )[results$Comparison]
  
  ft <- flextable(results) |>
    set_header_labels(
      Model = "Specification",
      Comparison = "Comparison",
      Ey_est = "Expected Minutes",
      Pr_est = "Participation (pp)",
      Eypos_est = "Minutes if Participating"
    )
  
  # Estimates and standard errors in the same cell
  ft <- compose(
    ft, j = "Ey_est",
    value = as_paragraph(
      as_chunk(sprintf("%.2f%s", results$Ey_est,
                       stars(results$Ey_p))),
      as_chunk(sprintf("\n(%.2f)", results$Ey_se))
    )
  )
  
  ft <- compose(
    ft, j = "Pr_est",
    value = as_paragraph(
      as_chunk(sprintf("%.2f%s", results$Pr_est,
                       stars(results$Pr_p))),
      as_chunk(sprintf("\n(%.2f)", results$Pr_se))
    )
  )
  
  ft <- compose(
    ft, j = "Eypos_est",
    value = as_paragraph(
      as_chunk(sprintf("%.2f%s", results$Eypos_est,
                       stars(results$Eypos_p))),
      as_chunk(sprintf("\n(%.2f)", results$Eypos_se))
    )
  )
  
  # Remove intermediate calculation columns
  ft <- delete_columns(
    ft,
    j = c(
      "Ey_se", "Ey_p",
      "Pr_se", "Pr_p",
      "Eypos_se", "Eypos_p"
    )
  )
  
  ft <- ft |>
    theme_booktabs() |>
    bold(part = "header") |>
    align(align = "center", part = "all") |>
    align(j = c("Model", "Comparison"),
          align = "left", part = "all") |>
    merge_v(j = "Model") |>
    valign(valign = "center", part = "all") |>
    autofit() |>
    set_table_properties(layout = "autofit", width = 1) |>
    fontsize(size = 9, part = "all") |>
    add_footer_lines(
      values = paste(
        "Standard errors in parentheses. * p < 0.10;",
        "** p < 0.05; *** p < 0.01.",
        "Expected Minutes = change in unconditional expected minutes;",
        "Participation (pp) = change in probability of positive activity,",
        "in percentage points; Minutes if Participating = change in",
        "expected minutes conditional on positive activity."
      )
    )
  
  if (!is.null(filename)) {
    save_as_docx(ft, path = filename)
  }
  
  return(ft)
}

### Run models ----
mdl_volunteer <- run_models("volunteer_mins")
mdl_civic     <- run_models("civic_mins")
mdl_sports    <- run_models("sports_mins")
mdl_social    <- run_models("social_mins")

### Marginal effects tables ----
ft_ame_volunteer <- format_ame_table(
  mdl_volunteer[c("Baseline", "Full Model")], "ame1-volunteer.docx"
)

ft_ame_civic <- format_ame_table(
  mdl_civic[c("Baseline", "Full Model")], "ame2-civic.docx"
)
# i don't plan to use these two. But they're here if I want them.
ft_ame_sports <- format_ame_table(
  mdl_sports[c("Baseline", "Full Model")], "ame3-sports.docx"
)

ft_ame_social <- format_ame_table(
  mdl_social[c("Baseline", "Full Model")], "ame4-social.docx"
)

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
    set_table_properties(layout = "autofit", width = 1)|>
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