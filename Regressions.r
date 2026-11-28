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

# Consistent publication-style formatting for every table.
format_table_style <- function(ft, left_cols = NULL) {
  ft <- ft |>
    theme_booktabs() |>
    bold(part = "header") |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all") |>
    valign(valign = "center", part = "all") |>
    autofit() |>
    set_table_properties(layout = "autofit", width = 1)
  
  if (!is.null(left_cols)) {
    available_cols <- ft$col_keys
    requested_cols <- intersect(left_cols, available_cols)
    
    # Fall back to the first column for modelsummary tables
    if (length(requested_cols) == 0 &&
        "term" %in% left_cols &&
        length(available_cols) > 0) {
      requested_cols <- available_cols[1]
    }
    
    if (length(requested_cols) > 0) {
      ft <- align(
        ft,
        j = requested_cols,
        align = "left",
        part = "all"
      )
    }
  }

  # Footnotes are consistently smaller and italicized.
  ft <- ft |>
    font(fontname = "Times New Roman", part = "footer") |>
    fontsize(size = 9, part = "footer") |>
    italic(part = "footer")

  ft
}

housing_data <-readRDS("data_0413_clean.rds")

# Dif ids by year
housing_data <- housing_data %>%
  mutate(strat_yr = interaction(year_cat, GEPSEUST, drop = TRUE),
         clust_yr = interaction(year_cat, GEPSEUCL, drop = TRUE))

# need to use this, i was getting insane results cause I wasn't.
# haven't decided if I'll use svyglm yet
svdes <- svydesign(
  ids = ~clust_yr, strata = ~strat_yr,
  weights = ~wt_pooled, nest = TRUE, data = housing_data
)
# Done to account for a few PSUs with 1 obs
options(survey.lonely.psu = "adjust")

# Models ----
# Regressions
# Using a suite to suggest original was unbiased

## Runs a suite of tobit models, with more controls in each.----
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
      weights = wt_pooled,
      dist = "gaussian",
      robust = TRUE,
      model = TRUE,
      cluster = clust_yr
    )
  })
}

# I accidentally deleted this at some point so claude rewrote
## Average marginal effects of homeownership ----
# Estimation sample and sampling weights from a fitted model
.ame_frame <- function(m) {
  mf <- m$model
  if (is.null(mf)) mf <- stats::model.frame(m)
  mf <- as.data.frame(mf)
  attr(mf, "terms") <- NULL
  w <- mf[["(weights)"]]
  if (is.null(w)) w <- m$prior.weights
  if (is.null(w)) w <- rep(1, nrow(mf))
  list(mf = mf, w = as.numeric(w))
}

.set_factor <- function(d, var, level) {
  d[[var]] <- factor(rep(level, nrow(d)), levels = levels(d[[var]]))
  d
}

# pred_fun(d) returns an n x k matrix of predicted outcomes for data d.
# Returns pooled and year-specific homeowner-renter gaps
.ame_table <- function(pred_fun, mf, w, year_sample = "own") {
  year_sample <- match.arg(year_sample, c("own", "all"))
  
  hh <- levels(mf$homeowner_cat)
  yy <- levels(mf$year_cat)
  
  gap <- function(d, wt) {
    M <- pred_fun(.set_factor(d, "homeowner_cat", hh[2])) -
      pred_fun(.set_factor(d, "homeowner_cat", hh[1]))
    
    colSums(M * wt) / sum(wt)
  }
  
  year_gap <- function(y) {
    if (year_sample == "own") {
      i <- mf$year_cat == y
      gap(mf[i, , drop = FALSE], w[i])
    } else {
      gap(.set_factor(mf, "year_cat", y), w)
    }
  }
  
  # Pooled estimate
  pooled <- gap(mf, w)
  
  # Individual year estimates
  annual <- lapply(yy, year_gap)
  names(annual) <- paste0("year_", yy)
  
  out <- do.call(rbind, c(
    list(pooled = pooled),
    annual
  ))
  
  out
}

# Tobit (survreg, left-censored at 0): Ey, Pr(y>0), E[y | y>0]
effects_ame <- function(m, year_sample = "own", weighted = TRUE) {
  fr <- .ame_frame(m)
  w  <- if (weighted) fr$w else rep(1, nrow(fr$mf))
  b  <- m$coefficients; b[is.na(b)] <- 0
  sigma <- m$scale
  Terms <- delete.response(terms(m))
  
  pred <- function(d) {
    X  <- model.matrix(Terms, d, xlev = m$xlevels,
                       contrasts.arg = m$contrasts)
    mu <- drop(X[, names(b), drop = FALSE] %*% b)
    z  <- mu / sigma
    Pr <- pnorm(z)
    lam <- exp(dnorm(z, log = TRUE) - pnorm(z, log.p = TRUE))  # inverse Mills
    cbind(Ey    = Pr * mu + sigma * dnorm(z),
          Pr    = Pr,
          Eypos = mu + sigma * lam)
  }
  .ame_table(pred, fr$mf, w, year_sample)
}

# Probit (svyglm quasibinomial, probit link): Pr in percentage points
effects_ame_probit <- function(m, year_sample = "own", weighted = TRUE) {
  fr <- .ame_frame(m)
  w  <- if (weighted) fr$w else rep(1, nrow(fr$mf))
  b  <- m$coefficients; b[is.na(b)] <- 0
  Terms <- delete.response(terms(m))
  
  pred <- function(d) {
    X <- model.matrix(Terms, d, xlev = m$xlevels,
                      contrasts.arg = m$contrasts)
    cbind(Pr = 100 * pnorm(drop(X[, names(b), drop = FALSE] %*% b)))
  }
  .ame_table(pred, fr$mf, w, year_sample)
}

# Avg marginal effects. Currently only for homeowner:year interaction.
# Avg marginal effects. Currently only for homeowner:year interaction.
ame_inference <- function(m, ame_fun = effects_ame) {
  
  ame_mat <- function(model) {
    r <- ame_fun(model)
    
    if (is.null(dim(r))) {
      r <- matrix(
        as.numeric(r),
        ncol = 1,
        dimnames = list(names(r), "Pr")
      )
    }
    
    as.matrix(r)
  }
  
  V <- vcov(m)
  b <- coef(m)
  
  # Check whether covariance matrix includes log(scale)
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
  
  pars <- b
  
  if (include_scale) {
    pars <- c(pars, setNames(log(m$scale), scale_name))
  }
  
  V <- V[names(pars), names(pars), drop = FALSE]
  
  calculate <- function(p) {
    mm <- m
    mm$coefficients <- p[names(b)]
    
    if (include_scale) {
      mm$scale <- exp(p[scale_name])
    }
    
    as.vector(t(ame_mat(mm)))
  }
  
  original <- ame_mat(m)
  estimates <- calculate(pars)
  
  # Numerical gradient
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
  
  # Identify each AME in the covariance matrix
  ame_names <- unlist(
    lapply(
      rownames(original),
      function(r) paste(r, colnames(original), sep = "::")
    )
  )
  
  rownames(G) <- ame_names
  colnames(G) <- names(pars)
  
  # Full covariance matrix of AMEs
  ame_var <- G %*% V %*% t(G)
  dimnames(ame_var) <- list(ame_names, ame_names)
  
  ame_se <- sqrt(pmax(diag(ame_var), 0))
  
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
    p = p,
    vcov = ame_var
  )
}

## Format marginal effects as flextable
format_ame_table <- function(
    models,
    filename = NULL,
    ame_fun  = effects_ame,
    measures = c("Ey", "Pr", "Eypos"),
    pr_scale = 100
) {
  
  stars <- function(p) {
    ifelse(p < 0.05, "*", "")
  }
  
  labels <- c(
    Ey    = "Expected Minutes",
    Pr    = "Participation (pp)",
    Eypos = "Minutes if Participating"
  )
  
  definitions <- c(
    Ey    = "Expected Minutes = change in unconditional expected minutes",
    Pr    = paste("Participation (pp) = change in probability of positive",
                  "activity, in percentage points"),
    Eypos = paste("Minutes if Participating = change in expected minutes",
                  "conditional on positive activity")
  )
  
  results <- do.call(rbind, lapply(names(models), function(model_name) {
    
    x <- ame_inference(models[[model_name]], ame_fun = ame_fun)
    
    out <- data.frame(
      Model = model_name,
      Comparison = rownames(x$estimate),
      row.names = NULL
    )
    
    for (m in measures) {
      k <- if (m == "Pr") pr_scale else 1   # proportion -> percentage points
      out[[paste0(m, "_est")]] <- x$estimate[, m] * k
      out[[paste0(m, "_se")]]  <- x$se[, m] * k
      out[[paste0(m, "_p")]]   <- x$p[, m]
    }
    
    out
  }))
  
  results$Comparison <- ifelse(
    tolower(results$Comparison) == "pooled",
    "Pooled",
    sub("^year_", "", tolower(results$Comparison))
  )
  
  ft <- flextable(results) |>
    set_header_labels(
      values = c(
        list(Model = "Specification", Comparison = "Comparison"),
        setNames(as.list(labels[measures]), paste0(measures, "_est"))
      )
    )
  
  # Estimates and standard errors in the same cell
  for (m in measures) {
    est_txt <- sprintf("%.2f%s", results[[paste0(m, "_est")]],
                       stars(results[[paste0(m, "_p")]]))
    se_txt  <- sprintf("\n(%.2f)", results[[paste0(m, "_se")]])
    
    ft <- compose(
      ft, j = paste0(m, "_est"),
      value = as_paragraph(as_chunk(est_txt), as_chunk(se_txt))
    )
  }
  
  # Remove intermediate calculation columns
  ft <- delete_columns(
    ft,
    j = c(paste0(measures, "_se"), paste0(measures, "_p"))
  )
  
  ft <- ft |>
    merge_v(j = "Model") |>
    add_footer_lines(
      values = paste0(
        "Standard errors in parentheses. * p < 0.05. ",
        paste(definitions[measures], collapse = "; "), "."
      )
    ) |>
    format_table_style(left_cols = c("Model", "Comparison"))
  
  if (!is.null(filename)) {
    save_as_docx(ft, path = filename)
  }
  
  invisible(ft)
}

## Probit/Intensity given participation ----
# Had Claude write this.
run_alt_models <- function(
    outcome, subset = NULL, intensity = FALSE, survey_design = svdes) {
  
  des <- survey_design
  
  # Subset the design itself (not the data) so the variance estimator
  # keeps the full strata/cluster structure
  subset_expr <- enquo(subset)
  if (!rlang::quo_is_null(subset_expr)) {
    keep <- rlang::eval_tidy(subset_expr, data = des$variables)
    des  <- des[which(keep), ]
  }
  
  if (intensity) {
    y   <- des$variables[[outcome]]
    des <- des[which(!is.na(y) & y > 0), ]
    lhs    <- outcome
    family <- gaussian()
  } else {
    lhs    <- paste0("I(", outcome, " > 0)")
    family <- quasibinomial(link = "probit")  # same estimates as probit,
    # no non-integer-weight warnings
  }
  
  # Drop factor levels emptied by subsetting (avoids NA coefficients)
  des$variables <- droplevels(des$variables)
  
  # Model pieces (same as run_models)
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
  
  lapply(rhs, function(r) {
    survey::svyglm(
      as.formula(paste(lhs, "~", r)),
      design = des,
      family = family
    )
  })
}

### Run models ----
mdl_volunteer <- run_models("volunteer_mins")
mdl_civic     <- run_models("civic_mins")
mdl_sports    <- run_models("sports_mins")
mdl_social    <- run_models("social_mins")

#### Robustness check models ----
mdl_vol_particip <- run_alt_models("volunteer_mins")
mdl_vol_intens <- run_alt_models("volunteer_mins", intensity = TRUE)

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
# Coefficient map
years <- levels(housing_data$year_cat)
reference_year <- years[1]

coef_names <- c(
  "(Intercept)" = "Constant",
  "homeowner_catHomeowner" = paste("Homeowner:", years[1])
)

# Include homeowner-year interactions, but omit year fixed effects
for (y in years[-1]) {
  coef_names[
    paste0("homeowner_catHomeowner:year_cat", y)
  ] <- paste("Homeowner ×", y)
}

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
                             model_names = names(models)) {
  
  spec_names <- c("Baseline", "Demographic Controls",
                  "Socioeconomic Controls", "Full Model")
  idx <- match(names(models), spec_names)
  
  pos <- attr(rows_controls, "position")
  rows_controls <- rows_controls[, c(1, idx + 1), drop = FALSE]
  names(rows_controls)[-1] <- as.character(seq_along(idx))
  attr(rows_controls, "position") <- pos
  
  ft <- modelsummary(
    models,
    coef_map = coef_mapping,
    model_names = model_names,
    stars = c("*" = 0.05),
    add_rows = rows_controls,
    gof_omit = "AIC|BIC|Log.Lik",
    output = "flextable"
  )
  
  ft <- ft |>
    add_footer_lines(
      values = paste0(
        "Reference year: ", reference_year, ". ",
        "Homeowner × year coefficients represent changes ",
        "in the homeowner-renter difference relative to ",
        "the reference year. ",
        "Standard errors in parentheses. * p < 0.05."
      )
    ) |>
    format_table_style(left_cols = "term")
  
  save_as_docx(ft, path = filename)
  
  return(ft)
}

# Compact main-results table ----
format_select_table <- function(
    models,
    filename = NULL,
    coef_mapping = coef_names,
    selected_years = c("2005", "2008", "2011")
) {
  
  models <- models[c("Baseline", "Full Model")]
  
  # Reference-year coefficient plus selected year interactions
  selected_terms <- c(
    "homeowner_catHomeowner",
    paste0("homeowner_catHomeowner:year_cat", selected_years)
  )
  
  missing_terms <- setdiff(selected_terms, names(coef_mapping))
  if (length(missing_terms) > 0) {
    stop("Missing coefficient mappings: ",
         paste(missing_terms, collapse = ", "))
  }
  
  selected_mapping <- coef_mapping[selected_terms]
  
  # Control rows
  rows_controls <- data.frame(
    term = c(
      "Year Interactions",
      "Demographic Controls",
      "Socioeconomic Controls",
      "Household Controls"
    ),
    `Baseline` = c("Yes", "", "", ""),
    `Full Model` = c("Yes", "Yes", "Yes", "Yes"),
    check.names = FALSE
  )
  
  attr(rows_controls, "position") <- "coef_end"
  
  ft <- modelsummary(
    models,
    coef_map = selected_mapping,
    model_names = c("Baseline", "Full Model"),
    stars = c("*" = 0.05),
    add_rows = rows_controls,
    gof_omit = "AIC|BIC|Log.Lik",
    output = "flextable"
  )
  
  ft <- ft |>
    add_footer_lines(
      values = paste0(
        "Reference year: ", reference_year, ". ",
        "Homeowner × year coefficients represent changes ",
        "in the homeowner-renter difference relative to ",
        "the reference year. ",
        "All specifications include year fixed effects and ",
        "homeowner-year interactions. ",
        "Robust standard errors clustered by PSU within year ",
        "in parentheses. * p < 0.05."
      )
    ) |>
    format_table_style(left_cols = "term")
  
  if (!is.null(filename)) {
    save_as_docx(ft, path = filename)
  }
  
  invisible(ft)
}


format_select_ame_table <- function(
    models,
    filename = NULL,
    selected_years = c("2005", "2008", "2011")
) {
  
  models <- models[c("Baseline", "Full Model")]
  
  # Calculate estimates and covariance matrices
  inference <- lapply(models, ame_inference)
  
  measures <- c("Ey", "Pr", "Eypos")
  
  measure_labels <- c(
    Ey = "Unconditional minutes",
    Pr = "Participation (pp)",
    Eypos = "Minutes if participating"
  )
  
  # Format estimates with significance stars
  format_est <- function(est, se, p, measure) {
    
    if (measure == "Pr") {
      est <- est * 100
      se <- se * 100
    }
    
    stars <- ifelse(p < 0.05, "*", "")
    
    sprintf("%.2f%s\n(%.2f)", est, stars, se)
  }
  
  # Calculate one comparison for one model and measure
  get_comparison <- function(x, comparison, measure) {
    
    if (comparison$type == "pooled") {
      
      key <- "pooled"
      
      est <- x$estimate[key, measure]
      se <- x$se[key, measure]
      p <- x$p[key, measure]
      
    } else if (comparison$type == "year") {
      
      key <- paste0("year_", comparison$year)
      
      est <- x$estimate[key, measure]
      se <- x$se[key, measure]
      p <- x$p[key, measure]
      
    } else {
      
      key_from <- paste0("year_", comparison$from, "::", measure)
      key_to   <- paste0("year_", comparison$to, "::", measure)
      
      L <- setNames(rep(0, nrow(x$vcov)), rownames(x$vcov))
      L[key_from] <- -1
      L[key_to] <- 1
      
      est <- x$estimate[
        paste0("year_", comparison$to), measure
      ] - x$estimate[
        paste0("year_", comparison$from), measure
      ]
      
      variance <- drop(t(L) %*% x$vcov %*% L)
      se <- sqrt(max(variance, 0))
      p <- 2 * pnorm(-abs(est / se))
    }
    
    format_est(est, se, p, measure)
  }
  
  # Pooled sample and annual comparisons
  comparisons <- list(
    "Pooled sample" = list(type = "pooled")
  )
  
  comparisons <- c(
    comparisons,
    setNames(
      lapply(selected_years, function(y) {
        list(type = "year", year = y)
      }),
      paste0("Year ", selected_years)
    )
  )
  
  # Add only the overall change in unconditional minutes
  comparisons[["Change: 2005–2011"]] <- list(
    type = "difference",
    from = "2005",
    to = "2011"
  )
  
  # Build table
  results <- do.call(
    rbind,
    lapply(names(comparisons), function(comparison_name) {
      
      comparison <- comparisons[[comparison_name]]
      
      current_measures <- if (comparison$type == "difference") {
        "Ey"
      } else {
        measures
      }
      
      do.call(
        rbind,
        lapply(current_measures, function(measure) {
          
          data.frame(
            Period = comparison_name,
            Outcome = measure_labels[[measure]],
            Baseline = get_comparison(
              inference[["Baseline"]], comparison, measure
            ),
            `Full Model` = get_comparison(
              inference[["Full Model"]], comparison, measure
            ),
            check.names = FALSE
          )
        })
      )
    })
  )
  
  rownames(results) <- NULL
  
  ft <- flextable(results) |>
    set_header_labels(
      Period = "Period",
      Outcome = "Outcome",
      Baseline = "Baseline",
      `Full Model` = "Full Model"
    ) |>
    add_footer_lines(
      values = c(
        "* p < 0.05.",
        paste0(
          "Average marginal effects on the homeowner-renter difference. ",
          "Participation estimates expressed in percentage points. ",
          "Standard errors in parentheses (delta method)."
        )
      )
    ) |>
    format_table_style(
      left_cols = c("Period", "Outcome")
    )
  
  if (!is.null(filename)) {
    save_as_docx(ft, path = filename)
  }
  
  invisible(ft)
}

# Plotting AME trendline

# Plotting AME trendline
plot_ame_trend <- function(
    m,
    filename = NULL,
    measure = "Ey"
) {
  
  x <- ame_inference(m)
  
  # Extract annual estimates only (exclude pooled estimate)
  annual_rows <- grep(
    "^year_",
    rownames(x$estimate),
    value = TRUE
  )
  
  years <- sub("^year_", "", annual_rows)
  keys <- paste0(annual_rows, "::", measure)
  
  estimates <- x$estimate[annual_rows, measure]
  variances <- diag(x$vcov)[keys]
  ses <- sqrt(pmax(variances, 0))
  
  plot_data <- data.frame(
    year = as.numeric(years),
    estimate = as.numeric(estimates),
    se = as.numeric(ses)
  ) %>%
    mutate(
      lower = estimate - 1.96 * se,
      upper = estimate + 1.96 * se
    )
  
  p <- ggplot(
    plot_data,
    aes(x = year, y = estimate)
  ) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      alpha = 0.20
    ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray40"
    ) +
    scale_x_continuous(
      breaks = plot_data$year
    ) +
    labs(
      x = "Year",
      y = "Homeowner-renter difference in expected minutes",
      caption = "Shaded region represents 95% confidence intervals."
    ) +
    theme_minimal(
      base_size = 12,
      base_family = "Times New Roman"
    ) +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )
  
  if (!is.null(filename)) {
    ggsave(
      filename,
      plot = p,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
  
  return(p)
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

# Selected Tobit table
ft_vol_select <- format_select_table(
  models = mdl_volunteer,
  filename = "reg1-vol_select.docx"
)

# Selected AME table, including year-to-year differences
ft_ame_vol_select <- format_select_ame_table(
  models = mdl_volunteer,
  filename = "ame1-volunteer-select.docx"
)

# Full annual AME graph (Full Model)
p_ame_vol <- plot_ame_trend(
  m = mdl_volunteer[["Full Model"]],
  filename = "ame1-volunteer-trend.png"
)

names(ft_list) <- names(model_tables)

# formatting alt tables. claude. Add participation/intensity label manually
format_alt_table <- function(particip, intens, filename = NULL,
                             coef_mapping = coef_names) {
  
  # Trailing spaces keep the names unique, since modelsummary needs
  # unique names. They display identically.
  models <- list(
    "Baseline"    = particip[["Baseline"]],
    "Full Model"  = particip[["Full Model"]],
    "Baseline "   = intens[["Baseline"]],
    "Full Model " = intens[["Full Model"]]
  )
  
  # Control rows: term + one column per model
  rows_controls <- data.frame(
    term = c("Demographic Controls", "Socioeconomic Controls",
             "Household Controls"),
    a = c("", "", ""),
    b = c("Yes", "Yes", "Yes"),
    c = c("", "", ""),
    d = c("Yes", "Yes", "Yes")
  )
  names(rows_controls)[2:5] <- names(models)
  attr(rows_controls, "position") <- "coef_end"
  
  ft <- modelsummary(
    models,
    coef_map = coef_mapping,
    stars = c("*" = 0.05),
    add_rows = rows_controls,
    gof_omit = "AIC|BIC|Log.Lik",
    output = "flextable"
  ) |>
    add_footer_lines(
      paste(
        "Design-based robust standard errors in parentheses.",
        "Participation (columns 1-2): probit for Pr(minutes > 0).",
        "Intensity (columns 3-4): linear model of minutes among participants.",
        "* p < 0.05."
      )
    ) |>
    format_table_style(left_cols = "term")
  
  if (!is.null(filename)) save_as_docx(ft, path = filename)
  
  ft
}
# files for alt model tables ----
ft_vol_alt <- format_alt_table(
  mdl_vol_particip, mdl_vol_intens, "reg5-vol_alt_table.docx"
)
# Probit AME
ft_ame_probit <- format_ame_table(
  mdl_vol_particip[c("Baseline", "Full Model")],
  filename = "ame5_vol_probit.docx",
  ame_fun  = effects_ame_probit,
  measures = "Pr",
  pr_scale = 1
)