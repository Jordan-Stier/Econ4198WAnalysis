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
      weights = TUFINLWGT,
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
# Returns pooled, 2005, 2011 homeowner-renter gaps and their difference.
.ame_table <- function(pred_fun, mf, w, year_sample = c("own", "all")) {
  year_sample <- match.arg(year_sample)
  hh <- levels(mf$homeowner_cat)   # c("Renter", "Homeowner")
  yy <- levels(mf$year_cat)        # c("2005", "2011")
  
  gap <- function(d, wt) {
    M <- pred_fun(.set_factor(d, "homeowner_cat", hh[2])) -
      pred_fun(.set_factor(d, "homeowner_cat", hh[1]))
    colSums(M * wt) / sum(wt)
  }
  year_gap <- function(y) {
    if (year_sample == "own") {      # average over that year's respondents
      i <- mf$year_cat == y
      gap(mf[i, , drop = FALSE], w[i])
    } else {                         # set year for everyone
      gap(.set_factor(mf, "year_cat", y), w)
    }
  }
  
  out <- rbind(pooled   = gap(mf, w),
               gap_2005 = year_gap(yy[1]),
               gap_2011 = year_gap(yy[2]))
  rbind(out, did = out["gap_2011", ] - out["gap_2005", ])
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
  
  # Coerce AME output to a matrix; a plain named vector becomes a "Pr" column
  ame_mat <- function(model) {
    r <- ame_fun(model)
    if (is.null(dim(r))) {
      r <- matrix(as.numeric(r), ncol = 1,
                  dimnames = list(names(r), "Pr"))
    }
    as.matrix(r)
  }
  
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
    
    as.vector(t(ame_mat(mm)))
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
  original <- ame_mat(m)
  
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

## Format marginal effects as flextable
format_ame_table <- function(
    models,
    filename = NULL,
    ame_fun  = effects_ame,
    measures = c("Ey", "Pr", "Eypos"),
    pr_scale = 100
) {
  
  stars <- function(p) {
    ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**",
                  ifelse(p < 0.10, "*", "")))
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
  
  results$Comparison <- c(
    pooled   = "Pooled",
    gap_2005 = "2005",
    gap_2011 = "2011",
    did      = "Change (2011 - 2005)"
  )[tolower(results$Comparison)]
  
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
      values = paste0(
        "Standard errors in parentheses. * p < 0.10; ",
        "** p < 0.05; *** p < 0.01. ",
        paste(definitions[measures], collapse = "; "), "."
      )
    )
  
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
    stars = TRUE,
    add_rows = rows_controls,
    gof_omit = "AIC|BIC|Log.Lik",
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
    stars = TRUE,
    add_rows = rows_controls,
    gof_omit = "AIC|BIC|Log.Lik",
    output = "flextable"
  ) |>
    add_footer_lines(
      paste(
        "Design-based robust standard errors in parentheses.",
        "Participation (columns 1-2): probit for Pr(minutes > 0).",
        "Intensity (columns 3-4): linear model of minutes among participants."
      )
    ) |>
    theme_booktabs() |>
    autofit() |>
    set_table_properties(layout = "autofit", width = 1) |>
    fontsize(size = 9, part = "all") |>
    align(align = "center", part = "all")
  
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