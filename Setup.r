# Written By Jordan Stier with (a lot of) assistance from chatgpt & claude.
# ATUS multiyear (2004-2019) data prep

# Packages ----
# Note: I plan to remove unnescessary ones. At some point.
pkgs <- c("data.table", "haven", "readxl", "dplyr", "tidyr", "ggplot2",
          "lmtest", "marginaleffects", "survey", "pandoc","modelsummary", "knitr",
          "AER", "censReg", "tibble", "flextable", "rlang")
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs)) install.packages(missing_pkgs)

library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(marginaleffects)
library(survey)
library(tibble)

# Paths: ADJUST to match your folder/file names ----
# Takes 0325, but due to some things like covid (+higher incomes capped lower
# in 2003), only 0413 is used 
data_dir  <- "../Dataset"
cps_path  <- file.path(data_dir, "atuscps_0325.dat")
resp_path <- file.path(data_dir, "atusresp_0325.dat")
sum_path  <- file.path(data_dir, "atussum_0325.dat")

# Reader ----
# The multiyear files are big, so only the needed columns are read.
# Header case differs between files (e.g. atussum uses lowercase 'tucaseid'
# and t-codes), so matching is case-insensitive and all names are uppercased.
# TUCASEID is a 14-digit ID, so it is read as character (safe for joins).
read_atus <- function(path, keep = character(), keep_regex = NULL) {
  hdr <- names(data.table::fread(path, nrows = 0))
  hit <- toupper(hdr) %in% toupper(keep)
  if (!is.null(keep_regex)) hit <- hit | grepl(keep_regex, hdr, ignore.case = TRUE)
  dt <- data.table::fread(path, select = hdr[hit], integer64 = "character",
                          data.table = FALSE)
  names(dt) <- toupper(names(dt))
  dt
}

# CPS file ----
cps <- read_atus(cps_path, keep = c(
  "TUCASEID", "TULINENO",
  "HEFAMINC", "HUFAMINC",   # family income; name differs across years
  "HETENURE",
  "PESEX", "PRTAGE", "PEEDUCA", "PEMARITL", "PRCITSHP", "PTDTRACE", "PEHRUSLT",
  "GEPSEUST", "GEPSEUCL", "GESTFIPS", "GTMETSTA"
))

need_cps <- c("TUCASEID", "TULINENO", "HETENURE", "PESEX", "PRTAGE", "PEEDUCA",
              "PEMARITL", "PRCITSHP", "PTDTRACE", "PEHRUSLT", "GESTFIPS",
              "GTMETSTA")
stopifnot(all(need_cps %in% names(cps)))

# Income: use whichever of HEFAMINC / HUFAMINC exists, turning negative
# (missing) codes into NA first so coalesce can fall through to the other one.
inc_cols <- intersect(c("HEFAMINC", "HUFAMINC"), names(cps))
stopifnot(length(inc_cols) > 0)
cps[inc_cols] <- lapply(cps[inc_cols], function(x) ifelse(x < 0, NA, x))
cps$income <- do.call(dplyr::coalesce, unname(as.list(cps[inc_cols])))

cps_trimmed <- cps %>%
  select(
    TUCASEID, TULINENO, income,
    HETENURE, PESEX, PRTAGE, PEEDUCA, PEMARITL, PRCITSHP, PTDTRACE, PEHRUSLT,
    any_of(c("GEPSEUST", "GEPSEUCL")),   # pseudo stratum / cluster, if present
    GESTFIPS, GTMETSTA
  ) %>%
  rename(
    homeowner = HETENURE, sex = PESEX, age = PRTAGE, education = PEEDUCA,
    married = PEMARITL, citizen = PRCITSHP, race = PTDTRACE,
    workhours = PEHRUSLT
  )
rm(cps)

# Respondent file ----
resp <- read_atus(resp_path, keep = c(
  "TUCASEID", "TULINENO", "TUYEAR", "TUFNWGTP",
  "TESCHENR", "TELFS", "TRDPFTPT", "TRSPPRES", "TRCHILDNUM", "TRTALONE"
))
# Fallback if TUYEAR is not in the file: first 4 digits of the case ID.
if (!"TUYEAR" %in% names(resp)) {
  resp$TUYEAR <- as.integer(substr(resp$TUCASEID, 1, 4))
}

resp_trimmed <- resp %>%
  rename(
    year = TUYEAR,
    inschool = TESCHENR, employed = TELFS, fulltime = TRDPFTPT,
    spouse_pres = TRSPPRES, children = TRCHILDNUM, timealone = TRTALONE
  )
rm(resp)

# Year window ----
# The source files cover 2003-2025; keep only the years used in the analysis.
# Filtering here as respondent file carries it and then restricting
# the other two tables to these respondents keeps everything consistent.
year_min <- 2004
year_max <- 2013
resp_trimmed <- resp_trimmed %>% filter(year >= year_min, year <= year_max)

# Activity summary file ----
# Only the activity columns that feed the outcome/control sums are read.
# Civic: the lexicon changed in 2008. "Waiting assoc. w/ civic obligations"
# is merged into 100383. 100299 is included for all years
# (the old 2011 block left it out).
act <- read_atus(
  sum_path, keep = "TUCASEID",
  keep_regex = "^t(1201|1202|1301|14|15|100201|100299|100383)"
)
act_trimmed <- act %>%
  mutate(
    volunteer_mins = rowSums(across(starts_with("T15")), na.rm = TRUE),
    social_mins    = rowSums(across(starts_with(c("T1201", "T1202"))), na.rm = TRUE),
    sports_mins    = rowSums(across(starts_with("T1301")), na.rm = TRUE),
    civic_mins     = rowSums(across(starts_with("T10")), na.rm = TRUE),
    # This is for controls
    relig_mins     = rowSums(across(starts_with("T14")), na.rm = TRUE)
  ) %>%
  select(TUCASEID, volunteer_mins, social_mins, sports_mins, civic_mins,
         relig_mins)
rm(act)

# Key checks before joining (a duplicated key would silently multiply rows)
stopifnot(
  !anyDuplicated(act_trimmed$TUCASEID),
  !anyDuplicated(resp_trimmed$TUCASEID),
  !anyDuplicated(cps_trimmed[c("TUCASEID", "TULINENO")])
)

# Joining ----
data_0413 <- act_trimmed %>%
  left_join(resp_trimmed, by = "TUCASEID") %>%
  left_join(cps_trimmed, by = c("TUCASEID", "TULINENO")) %>%
  relocate(TUCASEID, TULINENO, year, TUFNWGTP, homeowner, volunteer_mins) %>%
  mutate(
    # TUFNWGTP is scaled to a single year's population. When pooling years,
    # divide by the number of years pooled for population-level estimates.
    # (Recompute if you later subset to fewer years.)
    wt_pooled = TUFNWGTP / n_distinct(year),
    post2008  = as.integer(year >= 2008)
  )

stopifnot(nrow(data_0413) == nrow(act_trimmed))
print(table(data_0413$year, useNA = "ifany"))

# Keep owners/renters only. HETENURE: 1 = owned, 2 = rented,
# 3 = occupied without payment of rent; negatives are missing codes.
data_0413 <- data_0413 %>%
  filter(homeowner %in% 1:2) %>%
  mutate(owner = as.integer(homeowner == 1))

# Further refining, recoding categoricals ----
data_0413 <- data_0413 %>%
  mutate(
    # EDUCATION: Highest educational attainment.
    education_cat = case_when(
      education %in% 31:38 ~ "Less than high school",
      education == 39 ~ "High school graduate",
      education == 40 ~ "Some college, no degree",
      education %in% c(41, 42) ~ "Associate degree",
      education == 43 ~ "Bachelor's degree",
      education %in% 44:46 ~ "Master's degree or higher",
      TRUE ~ NA_character_
    ),
    education_cat = factor(
      education_cat,
      levels = c(
        "Less than high school",
        "High school graduate",
        "Some college, no degree",
        "Associate degree",
        "Bachelor's degree",
        "Master's degree or higher"
      ),
      ordered = FALSE
    ),
    
    # RACE: Preserve White and Black; combine everything else.
    # 1 = White only; 2 = Black only; 3 and up = other single-race and all
    # multiracial combinations. The number of multiracial codes grew over
    # 2003-2019, so this is open-ended.
    race_cat = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      race >= 3 ~ "Other / Multiracial",
      TRUE ~ NA_character_
    ),
    race_cat = factor(race_cat,
                      levels = c("White", "Black", "Other / Multiracial")),
    
    # SEX: 1 = male, 2 = female.
    sex_cat = factor(
      if_else(sex %in% 1:2, sex, NA_real_),
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    # MARITAL STATUS: Combine widowed, divorced, and separated.
    married_cat = case_when(
      married %in% 1:2 ~ "Married",
      married %in% c(3, 4, 5) ~ "Previously married",
      married == 6 ~ "Never married",
      TRUE ~ NA_character_
    ),
    married_cat = factor(married_cat),
    
    # CITIZENSHIP (PRCITSHP): 1-3 = native-born (US, PR/outlying area, or
    # born abroad to US parents), 4 = naturalized, 5 = not a citizen.
    # There was a bug in draft 2's code.
    citizen_cat = case_when(
      citizen %in% 1:3 ~ "Native-born citizen",
      citizen == 4     ~ "Naturalized citizen",
      citizen == 5     ~ "Not a citizen",
      TRUE             ~ NA_character_
    ),
    citizen_cat = factor(
      citizen_cat,
      levels = c("Native-born citizen", "Naturalized citizen", "Not a citizen")
    ),
    
    # INCOME: Brackets aren't equal size, recoding.
    income_cat = case_when(
      income %in% 1:5   ~ "Under $15,000",
      income %in% 6:9   ~ "$15,000-$34,999",
      income %in% 10:12 ~ "$35,000-$59,999",
      income %in% 13:14 ~ "$60,000-$99,999",
      income %in% 15:16 ~ "$100,000+",
      TRUE ~ NA_character_
    ),
    income_cat = factor(
      income_cat,
      levels = c(
        "Under $15,000",
        "$15,000-$34,999",
        "$35,000-$59,999",
        "$60,000-$99,999",
        "$100,000+"
      ),
      ordered = FALSE
    ),
    
    # HOMEOWNERSHIP: filtered to codes 1:2 above.
    homeowner_cat = factor(
      if_else(homeowner %in% 1:2, homeowner, NA_real_),
      levels = c(2, 1),
      labels = c("Renter", "Homeowner")
    ),
    
    # LABOR FORCE STATUS.
    # 1-2 = Employed
    # 3-4 = Unemployed (including temporary layoff)
    # 5 = Not in labor force
    employed_cat = case_when(
      employed %in% 1:2 ~ "Employed",
      employed %in% 3:4 ~ "Unemployed",
      employed == 5     ~ "Not in labor force",
      TRUE              ~ NA_character_
    ),
    employed_cat = factor(
      employed_cat,
      levels = c("Employed", "Unemployed", "Not in labor force")
    ),
    
    workhours_cat = case_when(
      workhours == -4 ~ "Hours vary",
      workhours >= 0 & workhours <= 20 ~ "0-20 hours",
      workhours > 20 & workhours <= 35 ~ "21-35 hours",
      workhours > 35 & workhours <= 45 ~ "36-45 hours",
      workhours > 45 ~ "46+ hours",
      TRUE ~ NA_character_
    ),
    workhours_cat = factor(
      workhours_cat,
      levels = c(
        "0-20 hours",
        "21-35 hours",
        "36-45 hours",
        "46+ hours",
        "Hours vary"
      )
    ),
    
    # FULL-TIME / PART-TIME STATUS.
    fulltime_cat = factor(
      if_else(fulltime %in% 1:2, fulltime, NA_real_),
      levels = 1:2,
      labels = c("Full-time", "Part-time")
    ),
    
    # SCHOOL ENROLLMENT.
    inschool_cat = factor(
      if_else(inschool %in% 1:2, inschool, NA_real_),
      levels = c(1, 2),
      labels = c("Enrolled", "Not enrolled")
    ),
    
    # SPOUSE/PARTNER PRESENT.
    spouse_pres_cat = factor(
      if_else(spouse_pres %in% 1:3, spouse_pres, NA_real_),
      levels = c(1, 2, 3),
      labels = c(
        "Spouse present",
        "Unmarried partner present",
        "Neither present"
      )
    ),
    
    metro_cat = factor(GTMETSTA),
    
    # Religious activity indicator (note: religious education, 140105, was
    # added to the lexicon in 2007; before that it was coded elsewhere).
    relig_cat = as.integer(relig_mins > 0),
    relig_cat = factor(
      if_else(relig_cat %in% 0:1, relig_cat, NA_real_),
      levels = c(0, 1),
      labels = c("Not Religious", "Religious")
    ),
    
    # YEAR AND POST-PERIOD INDICATORS.
    year_cat = factor(year),
    post2008_cat = factor(post2008, levels = c(0, 1)),
    owner_cat = factor(owner, levels = c(0, 1))
  )

# Filtering out original form of recoded vars
# (GESTFIPS is kept so a state-by-year merge is possible)
data_0413_clean <- data_0413 %>%
  select(
    -inschool, -employed, -fulltime, -spouse_pres, -income, -sex, -education,
    -married, -citizen, -homeowner)

saveRDS(data_0413_clean, "data_0413_clean.rds")