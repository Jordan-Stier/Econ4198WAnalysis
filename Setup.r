#install.packages("haven")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("lmtest")
#install.packages("marginaleffects")
#install.packages("survey")
#install.packages("modelsummary")
#install.packages("knitr")
#install.packages("modelsummary")
#install.packages("pandoc")
#install.packages("AER")
#install.packages("censReg")
library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(marginaleffects)
library(survey)
library(tibble)

# ATUS 2011----
# Reading in tables
cps_11<-read.table("../Dataset/atuscps_2011/atuscps_2011.dat",
                header=TRUE, sep=',')
respondent_11<-read.table("../Dataset/atusresp_2011/atusresp_2011.dat",
                       header = TRUE, sep = ',')
activity_11<-read.table("../Dataset/atussum_2011/atussum_2011.dat",
                     header = TRUE, sep = ',')

# Trimming.
cps_11_trimmed<-cps_11%>%select(
  TUCASEID,      # ATUS case/respondent ID
  TULINENO,      # Household member line number
  
  HEFAMINC,      # Family/household income
  HETENURE,      # Housing tenure: owned, rented, etc.
  
  PESEX,         # Sex
  PRTAGE,        # Age
  PEEDUCA,       # Educational attainment
  PEMARITL,      # Marital status
  PRCITSHP,      # Citizenship status
  PTDTRACE,      # Race
  PEHRUSLT,      # Usual hours worked per week
  
  GEPSEUST,      # Pseudo collapsed sampling stratum
  GEPSEUCL,      # Pseudo PSU/cluster
  GESTFIPS,       # State
  GTMETSTA       # Metropolitan area status
  ) %>% rename(
    sex=PESEX, homeowner=HETENURE, income=HEFAMINC,
    education=PEEDUCA, married=PEMARITL,
    citizen=PRCITSHP, race=PTDTRACE,
    age=PRTAGE, workhours=PEHRUSLT
  )

respondent_11_trimmed<-respondent_11%>%select(
  TUCASEID,      # ATUS case ID
  TULINENO,      # Household member line number
  TUFINLWGT,     # ATUS final survey weight
  
  TESCHENR,      # School enrollment status
  TELFS,         # Labor force status
  
  TRDPFTPT,      # Full-time/part-time work status
  TRSPPRES,      # Spouse/partner present
  TRCHILDNUM,    # Number of children
  TRTALONE       # Time spent alone (minutes)
  ) %>% rename(
    inschool=TESCHENR,
    employed=TELFS, fulltime=TRDPFTPT,
    spouse_pres=TRSPPRES, children=TRCHILDNUM,
    timealone=TRTALONE
  )

activity_11_trimmed<-activity_11%>%mutate(
    volunteer_mins = rowSums(
      across(starts_with("t15")),
      na.rm = TRUE
    ),
    social_mins = rowSums(
      across(c(starts_with("t1202"),starts_with("t1201"))),
      na.rm = TRUE
    ),
    sports_mins = rowSums(
      across(starts_with("t1301")),
      na.rm = TRUE
    ),
    civic_mins = rowSums(
      across(c(t100201, t100305)),
      na.rm = TRUE
    ),
    # This is for controls
    relig_mins = rowSums(
      across(starts_with("t14")),
      na.rm = TRUE
    )
  )%>%select(
  tucaseid,     # ATUS case ID
  volunteer_mins, # Volunteering
  social_mins, # Socializing
  sports_mins,   # Play sports
  civic_mins, # Civic obligations/participation
  relig_mins
  ) %>% rename(
    TUCASEID=tucaseid
  )

data_11 <- activity_11_trimmed %>%
  left_join(respondent_11_trimmed, by="TUCASEID") %>%
  left_join(cps_11_trimmed, by=c("TUCASEID","TULINENO"))%>% 
  relocate(TUCASEID, TULINENO, TUFINLWGT, homeowner, volunteer_mins)%>%
  mutate(owner = ifelse(homeowner == 1, 1, 0), year=2011)

# ATUS 2005----
#Note: Utilized AI to adapt my 2011 stuff
# Reading in tables
cps_05 <- read.table("../Dataset/atuscps_2005/atuscps_2005.dat",
                       header = TRUE, sep = ',')
respondent_05 <- read.table("../Dataset/atusresp_2005/atusresp_2005.dat",
                              header = TRUE, sep = ',')
activity_05 <- read.table("../Dataset/atussum_2005/atussum_2005.dat",
                            header = TRUE, sep = ',')

# Trimming CPS
cps_05_trimmed <- cps_05 %>%
  select(
    TUCASEID,      # ATUS case/respondent ID
    TULINENO,      # Household member line number
    
    HUFAMINC,      # Family/household income
    HETENURE,      # Housing tenure: owned, rented, etc.
    
    PESEX,         # Sex
    PRTAGE,        # Age
    PEEDUCA,       # Educational attainment
    PEMARITL,      # Marital status
    PRCITSHP,      # Citizenship status
    PTDTRACE,      # Race
    PEHRUSLT,      # Usual hours worked per week
    
    GEPSEUST,      # Pseudo collapsed sampling stratum
    GEPSEUCL,      # Pseudo PSU/cluster
    GESTFIPS,       # State
    GTMETSTA
  ) %>%
  rename(
    sex=PESEX, homeowner=HETENURE, income=HUFAMINC,
    education=PEEDUCA, married=PEMARITL, citizen=PRCITSHP,
    race=PTDTRACE, age=PRTAGE, workhours=PEHRUSLT
  )


# Trimming Respondent
respondent_05_trimmed <- respondent_05 %>%
  select(
    TUCASEID,      # ATUS case ID
    TULINENO,      # Household member line number
    TUFINLWGT,     # ATUS final survey weight
    
    TESCHENR,      # School enrollment status
    TELFS,         # Labor force status
    
    TRDPFTPT,      # Full-time/part-time work status
    TRSPPRES,      # Spouse/partner present
    TRCHILDNUM,    # Number of children
    TRTALONE       # Time spent alone
  ) %>%
  rename(
    inschool = TESCHENR,
    employed = TELFS,
    fulltime = TRDPFTPT,
    spouse_pres = TRSPPRES,
    children = TRCHILDNUM,
    timealone = TRTALONE
  )

# Trimming Activity Summary
activity_05_trimmed <- activity_05 %>%
  mutate(
    volunteer_mins = rowSums(
      across(starts_with("t15")),
      na.rm = TRUE
    ),
    social_mins = rowSums(
      across(c(starts_with("t1202"),starts_with("t1201"))),
      na.rm = TRUE
    ),
    sports_mins = rowSums(
      across(starts_with("t1301")),
      na.rm = TRUE
    ),
    civic_mins = rowSums(
      across(c(t100201, t100299, t100303)),
      na.rm = TRUE
    ),
    # This is for controls
    relig_mins = rowSums(
      across(starts_with("t14")),
      na.rm = TRUE
    )
  ) %>%
  select(
    TUCASEID,       # ATUS case ID
    volunteer_mins,
    social_mins,
    sports_mins,
    civic_mins, # Civic obligations/participation
    relig_mins
  )

# Joining
data_05 <- activity_05_trimmed %>%
  left_join(respondent_05_trimmed, by = "TUCASEID")%>%
  left_join(cps_05_trimmed, by = c("TUCASEID", "TULINENO"))%>%
  relocate(TUCASEID, TULINENO, TUFINLWGT, homeowner, volunteer_mins)%>%
  mutate(owner = ifelse(homeowner==1,1,0), year=2005)

data_0511 <- bind_rows(data_05, data_11)
data_0511 <- data_0511%>%mutate(post2008 = ifelse(year >= 2008, 1, 0))

# Further refining, recoding categoricals ----
data_0511 <- data_0511%>%filter(homeowner != 3)

data_0511 <- data_0511 %>%
  mutate(
    # EDUCATION OPTION 2: Highest educational attainment.
    # This retains the substantive credential categories.
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
    
    # RACE: Preserve White and Black; combine rare categories.
    # 1 = White; 2 = Black; 3-5 = other single-race categories;
    # 6-19 = multiracial combinations in the 2005/2011 coding.
    race_cat = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      #race %in% 3:5 ~ "Other single race",
      #race %in% 6:19 ~ "Multiracial",
      race %in% 3:19 ~ "Other / Multiracial",
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
    
    # CITIZENSHIP: Preserve the three substantive categories.
    citizen_cat = factor(
      if_else(citizen %in% 1:3, citizen, NA_real_),
      levels = c(1, 2, 3),
      labels = c(
        "Native-born citizen",
        "Naturalized citizen",
        "Not a citizen"
      )
    ),

    # INCOME: Brackets aren't equal size, recoding.
    # <$15,000 (lower)
    # <35,000 (lower-middle)
    # <$65,000 (middle)
    # <$100,000 (upper-middle)
    # >=$100,000 (upper)
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
    
    # HOMEOWNERSHIP: Existing filter excludes code 3.
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
    # Confirm the 2005 and 2011 codebooks before using this variable:
    # TRDPFTPT may contain additional nonemployment codes.
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
    
    relig_cat = as.integer(relig_mins >0),
    relig_cat = factor(
      if_else(relig_cat %in% 0:1, relig_cat, NA_real_),
      levels = c(0, 1),
      labels = c("Not Religious", "Religious")
    ),
    # YEAR AND POST-PERIOD INDICATORS.
    # Both are used in case I add more years for parallel-trends
    year_cat = factor(year),
    post2008_cat = factor(post2008, levels = c(0, 1)),
    owner_cat = factor(owner, levels = c(0, 1))
    )

# Filtering out original form of recoded vars
data_0511_clean <- data_0511 %>%
  select(
    -inschool, -employed, -fulltime, -spouse_pres, -income, -sex, -education,
    -married, -citizen, -homeowner, -GESTFIPS)

# Getting delinquency ----
# skip 8 for proper headers.
# deldat <- read_excel("../Dataset/area_report_by_year.xlsx", 
#                      sheet = "mortgage_delinq", skip=8, n_max = 52)
# del_lookup <- deldat %>% mutate(delinquency_rate = 
#             rowMeans(
#               across(c('Q4_2006','Q4_2007','Q4_2008','Q4_2009','Q4_2010')),
#               na.rm = TRUE
#             )) %>% select(state, delinquency_rate) %>%
#             mutate(
#               delinq_centered = delinquency_rate - 
#                 mean(delinquency_rate, na.rm = TRUE)
#             )
# 
# data_0511_clean <- data_0511_clean %>%
#   left_join(del_lookup, by="state")

saveRDS(data_0511_clean, "data_0511_clean.rds")
