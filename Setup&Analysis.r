#install.packages("haven")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("lmtest")
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)

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
  HEHOUSUT,      # Housing unit status
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
    socevent_mins = rowSums(
      across(starts_with("t1202")),
      na.rm = TRUE
    ),
    socgen_mins = rowSums(
      across(starts_with("t1201")),
      na.rm = TRUE
    ),
    sports_mins = rowSums(
      across(starts_with("t1301")),
      na.rm = TRUE
    )
  )%>%select(
  tucaseid,     # ATUS case ID
  t100201,      # Civic obligations/participation
  t120302,      # Tobacco/drug use
  t120404,      # Gambling
  t140101,      # Religious services
  volunteer_mins, # Volunteering
  socevent_mins, # Social event att/host
  socgen_mins,  # General socializing
  sports_mins   # Play sports
  ) %>% rename(
    TUCASEID=tucaseid,
    civic_mins=t100201, drug_mins=t120302,
    gamble_mins=t120404, religserv_mins=t140101
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
    HEHOUSUT,      # Housing unit status
    HETENURE,      # Housing tenure: owned, rented, etc.
    
    PESEX,         # Sex
    PRTAGE,        # Age
    PEEDUCA,       # Educational attainment
    PEMARITL,      # Marital status
    PRCITSHP,      # Citizenship status
    PTDTRACE,      # Race
    PEHRUSLT,      # Usual hours worked per week
    
    GEPSEUCL,
    GEPSEUST,
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
    socevent_mins = rowSums(
      across(starts_with("t1202")),
      na.rm = TRUE
    ),
    socgen_mins = rowSums(
      across(starts_with("t1201")),
      na.rm = TRUE
    ),
    sports_mins = rowSums(
      across(starts_with("t1301")),
      na.rm = TRUE
    )
  ) %>%
  select(
    TUCASEID,       # ATUS case ID
    t100201,        # Civic obligations/participation
    t120302,        # Tobacco/drug use
    t120404,        # Gambling
    t140101,        # Religious services
    volunteer_mins,
    socevent_mins,
    socgen_mins,
    sports_mins
  ) %>%
  rename(
    civic_mins = t100201,
    drug_mins = t120302,
    gamble_mins = t120404,
    religserv_mins = t140101
  )

# Joining
data_05 <- activity_05_trimmed %>%
  left_join(respondent_05_trimmed, by = "TUCASEID")%>%
  left_join(cps_05_trimmed, by = c("TUCASEID", "TULINENO"))%>%
  relocate(TUCASEID, TULINENO, TUFINLWGT, homeowner, volunteer_mins)%>%
  mutate(owner = ifelse(homeowner==1,1,0), year=2005)


# lazy models
model1 <- lm(volunteer_mins ~ owner, data=data_11)
summary(model1)

model2 <- lm(volunteer_mins ~ owner, data_05)
summary(model2)

data_0511 <- bind_rows(data_05, data_11)
data_0511 <- data_0511%>%mutate(post2008 = ifelse(year >= 2008, 1, 0))

model3 <- lm(volunteer_mins ~ owner * post2008, data=data_0511)
summary(model3)

# Summary Statistics Table. AI Assisted ----
make_summary <- function(data, var, label = var, percent_value = NULL) {
  
  x <- data[[var]]
  
  if (!is.null(percent_value)) {
    # Percentage of observations equal to the specified category
    pct <- mean(x == percent_value, na.rm = TRUE) * 100
    
    data.frame(
      Variable = label,
      N = sum(!is.na(x)),
      Mean = pct,
      SD = NA_real_
    )
    
  } else {
    # Regular continuous-variable summary
    data.frame(
      Variable = label,
      N = sum(!is.na(x)),
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE)
    )
  }
}

table1 <- bind_rows(
  
  make_summary(data_0511, "volunteer_mins", "Volunteer minutes"),
  make_summary(data_0511, "age", "Age"),
  make_summary(data_0511, "income", "Income"),
  make_summary(data_0511, "education", "Education"),
  make_summary(data_0511, "children", "Children"),
  make_summary(data_0511, "workhours", "Work hours"),
  
  make_summary(
    data_0511, "sex", "Female (%)",
    percent_value = 2
  ),
  
  make_summary(
    data_0511, "owner", "Homeowner (%)",
    percent_value = 1
  ),
  
  make_summary(
    data_0511, "married", "Married (%)",
    percent_value = 1
  ),
  
  make_summary(
    data_0511, "employed", "Employed (%)",
    percent_value = 1
  )
) %>%
  mutate(
    Mean = round(Mean, 2),
    SD = round(SD, 2)
  )

table1
write.csv(
  table1,
  "table1_descriptive_statistics.csv",
  row.names = FALSE
)