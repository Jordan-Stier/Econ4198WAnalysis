#install.packages("haven")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lmtest")
library(haven)
library(dplyr)
library(ggplot2)
library(lmtest)

#2011----
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
  relocate(TUCASEID, TULINENO, TUFINLWGT, homeowner, volunteer_mins)

#2005----
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
  relocate(TUCASEID, TULINENO, TUFINLWGT, homeowner, volunteer_mins)