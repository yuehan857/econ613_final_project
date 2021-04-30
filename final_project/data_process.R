library(haven)
library(tidyverse)

#### data processing 

### read in data ###############################################################

setwd("/Users/yuehan/Desktop/Duke/21Spring/final_project_data/data")

## 2012 ------------------------------------------------------------------------

crossyearid_2012 <- read_dta("2012crossyearid.dta", encoding = "GB18030")

famros_2012 <- read_dta("2012famros.dta", encoding = "GB18030")

family_2012 <- read_dta("2012family.dta", encoding = "GB18030")

child_2012 <- read_dta("2012child.dta", encoding = "GB18030")

adult_2012 <- read_dta("2012adult.dta", encoding = "GB18030")

## 2014 ------------------------------------------------------------------------

crossyearid_2014 <- read_dta("2014crossyearid.dta", encoding = "GB18030")

com_2014 <- read_dta("2014com.dta", encoding = "GB18030")

famconf_2014 <- read_dta("2014famconf.dta", encoding = "GB18030")

famecon_2014 <- read_dta("2014famecon.dta", encoding = "GB18030")

child_2014 <- read_dta("2014child.dta", encoding = "GB18030")

adult_2014 <- read_dta("2014adult.dta", encoding = "GB18030")

## 2016 ------------------------------------------------------------------------

crossyearid_2016 <- read_dta("2016crossyearid.dta", encoding = "GB18030")

famconf_2016 <- read_dta("2016famconf.dta", encoding = "GB18030")

famecon_2016 <- read_dta("2016famecon.dta", encoding = "GB18030")

child_2016 <- read_dta("2016child.dta", encoding = "GB18030")

adult_2016 <- read_dta("2016adult.dta", encoding = "GB18030")



### extract variable  ##########################################################

## dependent -------------------------------------------------------------------

temp <- family_2012 %>% select(subpopulation:releaseversion)

# family income

family_2012 %>% select(fincome2)

famecon_2014 %>% select(fincome2)

famecon_2016 %>% select(fincome2)

# family expense

family_2012 %>% select(expense)

famecon_2014 %>% select(expense)

famecon_2016 %>% select(expense)

## independent -----------------------------------------------------------------

adult_2012 %>% select(qi605_s_1:qi605_s_8, qi5014, qi1001m:qi1004)

adult_2014 %>% select(qi301_s_1:qi301_s_7, qi201_s_1:qi201_s_5)

adult_2016 %>% select(qi301_s_1:qi301_s_5, qi2001)

# community avg family income

com_2014 %>% select(ch6)

# family savings and total asset

family_2012 %>% select(savings, total_asset)

famecon_2014 %>% select(savings, total_asset)

famecon_2016 %>% select(savings, total_asset)

# family size

family_2012 %>% select(familysize)

famconf_2014 %>% select(familysize14)

famconf_2016 %>% select(familysize16)

# children size

child_2012 %>%
  select(pid, fid12) %>%
  group_by(fid12) %>%
  count()

child_2014 %>%
  select(pid, fid12) %>%
  group_by(fid12) %>%
  count()

child_2016 %>%
  select(pid, fid12) %>%
  group_by(fid12) %>%
  count()

# health condition

adult_2012 %>% select(qp201)
child_2012 %>% select(wl1)
adult_2012 %>% select(qp501)
child_2012 %>% select(wc401)

adult_2014 %>% select(qp201)
child_2014 %>% select(wl1)
adult_2014 %>% select(qp501)
child_2014 %>% select(wc401)

adult_2016 %>% select(qp201)
child_2016 %>% select(wl1)
adult_2016 %>% select(pc401)
child_2016 %>% select(pc401)


## other info: prime key -------------------------------------------------------

family_2012 %>% select(fid12, cid, countyid, provcd, urban12)

famecon_2014 %>%
  filter(cfps2012_finterv == 1) %>%
  select(fid12, cid14, countyid14, provcd14, urban14)

famecon_2016 %>%
  filter(cfps2014_interv == 1) %>%
  select(fid12, cid16, countyid16, provcd16, urban16)



### process and merge data #####################################################

## 2012 ------------------------------------------------------------------------

# adult

nrspi_health_2012 <- adult_2012 %>%
  # filter nrspi participant
  mutate(
    # participate but not receive
    participate_nrspi_1 = case_when(
      qi604_s_1 == 4 |
        qi604_s_2 == 4 |
        qi604_s_3 == 4 |
        qi604_s_4 == 4 |
        qi604_s_5 == 4 |
        qi604_s_6 == 4 |
        qi604_s_7 == 4 |
        qi604_s_8 == 4 ~ 1,
      TRUE ~ 0
    ),
    # participate and receive
    participate_nrspi_2 = case_when(
      qi5014 == 1 ~ 1,
      TRUE ~ 0
    ),
    # uncategorized
    participate_nrspi_3 = case_when(
      qi605_s_1 == 4 |
        qi605_s_2 == 4 |
        qi605_s_3 == 4 |
        qi605_s_4 == 4 |
        qi605_s_5 == 4 |
        qi605_s_6 == 4 |
        qi605_s_7 == 4 |
        qi605_s_8 == 4 ~ 1,
      TRUE ~ 0
    ),
  ) %>%
  mutate(
    participate_nrspi_1 = case_when(
      participate_nrspi_1 == 1 ~ 1,
      participate_nrspi_3 == 1 & cfps2012_age < 60 ~ 1,
      TRUE ~ 0
    ),
    participate_nrspi_2 = case_when(
      participate_nrspi_2 == 1 ~ 1,
      participate_nrspi_3 == 1 & cfps2012_age >= 60 ~ 1,
      TRUE ~ 0
    ),
  ) %>%
  # health condition
  mutate(
    if_health = case_when(
      # not health
      qp201 == 5 ~ 0,
      # health
      TRUE ~ 1
    ),
    if_hospital = case_when(
      # hospital
      qp501 == 1 ~ 1,
      # not hospital
      TRUE ~ 0
    )
  ) %>%
  group_by(fid12) %>%
  summarize(
    if_nrspi_1 = ifelse(sum(participate_nrspi_1) >= 1, 1, 0),    
    if_nrspi_2 = ifelse(sum(participate_nrspi_2) >= 1, 1, 0),
    if_health = ifelse(sum(if_health) >= 1, 1, 0),
    if_hospital = ifelse(sum(if_hospital) >= 1, 1, 0)
  )

# child

child_health_2012 <- child_2012 %>%
  mutate(
    if_health_child = case_when(
      # not health
      wl1 == 5 ~ 0,
      # health
      TRUE ~ 1
    ),
    if_hospital_child = case_when(
      # hospital
      wc401 == 1 ~ 1,
      # not hospital
      TRUE ~ 0
    )
  ) %>%
  group_by(fid12) %>%
  summarize(
    childrensize = n(),
    if_health_child = ifelse(sum(if_health_child) >= 1, 1, 0),
    if_hospital_child = ifelse(sum(if_hospital_child) >= 1, 1, 0)
  )

# family

main_2012 <- family_2012 %>% 
  mutate(
    saving_ratio = (fincome2-expense)/fincome2,
    log_consumption_ratio = log(expense/fincome2)
  ) %>%
  select(
    # prime key
    fid12, cid, countyid, provcd, urban12, cyear,
    # depedent
    saving_ratio, log_consumption_ratio,
    # control
    savings, total_asset, familysize
  ) 
  
# merge
df_2012 <- main_2012 %>% 
  merge(nrspi_health_2012, by="fid12") %>%
  merge(child_health_2012, by="fid12") %>%
  filter(urban12==0, familysize > childrensize, saving_ratio >= -10) %>%
  drop_na()

## 2014 ------------------------------------------------------------------------

nrspi_health_2014 <- adult_2014 %>%
  # filter nrspi participant
  mutate(
    participate_nrspi_1 = case_when(
      # participate but not receive
      qi301_s_1 == 6 |
        qi301_s_2 == 6 |
        qi301_s_3 == 6 |
        qi301_s_4 == 6 |
        qi301_s_5 == 6 |
        qi301_s_6 == 6 |
        qi301_s_7 == 6 ~ 1,
      TRUE ~ 0
    ),
    participate_nrspi_2 = case_when(
      qi201_s_1 == 6 |
        qi201_s_2 == 6 |
        qi201_s_3 == 6 |
        qi201_s_4 == 6 |
        qi201_s_5 == 6 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # health condition
  mutate(
    if_health = case_when(
      # not health
      qp201 == 5 ~ 0,
      # health
      TRUE ~ 1
    ),
    if_hospital = case_when(
      # hospital
      qp501 == 1 ~ 1,
      # not hospital
      TRUE ~ 0
    )
  ) %>%
  group_by(fid12) %>%
  summarize(
    if_nrspi_1 = ifelse(sum(participate_nrspi_1) >= 1, 1, 0),    
    if_nrspi_2 = ifelse(sum(participate_nrspi_2) >= 1, 1, 0),
    if_health = ifelse(sum(if_health) >= 1, 1, 0),
    if_hospital = ifelse(sum(if_hospital) >= 1, 1, 0)
  )

# child

child_health_2014 <- child_2014 %>%
  mutate(
    if_health_child = case_when(
      # not health
      wl1 == 5 ~ 0,
      # health
      TRUE ~ 1
    ),
    if_hospital_child = case_when(
      # hospital
      wc401 == 1 ~ 1,
      # not hospital
      TRUE ~ 0
    )
  ) %>%
  group_by(fid12) %>%
  summarize(
    childrensize = n(),
    if_health_child = ifelse(sum(if_health_child) >= 1, 1, 0),
    if_hospital_child = ifelse(sum(if_hospital_child) >= 1, 1, 0)
  )

# family

main_2014 <- famecon_2014 %>% 
  mutate(
    saving_ratio = (fincome2-expense)/fincome2,
    log_consumption_ratio = log(expense/fincome2)
  ) %>%
  filter(fid12 == fid14) %>%
  select(
    # prime key
    fid12, cid14, countyid14, provcd14, urban14, cyear,
    # depedent
    saving_ratio, log_consumption_ratio,
    # control
    savings, total_asset, familysize
  ) 

df_2014 <- main_2014 %>% 
  merge(nrspi_health_2014, by="fid12") %>%
  merge(child_health_2014, by="fid12") %>%
  filter(urban14==0, familysize > childrensize, saving_ratio >= -10) %>%
  drop_na()

## 2016 ------------------------------------------------------------------------

nrspi_health_2016 <- adult_2016 %>%
  # filter nrspi participant
  mutate(
    participate_nrspi_1 = case_when(
      # participate but not receive
      qi301_s_1 == 6 |
        qi301_s_2 == 6 |
        qi301_s_3 == 6 |
        qi301_s_4 == 6 |
        qi301_s_5 == 6 ~ 1,
      TRUE ~ 0
    ),
    # participate and receive
    participate_nrspi_2 = case_when(
      qi2001 == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # health condition
  mutate(
    if_health = case_when(
      # not health
      qp201 == 5 ~ 0,
      # health
      TRUE ~ 1
    ),
    if_hospital = case_when(
      # hospital
      pc401 == 1 ~ 1,
      # not hospital
      TRUE ~ 0
    )
  ) %>%
  group_by(fid12) %>%
  summarize(
    if_nrspi_1 = ifelse(sum(participate_nrspi_1) >= 1, 1, 0),    
    if_nrspi_2 = ifelse(sum(participate_nrspi_2) >= 1, 1, 0),
    if_health = ifelse(sum(if_health) >= 1, 1, 0),
    if_hospital = ifelse(sum(if_hospital) >= 1, 1, 0)
  )

# child

child_health_2016 <- child_2016 %>%
  mutate(
    if_health_child = case_when(
      # not health
      wl1 == 5 ~ 0,
      # health
      TRUE ~ 1
    ),
    if_hospital_child = case_when(
      # hospital
      pc401 == 1 ~ 1,
      # not hospital
      TRUE ~ 0
    )
  ) %>%
  group_by(fid12) %>%
  summarize(
    childrensize = n(),
    if_health_child = ifelse(sum(if_health_child) >= 1, 1, 0),
    if_hospital_child = ifelse(sum(if_hospital_child) >= 1, 1, 0)
  )

# family

main_2016 <- famecon_2016 %>% 
  mutate(
    saving_ratio = (fincome2-expense)/fincome2,
    log_consumption_ratio = log(expense/fincome2)
  ) %>%
  filter(fid12 == fid14, fid14 == fid16) %>%
  select(
    # prime key
    fid12, cid16, countyid16, provcd16, urban16, cyear,
    # depedent
    saving_ratio, log_consumption_ratio,
    # control
    savings, total_asset, familysize16
  ) 

df_2016 <- main_2016 %>% 
  merge(nrspi_health_2016, by="fid12") %>%
  merge(child_health_2016, by="fid12") %>%
  filter(urban16==0, familysize16 > childrensize, saving_ratio >= -10) %>%
  drop_na()

## combine 2012 - 2016 ---------------------------------------------------------

fid_12_16 <- df_2012$fid12 %>%
  intersect(df_2014$fid12) %>% 
  intersect(df_2016$fid12)

names(df_2014) <- colnames(df_2012)
names(df_2016) <- colnames(df_2012)

df <- bind_rows(df_2012,df_2014,df_2016) %>%
  filter(fid12 %in% fid_12_16) %>%
  select(-urban12) %>%
  mutate(
    cyear = case_when(
      cyear <= 2013 ~ 2012,
      cyear <= 2015 ~ 2014,
      cyear <= 2017 ~ 2016
    )
  )

table(df %>% select(cyear,if_nrspi_1))

table(df %>% select(cyear,if_nrspi_2))

if_nrspi_by_community <- df %>%
  group_by(cyear, cid) %>%
  summarize(
    if_nrspi_by_community = ifelse(sum(if_nrspi_1) >= 1|sum(if_nrspi_2) >= 1, 1, 0)
  )

table(if_nrspi_by_community %>% select(cyear,if_nrspi_by_community))

df_iv <- merge(df, if_nrspi_by_community, by = c("cid", "cyear"))



write_dta(df_iv, "/Users/yuehan/Desktop/Duke/21Spring/final_project_data/df.dta")



