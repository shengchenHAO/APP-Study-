library(dplyr)
library(tidyr)
setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 

load("table2.RData")
load("APP&MD dates.RData")
rm(APP_MD_date, MD_date)


discharge_Patid = filter(table2, Patid %in% readmit_N & Denominator >= 1)$Patid 

liver_conf_temp = liver_conf %>% 
  group_by(Patid) %>% 
  arrange(Patid, Disch_Date) %>% 
  summarise(Disch_Date = first(Disch_Date)) %>% 
  select(Patid, Disch_Date)

# First APP visit after discharge
APP_date = filter(APP_date, Patid %in% discharge_Patid)
APP_date = merge(APP_date, liver_conf_temp, all.x = T, by = "Patid")
APP_dat_temp = filter(APP_date, Fst_Dt > Disch_Date) %>% 
  mutate(time_lag = Fst_Dt - Disch_Date) %>% 
  arrange(Patid, time_lag) %>% 
  group_by(Patid) %>% 
  summarise(time_lag = first(time_lag))

'
Non App visit
'

load("First_Claim.RData")
temp = filter(temp, Patid %!in% APP_dat_temp$Patid & Patid %in% discharge_Patid)
temp = merge(temp, liver_conf_temp, all.x = T, by = "Patid")

temp = filter(temp, Fst_Dt > Disch_Date)

temp = temp %>% 
  mutate(time_lag_nonapp = Fst_Dt - Disch_Date) %>%
  group_by(Patid) %>% 
  arrange(Patid, time_lag_nonapp) %>%
  summarise(time_lag_nonapp = first(time_lag_nonapp))
  
'
summary and test
'

summary(as.numeric(temp$time_lag_nonapp))
summary(as.numeric(APP_dat_temp$time_lag))
wilcox.test(as.numeric(temp$time_lag_nonapp), as.numeric(APP_dat_temp$time_lag))


