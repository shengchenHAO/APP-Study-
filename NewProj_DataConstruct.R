setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 


# FIRST EXCLUDE BASED ON CODE BOOK 
GI_visit = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  GI_visit = rbind(GI_visit, filter(temp_data, Gastro == 1))
  
  rm(temp_data,name,year)
  gc()
}

GI_first_date = GI_visit %>% 
  select(Patid, Fst_Dt) %>% 
  distinct() %>%
  arrange(Patid, Fst_Dt) %>% 
  group_by(Patid) %>% 
  summarise(GI_first_date = first(Fst_Dt)) %>% 
  mutate(GI_first_date = as.Date(GI_first_date, "%Y-%m-%d"))

save(c(GI_visit, GI_first_date), file = "GI_First_Date.RData")
temp_patid = unique(GI_first_date$Patid)

Exclude_coverage_patid = c()
Exclude_Transplant_patid = c() 
Exclude_Bleeding_patid = c() 
Exclude_HCC_patid = c() 
for (year in 2001:2015){
  name = paste0("data_def_", year, ".RData") 
  load(name) 
  
  temp_data = filter(temp_data, Patid %in% temp_patid)
  temp_data = merge(temp_data, GI_first_date, by = "Patid", all.x = T)
  temp_data = mutate(temp_data, Bleeding = 1*(is.na(Bleed_date) == F & Fst_Dt >= Bleed_date & Fst_Dt < Lst_Date)) # create bleeding time varying variable
  
  Exclude_coverage_patid = unique(c(Exclude_coverage_patid, filter(temp_data, (Lst_Date - GI_first_date) < 365)$Patid))
  Exclude_Transplant_patid = unique(c(filter(temp_data, is.na(Trans_Dt) == F & (Trans_Dt - GI_first_date) <= 90 )$Patid))
  Exclude_Bleeding_patid = unique(c(Exclude_Bleeding_patid, filter(temp_data, Fst_Dt <= GI_first_date + 90 & Bleeding == 1)$Patid)) 
  Exclude_HCC_patid = unique(c(Exclude_HCC_patid, filter(temp_data, Fst_Dt <= GI_first_date + 180 & HCC == 1)$Patid))
  
  rm(temp_data,name,year)
  gc()
}
rm(temp_patid)
