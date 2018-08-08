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
  summarise(GI_first_date = first(Fst_Dt))

