library(dplyr) 
library(stringr) 
library(tidyr) 

load("flux_temp.RData")
# run in flux 
data_total = data_total %>% 
  merge(y = AB_vaccine_date, all.x = T, by = "Patid") %>% 
  merge(y = Screen_cancer_date, all.x = T, by = "Patid") %>% 
  merge(y = Endoscopy_date, all.x = T, by = "Patid") %>% 
  merge(y = Influenza_date, all.x = T, by = "Patid")  

save(data_total, file = "data merged dates.RData")