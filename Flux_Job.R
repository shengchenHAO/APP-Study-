library(dplyr) 
library(stringr) 
library(tidyr) 

load("GI_First_Date.RData")
temp_patid = unique(GI_first_date$Patid)
rm(GI_visit) 

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
  Exclude_Transplant_patid = unique(c(Exclude_Transplant_patid, filter(temp_data, is.na(Trans_Dt) == F & (Trans_Dt - GI_first_date) <= 90 )$Patid))
  Exclude_Bleeding_patid = unique(c(Exclude_Bleeding_patid, filter(temp_data, Fst_Dt <= GI_first_date + 90 & Bleeding == 1)$Patid)) 
  Exclude_HCC_patid = unique(c(Exclude_HCC_patid, filter(temp_data, Fst_Dt <= GI_first_date + 180 & HCC == 1)$Patid))
  
  rm(temp_data,name,year)
  gc()
}
rm(temp_patid)
save.image("output_Exclude.RData")