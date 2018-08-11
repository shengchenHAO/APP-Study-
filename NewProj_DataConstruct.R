setwd("X:/Shengchen Hao/Tapper Liver/R file/new/GI_proj") 
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

save.image("GI_First_Date.RData")
temp_patid = unique(GI_first_date$Patid)
rm(GI_visit) 

Exclude_coverage_patid = c()
Exclude_Transplant_patid = c() 
Exclude_Bleeding_patid = c() 
Exclude_HCC_patid = c() 
for (year in 2001:2015){ # run in FLUX
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

# get the total patids included in study
liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 
Patid_total = unique(liver_member_fixed$Patid)
'%!in%' = Negate('%in%')
Patid_total = Patid_total[Patid_total %!in% Exclude_coverage_patid & Patid_total %!in% Exclude_Bleeding_patid & Patid_total %!in% Exclude_HCC_patid & Patid_total %!in% Exclude_Transplant_patid & Patid_total %in% GI_first_date$Patid]

# get the data (def) for the patids above for one year after first GI
load("Flux_temp.RData")
data_total = data.frame()
for (year in 2001:2015){ # Run in FLUX
  name = paste0("data_def_", year, ".RData")
  load(name)
  
  temp_data = filter(temp_data, Patid %in% Patid_total)
  temp_data = merge(temp_data, GI_first_date, by = "Patid", all.x = T)
  temp_data = filter(temp_data, Fst_Dt >= GI_first_date & Fst_Dt <= GI_first_date + 365)
  data_total = rbind(data_total, temp_data)
  
  rm(temp_data,name,year)
  gc()
}
save(data_total, file = "one_year_data.RData")



# DATA CONSTRUCTING ------------------------------------------------------------------------
# Combine the Diag codes by Patid and Fst_Dt
# medical
for (year in 2001:2015){
  name = paste0("X:/Shengchen Hao/Tapper Liver/Medical Files/liver_med_", year, ".sas7bdat") 
  med = read_sas(name)  
  
  med =select(med, Patid, Dstatus, Fst_Dt, Pos, Proc_Cd, Prov)
  colnames(med) = c("Patid", "Dstatus", "Fst_Dt", "Position", "Proc", "Provider")
  
  save(med, file = paste0("medical_", year,".RData") ) 
  rm(med, name, year) 
  gc()
} 

# diag 
for(year in 2001:2015){
  name = paste0("X:/Shengchen Hao/Tapper Liver/Medical Files/liver_diag_", year, ".sas7bdat") 
  diag = read_sas(name)
  
  diag = select(diag, Patid, Diag, Fst_Dt)
  
  save(diag, file = paste0("diagnosis_", year,".RData") ) 
  rm(diag, name, year) 
  gc()
} 

CPT = function(data){
  temp = data %>% 
    group_by(Patid, Fst_Dt) %>%   
    summarise(Diag = paste0(Diag, collapse = ",")) %>% 
    ungroup() 
  return(temp)
}

for (year in 2001:2015){
  load(paste0("diagnosis_", year,".RData"))
  load(paste0("medical_", year,".RData"))
  
  diag = CPT(diag) 
  temp_data = merge(med, diag, by = c("Patid", "Fst_Dt"), all = T)  
  
  save(temp_data, file = paste0("data_", year, ".RData")) 
  rm(diag,med,temp_data) 
  gc()
}

load("person_year.RData")
for (year in 2001:2015){
  name = paste0("data_", year, ".RData") 
  load(name)  
  
  temp_data = merge(temp_data, Person_year, by = "Patid", all.x = T)
  temp_data$YEAR_OF_DEATH = NULL 
  temp_data$MONTH_OF_DEATH = NULL 
  temp_data$First_Claim_Date = NULL 
  
  save(temp_data, file = paste0("data_", year, ".RData") ) 
  rm(temp_data,name,year) 
  gc()
}
# -----------------------------------------------------------------------------------------

liver_provider = read_sas("X:/Shengchen Hao/Tapper Liver/Miscellaneous Files/liver_provider.sas7bdat")
code = "207RG0100X|207RI0008X|207RT0003X"
Gastro_id = unique(filter(liver_provider, grepl(code, Taxonomy1)|grepl(code, Taxonomy2))$Prov)
rm(liver_provider, code)

GI_providerID = data.frame()
for (year in 2001:2015){
  name = paste0("X:/Shengchen Hao/Tapper Liver/Medical Files/liver_med_", year, ".sas7bdat") 
  med = read_sas(name)  
  
  med =select(med, Patid, Dstatus, Fst_Dt, Pos, Proc_Cd, Prov)
  colnames(med) = c("Patid", "Dstatus", "Fst_Dt", "Position", "Proc", "Provider")
  GI_providerID = rbind(GI_providerID, select(filter(med, Provider %in% Gastro_id), Patid, Fst_Dt, Provider))
 
  rm(med, name, year) 
  gc()
} 


