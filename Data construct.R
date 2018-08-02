# set working directory 
setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 


# create the data we are goin to use  
# medical
for (year in 2001:2015){
  name = paste0("X:/Shengchen Hao/Tapper Liver/Medical Files/liver_med_", year, ".sas7bdat") 
  med = read_sas(name)  
  med =select(med, Patid, Pat_Planid, Clmid, Dstatus, Fst_Dt, Lst_Dt, Pos, Proc_Cd, Prov, Loc_Cd, Eligend) 
  name =paste0("medical_", year,".RData") 
  save(med, file = name) 
  rm(med) 
  gc()
  
} 

# diag 
for(year in 2001:2015){
  name = paste0("X:/Shengchen Hao/Tapper Liver/Medical Files/liver_diag_", year, ".sas7bdat") 
  diag = read_sas(name) 
  diag = select(diag, Patid, Pat_Planid, Clmid, Diag, Fst_Dt, Lst_Dt)
  name = paste0("diagnosis_", year,".RData") 
  save(diag, file = name) 
  rm(diag) 
  gc()
} 

# Grouping data by person date and claim  
CPT = function(data){
  temp = data %>% 
    group_by(Patid, Fst_Dt) %>%   
    summarise(Proc = paste0(Proc_Cd, collapse = ",")
              , Position = paste0(unique(Pos), collapse = ",")
              , Provider = paste0(unique(Prov), collapse = ",")
    ) %>% 
    ungroup() 
  return(temp)
}

# run CPT function over all the medical data from 2001 to 2015
for (year in 2001:2015){
  load(paste0("diagnosis_", year,".RData"))
  load(paste0("medical_", year,".RData"))
  med = CPT(med) 
  temp_data = merge(diag, med, by = c("Patid", "Fst_Dt"), all = T)  
  name = paste0("data_", year, ".RData")
  save(temp_data, file = name) 
  rm(diag,med,temp_data) 
  gc()
}



# get the prov id of Hepatology 
liver_provider = read_sas("X:/Shengchen Hao/Tapper Liver/Miscellaneous Files/liver_provider.sas7bdat")
Hepatology_id = unique(filter(liver_provider, grepl("207RI0008X|207RT0003X", Taxonomy1)|grepl("207RI0008X|207RT0003X", Taxonomy2))$Prov) 

# get the prov id of APP 
code = "363LS0200X|363A00000X|363AM0700X|363L00000X|363LA2100X|363LA2200X|363LC0200X|363LC1500X|363LF0000X|363LG0600X|363LP0808X|363LP2300X|363LW0102X|363LX0001X|363LX0106X|364S00000X|364SA2100X|364SA2200X|364SC0200X|364SC1501X|364SC2300X|364SE0003X|364SF0001X|364SG0600X|364SH0200X|364SH1100X|364SL0600X|364SM0705X|364SP0808X|364SP0809X|364SP0810X|364SP0811X|364SP0812X|364SP0813X|364SP2800X|364SR0400X|364SS0200X|364ST0500X|364SW0102X|364SX0106X|364SX0200X"
APP_id = unique(filter(liver_provider, grepl(code, Taxonomy1)|grepl(code, Taxonomy2))$Prov) 

# get the prov of MD 
code = "202C00000X|204F00000X|207PH0002X|207Q00000X|207QA0000X|207QA0401X|207QA0505X|207QB0002X|207QG0300X|207QH0002X|207R00000X|207RA0000X|207RA0001X|207RA0401X|207RB0002X|207RC0000X|207RC0001X|207RE0101X|207RG0100X|207RG0300X|207RH0000X|
207RH0002X|207RH0003X|207RH0005X|207RI0008X|207RI0011X|207RI0200X|207RN0300X|207RP1001X|207RR0500X|207RS0010X|207RS0012X|207RT0003X|207RX0202X|207V00000X|207VB0002X|207VC0200X|207VE0102X|207VF0040X|207VG0400X|207VH0002X|207VM0101X|
207VX0000X|207VX0201X|208100000X|2081H0002X|2081N0008X|2081P0004X|2081P0010X|2081P0301X|2081P2900X|2081S0010X|2083B0002X|2083P0500X|2084A0401X|2084B0002X|2084B0040X|2084D0003X|2084H0002X|2084N0008X|2084N0400X|2084N0402X|2084N0600X|2084P0015X|2084P0800X|2084P0802X|2084P0805X|2084P2900X|2084S0010X|2084S0012X|2084V0102X|208D00000X|208VP0000X|208VP0014X"
MD_id = unique(filter(liver_provider, grepl(code, Taxonomy1)|grepl(code, Taxonomy2))$Prov)

# Gastro 
code = "207RG0100X|207RI0008X|207RT0003X"
Gastro_id = unique(filter(liver_provider, grepl(code, Taxonomy1)|grepl(code, Taxonomy2))$Prov)

rm(liver_provider, code)



## HE_id
load("Pharmacy_data.RData")
HE_id = filter(liver_pharm, Brnd_Nm == "LACTULOSE" | Gnrc_Nm == "LACTULOSE" | Brnd_Nm == "RIFAXIMIN" |Brnd_Nm == "RIFAXIMIN" )
rm(liver_pharm)
gc()
HE_id = HE_id %>% 
  select(Patid, Fill_Dt) %>% 
  arrange(Patid, Fill_Dt) %>% 
  group_by(Patid) %>% 
  summarise(HE_Fill_Dt = first(Fill_Dt))

# find the first claim date in data  (not use for now)
temp = data.frame()
for (year in 2001:2015){
  name = paste0("X:/Shengchen Hao/Tapper Liver/Medical Files/liver_diag_", year, ".sas7bdat") 
  temp_data = read_sas(name) 
  temp_data = select(temp_data, Patid, Fst_Dt) 
  temp = rbind(temp, temp_data)  
  rm(temp_data, name)
  gc()
}
save(temp, file = "First_Claim.RData")
rm(temp) 
gc()


# charlson (data preparation for Computing charlson score)
# Merging all the diagnosis data with icd9 code and patid 
liver_diag = data.frame() 
for (year in 2001:2015){
  name = paste0("X:/Shengchen Hao/Tapper Liver/Medical Files/liver_diag_", year, ".sas7bdat") 
  temp_data = read_sas(name) 
  temp_data = select(temp_data, Patid, Diag) 
  liver_diag = rbind(liver_diag, temp_data)  
  rm(temp_data, name)
  gc()
}
save(liver_diag, file = "liver_diag.RData")
rm(liver_diag) 
gc()


# merging pharmacy data
liver_pharm = data.frame()
for (year in 2001:2015){
  name = paste0("X:/Shengchen Hao/Tapper Liver/Pharmacy Files/liver_pharm_",year ,".sas7bdat") 
  temp_data = read_sas(name) 
  temp_data = select(temp_data, Patid, Brnd_Nm, Fill_Dt, Gnrc_Nm) 
  liver_pharm = rbind(liver_pharm, temp_data)
  
  rm(temp_data,name) 
  gc()
} 
save(liver_pharm, file = "Pharmacy_data.RData") 
rm(liver_pharm) 
gc()



# find the patients who get liver transplant  
liver_trans = data.frame() 
for (year in 2001:2015){
  name = paste0("data_", year, ".RData") 
  load(name) 
  liver_trans = rbind(liver_trans, filter(temp_data, grepl("^505", Diag)|grepl("47135|47136", Proc))) 
  rm(temp_data) 
  gc()
  
} 
save(liver_trans, file = "transplant.RData")


# First liver cancer diag date  
liver_cancer = data.frame()
for (year in 2001:2015){
  name = paste0("data_", year, ".RData") 
  load(name) 
  liver_cancer = rbind(liver_cancer, dplyr::filter(temp_data, grepl("^1550", Diag))) 
  rm(temp_data) 
  gc()
}

liver_cancer = distinct(select(liver_cancer, Patid, Fst_Dt, Diag))

liver_cancer = liver_cancer %>% 
  dplyr::group_by(Patid) %>% 
  dplyr::mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  dplyr::arrange(Patid, Fst_Dt) %>% 
  dplyr::summarise(Fst_Dt = first(Fst_Dt)) 
save(liver_cancer, file = "liver_cancer.RData")


# merge censor dates into base data
load("person_year.RData")
for (year in 2001:2015){
  name = paste0("data_", year, ".RData") 
  load(name)  
  
  temp_data = merge(temp_data, Person_year, by = "Patid", all.x = T)
  
  save(temp_data, file = paste0("data_censor_", year, ".RData") ) 
  rm(temp_data,name,year) 
  gc()
}

# Run APP, Gastro visit function 
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name)  
  
  temp_data = APP(temp_data) 
  temp_data = Hepatology(temp_data)
  
  save(temp_data, file = paste0("data_censor_", year, ".RData") ) 
  rm(temp_data,name,year) 
  gc()
}



######################### 
Cirrhosis_all_patid = c()  
Cirrhosis_comp_patid = c()
Varices_patid = c() 
Ascites_patid = c()
SBP_patid = c() 
Alcoholic_cirrhosis_patid = c()
Alcoholic_liver_disease_patid = c()
Alcohol_use_patid = c()
Non_alcohol_patid = c()
HCC_patid = c()
Hepatitis_C_patid = c() 
Hepatitis_B_patid = c() 
Band_ligation_patid = c() 
#Falls_patid = c() 
#Intracranial_hemorrhage_patid = c() 
varice_band_treat_patid = c()

HE_patid = c() 

#Fracture_rib_patid = c() 
#Fracture_arm_patid = c() 
#Fracture_femur_patid = c() 
#Fracture_lower_patid = c() 
#Fracture_pelvic_patid = c() 
#Fracture_spinal_patid = c() 
#Fracture_hand_patid = c() 
#Fracture_clavicle_patid = c() 
#Fracture_skull_patid = c() 

Pneumonia_patid = c() 
Sepsis_patid = c()
Urinary_tract_infection_patid = c()
Cellulitis_patid = c()
Bacteremia_patid = c() 
Clostridium_patid = c()
Cholangitis_patid = c()
### Putting date into here

Get_patid = function(data){
  Cirrhosis_all_patid <<- rbind(Cirrhosis_all_patid, filter(data, grepl("^5712|^5715|^5716|^4560|^4561|^4562|^5722|^56723|^5724", Diag) & Patid %!in% Cirrhosis_all_patid$Patid)) 
  Cirrhosis_comp_patid <<- rbind(Cirrhosis_comp_patid, filter(data, grepl("^4560|^78959|^78951|^5722|^56723|^5724", Diag) & Patid %!in% Cirrhosis_comp_patid$Patid))
  Varices_patid <<- rbind(Varices_patid, filter(data, grepl("^4560|^4561|^4562", Diag) & Patid %!in% Varices_patid$Patid))
  Ascites_patid <<- rbind(Ascites_patid, filter(data, grepl("^7895", Diag) & Patid %!in% Ascites_patid$Patid))
  SBP_patid <<- rbind(SBP_patid, filter(data, grepl("^56723", Diag) & Patid %!in% SBP_patid$Patid))
  
  Alcoholic_cirrhosis_patid <<- rbind(Alcoholic_cirrhosis_patid, filter(data, grepl("^5712", Diag) & Patid %!in% Alcoholic_cirrhosis_patid$Patid))
  Alcoholic_liver_disease_patid <<- rbind(Alcoholic_liver_disease_patid, filter(data, grepl("^5712|^5713|^5711|79030|4255|53530|53531|^577|^3575|^291|^303|^980|^3050|^E8600|^V113", Diag) & Patid %!in% Alcoholic_liver_disease_patid$Patid))
  Alcohol_use_patid <<- rbind(Alcohol_use_patid, filter(data, grepl("^7903|^4255|^53530|53551|^577|3575|^291|^303|^3050|^V1130|E8600|^980", Diag) & Patid %!in% Alcohol_use_patid$Patid))
  Non_alcohol_patid <<- rbind(Non_alcohol_patid, filter(data, grepl("^5715",Diag) & Patid %!in% Non_alcohol_patid$Patid))
  HCC_patid <<- rbind(HCC_patid, filter(data, grepl("^1550", Diag) & Patid %!in% HCC_patid$Patid))
  Hepatitis_C_patid <<- rbind(Hepatitis_C_patid, filter(data, grepl("^07041|^07044|^07051|^07054|^0707|^V0262", Diag) & Patid %!in% Hepatitis_C_patid$Patid))
  Hepatitis_B_patid <<- rbind(Hepatitis_B_patid, filter(data, grepl("^07022|^07023|^07032|^07033|^0702|^0703|V0261", Diag) & Patid %!in% Hepatitis_B_patid$Patid))
 
 
 # Falls_patid <<- c(Falls_patid, unique(filter(data, grepl("^E88|^E9681|^E987", Diag))$Patid))
#  Intracranial_hemorrhage_patid <<- c(Intracranial_hemorrhage_patid, unique(filter(data, grepl("^852|^853", Diag))$Patid))
  
  HE_patid <<- rbind(HE_patid, filter(data, grepl("^5722", Diag) & Patid %!in% HE_patid$Patid))
  
# Fracture_rib_patid <<- c(Fracture_rib_patid, unique(filter(data, grepl("^8070|^8071|^8072|^8073|^8074", Diag))$Patid)) 
#  Fracture_arm_patid <<- c(Fracture_arm_patid, unique(filter(data, grepl("^812|^813", Diag))$Patid)) 
#  Fracture_femur_patid <<- c(Fracture_femur_patid, unique(filter(data, grepl("^820|^821", Diag))$Patid)) 
#  Fracture_lower_patid <<- c(Fracture_lower_patid, unique(filter(data, grepl("^823|^824|^825|^826|^827", Diag))$Patid) ) 
#  Fracture_pelvic_patid <<- c(Fracture_pelvic_patid, unique(filter(data, grepl("^808", Diag))$Patid)) 
#  Fracture_spinal_patid <<- c(Fracture_spinal_patid, unique(filter(data, grepl("^805", Diag))$Patid)) 
#  Fracture_hand_patid <<- c(Fracture_hand_patid, unique(filter(data, grepl("^815|^816|^817", Diag))$Patid)) 
#  Fracture_clavicle_patid <<- c(Fracture_clavicle_patid, unique(filter(data, grepl("^810", Diag))$Patid)) 
#  Fracture_skull_patid <<- c(Fracture_skull_patid, unique(filter(data, grepl("^800|^801|^802|^803|^804", Diag))$Patid))
  
  Pneumonia_patid <<- rbind(Pneumonia_patid, filter(data, grepl("^481|^482|^4830|4831|4838|4843|4845|4848|^485|^486", Diag) & Patid %!in% Pneumonia_patid$Patid))
  Sepsis_patid <<- rbind(Sepsis_patid, filter(data, grepl("99591|99592|78552|0380|0381|03811|03812|03819|^0382|^0383|^0384|03840|03841|03842|30843|03844|03849|^0388|^0389", Diag) & Patid %!in% Sepsis_patid$Patid))
  Urinary_tract_infection_patid <<- rbind(Urinary_tract_infection_patid, filter(data, grepl("5990|59010|59011|59080", Diag) & Patid %!in% Urinary_tract_infection_patid$Patid)) 
  Cellulitis_patid <<- rbind(Cellulitis_patid, filter(data, grepl("6820|6821|6822|6823|6824|6825|6826|6827|6828|6829", Diag) & Patid %!in% Cellulitis_patid$Patid))
  Bacteremia_patid <<- rbind(Bacteremia_patid, filter(data, grepl("^7907", Diag) & Patid %!in% Bacteremia_patid$Patid))
  Clostridium_patid <<- rbind(Clostridium_patid, filter(data, grepl("00845", Diag) & Patid %!in% Clostridium_patid$Patid)) 
  Cholangitis_patid <<- rbind(Cholangitis_patid, filter(data, grepl("5761", Diag) & Patid %!in% Cholangitis_patid$Patid))
}

for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name)  
  Get_patid(temp_data) 
  rm(temp_data, name, year) 
  gc()
}
save.image(file = "Patids.RData")

dfs<-Filter(function(x) is.data.frame(get(x)) , ls())
dfs = dfs[dfs != "HE_id"] # remove HE_id
for (name in dfs){
  assign(name,distinct(select(get(name), Patid, Fst_Dt))) 
}
for (name in dfs){
  assign(name, get(name) %>% arrange(Patid, Fst_Dt) %>% group_by(Patid) %>% summarise(temp_Date = first(Fst_Dt))) 
}

############ 
'%!in%' = Negate('%in%')
Cirrhosis_all = function(data, date_inf = Cirrhosis_all_patid){
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Cirrhosis_all = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL

  return(temp)
}

Cirrhosis_without = function(data, date_inf = Cirrhosis_comp_patid){ 
  
 
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Cirrhosis_comp = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  temp = mutate(temp, Cirrhosis_without = 1*(Cirrhosis_comp == 0 & Cirrhosis_all == 1 & Fst_Dt < Lst_Date)) 
  
  return(temp)
}

Varices = function(data, date_inf = Varices_patid){

  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Varices = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL

  return(temp)
}

Ascites = function(data, date_inf = Ascites_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Ascites = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
  return(temp)
}

Spontaneous_Bacterial_Peritonitis = function(data, date_inf = SBP_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Spontaneous_Bacterial_Peritonitis = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
  return(temp)
}


Alcoholic_cirrhosis = function(data, date_inf = Alcoholic_cirrhosis_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Alcoholic_cirrhosis = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
  return(temp)
}

Alcoholic_liver_disease = function(data, date_inf = Alcoholic_liver_disease_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Alcoholic_liver_disease = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
 
  return(temp)
}

Alcohol_use = function(data, date_inf = Alcohol_use_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Alcohol_use = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  gc()
  return(temp)
}

Non_alcohol = function(data){
  temp = mutate(data, Non_alcohol = 1*(Cirrhosis_all == 1 & Alcohol_use == 0 & Alcoholic_liver_disease == 0 & Hepatitis_C == 0 & Hepatitis_B == 0)) # everyone had cirhosis, those had no record of alcohol has non alcoholic cirrhsis
  return(temp)
}


HCC = function(data, date_inf = HCC_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, HCC = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
  return(temp)
}

Hepatitis_C = function(data, date_inf = Hepatitis_C_patid){ 
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Hepatitis_C = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  gc()
  return(temp)
}

Hepatitis_B = function(data, date_inf = Hepatitis_B_patid){
  
 
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Hepatitis_B = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
  return(temp)
}



HE = function(data, date_inf = HE_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = merge(temp, HE_id, by = "Patid", all.x = T)
  temp = mutate(temp, HE = 1*((is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date) | (is.na(HE_Fill_Dt) == F & Fst_Dt < Lst_Date & Fst_Dt >= HE_Fill_Dt)) ) 
  temp$temp_Date = NULL
  temp$HE_Fill_Dt = NULL

  return(temp)
}

################### Not using
Falls = function(data){
  temp = mutate(data, Falls = 1*(Patid %in% Falls_patid & Fst_Dt < Lst_Date)) 
  return(temp)
}

Intracranial_hemorrhage = function(data){
  temp = mutate(data, Intracranial_hemorrhage = 1*(Patid %in% Intracranial_hemorrhage_patid & Fst_Dt < Lst_Date)) 
  return(temp)
}

Fracture = function(data){
  temp = mutate(data, Fracture_arm = 1*(Patid %in% Fracture_arm_patid & Fst_Dt < Lst_Date), 
                Fracture_clavicle = 1*(Patid %in% Fracture_clavicle_patid & Fst_Dt < Lst_Date), 
                Fracture_femur = 1*(Patid %in% Fracture_femur_patid & Fst_Dt < Lst_Date), 
                Fracture_hand = 1*(Patid %in% Fracture_hand_patid & Fst_Dt < Lst_Date), 
                Fracture_lower = 1*(Patid %in% Fracture_lower_patid & Fst_Dt < Lst_Date), 
                Fracture_pelvic = 1*(Patid %in% Fracture_pelvic_patid & Fst_Dt < Lst_Date), 
                Fracture_rib = 1*(Patid %in% Fracture_rib_patid & Fst_Dt < Lst_Date), 
                Fracture_skull = 1*(Patid %in% Fracture_skull_patid & Fst_Dt < Lst_Date), 
                Fracture_spinal = 1*(Patid %in% Fracture_spinal_patid & Fst_Dt < Lst_Date)
                ) 
  return(temp)
}

# Fracture all 
Fracture_all = function(data){
  temp = mutate(data, Fracture_all = 1*((Fracture_arm == 1 |
                                          Fracture_clavicle == 1 | 
                                          Fracture_femur == 1 | 
                                          Fracture_hand == 1 | 
                                          Fracture_lower == 1 | 
                                          Fracture_pelvic == 1 | 
                                          Fracture_rib == 1 | 
                                          Fracture_skull == 1 | 
                                          Fracture_spinal == 1) & Fst_Dt < Lst_Date)) 
  return(temp)
} 

############################

Pneumonia = function(data, date_inf = Pneumonia_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Pneumonia = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
  return(temp)
}

Sepsis = function(data, date_inf = Sepsis_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Sepsis = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
  return(temp)
}

Urinary_tract_infection = function(data, date_inf = Urinary_tract_infection_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Urinary_tract_infection = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  gc()
  return(temp)
}

Cellulitis = function(data, date_inf = Cellulitis_patid){
  
 
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Cellulitis = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL

  return(temp)
}

Bacteremia = function(data, date_inf = Bacteremia_patid){ 
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Bacteremia = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
  return(temp)
}

Clostridium = function(data, date_inf = Clostridium_patid){
  
 
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Clostridium = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
  
  return(temp)
}

Cholangitis = function(data, date_inf = Cholangitis_patid){
  
  
  temp = merge(data, date_inf, by = "Patid", all.x = T)
  temp = mutate(temp, Cholangitis = 1*(is.na(temp_Date) == F & Fst_Dt < Lst_Date & Fst_Dt >= temp_Date)) 
  temp$temp_Date = NULL
 
  return(temp)
}


## By Claim 
vaccine = function(data){ # doesn't include the APP date filter, will do it when finding patids for table 2
  temp = mutate(data, A_vaccine = 1*(grepl("90632|90633|90634|90636", Proc) & Fst_Dt < Lst_Date), 
                B_vaccine = 1*(grepl("90739|90740|90743|90747|90636|90748", Proc) & Fst_Dt < Lst_Date), 
                Influenza_vaccine = 1*(grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc)
                                       & Fst_Dt < Lst_Date)) 
  return(temp)
}


## Screen liver cancer
cancer_screen = function(data){
  temp = mutate(data, CT = 1*(grepl("74177|74178|74160|74170", Proc) & Fst_Dt < Lst_Date & (Fst_Dt < Cancer_Diag_Date | is.na(Cancer_Diag_Date)) & (Fst_Dt >= APP_Dt | is.na(APP_Dt))),
                MRI = 1*(grepl("74183", Proc) & Fst_Dt < Lst_Date & (Fst_Dt < Cancer_Diag_Date | is.na(Cancer_Diag_Date)) & (Fst_Dt >= APP_Dt | is.na(APP_Dt)) ), 
                Ultrasound = 1*(grepl("76700|76705", Proc) & Fst_Dt < Lst_Date & (Fst_Dt < Cancer_Diag_Date | is.na(Cancer_Diag_Date)) & (Fst_Dt >= APP_Dt | is.na(APP_Dt)))) 
  return(temp)
} 


## Hepatology  

Hepatology = function(data){
  temp = separate(data, Provider, into = c("Prov1", "Prov2", "Prov3", "Prov4", "Prov5", "Prov6","Prov7","Prov8","Prov9","Prov10",
                                           "Prov11","Prov12","Prov13","Prov14","Prov15","Prov16","Prov17","Prov18","Prov19","Prov20"), sep = ",", extra = "drop", remove = F)  
  
  temp = dplyr::mutate(temp, Hepatology = 1*((Prov1 %in% Hepatology_id|Prov2 %in% Hepatology_id|Prov3 %in% Hepatology_id|Prov4 %in% Hepatology_id|Prov5 %in% Hepatology_id|
                                                Prov6 %in% Hepatology_id|Prov7 %in% Hepatology_id|Prov8 %in% Hepatology_id|Prov9 %in% Hepatology_id|Prov10 %in% Hepatology_id|
                                                Prov11 %in% Hepatology_id|Prov12 %in% Hepatology_id|Prov13 %in% Hepatology_id|Prov14 %in% Hepatology_id|Prov15 %in% Hepatology_id|
                                                Prov16 %in% Hepatology_id|Prov17 %in% Hepatology_id|Prov18 %in% Hepatology_id|Prov19 %in% Hepatology_id|Prov20 %in% Hepatology_id
                                              ) & Fst_Dt < Lst_Date))  
  
  temp = dplyr::mutate(temp, Gastro = 1*((Prov1 %in% Gastro_id|Prov2 %in% Gastro_id|Prov3 %in% Gastro_id|Prov4 %in% Gastro_id|Prov5 %in% Gastro_id|
                                          Prov6 %in% Gastro_id|Prov7 %in% Gastro_id|Prov8 %in% Gastro_id|Prov9 %in% Gastro_id|Prov10 %in% Gastro_id|
                                          Prov11 %in% Gastro_id|Prov12 %in% Gastro_id|Prov13 %in% Gastro_id|Prov14 %in% Gastro_id|Prov15 %in% Gastro_id|
                                          Prov16 %in% Gastro_id|Prov17 %in% Gastro_id|Prov18 %in% Gastro_id|Prov19 %in% Gastro_id|Prov20 %in% Gastro_id)
                                         & Fst_Dt < Lst_Date))
  
  
  temp$Prov1 = NULL 
  temp$Prov2 = NULL 
  temp$Prov3 = NULL 
  temp$Prov4 = NULL 
  temp$Prov5 = NULL
  temp$Prov6 = NULL 
  temp$Prov7 = NULL 
  temp$Prov8 = NULL 
  temp$Prov9 = NULL 
  temp$Prov10 = NULL
  temp$Prov11 = NULL 
  temp$Prov12 = NULL 
  temp$Prov13 = NULL 
  temp$Prov14 = NULL 
  temp$Prov15 = NULL
  temp$Prov16 = NULL 
  temp$Prov17 = NULL 
  temp$Prov18 = NULL 
  temp$Prov19 = NULL 
  temp$Prov20 = NULL
  
  return(temp)
}

#Advanced Provider (APP)
APP = function(data){
  temp = separate(data, Provider, into = c("Prov1", "Prov2", "Prov3", "Prov4", "Prov5", "Prov6","Prov7","Prov8","Prov9","Prov10",
                                           "Prov11","Prov12","Prov13","Prov14","Prov15","Prov16","Prov17","Prov18","Prov19","Prov20"), sep = ",", extra = "drop", remove = F)  
  
  temp = mutate(temp, APP = 1*((Prov1 %in% APP_id|Prov2 %in% APP_id|Prov3 %in% APP_id|Prov4 %in% APP_id|Prov5 %in% APP_id|
                                  Prov6 %in% APP_id|Prov7 %in% APP_id|Prov8 %in% APP_id|Prov9 %in% APP_id|Prov10 %in% APP_id|
                                  Prov11 %in% APP_id|Prov12 %in% APP_id|Prov13 %in% APP_id|Prov14 %in% APP_id|Prov15 %in% APP_id|
                                  Prov16 %in% APP_id|Prov17 %in% APP_id|Prov18 %in% APP_id|Prov19 %in% APP_id|Prov20 %in% APP_id)
                               & Fst_Dt < Lst_Date))
  
  temp = mutate(temp, MD = 1*((Prov1 %in% MD_id|Prov2 %in% MD_id|Prov3 %in% MD_id|Prov4 %in% MD_id|Prov5 %in% MD_id|
                                 Prov6 %in% MD_id|Prov7 %in% MD_id|Prov8 %in% MD_id|Prov9 %in% MD_id|Prov10 %in% MD_id|
                                 Prov11 %in% MD_id|Prov12 %in% MD_id|Prov13 %in% MD_id|Prov14 %in% MD_id|Prov15 %in% MD_id|
                                 Prov16 %in% MD_id|Prov17 %in% MD_id|Prov18 %in% MD_id|Prov19 %in% MD_id|Prov20 %in% MD_id)
                              & Fst_Dt < Lst_Date))
  
  temp$Prov1 = NULL 
  temp$Prov2 = NULL 
  temp$Prov3 = NULL 
  temp$Prov4 = NULL 
  temp$Prov5 = NULL
  temp$Prov6 = NULL 
  temp$Prov7 = NULL 
  temp$Prov8 = NULL 
  temp$Prov9 = NULL 
  temp$Prov10 = NULL
  temp$Prov11 = NULL 
  temp$Prov12 = NULL 
  temp$Prov13 = NULL 
  temp$Prov14 = NULL 
  temp$Prov15 = NULL
  temp$Prov16 = NULL 
  temp$Prov17 = NULL 
  temp$Prov18 = NULL 
  temp$Prov19 = NULL 
  temp$Prov20 = NULL
  
  return(temp)
} 

# Place of Service (by claim)


Pos = function(data){
  temp = mutate(data, Outpatient = 1*(grepl("11|12|14|15|17|49|65", Position)& Fst_Dt < Lst_Date), 
                Inpatient = 1*(grepl("21|22", Position)& Fst_Dt < Lst_Date), 
                Emergency = 1*(grepl("20|23", Position)& Fst_Dt < Lst_Date), 
                Hospice = 1*(grepl("34", Position)& Fst_Dt < Lst_Date), 
                Nursing_facility = 1*(grepl("31|32|61", Position)& Fst_Dt < Lst_Date))  
  return(temp)
  
}

# Outpatient visits (by claim)
outpatient = function(data){
  temp = mutate(data, out_new = 1*(grepl("99201|99202|99203|99204|99205", Proc)& Fst_Dt < Lst_Date), 
                out_established = 1*(grepl("99211|99212|99213|99214|99215", Proc)& Fst_Dt < Lst_Date)) 
  return(temp)
} 

# Inpatient visit (By Claim)
inpatient = function(data){
  temp = mutate(data, inpatient_initial_care = 1*(grepl("99221|99222|99223", Proc)& Fst_Dt < Lst_Date), 
                inpatient_subsequent_care = 1*(grepl("99231|99232|99233", Proc)& Fst_Dt < Lst_Date), 
                observation_initial_care = 1*(grepl("99218|99219|99220", Proc)& Fst_Dt < Lst_Date), 
                observation_subsequent_care = 1*(grepl("99224|99225|99226", Proc)& Fst_Dt < Lst_Date)
  ) 
  return(temp)
} 

Paracentesis = function(data){
  temp = mutate(data, Paracentesis = 1*((grepl("^5491", Diag) | grepl("49080|49081|49082|49083|49084", Proc)) & Fst_Dt < Lst_Date 
                                        & (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0)) 
                                        )) 
  return(temp)
}

Dialysis = function(data){
  temp = mutate(data, Dialysis = 1*(grepl("3995|5498|V451|V56", Diag) & Fst_Dt < Lst_Date & 
                                      (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0))
                                      ))
 
  return(temp) 
}

Transplant_evaluation = function(data){
  temp = mutate(data, Transplant_evaluation = 1*(grepl("V427", Diag) & Fst_Dt < Lst_Date & (Fst_Dt >= APP_Dt | is.na(APP_Dt))
                                                 ))
  return(temp)
}

Endoscopy = function(data){
  temp = mutate(data, Endoscopy = 1*(
    (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251", Proc) | grepl("43204|43244|43243", Proc)) 
    & Fst_Dt <= Lst_Date & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & 
      (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0))
    & (Fst_Dt >= APP_Dt | is.na(APP_Dt))
  ))
  
  return(temp)
}

TIPS = function(data){
  temp = mutate(data, TIPS = 1*(grepl("37182|37183|37140", Proc)|grepl("^391", Diag) & Fst_Dt < Lst_Date)) 
  return(temp)
}

Band_ligation = function(data){
  temp = mutate(data, Band_ligation = 1*(grepl("43205|43244|43251|43204|43243",Proc) & Fst_Dt < Lst_Date)) 
  return(temp)
}

varice_band_treat = function(data){
  temp = mutate(data, varice_band_treat = 1*(grepl("43204|43244|43243", Proc) & Fst_Dt < Lst_Date)) 
  return(temp)
}

define = function(data){
  output = data %>% 
    Cirrhosis_all() %>%
    
    Cirrhosis_without() %>%
    
    HE() %>%
    
    Varices() %>% 
    
    Ascites() %>% 
    
    Urinary_tract_infection() %>% 
    
    Cellulitis() %>% 
    
    Alcoholic_cirrhosis() %>%
    
    Alcoholic_liver_disease() %>%
    
    Alcohol_use() %>% 
    
    Hepatitis_C() %>% 
    
    Hepatitis_B() %>%
    
    Non_alcohol() %>%
    
    HCC() %>%
    
    #  Falls() %>% 
    
    # Intracranial_hemorrhage() %>%
    
    Spontaneous_Bacterial_Peritonitis() %>%
    
    #  Fracture() %>%
    
    #  Fracture_all() %>%
    
    #  Pos() %>%
    
    Bacteremia() %>% 
    
    Clostridium() %>%
    
    Cholangitis() %>% 
    
    TIPS() %>% 
    
    outpatient() %>%
    
    inpatient() %>% 
    
    Paracentesis() %>%
    
    Dialysis() %>% 
    
    Pneumonia() %>% 
    
    Sepsis() %>%
    
    Band_ligation() %>%
    
    Transplant_evaluation() %>% 
    
    vaccine() %>%
    
    varice_band_treat() %>%
    
    cancer_screen() %>%
    
    Endoscopy()
  
  return(output)
}

define1 = function(data){
  output = data %>% 
    Cirrhosis_all() %>%

    Cirrhosis_without() %>%
  
    Varices() %>% 
  
    Ascites() %>% 
  
    Spontaneous_Bacterial_Peritonitis() %>%

    TIPS() %>% 

    Alcoholic_cirrhosis() %>%
    
    Alcoholic_liver_disease() %>%
    
    Alcohol_use() %>% 

    Hepatitis_C() %>% 

    Hepatitis_B() %>%

    Non_alcohol()
  
  
  return(output)
}

define2 = function(data){
  output = data %>% 
    
    HCC() %>%
    
    Band_ligation() %>%
    
    vaccine() %>%
    
    varice_band_treat() %>%
    
    cancer_screen() %>%
    
    HE() %>%
    
    outpatient() %>%
    
    inpatient() %>% 
    
    Paracentesis() %>%
    
    Dialysis() %>% 
    
    Pneumonia() %>% 
    
    Sepsis() %>%
    
    Urinary_tract_infection() %>% 
    
    Cellulitis() %>% 
    
    Bacteremia() %>% 
    
    Clostridium() %>%
    
    Cholangitis() %>% 
    
    Transplant_evaluation() %>% 
    
    Endoscopy()
  
  return(output)
}

# Run over all 
for (year in 2009:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  temp_data = mutate(temp_data, Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d"), Lst_Date = as.Date(Lst_Date, "%Y-%m-%d"))
  
  # memory issue
  Patid_list = unique(temp_data$Patid)
  list1 = Patid_list[1 : floor(length(Patid_list)/2)]
  list2 = Patid_list[(floor(length(Patid_list)/2)+1) : length(Patid_list)]
  data1 = filter(temp_data, Patid %in% list1)
  data2 = filter(temp_data, Patid %in% list2)
  rm(temp_data)
  gc()
  
  data1 = define(data1)
  save(data1, file = "Process1.RData")
  rm(data1)
  gc()
  data2 = define(data2)
  gc()
  
  load("Process1.RData")
  temp_data = rbind(data1, data2) 
  rm(data1,data2)
  gc()
  
  save(temp_data, file = paste0("data_def_", year, ".RData")) 
  rm(temp_data, year, name) 
  gc()
  
}  

# Run over all (temp back up)
for (year in 2001:2015){
  name = paste0("data_def_", year, ".RData") 
  load(name) 
  
  temp_data = TIPS(temp_data) 
  temp_data = vaccine(temp_data) 
  temp_data = cancer_screen(temp_data)
  
  save(temp_data, file = paste0("data_def_", year, ".RData")) 
  rm(temp_data, year, name) 
  gc()
  
}  



# Get the date of APP service  
APP_MD_date = data.frame()
APP_date = data.frame() 
MD_date = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData")
  load(name) 
  APP_MD_date = rbind(APP_MD_date, select(filter(temp_data, APP == "1" & MD == "1"), Patid, Fst_Dt)) 
  APP_date = rbind(APP_date, select(filter(temp_data, APP == "1"), Patid, Fst_Dt)) 
  MD_date = rbind(MD_date, select(filter(temp_data, MD == "1"), Patid, Fst_Dt))
  rm(temp_data) 
  gc()
}

save.image("APP&MD dates.RData")
rm(APP_MD_date, APP_date, MD_date)
gc()


# get the date of Hepatology and Gastro service 
Hepatology_date = data.frame() 
Gastro_date = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData")
  load(name)
  
  Hepatology_date = rbind(Hepatology_date, select(filter(temp_data, Hepatology == 1), Patid, Fst_Dt))
  Gastro_date = rbind(Gastro_date, select(filter(temp_data, Gastro == 1), Patid, Fst_Dt)) 
  
  rm(name, year, temp_data) 
  gc()
}

save(list = c("Gastro_date", "Hepatology_date"), file = "Hep & Gastro date.RData")

load("APP&MD dates.RData")
APP_date = APP_date %>%  # the first App service date, all the outcomes in table 2 - 4 are defined if it happened after this date 
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  group_by(Patid) %>% 
  arrange(Patid, Fst_Dt) %>%   
  dplyr::summarise(APP_Dt = first(Fst_Dt))
rm(APP_MD_date, MD_date)

for (year in 2001:2015){ # merge into the censor data
  name = paste0("data_censor_", year, ".RData")
  load(name)
  
  temp_data = merge(temp_data, APP_date, all.x = T, by = "Patid")
  
  save(temp_data, file = paste0("data_censor_", year, ".RData") ) 
  rm(name, year, temp_data) 
  gc()
}



# Combining all the confinement data 
liver_conf = data.frame()
for (year in 2004:2015){
  loc = paste0("X:/Shengchen Hao/Tapper Liver/Confinement Files/liver_conf_", year, ".sas7bdat") 
  temp_data = read_sas(loc) 
  liver_conf = rbind(liver_conf, temp_data) 
  rm(temp_data) 
  gc()
}
save(liver_conf, file = "liver_conf.RData")



# Special Censor patients with first endoscopy as inpatient 
Censor_endoscopy_patid = c() 
temp_Endoscopy = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  temp_Endoscopy = rbind(temp_Endoscopy, dplyr::select(filter(temp_data, grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251", Proc))
                                                       , Patid, Fst_Dt, Proc, Lst_Date)) 
  rm(name, year, temp_data) 
  gc()
}
save(temp_Endoscopy, file = "temp_Endoscopy.RData")
temp_Endoscopy = distinct(temp_Endoscopy)
temp_Endoscopy = temp_Endoscopy %>% 
  group_by(Patid) %>% 
  arrange(Patid, Fst_Dt) %>% 
  summarise(Proc = first(Proc), Lst_Date = first(Lst_Date), Fst_Dt = first(Fst_Dt))
temp_Endoscopy = inpatient(temp_Endoscopy)  # run inpatient function (by claim)

Censor_endoscopy_patid = unique(filter(temp_Endoscopy, inpatient_initial_care == 1 | inpatient_subsequent_care == 1 | observation_initial_care == 1| observation_subsequent_care == 1)$Patid)
rm(temp_Endoscopy, inpatient)

save(Censor_endoscopy_patid, file = "censor_Endoscopy.RData")


# find the date of TIPS or bleeding  
temp_TIPS = data.frame() 
temp_bleed = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  temp_data = mutate(temp_data, Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d"), 
                                Lst_Date = as.Date(Lst_Date, "%Y-%m-%d"))
  
  temp_TIPS = rbind(temp_TIPS, dplyr::select(filter(temp_data, Fst_Dt < Lst_Date & (grepl("37182|37183|37140", Proc)|grepl("^391", Diag))), Patid, Fst_Dt
                                             ))
  temp_bleed = rbind(temp_bleed, dplyr::select(filter(temp_data, Fst_Dt < Lst_Date & grepl("^4560", Diag)), Patid, Fst_Dt
                                               ))
  
  rm(name,year,temp_data) 
  gc()
}

temp_TIPS = distinct(temp_TIPS) 
temp_bleed = distinct(temp_bleed) 

temp_TIPS = temp_TIPS %>% 
  group_by(Patid) %>% 
  dplyr::mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  dplyr::arrange(Patid, Fst_Dt) %>% 
  summarise(Fst_Dt = first(Fst_Dt)) 

temp_bleed = temp_bleed %>% 
  group_by(Patid) %>% 
  dplyr::mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  dplyr::arrange(Patid, Fst_Dt) %>% 
  summarise(Fst_Dt = first(Fst_Dt))
  
  
save(list = c("temp_TIPS", "temp_bleed"), file = "TIPS bleed date.RData") # This is used in building person_year table


# find the outcome of Varices screening (endoscopy)
varice_screen = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  temp_data = inpatient(temp_data)
  temp_data = outpatient(temp_data)
  temp_data = dplyr::mutate(temp_data, Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d"))
  
  varice_screen = rbind(varice_screen, filter(temp_data, (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251", Proc) | grepl("43204|43244|43243", Proc)) 
                                              & Fst_Dt <= Lst_Date & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & 
                                                (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0))
                                              & (Fst_Dt >= APP_Dt | is.na(APP_Dt))
                                              ))
  
  rm(temp_data,name,year)
  gc()
}

save(varice_screen, file = "Varices screening.RData") 


# Data prepare for outpatient visits
data_time_Proc = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  temp_data = distinct(dplyr::select(temp_data, Patid, Fst_Dt, Proc, Lst_Date, APP_Dt)) # including the APP date
  data_time_Proc = rbind(data_time_Proc, temp_data)
  
  rm(temp_data,name,year)
  gc()
}
save(data_time_Proc, file = "Time vary variable prepare CPT codes.Rdata")
rm(data_time_Proc) 
gc()


# time varying variable 


# APP/MD visit ratio (Not use for now) ####################################
# Provider data for APP/MD visit (Not use for now) 
data_time_Provider = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  temp_data = distinct(dplyr::select(temp_data, Patid, Fst_Dt, Provider, Lst_Date))
  gc()
  data_time_Provider = rbind(data_time_Provider, temp_data)
  
  rm(temp_data,name,year)
  gc()
}
save(data_time_Provider, file = "Time vary variable prepare Provider codes.Rdata")


data_time_Provider = distinct(data_time_Provider)
data_time_Provider = APP(data_time_Provider)
data_time_Provider = Hepatology(data_time_Provider)

data_time_Provider = data_time_Provider %>% 
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  arrange(Patid, Fst_Dt) %>%
  group_by(Patid) %>% 
  mutate(APP_count = cumsum(APP), 
         MD_count = cumsum(MD)) %>% 
  ungroup()


data_time_Provider$Hepatology =NULL 
data_time_Provider$Gastro = NULL

data_time_Provider = mutate(data_time_Provider, APP_MD_ratio = APP_count/MD_count) 
data_time_Provider = mutate(data_time_Provider, APP_MD_ratio = ifelse(APP_MD_ratio == "NaN"|APP_MD_ratio == Inf, NA, APP_MD_ratio)) # deal with missing value NaN
data_time_Provider = data_time_Provider %>% 
  group_by(Patid) %>% 
  mutate(max_ratio = max(APP_MD_ratio, na.rm = T)) %>% 
  ungroup()
save(data_time_Provider, file = "APP MD ratio.RData")
#############################################################################

# outpatient visit 
load("Time vary variable prepare CPT codes.Rdata") 
data_time_Proc = distinct(data_time_Proc)
data_time_Proc = outpatient(data_time_Proc)

data_time_Proc = data_time_Proc %>% 
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d"), 
         outpatient = 1*((out_established == 1 | out_new == 1) & (is.na(APP_Dt) | Fst_Dt >= APP_Dt))
         ) %>% 
  select(Patid, Fst_Dt, outpatient) %>% 
  distinct() %>%
  arrange(Patid, Fst_Dt) %>% 
  group_by(Patid) %>% 
  mutate(outpatient_count = cumsum(outpatient)) %>% 
  ungroup()

data_time_Proc = data_time_Proc %>% 
  group_by(Patid) %>% 
  mutate(max_visit = max(outpatient_count, na.rm = T)) %>% 
  ungroup()
save(data_time_Proc, file = "outpatient visit count.RData")

##################### Exclude those who received the outcome before the APP ################ 
Screen_cancer_exclude = c()
Endoscopy_exclude = c() 
Influenza_exclude = c() 
ABvaccine_exclude = c() 
Hosp_exclude = c() 

for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData")
  load(name)
  
  temp_data = inpatient(temp_data) 
  temp_data = outpatient(temp_data)
  
  Screen_cancer_exclude = c(Screen_cancer_exclude, unique(filter(temp_data, (grepl("74177|74178|74160|74170", Proc) | grepl("74183", Proc) | grepl("76700|76705", Proc)) & Fst_Dt < Lst_Date & Fst_Dt < APP_Dt)$Patid))
  Endoscopy_exclude = c(Endoscopy_exclude, unique(filter(temp_data, (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251", Proc) | grepl("43204|43244|43243", Proc)) 
                                                         & Fst_Dt <= Lst_Date & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & Fst_Dt < APP_Dt &
                                                           (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0))
                                                         )$Patid))
  Influenza_exclude = c(Influenza_exclude, unique(filter(temp_data, grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc)
                                                         & Fst_Dt < Lst_Date & Fst_Dt < APP_Dt)$Patid))
  ABvaccine_exclude = c(ABvaccine_exclude, unique(filter(temp_data, (grepl("90632|90633|90634|90636", Proc) | grepl("90739|90740|90743|90747|90636|90748", Proc)) & Fst_Dt < Lst_Date & Fst_Dt < APP_Dt)$Patid))
  Hosp_exclude = c(Hosp_exclude, unique(filter(temp_data, (inpatient_initial_care == 1 | inpatient_subsequent_care == 1 | observation_initial_care == 1| observation_subsequent_care == 1) & Fst_Dt < APP_Dt)$Patid))
  
  rm(name, year, temp_data) 
  gc()
}

rm(outpatient, inpatient)

Influenza_exclude = unique(Influenza_exclude) 
Screen_cancer_exclude = unique(Screen_cancer_exclude)
Hosp_exclude = unique(Hosp_exclude)
Endoscopy_exclude = unique(Endoscopy_exclude)
ABvaccine_exclude = unique(ABvaccine_exclude)

save.image("Exclude Patids.RData")
