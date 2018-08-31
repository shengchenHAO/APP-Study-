setwd("X:/Shengchen Hao/Tapper Liver/R file/new/GI_proj") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 
library(coin) #Permutation test

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
Patid_total = Patid_total[Patid_total %!in% Exclude_coverage_patid & Patid_total %!in% Exclude_Bleeding_patid & 
                            Patid_total %!in% Exclude_HCC_patid & Patid_total %!in% Exclude_Transplant_patid & Patid_total %in% GI_first_date$Patid]
rm(liver_member_fixed)
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
Gastro_unique_ID = select(filter(liver_provider, Prov %in% Gastro_id), Prov_Unique, Prov)
rm(liver_provider, code)

GI_provider = data.frame()
for (year in 2001:2015){ # get the first GI's id from Medical data
  name = paste0("X:/Shengchen Hao/Tapper Liver/Medical Files/liver_med_", year, ".sas7bdat") 
  med = read_sas(name)  
  
  med =select(med, Patid, Dstatus, Fst_Dt, Pos, Proc_Cd, Prov)
  colnames(med) = c("Patid", "Dstatus", "Fst_Dt", "Position", "Proc", "Provider")
  GI_provider = rbind(GI_provider, select(filter(med, Provider %in% Gastro_id), Patid, Fst_Dt, Provider))
 
  rm(med, name, year) 
  gc()
} 
save(GI_provider, file = "GI_History.RData")

GI_providerID = GI_provider %>% 
  distinct() %>%
  arrange(Patid, Fst_Dt) %>% 
  group_by(Patid) %>% 
  summarise(Gastro_ID = first(Provider))
save(GI_providerID, file = "GI_ID.RData")

load("one_year_data.RData") # Run by Flux
data_total = merge(data_total, GI_providerID, by = "Patid", all.x = T)
save(data_total, file = "data_with_GI.RData")

# Delete the columns that not using ----------------------------------------------------------
data_total$SBRT = NULL 
data_total$TACE = NULL 
data_total$RFA = NULL 
data_total$partial_hep = NULL 
data_total$liver_resection = NULL 
data_total$Cryo = NULL
data_total$Ablation = NULL 
data_total$Paracentesis = NULL 
data_total$Dialysis = NULL
data_total$out_new = NULL 
data_total$out_established = NULL
data_total$Transplant_evaluation = NULL 
data_total$inpatient_initial_care = NULL 
data_total$inpatient_subsequent_care = NULL
data_total$observation_initial_care = NULL
data_total$observation_subsequent_care = NULL
save(data_total, file = "one_year_data.RData")
# ---------------------------------------------------------------------------------------------

# Count the total number of patient seen by each GI -------------------------------------------

GI_Included_ID = unique(data_total$Gastro_ID)
GI_patient_num = GI_provider %>% 
  dplyr::filter(Provider %in% GI_Included_ID) %>%
  select(Patid, Provider) %>% 
  merge(y = Gastro_unique_ID, by.x = "Provider", by.y = "Prov", all.x = T) %>%
  group_by(Prov_Unique) %>% 
  summarise(Patient_Num = n_distinct(Patid))
  
# ---------------------------------------------------------------------------------------------
# Define new variables (not based on APP date)
load("one_year_data.RData")

outpatient = function(data){
  temp = mutate(data, out_new = 1*(grepl("99201|99202|99203|99204|99205", Proc)), 
                out_established = 1*(grepl("99211|99212|99213|99214|99215", Proc))
                ) 
  return(temp)
} 
inpatient = function(data){
  temp = mutate(data, inpatient_initial_care = 1*(grepl("99221|99222|99223", Proc)), 
                inpatient_subsequent_care = 1*(grepl("99231|99232|99233", Proc)), 
                observation_initial_care = 1*(grepl("99218|99219|99220", Proc)), 
                observation_subsequent_care = 1*(grepl("99224|99225|99226", Proc))
  ) 
  return(temp)
} 

Endoscopy = function(data){
  temp = mutate(data, Endoscopy = 1*(
    (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251", Proc) | grepl("43204|43244|43243", Proc)) 
    & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & 
    (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0))
    ))
  
  return(temp)
}
cancer_screen = function(data){
  temp = mutate(data, CT = 1*(grepl("74177|74178|74160|74170", Proc) & (Fst_Dt < Cancer_Diag_Date | is.na(Cancer_Diag_Date))),
                MRI = 1*(grepl("74183", Proc) & (Fst_Dt < Cancer_Diag_Date | is.na(Cancer_Diag_Date))), 
                Ultrasound = 1*(grepl("76700|76705", Proc) & (Fst_Dt < Cancer_Diag_Date | is.na(Cancer_Diag_Date)))
                ) 
  return(temp)
} 
data_total = data_total %>% 
  outpatient() %>% 
  inpatient() %>% 
  Endoscopy() %>% 
  cancer_screen()

save(data_total, file = "one_year_data.RData")
#  ---------------------------------------------------------------------------------------------

AB_vaccine_patid = unique(filter(data_total, A_vaccine == 1 | B_vaccine == 1)$Patid)
Screen_cancer_patid = unique(filter(data_total, CT == 1 | MRI == 1 | Ultrasound == 1)$Patid)
Endoscopy_patid = unique(filter(data_total, Endoscopy == 1)$Patid)
  
data_total = mutate(data_total, GI_vaccine = 1*(Patid %in% AB_vaccine_patid), 
                    GI_Screen = 1*(Patid %in% Screen_cancer_patid), 
                    GI_Endoscopy = 1*(Patid %in% Endoscopy_patid)
                    )
data_total = mutate(data_total, High_Std = 1*(GI_Screen == 1 & GI_Endoscopy == 1)) # High standard, may change 

data_total = merge(x = data_total, y = Gastro_unique_ID, by.x = "Gastro_ID", by.y = "Prov", all.x = T) # run in flux
data_total$Provider = NULL 
save(data_total, file = "one_year_data.RData")

High_Std_num = data_total %>% 
  select(Patid, Prov_Unique, High_Std) %>% 
  filter(High_Std == 1) %>%
  distinct() %>% 
  group_by(Prov_Unique) %>% 
  summarise(High_Std_num = n_distinct(Patid))

Patient_included_num = data_total %>% 
  select(Patid, Prov_Unique) %>% 
  group_by(Prov_Unique) %>% 
  summarise(Patient_included_num = n_distinct(Patid))
  
GI_patient_num = merge(GI_patient_num, High_Std_num, all.x = T, by = "Prov_Unique")
GI_patient_num = mutate(GI_patient_num, High_Std_num = ifelse(is.na(High_Std_num), 0, High_Std_num))
GI_patient_num = merge(GI_patient_num, Patient_included_num, all.x = T, by = "Prov_Unique")
GI_patient_num = mutate(GI_patient_num, Prop = High_Std_num/Patient_included_num) # this is the table for GI perspective


#  ---------------------------------------------------------------------------------------------
#  Create Table by Patid
load("person_year.RData")
load("GI_First_Date.RData")
Patid_total = unique(data_total$Patid)
Person_year = filter(Person_year, Patid %in% Patid_total)
Person_year = merge(x = Person_year, y = GI_providerID, by = "Patid", all.x = T)  # Provider ID,but not unique
Person_year = merge(x = Person_year, y = Gastro_unique_ID, by.x = "Gastro_ID", by.y = "Prov", all.x = T)  # merge the unique GI ID
Person_year = merge(x= Person_year, y = GI_first_date, all.x = T, by = "Patid")  
Person_year = mutate(Person_year, Survival_Time = Lst_Date - GI_first_date) # survival time is is last date - first GI date

Mean_survival = Person_year %>% 
  group_by(Prov_Unique) %>%
  summarise(Mean_survival = mean(as.numeric(Survival_Time)))

GI_patient_num = merge(GI_patient_num, Mean_survival, by = "Prov_Unique", all.x = T)

library(ggplot2)
qplot(x = Prop, y = Mean_survival, data = GI_patient_num)
qplot(x = Patient_Num, y = Mean_survival, data = GI_patient_num, xlim = c(0, 1000))


# T - Test 
GI_patient_num = mutate(GI_patient_num, Experienced = 1*(Patient_Num >= 25))

Experienced = sapply(c(1:400), function(Threshold){mean(filter(GI_patient_num, Patient_Num >= Threshold)$Mean_survival)})
UnExperienced = sapply(c(1:400), function(Threshold){mean(filter(GI_patient_num, Patient_Num < Threshold)$Mean_survival)})
temp_plot = data.frame(x = c(1:400), Experienced = Experienced, UnExperienced = UnExperienced)
rm(Experienced,UnExperienced)
ggplot(temp_plot, aes(x)) + 
  geom_line(aes(y = Experienced, colour = "Experienced")) + 
  geom_line(aes(y = UnExperienced, colour = "UnExperienced")) + 
  xlab("Threshold") + ylab("Mean Survival Time")


qplot(GI_patient_num$Prop) +stat_bin(binwidth = 0.05)


#  ---------------------------------------------------------------------------------------------
# multi-level treatment variable (0/3, 1/3, 2/3, 3/3)
data_total = data_total %>% 
  mutate(Treatment = GI_vaccine+GI_Screen+GI_Endoscopy) 
temp = data_total %>% 
  select(Patid, Treatment) %>% 
  distinct()

Person_year = merge(Person_year, temp, all.x = T, by = "Patid") 
rm(temp)

High_quality = Person_year %>% 
  group_by(Prov_Unique) %>% 
  summarise(Quality_measure = sum(Treatment)/3) %>% 
  merge(y = select(GI_patient_num, Prov_Unique, Patient_Num, Patient_included_num), by.x = "Prov_Unique", by.y = "Prov_Unique", all.x = T) %>%
  mutate(Quality_measure = Quality_measure/Patient_included_num)
# Quality measure
qplot(High_quality$Quality_measure) +stat_bin(binwidth = 0.01)
qplot(y = sort(High_quality$Quality_measure)) + ylab("Quality")
summary(High_quality$Quality_measure)
summary(filter(High_quality, Patient_Num >= 25)$Quality_measure)

temp = select(High_quality, Patient_Num, Quality_measure)
temp = mutate(temp, Exp = 1*(Patient_Num >= 25))
independence_test(Quality_measure ~ Exp, data = temp)
rm(temp)

# GI volume 
qplot(filter(High_quality, Patient_Num >= 25 & Patient_Num <= 700)$Patient_Num, binwidth = 5) + xlab("Number of Patient visit(GI volume)")
qplot(y = sort(High_quality$Patient_Num)) + ylab("GI Volume") + xlab("GIs")
summary(filter(High_quality, Patient_Num >= 25)$Patient_Num)

#  ---------------------------------------------------------------------------------------------
save.image("data prepare.RData")
