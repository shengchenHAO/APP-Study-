# set working directory 
setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr)

liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 
liver_ses = read_sas("X:/Shengchen Hao/Tapper Liver/Member Files/liver_ses.sas7bdat")
load("APP&MD dates.RData") 
APP_patid = unique(APP_date$Patid)
rm(MD_date, APP_date,APP_MD_date)
# filter patients that have at least 3 month enrollment 
load("person_year.RData") 
Patid_Total = unique(dplyr::filter(Person_year, Person_year > 0.25)$Patid)

# Sex 
temp = liver_member_fixed %>%
  dplyr::mutate(APP = 1*(Patid %in% APP_patid)) %>% 
  dplyr::group_by(Patid) %>% 
  arrange(Patid, Gdr_Cd) %>%
  dplyr::summarise(APP = first(APP), Gdr_Cd = first(Gdr_Cd))
  
table(temp$Gdr_Cd,temp$APP)


# race  
temp = liver_member_fixed %>%
  dplyr::mutate(APP = 1*(Patid %in% APP_patid)) %>% 
  dplyr::group_by(Patid) %>% 
  arrange(Patid, Race) %>%
  dplyr::summarise(APP = first(APP), Race = first(Race))

table(temp$Race,temp$APP)
rm(temp)

# Education 
load("APP_patid_pld.RData") 
liver_ses = dplyr::mutate(liver_ses, APP = 1*(PATID %in% APP_patid_old))
table(liver_ses$D_EDUCATION_LEVEL_CODE, liver_ses$APP)


# diseases 
AC_patid = c()
Hepatitis_C_patid = c()
Non_alcohol_patid = c() 
Cirrhosis_complication_patid = c() 
HE_patid = c() 
Ascites_patid = c() 
Varices_patid = c() 
HCC_patid = c() 
Gastro_patid = c()
Gastro_only_patid = c()
Hepatology_patid = c()
Transplant_Evaluation_patid = c()
SBP_patid = c()
TIPS_patid = c()
Pneumonia_patid = c() 
Sepsis_patid = c()
Urinary_tract_infection_patid = c()
Cellulitis_patid = c()
Bacteremia_patid = c() 
Clostridium_patid = c()
Cholangitis_patid = c()
Paracentesis_patid = c() 
Dialysis_patid = c() 


for (year in 2001:2015){
  name = paste0("data_def_", year, ".RData")
  load(name) 
  
  AC_patid = c(AC_patid, unique(filter(temp_data, Alcoholic_cirrhosis == 1 | Alcohol_use == 1 | Alcoholic_liver_disease == 1)$Patid))
  Hepatitis_C_patid = c(Hepatitis_C_patid, unique(filter(temp_data, Hepatitis_C == 1)$Patid))
  Non_alcohol_patid = c(Non_alcohol_patid, unique(filter(temp_data, Non_alcohol == 1)$Patid))
  Cirrhosis_complication_patid = c(Cirrhosis_complication_patid, unique(filter(temp_data, grepl("4560|78959|78951|5722|56723|5724", Diag))$Patid))
  HE_patid = c(HE_patid, unique(filter(temp_data, HE == 1)$Patid))
  Ascites_patid = c(Ascites_patid, unique(filter(temp_data, Ascites == 1)$Patid))
  Varices_patid = c(Varices_patid, unique(filter(temp_data, Varices == 1)$Patid))
  HCC_patid = c(HCC_patid, unique(filter(temp_data, HCC == 1)$Patid)) 
  Gastro_patid = c(Gastro_patid, unique(filter(temp_data, Gastro == 1)$Patid))
  Gastro_only_patid = c(Gastro_only_patid, unique(filter(temp_data, Gastro == 1 & Hepatology == 0)$Patid))
  Hepatology_patid = c(Hepatology_patid, unique(filter(temp_data, Hepatology == 1)$Patid))
  Transplant_Evaluation_patid = c(Transplant_Evaluation_patid, unique(filter(temp_data, Transplant_evaluation == 1)$Patid)) 
  SBP_patid = c(SBP_patid, unique(filter(temp_data, Spontaneous_Bacterial_Peritonitis == 1)$Patid)) 
  TIPS_patid = c(TIPS_patid, unique(filter(temp_data, TIPS == 1 & (Fst_Dt >= APP_Dt | is.na(APP_Dt)))$Patid)) # Procdure
  Pneumonia_patid = c(Pneumonia_patid, unique(filter(temp_data, Pneumonia == 1)$Patid)) 
  Sepsis_patid = c(Sepsis_patid, unique(filter(temp_data, Sepsis == 1)$Patid))
  Urinary_tract_infection_patid = c(Urinary_tract_infection_patid, unique(filter(temp_data, Urinary_tract_infection == 1)$Patid))
  Cellulitis_patid = c(Cellulitis_patid, unique(filter(temp_data, Cellulitis == 1)$Patid))
  Bacteremia_patid = c(Bacteremia_patid, unique(filter(temp_data, Bacteremia == 1)$Patid)) 
  Clostridium_patid = c(Clostridium_patid, unique(filter(temp_data, Clostridium == 1)$Patid))
  Cholangitis_patid = c(Cholangitis_patid, unique(filter(temp_data, Cholangitis == 1)$Patid))
  Paracentesis_patid = c(Paracentesis_patid, unique(filter(temp_data, Paracentesis == 1 & (Fst_Dt >= APP_Dt | is.na(APP_Dt)))$Patid)) #Procdure
  Dialysis_patid = c(Dialysis_patid, unique(filter(temp_data, Dialysis == 1 & (Fst_Dt >= APP_Dt | is.na(APP_Dt)))$Patid)) #Procdure
               
  rm(temp_data, name, year) 
  gc()
  
}



save.image("table1.Rdata")

data_table1 = data.frame(unique(liver_member_fixed$Patid)) 
colnames(data_table1) = "Patid" 
data_table1 = data_table1 %>%
  mutate(APP = 1*(Patid %in% APP_patid), 
         AC = 1*(Patid %in% AC_patid), 
         Hepatitis_C = 1*(Patid %in% Hepatitis_C_patid), 
         NAC = 1*(Patid %in% Non_alcohol_patid), 
         Cirrhosis_complication = 1*(Patid %in% Cirrhosis_complication_patid),
         HE = 1*(Patid %in% HE_patid), 
         Ascites = 1*(Patid %in% Ascites_patid), 
         Varices = 1*(Patid %in% Varices_patid), 
         HCC = 1*(Patid %in% HCC_patid)
         )
table(data_table1$AC, data_table1$APP)
table(data_table1$Hepatitis_C, data_table1$APP)
table(data_table1$NAC, data_table1$APP)
table(data_table1$Cirrhosis_complication, data_table1$APP)
table(data_table1$HE, data_table1$APP)
table(data_table1$Ascites, data_table1$APP)
table(data_table1$Varices, data_table1$APP)
table(data_table1$HCC, data_table1$APP)


# Hepatology and Gastro
data_table1 = data_table1 %>% 
  mutate(Gastro = 1*(Patid %in% Gastro_patid), 
         Hepatology = 1*(Patid %in% Hepatology_patid)
         )

table(data_table1$Gastro, data_table1$APP)
table(data_table1$Hepatology, data_table1$APP) 

# age  
load("person_year.RData") 
summary(filter(Person_year, Patid %in% APP_patid)$Age)
'%!in%' = Negate('%in%')
summary(filter(Person_year, Patid %!in% APP_patid)$Age)

# Person year of follow up 
summary(filter(Person_year, Patid %in% APP_patid)$Person_year)
summary(filter(Person_year, Patid %!in% APP_patid)$Person_year)



# APP/MD VISTI RATIO (Not use)
load("APP MD ratio.RData")
temp = data_time_Provider %>% 
  group_by(Patid) %>% 
  summarise(max_ratio = first(max_ratio))
summary(temp$max_ratio)
rm(temp)

# outpatient visit 
load("outpatient visit count.RData")
temp = data_time_Proc %>% 
  group_by(Patid) %>% 
  summarise(max_visit = first(max_visit))

summary(filter(temp, Patid %in% APP_patid))
summary(filter(temp, Patid %!in% APP_patid))
rm(temp)
# Summary statistics 
Person_year = Person_year %>%
  mutate(Status = ifelse(Death_date != Lst_Date| is.na(Death_date), 1, 2)) %>% 
  mutate(Status = ifelse(is.na(Death_date) == F & Death_date <= Lst_Date + 90, 2, Status)) %>% 
  mutate(Status = ifelse(is.na(Trans_Dt)==F, 3, Status))
summary(as.factor(Person_year$Status))
summary(filter(Person_year, Status == 1)$Person_year)
summary(filter(Person_year, Status == 2)$Person_year)
summary(filter(Person_year, Status == 3)$Person_year)
