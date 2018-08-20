# New Table One 
setwd("X:/Shengchen Hao/Tapper Liver/R file/new/GI_proj") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 

Patid_High_Std = unique(filter(data_total, High_Std == 1)$Patid) 
Person_year = mutate(Person_year, High_Std = 1*(Patid %in% Patid_High_Std))

liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 
temp = select(liver_member_fixed, Patid, Race, Gdr_Cd)
temp = distinct(temp) %>% 
  arrange(Patid, Gdr_Cd, Race) %>% 
  group_by(Patid) %>% 
  summarise(Race = first(Race), 
            Sex = first(Gdr_Cd))

Person_year = merge(Person_year, temp, by = "Patid", all.x = T)
table(Person_year$Sex, Person_year$High_Std)
table(Person_year$Race, Person_year$High_Std)
rm(temp)

# Charlson Index 
load("charlson index.RData")
index = dplyr::select(charlson9, Patid, index)
Person_year = merge(Person_year, index, by = "Patid", all.x = T)
rm(index, charlson9)
table(Person_year$index, Person_year$High_Std)

# Others 
AC_patid = c()
Hepatitis_C_patid = c()
Hepatitis_B_patid = c()
Non_alcohol_patid = c() 
Cirrhosis_complication_patid = c() 
HE_patid = c() 
Ascites_patid = c() 
Varices_patid = c() 
HCC_patid = c() 
Hepatology_patid = c()

# Only count before and one year after GI visit -----------------------------------------------------------------------------

AC_patid = c(AC_patid, unique(filter(data_total, Alcoholic_cirrhosis == 1 | Alcohol_use == 1 | Alcoholic_liver_disease == 1)$Patid))
Hepatitis_C_patid = c(Hepatitis_C_patid, unique(filter(data_total, Hepatitis_C == 1)$Patid))
Non_alcohol_patid = c(Non_alcohol_patid, unique(filter(data_total, Non_alcohol == 1)$Patid))
Cirrhosis_complication_patid = c(Cirrhosis_complication_patid, unique(filter(data_total, Cirrhosis_comp == 1)$Patid))
HE_patid = c(HE_patid, unique(filter(data_total, HE == 1)$Patid))
Ascites_patid = c(Ascites_patid, unique(filter(data_total, Ascites == 1)$Patid))
Varices_patid = c(Varices_patid, unique(filter(data_total, Varices == 1)$Patid))
HCC_patid = c(HCC_patid, unique(filter(data_total, HCC == 1)$Patid)) 
Hepatology_patid = c(Hepatology_patid, unique(filter(data_total, Hepatology == 1)$Patid))

# ---------------------------------------------------------------------------------------------------------------------------

# count all the time 

for (year in 2001:2015){
  name = paste0("data_def_", year, ".RData")
  load(name) 
  
  AC_patid = c(AC_patid, unique(filter(temp_data, Alcoholic_cirrhosis == 1 | Alcohol_use == 1 | Alcoholic_liver_disease == 1)$Patid))
  Hepatitis_C_patid = c(Hepatitis_C_patid, unique(filter(temp_data, Hepatitis_C == 1)$Patid))
  Hepatitis_B_patid = c(Hepatitis_B_patid, unique(filter(temp_data, Hepatitis_B == 1)$Patid))
  Non_alcohol_patid = c(Non_alcohol_patid, unique(filter(temp_data, Non_alcohol == 1)$Patid))
  Cirrhosis_complication_patid = c(Cirrhosis_complication_patid, unique(filter(temp_data, Cirrhosis_comp == 1)$Patid))
  HE_patid = c(HE_patid, unique(filter(temp_data, HE == 1)$Patid))
  Ascites_patid = c(Ascites_patid, unique(filter(temp_data, Ascites == 1)$Patid))
  Varices_patid = c(Varices_patid, unique(filter(temp_data, Varices == 1)$Patid))
  HCC_patid = c(HCC_patid, unique(filter(temp_data, HCC == 1)$Patid)) 
  Hepatology_patid = c(Hepatology_patid, unique(filter(temp_data, Hepatology == 1)$Patid))
  
  rm(temp_data, name, year) 
  gc()
  
}

# ---------------------------------------------------------------------------------------------------------------------------


Person_year = mutate(Person_year, AC = 1*(Patid %in% AC_patid), 
                     Hepatitis_C = 1*(Patid %in% Hepatitis_C_patid),
                     Hepatitis_B = 1*(Patid %in% Hepatitis_B_patid),
                     Non_alcohol = 1*(Patid %in% Non_alcohol_patid), 
                     Cirrhosis_complication = 1*(Patid %in% Cirrhosis_complication_patid), 
                     HE = 1*(Patid %in% HE_patid), 
                     Ascites = 1*(Patid %in% Ascites_patid), 
                     Varices = 1*(Patid %in% Varices_patid), 
                     HCC = 1*(Patid %in% HCC_patid), 
                     Hepatology = 1*(Patid %in% Hepatology_patid))

rm(Mean_survival, Patient_included_num, liver_member_fixed, High_Std_num)

table(Person_year$AC, Person_year$High_Std)
table(Person_year$Hepatitis_C, Person_year$High_Std)
table(Person_year$Non_alcohol, Person_year$High_Std)
table(Person_year$Cirrhosis_complication, Person_year$High_Std)
table(Person_year$HE, Person_year$High_Std)
table(Person_year$Ascites, Person_year$High_Std)
table(Person_year$Varices, Person_year$High_Std)
table(Person_year$HCC, Person_year$High_Std)
table(Person_year$Hepatology, Person_year$High_Std)


temp = matrix(table(Person_year$Hepatology, Person_year$High_Std), nrow = 2)
chisq.test(temp)

summary(Person_year$Age)
summary(filter(Person_year, High_Std == 1)$Age)
summary(filter(Person_year, High_Std == 0)$Age)
t.test(filter(Person_year, High_Std == 1)$Age, filter(Person_year, High_Std == 0)$Age)










