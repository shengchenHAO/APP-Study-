library(dplyr) 
library(stringr) 
library(tidyr) 

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

save.image("Flux_output.RData")