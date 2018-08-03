setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 

liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 

HCC_patid = c()
SBRT_patid = c()
TACE_patid = c() 
Total_ablation_patid = c() 
RFA_patid = c() 
Cryo_patid = c() 
Hepatectomy_patid = c() 
liver_transplant_patid = c() 

for (year in 2001:2015){
  name = paste0("data_def_", year, ".RData") 
  load(name) 
  
  HCC_patid = c(HCC_patid, unique(filter(temp_data, HCC == 1)$Patid))
  SBRT_patid = c(SBRT_patid, unique(filter(temp_data, SBRT == 1)$Patid))
  TACE_patid = c(TACE_patid, unique(filter(temp_data, TACE == 1)$Patid)) 
  Total_ablation_patid = c(Total_ablation_patid, unique(filter(temp_data, Ablation ==1 | RFA == 1 | Cryo == 1)$Patid))
  RFA_patid = c(RFA_patid, unique(filter(temp_data, RFA == 1)$Patid))
  Cryo_patid = c(Cryo_patid, unique(filter(temp_data, Cryo == 1)$Patid)) 
  Hepatectomy_patid = c(Hepatectomy_patid, unique(filter(temp_data, (grepl("47120", Proc) & Fst_Dt < Lst_Date) | partial_hep == 1)$Patid))
  liver_transplant_patid = c(liver_transplant_patid, unique(filter(temp_data, grepl("^505", Diag)|grepl("47135|47136", Proc))$Patid))
  
  rm(temp_data, year, name) 
  gc()
  
}  


Count_table = data.frame(unique(liver_member_fixed$Patid)) 
colnames(Count_table) = "Patid" 

Count_table1 = Count_table %>%  
  mutate(HCC = 1*(Patid %in% HCC_patid), 
         SBRT = 1*(Patid %in% SBRT_patid), 
         TACE = 1*(Patid %in% TACE_patid),
         Total_ablation = 1*(Patid %in% Total_ablation_patid), 
         RFA = 1*(Patid %in% RFA_patid), 
         Cryo = 1*(Patid %in% Cryo_patid), 
         Hepatectomy = 1*(Patid %in% Hepatectomy_patid), 
         liver_transplant = 1*(Patid %in% liver_transplant_patid)
         )

Count_table1 = filter(Count_table1, HCC == 1)
save(Count_table1, file = "Count table1.RData")

apply(Count_table1, 2, sum)


# time senstive version 
SBRT_patid = c()
TACE_patid = c() 
Total_ablation_patid = c() 
RFA_patid = c() 
Cryo_patid = c() 
Hepatectomy_patid = c() 
liver_transplant_patid = c() 

for (year in 2001:2015){
  name = paste0("data_def_", year, ".RData") 
  load(name) 
  
  SBRT_patid = c(SBRT_patid, unique(filter(temp_data, SBRT == 1 & HCC == 1)$Patid))
  TACE_patid = c(TACE_patid, unique(filter(temp_data, TACE == 1 & HCC == 1)$Patid)) 
  Total_ablation_patid = c(Total_ablation_patid, unique(filter(temp_data, (Ablation ==1 | RFA == 1 | Cryo == 1) & HCC == 1 )$Patid))
  RFA_patid = c(RFA_patid, unique(filter(temp_data, RFA == 1 & HCC == 1)$Patid))
  Cryo_patid = c(Cryo_patid, unique(filter(temp_data, Cryo == 1 & HCC == 1)$Patid)) 
  Hepatectomy_patid = c(Hepatectomy_patid, unique(filter(temp_data, (grepl("47120", Proc) & Fst_Dt < Lst_Date & HCC == 1) | (partial_hep == 1 & HCC == 1))$Patid))
  liver_transplant_patid = c(liver_transplant_patid, unique(filter(temp_data, (grepl("^505", Diag)|grepl("47135|47136", Proc)) & HCC == 1)$Patid))
  
  rm(temp_data, year, name) 
  gc()
  
}  


Count_table2 = Count_table %>%  
  mutate(HCC = 1*(Patid %in% HCC_patid), 
         SBRT = 1*(Patid %in% SBRT_patid), 
         TACE = 1*(Patid %in% TACE_patid),
         Total_ablation = 1*(Patid %in% Total_ablation_patid), 
         RFA = 1*(Patid %in% RFA_patid), 
         Cryo = 1*(Patid %in% Cryo_patid), 
         Hepatectomy = 1*(Patid %in% Hepatectomy_patid), 
         liver_transplant = 1*(Patid %in% liver_transplant_patid)
  )

Count_table2 = filter(Count_table2, HCC == 1)
apply(Count_table1, 2, sum)

