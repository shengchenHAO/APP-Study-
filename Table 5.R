setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 





# influenza vaccine 
load("table2.RData")
load("Time vary variable prepare CPT codes.Rdata")
temp_influenza = distinct(data_time_Proc) %>% 
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  mutate(Influenza_vaccine = 1*(grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc) 
                                & Fst_Dt < Lst_Date & (Fst_Dt >= APP_Dt | is.na(APP_Dt)))) %>%
  
  select(Patid, Fst_Dt, Influenza_vaccine) %>% 
  distinct() %>% # because there could e duplicated rows in the data, and patients should only got one vaccine per day
  group_by(Patid) %>% 
  arrange(Patid, Fst_Dt) %>% 

  mutate(Influenza_vaccine_count = cumsum(Influenza_vaccine)) %>% 
  mutate(max_influenza = max(Influenza_vaccine_count, na.rm = T)) %>%
  summarise(max_influenza = first(max_influenza))

table2 = merge(table2, temp_influenza, by = "Patid", all.x = T)
rm(temp_influenza, data_time_Proc) 
gc()


#  create table 5
table5 = table2
rm(table2)
table5 = filter(table5, Denominator >= 1) # load the data, and change based on need

table5 = mutate(table5, max_influenza_peryear = max_influenza/Denominator)
summary(filter(table5, APP == 1)$max_influenza_peryear)
summary(filter(table5, APP == 0)$max_influenza_peryear)


# Screen for liver cancer
data_table4_liver_cancer = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData")
  load(name)
  
  temp_data = mutate(temp_data, Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d"))
  data_table4_liver_cancer = rbind(data_table4_liver_cancer, filter(temp_data, (grepl("74177|74178|74160|74170", Proc)| # CT
                                                                                  grepl("74183", Proc)| #MRI
                                                                                  grepl("76700|76705", Proc)) &# Ultrasound
                                                                      Fst_Dt < Lst_Date & (Fst_Dt < Cancer_Diag_Date|is.na(Cancer_Diag_Date)) & 
                                                                      (Fst_Dt >= APP_Dt | is.na(APP_Dt)))
  ) 
  
  
  rm(temp_data,name,year)
  gc()
}
save(data_table4_liver_cancer, file = "table4 liver cancer.RData")



load("table4 liver cancer.RData")
temp_screen = data_table4_liver_cancer %>% 
  select(Patid, Fst_Dt, Person_year) %>%
  distinct() %>%
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  mutate(liver_cancer_screen = 1) %>% 
  group_by(Patid) %>% 
  arrange(Patid, Fst_Dt) %>% 
  mutate(liver_cancer_screen_count = cumsum(liver_cancer_screen)) %>% 
  mutate(max_screen = max(liver_cancer_screen_count, na.rm = T)) %>%
  summarise(max_screen = first(max_screen))

table5 = merge(table5, temp_screen, by ="Patid", all.x = T)
rm(temp_screen, data_table4_liver_cancer)
gc()

table5 = mutate(table5, max_screen_peryear = max_screen/Denominator)
summary(filter(table5, APP == 1)$max_screen_peryear)
summary(filter(table5, APP == 0)$max_screen_peryear)


## readmit per discharge 
# get the number of discharge 
load("liver_conf.RData")
liver_conf = mutate(liver_conf, Readmission = 1*(time_diff <= 30 & grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date < Lst_Date & (Admit_Date >= APP_Dt | is.na(APP_Dt))), 
                    Discharge = 1*(grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date < Lst_Date & (Admit_Date >= APP_Dt | is.na(APP_Dt))))

temp_conf = liver_conf %>% 
  group_by(Patid) %>% 
  arrange(Patid, Admit_Date) %>% 
  mutate(Discharge_count = cumsum(Discharge), 
         Readmission_count = cumsum(Readmission)) %>%
  mutate(max_discharge = max(Discharge_count, na.rm = T), 
         max_readmission = max(Readmission_count, na.rm = T)) %>% 
  summarise(max_discharge = first(max_discharge), 
            max_readmission = first(max_readmission))
  
  
temp_conf = filter(temp_conf, max_discharge > 0) # the den of discharge
table5 = merge(table5, temp_conf, all.x = T, by = "Patid")
rm(temp_conf, liver_conf)
gc()

table5 = mutate(table5, Readmission_per_discharge = max_readmission/max_discharge)
summary(filter(table5, APP == 1 & Patid %in% readmit_N)$Readmission_per_discharge)
summary(filter(table5, APP == 0 & Patid %in% readmit_N)$Readmission_per_discharge)

table5 = mutate(table5, bed_days_peryear = bed_days/Denominator)  
summary(filter(table5, Patid %in% Beddays_patid & APP == 1)$bed_days_peryear)
summary(filter(table5, Patid %in% Beddays_patid & APP == 0)$bed_days_peryear)


# Endoscopy 
load("Varices screening.RData")
temp_endoscopy = varice_screen %>% 
  select(Patid, Fst_Dt) %>% 
  distinct() %>% 
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  mutate(Endoscopy = 1) %>%
  group_by(Patid) %>% 
  arrange(Patid, Fst_Dt) %>% 
  mutate(Endoscopy_count = cumsum(Endoscopy)) %>% 
  mutate(max_Endoscopy = max(Endoscopy_count, na.rm = T)) %>% 
  summarise(max_Endoscopy = first(max_Endoscopy))

table5 = merge(table5, temp_endoscopy, all.x = T, by = "Patid")
table5 = mutate(table5, Endoscopy_peryear = max_Endoscopy/Denominator)

summary(filter(table5, APP == 1 & Patid %!in% Censor_endoscopy_patid)$Endoscopy_peryear)
summary(filter(table5, APP == 0 & Patid %!in% Censor_endoscopy_patid)$Endoscopy_peryear)
rm(temp_endoscopy, varice_screen)

save.image("table5.RData")

#   


output = MASS::glm.nb(max_readmission ~ APP + offset(log(max_discharge)), data = filter(table5, Patid %in% readmit_N))
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci

output = MASS::glm.nb(max_screen ~ APP + offset(log(Denominator)), data = table5)
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci

output = MASS::glm.nb(max_influenza ~ APP + offset(log(Denominator)), data = table5)
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci

output = MASS::glm.nb(bed_days ~ APP + offset(log(Denominator)), data =  filter(table5, Patid %in% Beddays_patid))
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci

output = MASS::glm.nb(max_Endoscopy ~ APP + offset(log(Denominator)), data =  filter(table5, Patid %!in% Censor_endoscopy_patid))
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci

# adjust 
output = MASS::glm.nb(max_screen ~ offset(log(Denominator)) + 
                      APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit,
                      data = table5)
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci[1,]

output = MASS::glm.nb(max_readmission ~ offset(log(max_discharge)) +
                      APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit,
                      data = filter(table5, Patid %in% readmit_N))
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci[1,]

output = MASS::glm.nb(max_influenza ~ offset(log(Denominator)) +
                      APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit,
                      data = table5)
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci[1,]

output = MASS::glm.nb(bed_days ~ offset(log(Denominator))+
                      APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit,
                      data =  filter(table5, Patid %in% Beddays_patid))
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci[1,]

output = MASS::glm.nb(max_Endoscopy ~ offset(log(Denominator))+
                      APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit,
                      data =  filter(table5, Patid %!in% Censor_endoscopy_patid))
logistic.regression.or.ci(output)$regression.table 
logistic.regression.or.ci(output)$slopes.ci[1,]













