setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 




############################## Half year APP #####################################
liver_APP_tracking = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  temp_data = select(temp_data, Patid, Fst_Dt, APP_Dt)
  liver_APP_tracking = rbind(liver_APP_tracking, temp_data)
  
  rm(temp_data,name,year)
  gc()
}

save(liver_APP_tracking, file = "liver_APP_tracking.RData")


Tracking = function(data){
  output = data %>% 
    dplyr::mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
    dplyr::mutate(APP_Dt = as.Date(APP_Dt, "%Y-%m-%d")) %>% 
    dplyr::group_by(Patid) %>% 
    dplyr::arrange(Patid, Fst_Dt) %>% 
    dplyr::mutate(Before = 1*((Fst_Dt - APP_Dt)<= (-180) )) %>% 
    dplyr::mutate(After = 1*((Fst_Dt - APP_Dt) >= 180)) %>% 
    dplyr::summarise(Before_test = sum(Before), 
                  After_test = sum(After)) %>% 
    ungroup() 
  return(output)
}


liver_tracking = Tracking(liver_APP_tracking)
rm(liver_APP_tracking)
APP_table3 = unique(filter(liver_tracking, Before_test > 0 & After_test > 0)$Patid)
save(APP_table3, file = "Patid_table3.RData")



### the patids are good, we can jump in from here
liver_med = data.frame() 
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  liver_med = rbind(liver_med, select(dplyr::filter(temp_data, Patid %in% APP_table3),Patid,Fst_Dt,Diag,Proc,Trans_Dt,Lst_Date,Cancer_Diag_Date,Bleed_date,TIPS_date,Death_date,Person_year,Age, APP_Dt))
  
  rm(temp_data,name,year) 
  gc()
}
save(liver_med, file = "Medical_Table3.RData")

liver_med = filter(liver_med, (Fst_Dt < APP_Dt & Fst_Dt > APP_Dt-180)|(Fst_Dt > APP_Dt & Fst_Dt < APP_Dt+180)) # filter out the data within 1 year range

Censor_Table3 = function(data){
  output = data %>% 
    dplyr::mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>%  
    dplyr::mutate(Censor = 1*(Fst_Dt > Lst_Date)) # label those get diag code after death 
  
  return(output)
}


liver_med = Censor_Table3(liver_med)
liver_med = inpatient(liver_med)
liver_med = outpatient(liver_med)
save(liver_med, file = "Table3_data_complete.RData")
############################# Endoscopy############################################## 
load("Table3_data_complete.RData")

Endoscopy = function(data){
  patid_pre = unique(filter(data, Censor == 0 & (Fst_Dt < APP_Dt & Fst_Dt > APP_Dt-180) & (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251",Proc)|grepl("43204|43244|43243", Proc)) 
                            & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & 
                              (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0)))$Patid) 
  patid_post = unique(filter(data, Censor == 0 & (Fst_Dt > APP_Dt & Fst_Dt < APP_Dt+180) & (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251",Proc)|grepl("43204|43244|43243", Proc)) 
                             & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & 
                               (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0)))$Patid)
  output = data %>% 
    dplyr::mutate(Endoscopy_pre = 1*(Patid %in% patid_pre), 
                  Endoscopy_post = 1*(Patid %in% patid_post))
  return(output)
}
liver_med = Endoscopy(liver_med)
gc()

patid_Endoscopy_pre = unique(filter(liver_med, Endoscopy_pre == 1)$Patid)
patid_Endoscopy_post = unique(filter(liver_med, Endoscopy_post == 1)$Patid)



############################ readmission##############################################
#confinement data
# Filter out the patients for table 3 
load("liver_conf.RData") 
liver_conf = filter(liver_conf, as.Date(Admit_Date, "%Y-%m-%d") <= Lst_Date)
liver_conf_table3 = dplyr::filter(liver_conf, Patid %in% APP_table3)
save(liver_conf_table3, file = "Confinement_Table3.RData") 
rm(liver_conf)
gc()



liver_conf_table3 = merge(liver_conf_table3, APP_date, all.x = T, by = "Patid") # merge the first APP service date
liver_conf_table3 = liver_conf_table3 %>% 
  dplyr::select(Patid, Admit_Date, Diag1,Diag2,Diag3,Diag4,Diag5, Disch_Date, Dstatus, Los, last_admit, time_diff, last_disc, APP_Dt, Lst_Date) %>%  
  dplyr::mutate(Readmit_pre = 1*(Admit_Date <  APP_Dt & Admit_Date > APP_Dt-180), 
         Readmit_post = 1*(Admit_Date > APP_Dt & Admit_Date < APP_Dt + 180)) 
  

patid_readmit_pre = unique(filter(liver_conf_table3, time_diff <= 30 & grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & 
                                Readmit_pre == 1)$Patid)
patid_readmit_post = unique(filter(liver_conf_table3, time_diff <= 30 & grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & 
                                    Readmit_post == 1)$Patid)

readmit_N_table3 = unique(filter(liver_conf_table3, grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & 
                            Admit_Date < APP_Dt + 180 & Admit_Date > APP_Dt - 180)$Patid)

####### Hospitalized ########### 

patid_hospital_pre = unique(filter(liver_conf_table3, Readmit_pre == 1)$Patid) 
patid_hospital_post = unique(filter(liver_conf_table3, Readmit_post == 1)$Patid) 

patid_hospital_pre = unique(c(patid_hospital_pre, unique(filter(liver_med, (inpatient_initial_care == 1 | inpatient_subsequent_care == 1 | observation_initial_care == 1| observation_subsequent_care == 1) & 
                                                           Fst_Dt < APP_Dt)$Patid)))
patid_hospital_post = unique(c(patid_hospital_post, unique(filter(liver_med, (inpatient_initial_care == 1 | inpatient_subsequent_care == 1 | observation_initial_care == 1| observation_subsequent_care == 1) & 
                                                                  Fst_Dt > APP_Dt)$Patid)))

########################### Influenza vaccine ##########################################################
Influenza_vaccine = function(data){
  patid_pre = unique(filter(data, Censor == 0 & (Fst_Dt < APP_Dt & Fst_Dt > APP_Dt-180) & (grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc)))$Patid) 
  patid_post = unique(filter(data, Censor == 0 & (Fst_Dt > APP_Dt & Fst_Dt < APP_Dt+180) & (grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc)))$Patid)
  output = data %>% 
    dplyr::mutate(Influenza_vaccine_pre = 1*(Patid %in% patid_pre), 
                  Influenza_vaccine_post = 1*(Patid %in% patid_post))
  return(output)
}
liver_med = Influenza_vaccine(liver_med)

patid_Influenza_vaccine_pre = unique(filter(liver_med, Influenza_vaccine_pre == 1)$Patid)
patid_Influenza_vaccine_post = unique(filter(liver_med, Influenza_vaccine_post == 1)$Patid)

######################### HAV/HBV Vaccine ##########################################################
A_B_vaccine = function(data){
  patid_pre = unique(filter(data, Censor == 0 & (Fst_Dt < APP_Dt & Fst_Dt > APP_Dt-180) & (grepl("90632|90633|90634|90636", Proc)|grepl("90739|90740|90743|90747|90636|90748", Proc)) )$Patid) 
  patid_post = unique(filter(data, Censor == 0 & (Fst_Dt > APP_Dt & Fst_Dt < APP_Dt+180) & (grepl("90632|90633|90634|90636", Proc)|grepl("90739|90740|90743|90747|90636|90748", Proc)) )$Patid)
  output = data %>% 
    dplyr::mutate(A_B_vaccine_pre = 1*(Patid %in% patid_pre), 
                  A_B_vaccine_post = 1*(Patid %in% patid_post))
  return(output)
}

liver_med = A_B_vaccine(liver_med)


patid_A_B_vaccine_pre = unique(filter(liver_med, A_B_vaccine_pre == 1)$Patid)
patid_A_B_vaccine_post = unique(filter(liver_med, A_B_vaccine_post == 1)$Patid)

######################### Screen for liver cancer ##########################################################
Screen_liver_cancer = function(data){
  patid_pre = unique(filter(data, Censor == 0 & (Fst_Dt < Cancer_Diag_Date|is.na(Cancer_Diag_Date)) & (Fst_Dt < APP_Dt & Fst_Dt > APP_Dt-180) & (grepl("74177|74178|74160|74170", Proc)|grepl("74183", Proc)|grepl("76700|76705", Proc)) )$Patid) 
  patid_post = unique(filter(data, Censor == 0 & (Fst_Dt < Cancer_Diag_Date|is.na(Cancer_Diag_Date)) & (Fst_Dt > APP_Dt & Fst_Dt < APP_Dt+180) & (grepl("74177|74178|74160|74170", Proc)|grepl("74183", Proc)|grepl("76700|76705", Proc)) )$Patid)
  output = data %>% 
    dplyr::mutate(Screen_liver_cancer_pre = 1*(Patid %in% patid_pre), 
                  Screen_liver_cancer_post = 1*(Patid %in% patid_post))
  return(output)
}
liver_med = Screen_liver_cancer(liver_med)
patid_liver_cancer_pre = unique(filter(liver_med, Screen_liver_cancer_pre == 1)$Patid)
patid_liver_cancer_post = unique(filter(liver_med, Screen_liver_cancer_post == 1)$Patid)



######################## Hospital bed days ###################################################### 

temp_conf_table3_pre = liver_conf_table3 %>% 
  filter(Admit_Date < APP_Dt & Admit_Date > APP_Dt-180) %>% 
  filter(Los >= 0) %>%
  group_by(Patid) %>% 
  mutate(Bed_days = sum(Los)) %>% 
  dplyr::select(Patid, Bed_days) 
temp_conf_table3_pre = distinct(temp_conf_table3_pre)

temp_conf_table3_post = liver_conf_table3 %>% 
  filter(Admit_Date > APP_Dt & Admit_Date < APP_Dt+180) %>% 
  filter(Los >= 0) %>%
  group_by(Patid) %>% 
  mutate(Bed_days = sum(Los)) %>% 
  dplyr::select(Patid, Bed_days)
temp_conf_table3_post = distinct(temp_conf_table3_post)

bed_days_N_table3 = unique(filter(liver_conf_table3, Los >= 0 & Admit_Date <= Lst_Date & 
                                    Admit_Date < APP_Dt + 180 & Admit_Date > APP_Dt - 180)$Patid) #



########################### RIFAXIMIN ########################################

load("Pharmacy_data.RData") 
load("person_year.RData")

rifaximin_N_table3 = unique(filter(liver_conf_table3, grepl("^5722",Diag1)|grepl("^5722",Diag2)|grepl("^5722",Diag3)|grepl("^5722",Diag4)|grepl("^5722",Diag5))$Patid)

liver_pharm_table3 = filter(liver_pharm, grepl("RIFAXIMIN", Brnd_Nm) | grepl("RIFAXIMIN", Gnrc_Nm)) 
rm(liver_pharm)
gc()
liver_pharm_table3 = merge(liver_pharm_table3, APP_date, by = "Patid", all.x = T)
liver_pharm_table3 = liver_pharm_table3 %>% 
  mutate(Fill_Dt = as.Date(Fill_Dt, "%Y-%m-%d"), 
         APP_Dt = as.Date(APP_Dt, "%Y-%m-%d")) %>%
  filter(is.na(APP_Dt) == F & Fill_Dt < APP_Dt + 180 & Fill_Dt > APP_Dt - 180)

temp = select(Person_year, Patid, Lst_Date)
liver_pharm_table3 = merge(liver_pharm_table3, temp, by = "Patid", all.x = T)
rm(temp, Person_year)


patid_rifaximin_pre = unique(filter(liver_pharm_table3, Fill_Dt < APP_Dt & Fill_Dt < Lst_Date)$Patid)
patid_rifaximin_post = unique(filter(liver_pharm_table3, Fill_Dt > APP_Dt & Fill_Dt < Lst_Date)$Patid)

patid_rifaximin_pre = patid_rifaximin_pre[patid_rifaximin_pre %in% rifaximin_N_table3]
patid_rifaximin_post = patid_rifaximin_post[patid_rifaximin_post %in% rifaximin_N_table3]
################################ logistic regression ##################################################
load("table2.RData")

table3_pre = data.frame(APP_table3) %>% 
  mutate(Patid = APP_table3) %>%
  mutate(APP = 0) %>% 
  mutate(Endoscopy = 1*(Patid %in% patid_Endoscopy_pre), 
         A_B_vaccine = 1*(Patid %in% patid_A_B_vaccine_pre), 
         Influenza_vaccine = 1*(Patid %in% patid_Influenza_vaccine_pre), 
         Hospitalized = 1*(Patid %in% patid_hospital_pre), 
         Screen_liver_cancer = 1*(Patid %in% patid_liver_cancer_pre), 
         Readmit = 1*(Patid %in% patid_readmit_pre), 
         Rifaximin = 1*(Patid %in% patid_rifaximin_pre)
         ) 
  
table3_pre = merge(table3_pre, temp_conf_table3_pre, by = "Patid", all.x = T)

table3_post = data.frame(APP_table3) %>% 
  mutate(Patid = APP_table3) %>%
  mutate(APP = 1) %>%
  mutate(Endoscopy = 1*(Patid %in% patid_Endoscopy_post), 
         A_B_vaccine = 1*(Patid %in% patid_A_B_vaccine_post), 
         Influenza_vaccine = 1*(Patid %in% patid_Influenza_vaccine_post), 
         Hospitalized = 1*(Patid %in% patid_hospital_post), 
         Screen_liver_cancer = 1*(Patid %in% patid_liver_cancer_post),
         Readmit = 1*(Patid %in% patid_readmit_post),
         Rifaximin = 1*(Patid %in% patid_rifaximin_post)
        ) 
  
table3_post = merge(table3_post, temp_conf_table3_post, by = "Patid", all.x = T)

table3 = rbind(table3_pre, table3_post)

output <- glm(Endoscopy ~ APP, data=filter(table3, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output) 

output <- glm(Influenza_vaccine ~ APP, data=table3, family=binomial)
logistic.regression.or.ci(output) 

output <- glm(Hospitalized ~ APP, data=table3, family=binomial)
logistic.regression.or.ci(output) 

output <- glm(Readmit ~ APP, data=filter(table3, Patid %in% readmit_N_table3), family=binomial)
logistic.regression.or.ci(output) 

output <- glm(Screen_liver_cancer ~ APP, data=table3, family=binomial)
logistic.regression.or.ci(output) 

output <- glm(A_B_vaccine ~ APP, data=table3, family=binomial)
logistic.regression.or.ci(output) 

output <- glm(Rifaximin ~ APP, data=filter(table3, Patid %in% rifaximin_N_table3), family=binomial)
logistic.regression.or.ci(output) 

output = MASS::glm.nb(Bed_days ~ APP, data = filter(table3, Patid %in% bed_days_N_table3))
logistic.regression.or.ci(output)

# merge table2 variables 
temp = select(table2, Patid,AC,Hepatitis_C,Non_alcohol,Cirrhosis_complication,HE,Ascites,Varices,Gastro_only,
              Hepatology,Race,Sex,score,Age,SBP,TIPS,Transplant_Evaluation,HCC,Shared_visit,APP_Gastro,APP_Hepatology)

table3$APP_table3 =NULL
table3 = merge(table3, temp, by = "Patid", all.x = T)



output <- glm(Endoscopy ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology,
              data = filter(table3, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)

output <- glm(Screen_liver_cancer ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology, 
              data=table3, family=binomial)
logistic.regression.or.ci(output)

output <- glm(Influenza_vaccine ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology, 
              data=table3, family=binomial)
logistic.regression.or.ci(output)

output <- glm(A_B_vaccine ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology, 
              data=table3, family=binomial)
logistic.regression.or.ci(output)

output <- glm(Hospitalized ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology, 
              data=table3, family=binomial)
logistic.regression.or.ci(output)

output <- glm(Rifaximin ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology, 
              data=filter(table3, Patid %in% rifaximin_N_table3), family=binomial)
logistic.regression.or.ci(output)

output <- glm(Readmit ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology, 
              data=filter(table3, Patid %in% readmit_N_table3), family=binomial)
logistic.regression.or.ci(output)

output = MASS::glm.nb(Bed_days ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology, 
                      data = filter(table3, Patid %in% bed_days_N_table3))
logistic.regression.or.ci(output)




rm(temp)











save.image("table3.RData")
