setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 

load("APP&MD dates.RData")
APP_patid = unique(APP_date$Patid)
rm(APP_date, MD_date, APP_MD_date)

Table3a_group1 = data.frame()
Table3a_group2 = data.frame()
'%!in%' = Negate('%in%')
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  Table3a_group1 = rbind(Table3a_group1, filter(temp_data, Gastro == 1 | APP == 1))
  Table3a_group2 = rbind(Table3a_group2, filter(temp_data, Gastro == 1 & APP == 0 & Patid %!in% APP_patid))
  
  rm(temp_data,name,year)
  gc()
}


save.image("Table3a new.RData")




# Group1: first APP happend as shared visit with Gastro
Table3a_group1 = Table3a_group1 %>% 
  select(Patid, Fst_Dt, APP, Gastro) %>% 
  distinct() %>%
  arrange(Patid, Fst_Dt) %>% 
  group_by(Patid) %>% 
  mutate(APP_count = cumsum(APP)) %>% 
  ungroup()

patid_group1 = unique(filter(Table3a_group1, APP_count == 1 & Gastro == 1 & APP == 1)$Patid)
rm(Table3a_group1)

# collecting data for group 1 
Group1 = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  Group1 = rbind(Group1, filter(temp_data, Patid %in% patid_group1))
  
  rm(temp_data,name,year)
  gc()
}


Group1_Data = filter(Group1, (Fst_Dt >= APP_Dt - 180) & (Fst_Dt <=  APP_Dt + 180)) # The 1 year period data used in table 3a 
patid_group1 = unique(filter(Group1_Data, APP_Dt <= Lst_Date - 180)$Patid) # filter out those lose coverage in the 6 month after First APP shered with Gastro
Group1_Data = filter(Group1_Data, Patid %in% patid_group1)

# Exclude those had vaccine before first APP date
Influenza_exclude = unique(filter(Group1, grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc) & Fst_Dt < APP_Dt)$Patid)
AB_exclude = unique(filter(Group1, (grepl("90632|90633|90634|90636", Proc) | grepl("90739|90740|90743|90747|90636|90748", Proc)) & Fst_Dt < APP_Dt)$Patid)

Group1_Data = Group1_Data %>% 
  inpatient() %>% 
  outpatient()


# Endoscopy 
Endoscopy = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < APP_Dt & Fst_Dt > APP_Dt-180) & (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251",Proc)|grepl("43204|43244|43243", Proc)) 
                            & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & 
                              (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0)))$Patid) 
  patid_post = unique(filter(data, (Fst_Dt >= APP_Dt & Fst_Dt < APP_Dt+180) & (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251",Proc)|grepl("43204|43244|43243", Proc)) 
                             & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & 
                               (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0)))$Patid)
  output = data %>% 
    dplyr::mutate(Endoscopy_pre = 1*(Patid %in% patid_pre), 
                  Endoscopy_post = 1*(Patid %in% patid_post))
  return(output)
}

Group1_Data = Endoscopy(Group1_Data)
Group1_Endoscopy_pre = unique(filter(Group1_Data, Endoscopy_pre == 1)$Patid)
Group1_Endoscopy_post = unique(filter(Group1_Data, Endoscopy_post == 1)$Patid)


# Screen 
Screen_liver_cancer = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < Cancer_Diag_Date|is.na(Cancer_Diag_Date)) & (Fst_Dt < APP_Dt & Fst_Dt > APP_Dt-180) & (grepl("74177|74178|74160|74170", Proc)|grepl("74183", Proc)|grepl("76700|76705", Proc)) )$Patid) 
  patid_post = unique(filter(data, (Fst_Dt < Cancer_Diag_Date|is.na(Cancer_Diag_Date)) & (Fst_Dt >= APP_Dt & Fst_Dt < APP_Dt+180) & (grepl("74177|74178|74160|74170", Proc)|grepl("74183", Proc)|grepl("76700|76705", Proc)) )$Patid)
  output = data %>% 
    dplyr::mutate(Screen_liver_cancer_pre = 1*(Patid %in% patid_pre), 
                  Screen_liver_cancer_post = 1*(Patid %in% patid_post))
  return(output)
}
Group1_Data = Screen_liver_cancer(Group1_Data)
Group1_liver_cancer_pre = unique(filter(Group1_Data, Screen_liver_cancer_pre == 1)$Patid)
Group1_liver_cancer_post = unique(filter(Group1_Data, Screen_liver_cancer_post == 1)$Patid)

# influenza 
Influenza_vaccine = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < APP_Dt & Fst_Dt > APP_Dt-180) & (grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc)))$Patid) 
  patid_post = unique(filter(data, (Fst_Dt >= APP_Dt & Fst_Dt < APP_Dt+180) & (grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc)))$Patid)
  output = data %>% 
    dplyr::mutate(Influenza_vaccine_pre = 1*(Patid %in% patid_pre), 
                  Influenza_vaccine_post = 1*(Patid %in% patid_post))
  return(output)
}
Group1_Data = Influenza_vaccine(Group1_Data)
Group1_Influenza_vaccine_pre = unique(filter(Group1_Data, Influenza_vaccine_pre == 1)$Patid)
Group1_Influenza_vaccine_post = unique(filter(Group1_Data, Influenza_vaccine_post == 1)$Patid)

#HAV/HBV Vaccine 
A_B_vaccine = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < APP_Dt & Fst_Dt > APP_Dt-180) & (grepl("90632|90633|90634|90636", Proc)|grepl("90739|90740|90743|90747|90636|90748", Proc)) )$Patid) 
  patid_post = unique(filter(data, (Fst_Dt >= APP_Dt & Fst_Dt < APP_Dt+180) & (grepl("90632|90633|90634|90636", Proc)|grepl("90739|90740|90743|90747|90636|90748", Proc)) )$Patid)
  output = data %>% 
    dplyr::mutate(A_B_vaccine_pre = 1*(Patid %in% patid_pre), 
                  A_B_vaccine_post = 1*(Patid %in% patid_post))
  return(output)
}

Group1_Data = A_B_vaccine(Group1_Data)
Group1_A_B_vaccine_pre = unique(filter(Group1_Data, A_B_vaccine_pre == 1)$Patid)
Group1_A_B_vaccine_post = unique(filter(Group1_Data, A_B_vaccine_post == 1)$Patid)




# confinement data 
load("liver_conf.RData") 
Group1_conf = filter(liver_conf, Patid %in% patid_group1)
Group1_conf = filter(Group1_conf, Admit_Date >= APP_Dt - 180 & Admit_Date <= APP_Dt + 180)


# readmit 
Group1_readmit_pre = unique(filter(Group1_conf, time_diff <= 30 & grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date < APP_Dt)$Patid)
Group1_readmit_post = unique(filter(Group1_conf, time_diff <= 30 & grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date >= APP_Dt)$Patid)
readmit_N_Group1 = unique(filter(Group1_conf, grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc))$Patid)

# bed days
temp_conf_table3_pre = Group1_conf %>% 
  filter(Admit_Date < APP_Dt & Admit_Date >= APP_Dt-180 & Admit_Date <= Lst_Date) %>% 
  filter(Los >= 0) %>%
  group_by(Patid) %>% 
  mutate(Bed_days = sum(Los)) %>% 
  dplyr::select(Patid, Bed_days) 
temp_conf_table3_pre = distinct(temp_conf_table3_pre)

temp_conf_table3_post = Group1_conf %>% 
  filter(Admit_Date > APP_Dt & Admit_Date <= APP_Dt+180 & Admit_Date <= Lst_Date) %>% 
  filter(Los >= 0) %>%
  group_by(Patid) %>% 
  mutate(Bed_days = sum(Los)) %>% 
  dplyr::select(Patid, Bed_days)
temp_conf_table3_post = distinct(temp_conf_table3_post)

bed_days_N_Group1 = unique(filter(Group1_conf, Los >= 0)$Patid)


# rifaximin
load("Pharmacy_data.RData") 
load("APP&MD dates.RData")

APP_date = APP_date %>%  
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  arrange(Patid, Fst_Dt) %>% 
  group_by(Patid) %>% 
  dplyr::summarise(APP_Dt = first(Fst_Dt))
rm(APP_MD_date, MD_date)


rifaximin_N = unique(filter(liver_conf, grepl("^5722",Diag1)|grepl("^5722",Diag2)|grepl("^5722",Diag3)|grepl("^5722",Diag4)|grepl("^5722",Diag5))$Patid) # denominator for rifaximin
liver_pharm_Group1 = filter(liver_pharm, (grepl("RIFAXIMIN", Brnd_Nm) | grepl("RIFAXIMIN", Gnrc_Nm)) & Patid %in% patid_group1) 

liver_pharm_Group1 = merge(liver_pharm_Group1, APP_date, by = "Patid", all.x = T)
liver_pharm_Group1 = liver_pharm_Group1 %>% 
  mutate(Fill_Dt = as.Date(Fill_Dt, "%Y-%m-%d"), 
         APP_Dt = as.Date(APP_Dt, "%Y-%m-%d")) %>%
  filter(Fill_Dt <= APP_Dt + 180 & Fill_Dt >= APP_Dt - 180)

Group1_rifaximin_pre = unique(filter(liver_pharm_Group1, Fill_Dt < APP_Dt)$Patid)
Group1_rifaximin_post = unique(filter(liver_pharm_Group1, Fill_Dt >= APP_Dt)$Patid)

Group1_rifaximin_pre = Group1_rifaximin_pre[Group1_rifaximin_pre %in% rifaximin_N]
Group1_rifaximin_post = Group1_rifaximin_post[Group1_rifaximin_post %in% rifaximin_N]


# transplant evaluation 
Transplant_evaluation = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < APP_Dt & Fst_Dt >= APP_Dt-180) & grepl("V427", Diag))$Patid) 
  patid_post = unique(filter(data, (Fst_Dt >= APP_Dt & Fst_Dt <= APP_Dt+180) & grepl("V427", Diag))$Patid)
  output = data %>% 
    dplyr::mutate(Transplant_pre = 1*(Patid %in% patid_pre), 
                  Transplant_post = 1*(Patid %in% patid_post))
  return(output)
}
Group1_Data = Transplant_evaluation(Group1_Data)
Group1_Transplant_pre = unique(filter(Group1_Data, Transplant_pre == 1)$Patid)
Group1_Transplant_post = unique(filter(Group1_Data, Transplant_post == 1)$Patid)




# Build table for Group1 
load("table2.RData")

Group1_pre = data.frame(patid_group1) %>% 
  mutate(Patid = patid_group1) %>%
  mutate(APP = 0) %>% 
  mutate(Endoscopy = 1*(Patid %in% Group1_Endoscopy_pre), 
         A_B_vaccine = 1*(Patid %in% Group1_A_B_vaccine_pre), 
         Influenza_vaccine = 1*(Patid %in% Group1_Influenza_vaccine_pre), 
         Screen_liver_cancer = 1*(Patid %in% Group1_liver_cancer_pre), 
         Readmit = 1*(Patid %in% Group1_readmit_pre), 
         Rifaximin = 1*(Patid %in% Group1_rifaximin_pre), 
         Transplant_evaluation = 1*(Patid %in% Group1_Transplant_pre)
  ) 

Group1_pre = merge(Group1_pre, temp_conf_table3_pre, by = "Patid", all.x = T)

Group1_post = data.frame(patid_group1) %>% 
  mutate(Patid = patid_group1) %>%
  mutate(APP = 1) %>%
  mutate(Endoscopy = 1*(Patid %in% Group1_Endoscopy_post), 
         A_B_vaccine = 1*(Patid %in% Group1_A_B_vaccine_post), 
         Influenza_vaccine = 1*(Patid %in% Group1_Influenza_vaccine_post), 
         Screen_liver_cancer = 1*(Patid %in% Group1_liver_cancer_post),
         Readmit = 1*(Patid %in% Group1_readmit_post),
         Rifaximin = 1*(Patid %in% Group1_rifaximin_post), 
         Transplant_evaluation = 1*(Patid %in% Group1_Transplant_post)
  ) 

Group1_post = merge(Group1_post, temp_conf_table3_post, by = "Patid", all.x = T)
rm(temp_conf_table3_post, temp_conf_table3_pre)

Group1_table = rbind(Group1_pre, Group1_post)

'%!in%' = Negate('%in%')
table(Group1_table$Screen_liver_cancer, Group1_table$APP)
table(filter(Group1_table, Patid %!in% Censor_endoscopy_patid)$Endoscopy, filter(Group1_table, Patid %!in% Censor_endoscopy_patid)$APP) # different Denominator
table(Group1_table$Influenza_vaccine, Group1_table$APP)
table(Group1_table$A_B_vaccine, Group1_table$APP)
table(filter(Group1_table, Patid %in% rifaximin_N)$Rifaximin, filter(Group1_table, Patid %in% rifaximin_N)$APP)
table(filter(Group1_table, Patid %in% readmit_N_Group1)$Readmit, filter(Group1_table, Patid %in% readmit_N_Group1)$APP) # different Denominator
table(filter(Group1_table, Patid %in% Transplant_Evaluation_N)$Transplant_evaluation, filter(Group1_table, Patid %in% Transplant_Evaluation_N)$APP)

output <- glm(Screen_liver_cancer ~ APP, data=Group1_table, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Endoscopy ~ APP, data=filter(Group1_table, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Influenza_vaccine ~ APP, data=Group1_table, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(A_B_vaccine ~ APP, data=Group1_table, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Readmit ~ APP, data=filter(Group1_table, Patid %in% readmit_N_Group1), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Rifaximin ~ APP, data=filter(Group1_table, Patid %in% rifaximin_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Transplant_evaluation ~ APP, data=filter(Group1_table, Patid %in% Transplant_Evaluation_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci


temp = select(table2, Patid,AC,Hepatitis_C,Non_alcohol,Cirrhosis_complication,HE,Ascites,Varices,
            Race,Sex,score,Age,SBP,TIPS,HCC,Pneumonia,Sepsis,Urinary_tract_infection,Cellulitis,Bacteremia,Clostridium,Cholangitis,Paracentesis,Dialysis,max_visit)
Group1_table = merge(Group1_table, temp, by = "Patid", all.x = T)



formula = "~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Race+Sex+score+Age+SBP+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit"
output <- glm(paste("Endoscopy", formula, sep = " "),
              data = filter(Group1_table, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Screen_liver_cancer", formula, sep = " "), 
              data=Group1_table, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Influenza_vaccine", formula), 
              data=Group1_table, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("A_B_vaccine", formula), 
              data=Group1_table, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Rifaximin", formula), 
              data=filter(Group1_table, Patid %in% rifaximin_N), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Readmit", formula), 
              data=filter(Group1_table, Patid %in% readmit_N_Group1), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Transplant_evaluation ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+Ascites+Varices+Race+Sex+score+Age+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Dialysis+max_visit, 
              data=filter(Group1_table, Patid %in% Transplant_Evaluation_N), family=binomial) # exclude HE, SBP, Paracentesis
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

save.image("Group1 complete.RData")







load("Table3a new.RData")
load("person_year.RData")
rm(Table3a_group1)

Gastro_Date = Table3a_group2 %>% 
  arrange(Patid, Fst_Dt) %>% 
  group_by(Patid) %>% 
  summarise(Gastro_Date = first(Fst_Dt))

Person_year = merge(Person_year, Gastro_Date, by = "Patid", all.x = T)

Group2_patid = unique(filter(Person_year, Lst_Date >= (Gastro_Date + 180) & Eligeff <= (Gastro_Date - 180) )$Patid)


# collecting data for group 2 
Group2_data = data.frame()
for (year in 2001:2015){
  name = paste0("data_censor_", year, ".RData") 
  load(name) 
  
  temp_data = merge(temp_data, Gastro_Date, by = "Patid", all.x = T)
  Group2_data = rbind(Group2_data, filter(temp_data, Patid %in% Group2_patid & Fst_Dt <= Gastro_Date + 180 & Fst_Dt >= Gastro_Date - 180))
  
  rm(temp_data,name,year)
  gc()
}
save(Group2_data, file = "Group 2 data.RData")

Group2_data = Group2_data %>% 
  inpatient() %>% 
  outpatient()



# Endoscopy 
Endoscopy = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < Gastro_Date) & (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251",Proc)|grepl("43204|43244|43243", Proc)) 
                            & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & 
                              (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0)))$Patid) 
  patid_post = unique(filter(data, (Fst_Dt > Gastro_Date) & (grepl("^4516", Diag)|grepl("43200|43202|43204|43234|43235|43239|43243|43244|43255|43227|43204|43205|43251",Proc)|grepl("43204|43244|43243", Proc)) 
                             & (Fst_Dt <= TIPS_date|is.na(TIPS_date)) & (Fst_Dt <= Bleed_date|is.na(Bleed_date)) & 
                               (out_new == 1 | out_established == 1|(inpatient_initial_care == 0 & inpatient_subsequent_care == 0 & observation_initial_care == 0 & observation_subsequent_care == 0)))$Patid)
  output = data %>% 
    dplyr::mutate(Endoscopy_pre = 1*(Patid %in% patid_pre), 
                  Endoscopy_post = 1*(Patid %in% patid_post))
  return(output)
}

Group2_data = Endoscopy(Group2_data)
Group2_Endoscopy_pre = unique(filter(Group2_data, Endoscopy_pre == 1)$Patid)
Group2_Endoscopy_post = unique(filter(Group2_data, Endoscopy_post == 1)$Patid)


# Screen 
Screen_liver_cancer = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < Cancer_Diag_Date|is.na(Cancer_Diag_Date)) & (Fst_Dt < Gastro_Date) & (grepl("74177|74178|74160|74170", Proc)|grepl("74183", Proc)|grepl("76700|76705", Proc)) )$Patid) 
  patid_post = unique(filter(data, (Fst_Dt < Cancer_Diag_Date|is.na(Cancer_Diag_Date)) & (Fst_Dt > Gastro_Date) & (grepl("74177|74178|74160|74170", Proc)|grepl("74183", Proc)|grepl("76700|76705", Proc)) )$Patid)
  output = data %>% 
    dplyr::mutate(Screen_liver_cancer_pre = 1*(Patid %in% patid_pre), 
                  Screen_liver_cancer_post = 1*(Patid %in% patid_post))
  return(output)
}
Group2_data = Screen_liver_cancer(Group2_data)
Group2_liver_cancer_pre = unique(filter(Group2_data, Screen_liver_cancer_pre == 1)$Patid)
Group2_liver_cancer_post = unique(filter(Group2_data, Screen_liver_cancer_post == 1)$Patid)

# influenza 
Influenza_vaccine = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < Gastro_Date) & (grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc)))$Patid) 
  patid_post = unique(filter(data, (Fst_Dt > Gastro_Date) & (grepl("90682|90683|90685|90686|90687|90688|90756|90664|90666|90667|90668|90748|90660|90662|90672|90673|90630|90656|90658|90653|90674|90749", Proc)))$Patid)
  output = data %>% 
    dplyr::mutate(Influenza_vaccine_pre = 1*(Patid %in% patid_pre), 
                  Influenza_vaccine_post = 1*(Patid %in% patid_post))
  return(output)
}
Group2_data = Influenza_vaccine(Group2_data)
Group2_Influenza_vaccine_pre = unique(filter(Group2_data, Influenza_vaccine_pre == 1)$Patid)
Group2_Influenza_vaccine_post = unique(filter(Group2_data, Influenza_vaccine_post == 1)$Patid)

#HAV/HBV Vaccine 
A_B_vaccine = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < Gastro_Date) & (grepl("90632|90633|90634|90636", Proc)|grepl("90739|90740|90743|90747|90636|90748", Proc)) )$Patid) 
  patid_post = unique(filter(data, (Fst_Dt > Gastro_Date) & (grepl("90632|90633|90634|90636", Proc)|grepl("90739|90740|90743|90747|90636|90748", Proc)) )$Patid)
  output = data %>% 
    dplyr::mutate(A_B_vaccine_pre = 1*(Patid %in% patid_pre), 
                  A_B_vaccine_post = 1*(Patid %in% patid_post))
  return(output)
}

Group2_data = A_B_vaccine(Group2_data)
Group2_A_B_vaccine_pre = unique(filter(Group2_data, A_B_vaccine_pre == 1)$Patid)
Group2_A_B_vaccine_post = unique(filter(Group2_data, A_B_vaccine_post == 1)$Patid)



# confinement data 
load("liver_conf.RData") 
Group2_conf = filter(liver_conf, Patid %in% Group2_patid) 
Group2_conf = merge(Group2_conf, Gastro_Date, by = "Patid", all.x = T)
Group2_conf = filter(Group2_conf, Admit_Date >= Gastro_Date - 180 & Admit_Date <= Gastro_Date + 180)


# readmit 
Group2_readmit_pre = unique(filter(Group2_conf, time_diff <= 30 & grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date < Gastro_Date)$Patid)
Group2_readmit_post = unique(filter(Group2_conf, time_diff <= 30 & grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date > Gastro_Date)$Patid)
readmit_N_Group2 = unique(filter(Group2_conf, grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc))$Patid)


# rifaximin
load("Pharmacy_data.RData") 

rifaximin_N = unique(filter(liver_conf, grepl("^5722",Diag1)|grepl("^5722",Diag2)|grepl("^5722",Diag3)|grepl("^5722",Diag4)|grepl("^5722",Diag5))$Patid) # denominator for rifaximin
liver_pharm_Group2 = filter(liver_pharm, (grepl("RIFAXIMIN", Brnd_Nm) | grepl("RIFAXIMIN", Gnrc_Nm)) & Patid %in% Group2_patid) 

liver_pharm_Group2 = merge(liver_pharm_Group2, Gastro_Date, by = "Patid", all.x = T)
liver_pharm_Group2 = liver_pharm_Group2 %>% 
  mutate(Fill_Dt = as.Date(Fill_Dt, "%Y-%m-%d"), 
         APP_Dt = as.Date(Gastro_Date, "%Y-%m-%d")) %>%
  filter(Fill_Dt <= Gastro_Date + 180 & Fill_Dt >= Gastro_Date - 180)

Group2_rifaximin_pre = unique(filter(liver_pharm_Group2, Fill_Dt < Gastro_Date)$Patid)
Group2_rifaximin_post = unique(filter(liver_pharm_Group2, Fill_Dt > Gastro_Date)$Patid)

Group2_rifaximin_pre = Group2_rifaximin_pre[Group2_rifaximin_pre %in% rifaximin_N]
Group2_rifaximin_post = Group2_rifaximin_post[Group2_rifaximin_post %in% rifaximin_N]


# transplant evaluation 
Transplant_evaluation = function(data){
  patid_pre = unique(filter(data, (Fst_Dt < Gastro_Date) & grepl("V427", Diag))$Patid) 
  patid_post = unique(filter(data, (Fst_Dt > Gastro_Date) & grepl("V427", Diag))$Patid)
  output = data %>% 
    dplyr::mutate(Transplant_pre = 1*(Patid %in% patid_pre), 
                  Transplant_post = 1*(Patid %in% patid_post))
  return(output)
}
Group2_data = Transplant_evaluation(Group2_data)
Group2_Transplant_pre = unique(filter(Group2_data, Transplant_pre == 1)$Patid)
Group2_Transplant_post = unique(filter(Group2_data, Transplant_post == 1)$Patid)

rm(liver_conf, liver_pharm_Group2, liver_pharm, Table3a_group2, Person_year)

load("table2.RData")
rm(liver_conf, liver_member_fixed, Person_year)

Group2_pre = data.frame(Group2_patid) %>% 
  mutate(Patid = Group2_patid) %>%
  mutate(Gastro = 0) %>% 
  mutate(Endoscopy = 1*(Patid %in% Group2_Endoscopy_pre), 
         A_B_vaccine = 1*(Patid %in% Group2_A_B_vaccine_pre), 
         Influenza_vaccine = 1*(Patid %in% Group2_Influenza_vaccine_pre), 
         Screen_liver_cancer = 1*(Patid %in% Group2_liver_cancer_pre), 
         Readmit = 1*(Patid %in% Group2_readmit_pre), 
         Rifaximin = 1*(Patid %in% Group2_rifaximin_pre), 
         Transplant_evaluation = 1*(Patid %in% Group2_Transplant_pre)
  ) 



Group2_post = data.frame(Group2_patid) %>% 
  mutate(Patid = Group2_patid) %>%
  mutate(Gastro = 1) %>%
  mutate(Endoscopy = 1*(Patid %in% Group2_Endoscopy_post), 
         A_B_vaccine = 1*(Patid %in% Group2_A_B_vaccine_post), 
         Influenza_vaccine = 1*(Patid %in% Group2_Influenza_vaccine_post), 
         Screen_liver_cancer = 1*(Patid %in% Group2_liver_cancer_post),
         Readmit = 1*(Patid %in% Group2_readmit_post),
         Rifaximin = 1*(Patid %in% Group2_rifaximin_post), 
         Transplant_evaluation = 1*(Patid %in% Group2_Transplant_post)
  ) 


Group2_table = rbind(Group2_pre, Group2_post)
rm(Group2_pre, Group2_post, Group2_conf)

table(Group2_table$Screen_liver_cancer, Group2_table$Gastro)
table(filter(Group2_table, Patid %!in% Censor_endoscopy_patid)$Endoscopy, filter(Group2_table, Patid %!in% Censor_endoscopy_patid)$Gastro) # different Denominator
table(Group2_table$Influenza_vaccine, Group2_table$Gastro)
table(Group2_table$A_B_vaccine, Group2_table$Gastro)
table(filter(Group2_table, Patid %in% rifaximin_N)$Rifaximin, filter(Group2_table, Patid %in% rifaximin_N)$Gastro)
table(filter(Group2_table, Patid %in% readmit_N_Group2)$Readmit, filter(Group2_table, Patid %in% readmit_N_Group2)$Gastro) # different Denominator
table(filter(Group2_table, Patid %in% Transplant_Evaluation_N)$Transplant_evaluation, filter(Group2_table, Patid %in% Transplant_Evaluation_N)$Gastro)


output <- glm(Screen_liver_cancer ~ Gastro, data=Group2_table, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Endoscopy ~ Gastro, data=filter(Group2_table, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Influenza_vaccine ~ Gastro, data=Group2_table, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(A_B_vaccine ~ Gastro, data=Group2_table, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Readmit ~ Gastro, data=filter(Group2_table, Patid %in% readmit_N_Group2), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Rifaximin ~ Gastro, data=filter(Group2_table, Patid %in% rifaximin_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Transplant_evaluation ~ Gastro, data=filter(Group2_table, Patid %in% Transplant_Evaluation_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci



temp = select(table2, Patid,AC,Hepatitis_C,Non_alcohol,Cirrhosis_complication,HE,Ascites,Varices,
              Race,Sex,score,Age,SBP,TIPS,HCC,Pneumonia,Sepsis,Urinary_tract_infection,Cellulitis,Bacteremia,Clostridium,Cholangitis,Paracentesis,Dialysis,max_visit)
Group2_table = merge(Group2_table, temp, by = "Patid", all.x = T)



formula = "~ Gastro+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Race+Sex+score+Age+SBP+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit"
output <- glm(paste("Endoscopy", formula, sep = " "),
              data = filter(Group2_table, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Screen_liver_cancer", formula, sep = " "), 
              data=Group2_table, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Influenza_vaccine", formula), 
              data=Group2_table, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("A_B_vaccine", formula), 
              data=Group2_table, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Rifaximin", formula), 
              data=filter(Group2_table, Patid %in% rifaximin_N), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Readmit", formula), 
              data=filter(Group2_table, Patid %in% readmit_N_Group2), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Transplant_evaluation ~ Gastro+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+Ascites+Varices+Race+Sex+score+Age+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Dialysis+max_visit, 
              data=filter(Group2_table, Patid %in% Transplant_Evaluation_N), family=binomial) # exclude HE, SBP, Paracentesis
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

save.image("Group2 complete.RData")



