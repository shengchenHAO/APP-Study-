setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 



## Select population with at least three month 
load("person_year.RData")
POP_patid = unique(filter(Person_year, Person_year >= 0.25)$Patid)


Screen_cancer_patid = c() 
Hosp_patid = c() # Hospitalized based on CPT codes
Influenza_vaccine_patid = c() 
AB_vaccine = c() 

for (year in 2001:2015){
  name = paste0("data_def_", year, ".RData")
  load(name)
  
  Screen_cancer_patid = c(Screen_cancer_patid, unique(filter(temp_data, CT == 1 | MRI == 1| Ultrasound == 1)$Patid)) 
  Hosp_patid = c(Hosp_patid, unique(filter(temp_data, (inpatient_initial_care == 1 | inpatient_subsequent_care == 1 | observation_initial_care == 1| observation_subsequent_care == 1) & (Fst_Dt >= APP_Dt | is.na(APP_Dt)) )$Patid))
  Influenza_vaccine_patid = c(Influenza_vaccine_patid, unique(filter(temp_data, Influenza_vaccine == 1 & (Fst_Dt >= APP_Dt | is.na(APP_Dt)) )$Patid)) 
  AB_vaccine = c(AB_vaccine, unique(filter(temp_data, (A_vaccine == 1 | B_vaccine == 1) & (Fst_Dt >= APP_Dt | is.na(APP_Dt)) )$Patid)) 
  
  rm(name, year, temp_data)
  gc()
}



load("liver_conf.RData")

# confinement data prepare
Readmit = function(data){
  temp = data %>%  
    dplyr::mutate(Disch_Date = as.Date(Disch_Date, "%Y-%m-%d")) %>% 
    dplyr::mutate(Admit_Date = as.Date(Admit_Date, "%Y-%m-%d")) %>%
    dplyr::group_by(Patid) %>% 
    dplyr::arrange(Patid, Admit_Date) %>%  
    dplyr::mutate(last_admit = dplyr::lag(Disch_Date, n = 1, default = NA)) %>% 
    dplyr::mutate(last_disc = dplyr::lag(Dstatus, n = 1, default = NA)) %>%
    dplyr::mutate(time_diff = as.Date(Admit_Date, "%Y-%m-%d") - as.Date(last_admit, "%Y-%m-%d"))  
  return(temp)
}

liver_conf = Readmit(liver_conf)
liver_conf = merge(liver_conf, Person_year, by = "Patid", all.x = T)

load("APP&MD dates.RData")
APP_date = APP_date %>%  
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  group_by(Patid) %>% 
  arrange(Patid, Fst_Dt) %>%   
  dplyr::summarise(APP_Dt = first(Fst_Dt))

rm(APP_MD_date, MD_date)

liver_conf = merge(liver_conf, APP_date, all.x = T, by = "Patid") # merge the first APP date 
# Compute the time denominator for all the numeric outcome (bed days or Table 4)

save(liver_conf, file = "liver_conf.RData")  # same name as the constructing 


Hosp_patid = c(Hosp_patid, unique(filter(liver_conf, Patid %in% POP_patid & (Admit_Date > APP_Dt | is.na(APP_Dt)))$Patid))
Hosp_patid = unique(Hosp_patid)

# create table
liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 

table2 = data.frame(unique(liver_member_fixed$Patid)) 
colnames(table2) = "Patid"
table2 = table2 %>% 
  mutate(Screen_cancer = 1*(Patid %in% Screen_cancer_patid), 
         Influenza_vaccine = 1*(Patid %in% Influenza_vaccine_patid), 
         AB_vaccine = 1*(Patid %in% AB_vaccine), 
         Hospitalized = 1*(Patid %in% Hosp_patid) 

         )
load("Varices screening.RData")
Endoscopy_patid = unique(varice_screen$Patid)
table2 = mutate(table2, Endoscopy = 1*(Patid %in% Endoscopy_patid))
rm(varice_screen) 
gc()

### Important Endoscopy Censor 
load("censor_Endoscopy.RData")
table2 = mutate(table2, Endoscopy = ifelse(Patid %in% Censor_endoscopy_patid, 0, Endoscopy)) # actually excluded in the denominator 



# Radmission within 30 days 
readmit_patid = unique(filter(liver_conf, time_diff <= 30 & grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date < Lst_Date & 
                                (Admit_Date >= APP_Dt | is.na(APP_Dt)))$Patid)
readmit_N = unique(filter(liver_conf, grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date < Lst_Date)$Patid)
table2 = mutate(table2, Readmit = 1*(Patid %in% readmit_patid))


# bed days 
temp_beddays_NOAPP = liver_conf %>% # those who didn't have APP
  filter(Admit_Date < Lst_Date & is.na(APP_Dt)) %>% 
  filter(Los >= 0) %>%
  group_by(Patid) %>% 
  dplyr::summarise(bed_days = sum(Los))

temp_beddays = liver_conf %>% # those who had APP before
  filter(Admit_Date < Lst_Date & is.na(APP_Dt) == F) %>% 
  filter(Los >=0) %>% 
  filter(Admit_Date >= APP_Dt) %>% # only count those bed days after first APP 
  group_by(Patid) %>% 
  dplyr::summarise(bed_days = sum(Los))

temp_beddays = rbind(temp_beddays, temp_beddays_NOAPP)
rm(temp_beddays_NOAPP)

Beddays_patid = unique(filter(liver_conf, Los >= 0 & Admit_Date < Lst_Date)$Patid)
temp = data.frame(Beddays_patid) 
colnames(temp) = "Patid" 
temp = merge(temp, temp_beddays, by = "Patid", all.x = T)
temp = mutate(temp, bed_days = ifelse(is.na(bed_days), 0, bed_days))

table2 = merge(table2, temp, all.x = T, by = "Patid")
rm(temp_beddays, liver_member_fixed, Person_year,temp)
gc()

# rifaximin after discharge
load("Pharmacy_data.RData")
load("APP&MD dates.RData")
rm(APP_MD_date, MD_date)
APP_date = APP_date %>%  
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  group_by(Patid) %>% 
  arrange(Patid, Fst_Dt) %>%   
  dplyr::summarise(APP_Dt = first(Fst_Dt))
liver_pharm = merge(liver_pharm, APP_date, by = "Patid", all.x = T)


rifaximin = unique(filter(liver_pharm, (grepl("RIFAXIMIN", Brnd_Nm) | grepl("RIFAXIMIN", Gnrc_Nm)) & (Fill_Dt >= APP_Dt | is.na(APP_Dt)) )$Patid)
temp_rifaximin = unique(filter(liver_conf, grepl("^5722",Diag1)|grepl("^5722",Diag2)|grepl("^5722",Diag3)|grepl("^5722",Diag4)|grepl("^5722",Diag5)
                               )$Patid)
rifaximin = rifaximin[ rifaximin %in% temp_rifaximin ]# find the intersection

table2 = mutate(table2, rifaximin = 1*(Patid %in% rifaximin))
 
rifaximin_patid = temp_rifaximin
rm(temp_rifaximin, liver_pharm)



## Transplant evaluation 
Transplant_Evaluation_N = c() # Denominator for tranplant evaluation

for (year in 2001:2015){ # already get tranplant patids from data of table 1
  name = paste0("data_def_", year, ".RData")
  load(name)
  
  Transplant_Evaluation_N = c(Transplant_Evaluation_N, unique(filter(temp_data, HE == 1 | Spontaneous_Bacterial_Peritonitis == 1 | Paracentesis == 1)$Patid)) 
 
  rm(name, year, temp_data)
  gc()
}

# actually we can do this way 
Transplant_Evaluation_N = unique(c(HE_patid, Paracentesis_patid, SBP_patid))


# APP 
APP_patid = unique(APP_date$Patid)
table2 = mutate(table2, APP = 1*(Patid %in% APP_patid))

##################
## IMPORTANT
##################
table2 = filter(table2, Patid %in% POP_patid)


# merge the person year and last date to table 2
load("person_year.RData") 
temp = select(Person_year, Patid, Lst_Date, Person_year)
table2 = merge(table2, temp, by = "Patid", all.x = T)
rm(temp)
table2 = merge(table2, APP_date, all.x = T, by = "Patid")
table2 = mutate(table2, Denominator = ifelse(is.na(APP_Dt), Person_year, (Lst_Date - APP_Dt)/365)) # Compute the time denominator for all the numeric outcome (bed days or Table 4)
#table2 = mutate(table2, bed_days = ifelse(Patid %in% Beddays_patid & is.na(bed_days), 0, bed_days))

# Keep those who had at least 3 month of records after their first APP date
#table2 = filter(table2, Denominator >= 0.5)

# Need the patids from table 1 
load("APP&MD dates.RData")
rm(APP_date, MD_date)

# find the first date of screen
load("table4 liver cancer.RData")
screen_date = data_table4_liver_cancer %>% 
  select(Patid, Fst_Dt) %>% 
  arrange(Patid, Fst_Dt) %>% 
  group_by(Patid) %>% 
  summarise(Screen_Date = first(Fst_Dt))
rm(data_table4_liver_cancer)



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
Hepatology_only_patid = c()
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


for (year in 2013:2015){
  name = paste0("data_def_", year, ".RData")
  load(name) 
  
  # memory issue
  Patid_list = unique(temp_data$Patid)
  list1 = Patid_list[1 : floor(length(Patid_list)/3)]
  list2 = Patid_list[(floor(length(Patid_list)/3)+1) : floor(length(Patid_list)*2/3)]
  list3 = Patid_list[Patid_list %!in% list1 & Patid_list %!in% list2]
  
  data1 = filter(temp_data, Patid %in% list1)
  data2 = filter(temp_data, Patid %in% list2)
  data3 = filter(temp_data, Patid %in% list3)
  save(data2, file = "Process2.RData")
  save(data3, file = "Process3.RData")
  rm(temp_data,data2, data3)
  gc()
  
  
  data1 = merge(data1, screen_date, by = "Patid", all.x = T)
  save(data1, file = "Process1.RData") 
  rm(data1) 
  gc()
  
  load("Process2.RData")
  data2 = merge(data2, screen_date, by = "Patid", all.x = T)
  save(data2, file = "Process2.RData")
  rm(data2)
  gc()
  
  load("Process3.RData")
  data3 = merge(data3, screen_date, by = "Patid", all.x = T)
  gc()
  
  load("Process1.RData")
  load("Process2.RData")
  temp_data = rbind(data1,data2, data3)
  rm(data1,data2, data3)
  gc()
  
  AC_patid = c(AC_patid, unique(filter(temp_data, (Alcoholic_cirrhosis == 1 | Alcohol_use == 1 | Alcoholic_liver_disease == 1) & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Hepatitis_C_patid = c(Hepatitis_C_patid, unique(filter(temp_data, Hepatitis_C == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Non_alcohol_patid = c(Non_alcohol_patid, unique(filter(temp_data, Non_alcohol == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Cirrhosis_complication_patid = c(Cirrhosis_complication_patid, unique(filter(temp_data, grepl("4560|78959|78951|5722|56723|5724", Diag) & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  HE_patid = c(HE_patid, unique(filter(temp_data, HE == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Ascites_patid = c(Ascites_patid, unique(filter(temp_data, Ascites == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Varices_patid = c(Varices_patid, unique(filter(temp_data, Varices == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  HCC_patid = c(HCC_patid, unique(filter(temp_data, HCC == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid)) 
  Gastro_patid = c(Gastro_patid, unique(filter(temp_data, Gastro == 1)$Patid))
  Gastro_only_patid = c(Gastro_only_patid, unique(filter(temp_data, Gastro == 1 & Hepatology == 0 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid)) # for adjusting
  Hepatology_patid = c(Hepatology_patid, unique(filter(temp_data, Hepatology == 1)$Patid))
  Hepatology_only_patid = c(Hepatology_only_patid, unique(filter(temp_data, Hepatology == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid)) # for adjusting
 # Transplant_Evaluation_patid = c(Transplant_Evaluation_patid, unique(filter(temp_data, Transplant_evaluation == 1)$Patid)) 
  SBP_patid = c(SBP_patid, unique(filter(temp_data, Spontaneous_Bacterial_Peritonitis == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid)) 
  TIPS_patid = c(TIPS_patid, unique(filter(temp_data, TIPS == 1 & (Fst_Dt >= APP_Dt | is.na(APP_Dt)) & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid)) # Procdure
  Pneumonia_patid = c(Pneumonia_patid, unique(filter(temp_data, Pneumonia == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid)) 
  Sepsis_patid = c(Sepsis_patid, unique(filter(temp_data, Sepsis == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Urinary_tract_infection_patid = c(Urinary_tract_infection_patid, unique(filter(temp_data, Urinary_tract_infection == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Cellulitis_patid = c(Cellulitis_patid, unique(filter(temp_data, Cellulitis == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Bacteremia_patid = c(Bacteremia_patid, unique(filter(temp_data, Bacteremia == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid)) 
  Clostridium_patid = c(Clostridium_patid, unique(filter(temp_data, Clostridium == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Cholangitis_patid = c(Cholangitis_patid, unique(filter(temp_data, Cholangitis == 1 & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid))
  Paracentesis_patid = c(Paracentesis_patid, unique(filter(temp_data, Paracentesis == 1 & (Fst_Dt >= APP_Dt | is.na(APP_Dt)) & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid)) #Procdure
  Dialysis_patid = c(Dialysis_patid, unique(filter(temp_data, Dialysis == 1 & (Fst_Dt >= APP_Dt | is.na(APP_Dt)) & (is.na(Screen_Date) | Fst_Dt <= Screen_Date))$Patid)) #Procdure
  
  save("!", file = paste0(year))
  rm(temp_data, name, year) 
  gc()
  
}

save.image("Screen adjusting variables.RData")

table2 = mutate(table2, AC = 1*(Patid %in% AC_patid), 
                Hepatitis_C = 1*(Patid %in% Hepatitis_C_patid), 
                Non_alcohol = 1*(Patid %in% Non_alcohol_patid), 
                Cirrhosis_complication = 1*(Patid %in% Cirrhosis_complication_patid), 
                HE = 1*(Patid %in% HE_patid), 
                Ascites = 1*(Patid %in% Ascites_patid), 
                Varices = 1*(Patid %in% Varices_patid),
                Gastro = 1*(Patid %in% Gastro_patid),
                Gastro_only = 1*(Patid %in% Gastro_only_patid), 
                Hepatology = 1*(Patid %in% Hepatology_patid),
                Hepatology_only = 1*(Patid %in% Hepatology_only_patid),
                SBP = 1*(Patid %in% SBP_patid), 
                Transplant_Evaluation = 1*(Patid %in% Transplant_Evaluation_patid), 
                TIPS = 1*(Patid %in% TIPS_patid), 
                HCC = 1*(Patid %in% HCC_patid), 
                Pneumonia = 1*(Patid %in% Pneumonia_patid), 
                Sepsis = 1*(Patid %in% Sepsis_patid), 
                Urinary_tract_infection = 1*(Patid %in% Urinary_tract_infection_patid), 
                Cellulitis = 1*(Patid %in% Cellulitis_patid), 
                Bacteremia = 1*(Patid %in% Bacteremia_patid), 
                Clostridium = 1*(Patid %in% Clostridium_patid), 
                Cholangitis = 1*(Patid %in% Cholangitis_patid), 
                Paracentesis = 1*(Patid %in% Paracentesis_patid), 
                Dialysis = 1*(Patid %in% Dialysis_patid))

rm(APP_MD_date)

# fill the table
#load("Exclude Patids.RData") # not use for now


'%!in%' = Negate('%in%')
table(filter(table2, Denominator >= 1)$Screen_cancer, filter(table2, Denominator >= 1)$APP)
table(filter(table2, Patid %!in% Censor_endoscopy_patid & Denominator >= 1)$Endoscopy, filter(table2, Patid %!in% Censor_endoscopy_patid & Denominator >= 1)$APP) # different Denominator
table(filter(table2, Denominator >= 1)$Influenza_vaccine, filter(table2, Denominator >= 1)$APP)
table(filter(table2, Denominator >= 1)$AB_vaccine, filter(table2, Denominator >= 1)$APP)
table(filter(table2, Denominator >= 1)$Hospitalized, filter(table2, Denominator >= 1)$APP)
table(filter(table2, Patid %in% rifaximin_patid & Denominator >= 1)$rifaximin, filter(table2, Patid %in% rifaximin_patid & Denominator >= 1)$APP)
table(filter(table2, Patid %in% readmit_N & Denominator >= 1)$Readmit, filter(table2, Patid %in% readmit_N & Denominator >= 1)$APP) # different Denominator
table(filter(table2, Denominator >= 1 & Patid %in% Transplant_Evaluation_N)$Transplant_Evaluation, filter(table2, Denominator >= 1 & Patid %in% Transplant_Evaluation_N)$APP)

summary(filter(table2, APP ==1 & Patid %in% Beddays_patid & Denominator >= 1)$bed_days)
summary(filter(table2, APP ==0 & Patid %in% Beddays_patid & Denominator >= 1)$bed_days)
sum(filter(table2, Denominator >= 1)$Patid %in% Beddays_patid) # number of denominator in Hospital bed days 


# logistic regression 
logistic.regression.or.ci <- function(regress.out, level=0.95)
{
  ################################################################
  #                                                              #
  #  This function takes the output from a glm                   #
  #  (logistic model) command in R and provides not              #
  #  only the usual output from the summary command, but         #
  #  adds confidence intervals for all coefficients and OR's.    #
  #                                                              #
  #  This version accommodates multiple regression parameters    #
  #                                                              #
  ################################################################
  usual.output <- summary(regress.out)
  z.quantile <- qnorm(1-(1-level)/2)
  number.vars <- length(regress.out$coefficients)
  OR <- exp(regress.out$coefficients[-1])
  temp.store.result <- matrix(rep(NA, number.vars*2), nrow=number.vars)
  for(i in 1:number.vars)
  {
    temp.store.result[i,] <- summary(regress.out)$coefficients[i] +
      c(-1, 1) * z.quantile * summary(regress.out)$coefficients[i+number.vars]
  }
  intercept.ci <- temp.store.result[1,]
  slopes.ci <- temp.store.result[-1,]
  OR.ci <- exp(slopes.ci)
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci,
                 slopes.ci = slopes.ci, OR=OR, OR.ci = OR.ci)
  return(output)
}

Coverage = 1 # coverage time after APP 


output <- glm(Endoscopy ~ APP, data=filter(table2, Patid %!in% Censor_endoscopy_patid & Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Screen_cancer ~ APP, data=filter(table2, Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Influenza_vaccine ~ APP, data=filter(table2, Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(AB_vaccine ~ APP, data= filter(table2, Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Hospitalized ~ APP, data=filter(table2, Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(rifaximin ~ APP, data=filter(table2, Patid %in% rifaximin_patid & Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Readmit ~ APP, data=filter(table2, Patid %in% readmit_N & Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output = MASS::glm.nb(bed_days ~ APP+offset(log(Denominator)), data = filter(table2, Patid %in% Beddays_patid & Denominator >= Coverage))
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Transplant_Evaluation ~ APP, data=filter(table2, Denominator >= Coverage & Patid %in% Transplant_Evaluation_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

# adjust odd ratio 

############ not use ##################
load("APP MD ratio.RData")
temp = data_time_Provider %>% 
  group_by(Patid) %>% 
  summarise(max_ratio = first(max_ratio))
rm(data_time_Provider)
gc()
table2 = merge(table2,temp,by = "Patid", all.x = T)
#######################################

load("outpatient visit count.RData")
temp = data_time_Proc %>% 
  group_by(Patid) %>% 
  summarise(max_visit = first(max_visit))
rm(data_time_Proc)
gc()
table2 = merge(table2,temp,by = "Patid", all.x = T) 
rm(temp)

# Gender and Race
liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 
temp = distinct(select(liver_member_fixed, Patid, Gdr_Cd, Race)) 
temp = temp%>% 
  group_by(Patid) %>% 
  arrange(Patid, Race) %>% 
  summarise(Gdr_Cd = first(Gdr_Cd), Race = first(Race)) %>% 
  ungroup()

table2 = merge(table2, temp, by="Patid",all.x = T) 

# charlson index
load("charlson index.RData") 

temp = select(charlson9, Patid, score) 
rm(charlson9) 
table2 = merge(table2, temp, by = "Patid", all.x = T) 
rm(temp)

# Age 
load("person_year.RData")
temp = select(Person_year, Patid, Age) 
table2 = merge(table2, temp, by = "Patid", all.x = T)
rm(temp)

# APP Hep and APP Gastro interaction 
table2 = table2 %>% 
  mutate(APP_Gastro = 1*(APP == 1 & Gastro_only == 1), 
                APP_Hepatology = 1*(APP == 1 & Hepatology == 1)
                ) %>%
  mutate(Sex = ifelse(Gdr_Cd == "M", 1, ifelse(Gdr_Cd == "F", 2, NA))) %>% 
  mutate(Race = ifelse(Race == "U"|Race == "", NA, Race)) 

# now we have the table for adjustted regression 
formula = "~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Race+Sex+score+Age+SBP+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit+Gastro_only+Hepatology"


output <- glm(paste("Endoscopy", formula, sep = " "),
              data = filter(table2, Patid %!in% Censor_endoscopy_patid & Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Screen_cancer", formula, sep = " "), 
              data=filter(table2, Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Influenza_vaccine", formula), 
              data=filter(table2, Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("AB_vaccine", formula), 
              data=filter(table2, Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Hospitalized", formula), 
              data=filter(table2, Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("rifaximin", formula), 
              data=filter(table2, Patid %in% rifaximin_patid & Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Readmit", formula), 
              data=filter(table2, Patid %in% readmit_N & Denominator >= Coverage), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output = MASS::glm.nb(paste("bed_days", formula, "+offset(log(Denominator))"), 
                      data = filter(table2, Patid %in% Beddays_patid & Denominator >= Coverage))
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Transplant_Evaluation ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+Ascites+Varices+Race+Sex+score+Age+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Dialysis+max_visit+Gastro_only+Hepatology, 
              data=filter(table2, Denominator >= Coverage & Patid %in% Transplant_Evaluation_N), family=binomial) # exclude HE, SBP, Paracentesis
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

save.image("table2.RData")




################## Mathcing ####################

library(MatchIt)
library(knitr)

set.seed(1234)
match.it <- matchit(as.factor(APP) ~ Denominator+Person_year, data = dplyr::select(table2, Patid, APP, Person_year, Denominator), method="nearest", ratio=1)
a <- summary(match.it)
kable(a$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Sample sizes')
rm(a)
save(match.it, file = "Matching result.RData")
Matched_patid <- match.data(match.it)$Patid


Coverage = 0.5
output <- glm(Endoscopy ~ APP, data=filter(table2, Patid %!in% Censor_endoscopy_patid & Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Screen_cancer ~ APP, data=filter(table2, Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Influenza_vaccine ~ APP, data=filter(table2, Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(AB_vaccine ~ APP, data= filter(table2, Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Hospitalized ~ APP, data=filter(table2, Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(rifaximin ~ APP, data=filter(table2, Patid %in% rifaximin_patid & Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Readmit ~ APP, data=filter(table2, Patid %in% readmit_N & Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output = MASS::glm.nb(bed_days ~ APP+offset(log(Denominator)), data = filter(table2, Patid %in% Beddays_patid & Denominator >= Coverage & Patid %in% Matched_patid))
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Transplant_Evaluation ~ APP, data=filter(table2, Denominator >= Coverage & Patid %in% Transplant_Evaluation_N & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci





output <- glm(Endoscopy ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis,
              data = filter(table2, Patid %!in% Censor_endoscopy_patid & Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Screen_cancer ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis, 
              data=filter(table2, Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Influenza_vaccine ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis, 
              data=filter(table2, Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(AB_vaccine ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis, 
              data=filter(table2, Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Hospitalized ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis, 
              data=filter(table2, Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(rifaximin ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis, 
              data=filter(table2, Patid %in% rifaximin_patid & Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Readmit ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis, 
              data=filter(table2, Patid %in% readmit_N & Denominator >= Coverage & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output = MASS::glm.nb(bed_days ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+Transplant_Evaluation+HCC+Shared_visit+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+offset(log(Denominator)), 
                      data = filter(table2, Patid %in% Beddays_patid & Denominator >= Coverage & Patid %in% Matched_patid))
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Transplant_Evaluation ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Gastro_only+Hepatology+Race+Sex+score+Age+SBP+TIPS+HCC+Shared_visit+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis, 
              data=filter(table2, Denominator >= Coverage & Patid %in% Transplant_Evaluation_N & Patid %in% Matched_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

