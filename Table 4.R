setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 

load("table2.RData")
load("person_year.RData")
load("liver_conf.RData")


temp_death = Person_year %>% 
  mutate(Status = ifelse(Death_date != Lst_Date| is.na(Death_date), 1, 2)) %>% 
  mutate(Status = ifelse(is.na(Death_date) == F & Death_date <= Lst_Date + 90, 2, Status)) 
death_patid = unique(filter(temp_death, Status == 2)$Patid) # find the death patids based on the rule from Cox model

table4 = filter(table2, Denominator >= 1) # 1 year coverage after APP
rm(temp_death)
gc()


# 1. No APP  and NO GASTRO/HEPATOLOGY vs Gastro/hepatology WITHOUT APP
'%!in%' = Negate('%in%')
table4_1 = filter(table4, (APP == 0 & Gastro == 0 & Hepatology == 0) | (APP == 0 & (Gastro == 1 | Hepatology == 1)))
table4_1 = mutate(table4_1, Gastro_Hep = 1*(Gastro == 1 | Hepatology == 1))
table4_1 = mutate(table4_1, Death = 1*(Patid %in% death_patid))

table(table4_1$Screen_cancer, table4_1$Gastro_Hep)
table(filter(table4_1, Patid %!in% Censor_endoscopy_patid)$Endoscopy, filter(table4_1, Patid %!in% Censor_endoscopy_patid)$Gastro_Hep)
table(table4_1$Influenza_vaccine, table4_1$Gastro_Hep)
table(table4_1$Hospitalized, table4_1$Gastro_Hep)
table(table4_1$AB_vaccine, table4_1$Gastro_Hep)
table(filter(table4_1, Patid %in% rifaximin_patid)$rifaximin, filter(table4_1, Patid %in% rifaximin_patid)$Gastro_Hep)
table(filter(table4_1, Patid %in% readmit_N)$Readmit, filter(table4_1, Patid %in% readmit_N)$Gastro_Hep)
table(table4_1$Death, table4_1$Gastro_Hep)
table(filter(table4_1, Patid %in% Transplant_Evaluation_N)$Transplant_Evaluation, filter(table4_1, Patid %in% Transplant_Evaluation_N)$Gastro_Hep)

summary(filter(table4_1, Gastro_Hep == 0 & Patid %in% Beddays_patid)$bed_days)
summary(filter(table4_1, Gastro_Hep == 1 & Patid %in% Beddays_patid)$bed_days)

# N for bed days 
length(unique(filter(liver_conf, Los >= 0 & Admit_Date < Lst_Date & Patid %in% unique(table4_1$Patid))$Patid)) 

# N for Readmission 
length(unique(filter(liver_conf, grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date < Lst_Date & Patid %in% unique(table4_1$Patid))$Patid))


output <- glm(Endoscopy ~ Gastro_Hep, data=filter(table4_1, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Screen_cancer ~ Gastro_Hep, data=table4_1, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Influenza_vaccine ~ Gastro_Hep, data=table4_1, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(AB_vaccine ~ Gastro_Hep, data=table4_1, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Hospitalized ~ Gastro_Hep, data=table4_1, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output = MASS::glm.nb(bed_days ~ Gastro_Hep+offset(log(Denominator)), data = filter(table4_1, Patid %in% Beddays_patid))
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(rifaximin ~ Gastro_Hep, data=filter(table4_1, Patid %in% rifaximin_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Readmit ~ Gastro_Hep, data=filter(table4_1, Patid %in% readmit_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Death ~ Gastro_Hep, data=table4_1, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci 

output <- glm(Transplant_Evaluation ~ Gastro_Hep, data=filter(table4_1, Patid %in% Transplant_Evaluation_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

# adjust odd ratio 
formula = "~ Gastro_Hep+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Race+Sex+score+Age+SBP+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit"

output <- glm(paste("Endoscopy", formula, sep = " "),
              data = filter(table4_1, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Screen_cancer ~ Gastro_Hep+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Race+Sex+score+Age+SBP+TIPS+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit, 
              data=table4_1, family=binomial) #No HCC
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Influenza_vaccine", formula, sep = " "), 
              data=table4_1, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("AB_vaccine", formula, sep = " "), 
              data=table4_1, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Hospitalized", formula, sep = " "), 
              data=table4_1, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("rifaximin", formula, sep = " "), 
              data=filter(table4_1, Patid %in% rifaximin_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Readmit", formula, sep = " "), 
              data=filter(table4_1, Patid %in% readmit_N), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output = MASS::glm.nb(paste("bed_days", formula, "+offset(log(Denominator))"), 
                      data = filter(table4_1, Patid %in% Beddays_patid))
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Death", formula, sep = " "), 
              data=table4_1, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Transplant_Evaluation ~ Gastro_Hep+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+Ascites+Varices+Race+Sex+score+Age+TIPS+HCC+Shared_visit+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Dialysis, 
              data=filter(table4_1, Patid %in% Transplant_Evaluation_N), family=binomial) # exclude HE, SBP, Paracentesis
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]


# 2. Gastro WITHOUT APP vs  ANY APP (NO GASTRO/HEPATOLOGY visits)
table4_2 = filter(table4, ((Gastro == 1 | Hepatology == 1) & APP == 0) | (APP == 1 & Gastro == 0 & Hepatology == 0) )
table4_2 = mutate(table4_2, Gastro_Hep = 1*(Gastro == 1 | Hepatology == 1))
table4_2 = mutate(table4_2, Death = 1*(Patid %in% death_patid))

# N for bed days 
length(unique(filter(liver_conf, Los >= 0 & Admit_Date < Lst_Date & Patid %in% unique(table4_2$Patid))$Patid)) 

# N for Readmission 
length(unique(filter(liver_conf, grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date < Lst_Date & Patid %in% unique(table4_2$Patid))$Patid))

# N for rifaximin 
length(unique(filter(table4_2, Patid %in% rifaximin_patid)$Patid))


table(table4_2$Screen_cancer, table4_2$APP)
table(filter(table4_2, Patid %!in% Censor_endoscopy_patid)$Endoscopy, filter(table4_2, Patid %!in% Censor_endoscopy_patid)$APP)
table(table4_2$Influenza_vaccine, table4_2$APP)
table(table4_2$AB_vaccine, table4_2$APP)
table(table4_2$Hospitalized, table4_2$APP)
table(filter(table4_2, Patid %in% rifaximin_patid)$rifaximin, filter(table4_2, Patid %in% rifaximin_patid)$APP)
table(filter(table4_2, Patid %in% readmit_N)$Readmit, filter(table4_2, Patid %in% readmit_N)$APP)
table(table4_2$Death, table4_2$APP)
table(filter(table4_2, Patid %in% Transplant_Evaluation_N)$Transplant_Evaluation, filter(table4_2, Patid %in% Transplant_Evaluation_N)$APP)

summary(filter(table4_2, APP == 0 & Patid %in% Beddays_patid)$bed_days)
summary(filter(table4_2, APP == 1 & Patid %in% Beddays_patid)$bed_days)


output <- glm(Endoscopy ~ APP, data=filter(table4_2, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Screen_cancer ~ APP, data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Influenza_vaccine ~ APP, data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(AB_vaccine ~ APP, data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Hospitalized ~ APP, data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output = MASS::glm.nb(bed_days ~ APP+offset(log(Denominator)) , data = filter(table4_2, Patid %in% Beddays_patid))
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(rifaximin ~ APP, data=filter(table4_2, Patid %in% rifaximin_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Readmit ~ APP, data=filter(table4_2, Patid %in% readmit_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Death ~ APP, data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Transplant_Evaluation ~ APP, data=filter(table4_2, Patid %in% Transplant_Evaluation_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci 


# adjust regression

formula = "~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Race+Sex+score+Age+SBP+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit"


output <- glm(paste("Endoscopy", formula, sep = " "),
              data = filter(table4_2, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Screen_cancer ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Race+Sex+score+Age+SBP+TIPS+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit, 
              data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Influenza_vaccine", formula, sep = " "), 
              data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("AB_vaccine", formula), 
              data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Hospitalized", formula), 
              data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("rifaximin", formula), 
              data=filter(table4_2, Patid %in% rifaximin_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Readmit", formula), 
              data=filter(table4_2, Patid %in% readmit_N), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output = MASS::glm.nb(paste("bed_days", formula, "+offset(log(Denominator))"), 
                      data = filter(table4_2, Patid %in% Beddays_patid))
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Death", formula, sep = " "), 
              data=table4_2, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Transplant_Evaluation ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+Ascites+Varices+Race+Sex+score+Age+TIPS+HCC+Shared_visit+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Dialysis, 
              data=filter(table4_2, Patid %in% Transplant_Evaluation_N), family=binomial) # exclude HE, SBP, Paracentesis
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]






# GASTRO/HEPATOLGOY WITHOUT APP vs GASTRO WITH APP (SAME DAY)
load("APP&MD dates.RData")
load("Hep & Gastro date.RData")
APP_date = distinct(APP_date)
Gastro_date = distinct(Gastro_date) %>% 
  mutate(Gastro = 1)

APP_Gastro_sameday = merge(APP_date, Gastro_date, by = c("Patid", "Fst_Dt"), all.x = T)
APP_Gastro_sameday = distinct(filter(APP_Gastro_sameday, Gastro == 1))
patid_APP_Gastro_sameday = unique(APP_Gastro_sameday$Patid)

# create table 4_3
table4_3 = mutate(table4, Gastro_Hep = 1*(Gastro == 1 | Hepatology == 1), 
                  APP_Gastro_sameday = 1*(Patid %in% patid_APP_Gastro_sameday))
table4_3 = mutate(table4_3, Death = 1*(Patid %in% death_patid))
table4_3 = filter(table4_3, (Gastro_Hep == 1 & APP == 0)|(APP_Gastro_sameday == 1))


table(table4_3$Screen_cancer, table4_3$APP)
table(filter(table4_3, Patid %!in% Censor_endoscopy_patid)$Endoscopy, filter(table4_3, Patid %!in% Censor_endoscopy_patid)$APP)
table(table4_3$Influenza_vaccine, table4_3$APP)
table(table4_3$Hospitalized, table4_3$APP)
table(table4_3$AB_vaccine, table4_3$APP)
table(filter(table4_3, Patid %in% rifaximin_patid)$rifaximin, filter(table4_3, Patid %in% rifaximin_patid)$APP)
table(filter(table4_3, Patid %in% readmit_N)$Readmit, filter(table4_3, Patid %in% readmit_N)$APP)
table(table4_3$Death, table4_3$APP)
table(filter(table4_3, Patid %in% Transplant_Evaluation_N)$Transplant_Evaluation, filter(table4_3, Patid %in% Transplant_Evaluation_N)$APP)

summary(filter(table4_3, APP == 0 & Patid %in% Beddays_patid)$bed_days)
summary(filter(table4_3, APP == 1 & Patid %in% Beddays_patid)$bed_days)

# N for bed days 
length(unique(filter(liver_conf, Los >= 0 & Admit_Date < Lst_Date & Patid %in% unique(table4_3$Patid))$Patid)) 

# N for Readmission 
length(unique(filter(liver_conf, grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date < Lst_Date & Patid %in% unique(table4_3$Patid))$Patid))

# N for rifaximin 
length(unique(filter(table4_3, Patid %in% rifaximin_patid)$Patid))



output <- glm(Endoscopy ~ APP, data=filter(table4_3, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Screen_cancer ~ APP, data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Influenza_vaccine ~ APP, data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(AB_vaccine ~ APP, data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Hospitalized ~ APP, data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output = MASS::glm.nb(bed_days ~ APP+offset(log(Denominator)), data = filter(table4_3, Patid %in% Beddays_patid))
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(rifaximin ~ APP, data=filter(table4_3, Patid %in% rifaximin_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Readmit ~ APP, data=filter(table4_3, Patid %in% readmit_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Death ~ APP, data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Transplant_Evaluation ~ APP, data = filter(table4_3, Patid %in% Transplant_Evaluation_N), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci



# adjust regression

formula = "~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Race+Sex+score+Age+SBP+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit"



output <- glm(paste("Endoscopy", formula, sep = " "),
              data = filter(table4_3, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Screen_cancer ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+HE+Ascites+Varices+Race+Sex+score+Age+SBP+TIPS+HCC+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit, 
              data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Influenza_vaccine", formula, sep = " "), 
              data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("AB_vaccine", formula), 
              data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Hospitalized", formula), 
              data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("rifaximin", formula), 
              data=filter(table4_3, Patid %in% rifaximin_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Readmit", formula), 
              data=filter(table4_3, Patid %in% readmit_N), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output = MASS::glm.nb(paste("bed_days", formula, "+offset(log(Denominator))"), 
                      data = filter(table4_3, Patid %in% Beddays_patid))
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(paste("Death", formula, sep = " "), 
              data=table4_3, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

output <- glm(Transplant_Evaluation ~ APP+AC+Hepatitis_C+Non_alcohol+Cirrhosis_complication+Ascites+Varices+Race+Sex+score+Age+TIPS+HCC+Shared_visit+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Dialysis, 
              data=filter(table4_3, Patid %in% Transplant_Evaluation_N), family=binomial) # exclude HE, SBP, Paracentesis
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]



save.image("table4.RData")




