
setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 


temp = read_sas("X:/Shengchen Hao/Tapper Liver/Member Files/liver_diagnosis.sas7bdat") %>% 
  mutate(LIVER_DT = as.Date(LIVER_DT,  "%Y-%m-%d")) %>% 
  filter(FIRST_LIVER == 1) %>% 
  arrange(Patid, LIVER_DT) %>% 
  group_by(Patid) %>% 
  summarise(LIVER_DT = first(LIVER_DT))
  
load("person_year.RData")
Person_year_updated = merge(Person_year, temp, all.x = T, by = "Patid")
load("APP&MD dates.RData") 
rm(APP_MD_date, name, year, MD_date,temp)

APP_date = APP_date %>% 
  arrange(Patid, Fst_Dt) %>% 
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>%
  group_by(Patid) %>% 
  summarise(APP_Dt = first(Fst_Dt))

Person_year_updated = merge(Person_year_updated, APP_date, all.x = T, by = "Patid")
Person_year_updated = mutate(Person_year_updated, time_diag = as.numeric(LIVER_DT - Eligeff)) 
summary(filter(Person_year_updated, is.na(APP_Dt) == F)$time_diag) # start date to first diag of cirrhosis within APP patients 
summary(filter(Person_year_updated, is.na(APP_Dt) == T)$time_diag) # start date to first diag of cirrhosis within APP patients 


Person_year_updated = mutate(Person_year_updated, time_coverage = as.numeric(as.Date("2015-12-31", "%Y-%m-%d") - Eligend))
length(filter(Person_year_updated, is.na(Trans_Dt) & 
                (is.na(Death_date) | (is.na(Death_date) == F & Death_date > 90 )) & 
                time_coverage <= 180)$Patid)

length(filter(Person_year_updated, is.na(Death_date) == F & is.na(Trans_Dt) & Death_date <= 90 + Lst_Date)$Patid) # confirming the number from paper 


# Cox model part 

library("survival")
library("survminer")

load("table2.RData") # this is version with screen included, so need to use original version from table 1 
load("table1.RData")
#load("APP&MD dates.RData")

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



Person_year_updated = mutate(Person_year_updated, Cirr_before_APP = 1*(LIVER_DT <= APP_Dt & is.na(APP_Dt) == F))

temp = select(Person_year_updated, Patid, Death_date, Trans_Dt, Cirr_before_APP,LIVER_DT)
table2 = merge(table2, temp, by = "Patid", all.x = T)
table2 = filter(table2, Denominator >= 1) # at least 6 months coverage after APP date
rm(temp)

# 2: death 1: censor 3: transplant
table2 = table2 %>%
  mutate(Status = ifelse(Death_date != Lst_Date| is.na(Death_date), 1, 2)) %>% 
  mutate(Status = ifelse(is.na(Death_date) == F & Death_date <= Lst_Date + 90, 2, Status)) %>% 
  #  mutate(Status = ifelse(is.na(Trans_Dt)==F, 3, Status)) %>%
  mutate(time = as.numeric(Lst_Date - LIVER_DT)) %>% 
  mutate(Sex = ifelse(Gdr_Cd == "M", 1, ifelse(Gdr_Cd == "F", 2, NA))) %>% 
  mutate(Race = ifelse(Race == "U"|Race == "", NA, Race)) # two kind of missing value in Race

# change White to baseline
table2 = mutate(table2, Race = ifelse(Race == "W", 1, ifelse(Race == "A",2, ifelse(Race == "B", 3, ifelse(Race == "H", 4, Race)))))

# at least 1 year coverage after first diag of cirrhosis
table2 = filter(table2, time >=180) 
# cox model #############

covariates <- c("Age", "Sex", "score", "AC", "Hepatitis_C", "Non_alcohol", "Ascites", "Varices", "HE", "HCC", "APP", "Gastro_only", "Hepatology", "SBP", "TIPS",
                "APP_Gastro", "APP_Hepatology","Pneumonia","Sepsis","Urinary_tract_infection","Cellulitis","Bacteremia","Clostridium","Cholangitis","Paracentesis","Dialysis", "max_visit", "Shared_visit", "Cirr_before_APP")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, Status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = table2)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=4);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 4)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],4)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
temp = as.data.frame(res)
write.csv(temp, file = "single var cox model.csv")

# Race univariate
res.cox <- coxph(Surv(time, Status) ~ Race, data = table2)
summary(res.cox)

# multivariate 
res.cox <- coxph(Surv(time, Status) ~ Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+APP+Gastro_only+Hepatology+SBP+TIPS+Race+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit+Shared_visit+Cirr_before_APP,
                 data = table2)
x = data.frame(summary(res.cox)$conf.int)
x$exp..coef. = NULL 
x$Var = rownames(x)
x = mutate(x, lower..95 = round(lower..95, digits = 3), upper..95 = round(upper..95, digits = 3), exp.coef. = round(exp.coef., digits = 3)) 
x = mutate(x, result = paste0(exp.coef., " ", "(", lower..95, " ", upper..95,")")) 

write.csv(x, file = "multivariate cox.csv")
rm(x, temp)



# patients only with APP 


res.cox <- coxph(Surv(time, Status) ~ Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+APP+Gastro_only+Hepatology+SBP+TIPS+Race+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit+Shared_visit+Cirr_before_APP,
                 data = filter(table2, is.na(APP_Dt) == F))
x = data.frame(summary(res.cox)$conf.int)
x$exp..coef. = NULL 
x$Var = rownames(x)
x = mutate(x, lower..95 = round(lower..95, digits = 3), upper..95 = round(upper..95, digits = 3), exp.coef. = round(exp.coef., digits = 3)) 
x = mutate(x, result = paste0(exp.coef., " ", "(", lower..95, " ", upper..95,")")) 

write.csv(x, file = "multivariate cox.csv")
rm(x, temp)




# APP as time varying variable 

table2 = mutate(table2, APP_Cirrhosis = round(as.numeric(APP_Dt - LIVER_DT)/30),
                time = as.numeric(Lst_Date - LIVER_DT))

res.cox <- coxph(Surv(time, Status) ~ Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+APP+Gastro_only+Hepatology+SBP+TIPS+Race+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit+Shared_visit+Cirr_before_APP+APP_Cirrhosis,
                 data = filter(table2, is.na(APP_Dt) == F & APP_Dt > LIVER_DT))
x = data.frame(summary(res.cox)$conf.int)
x$exp..coef. = NULL 
x$Var = rownames(x)
x = mutate(x, lower..95 = round(lower..95, digits = 3), upper..95 = round(upper..95, digits = 3), exp.coef. = round(exp.coef., digits = 3)) 
x = mutate(x, result = paste0(exp.coef., " ", "(", lower..95, " ", upper..95,")")) 

write.csv(x, file = "multivariate cox.csv")
rm(x, temp)

