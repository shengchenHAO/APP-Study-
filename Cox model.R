setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 

#install.packages(c("survival", "survminer"))
library("survival")
library("survminer")

load("table2.RData") # this is version with screen included, so need to use original version from table 1 
load("table1.RData")
load("person_year.RData")
load("APP&MD dates.RData")
rm(APP_date, MD_date)


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



temp = select(Person_year, Patid, Death_date, Trans_Dt)
table2 = merge(table2, temp, by = "Patid", all.x = T)
table2 = filter(table2, Denominator >= 1) # at least 6 months coverage after APP date
rm(temp)

# 2: death 1: censor 3: transplant
table2 = table2 %>%
  mutate(Status = ifelse(Death_date != Lst_Date| is.na(Death_date), 1, 2)) %>% 
  mutate(Status = ifelse(is.na(Death_date) == F & Death_date <= Lst_Date + 90, 2, Status)) %>% 
#  mutate(Status = ifelse(is.na(Trans_Dt)==F, 3, Status)) %>%
  mutate(time = 365*(Person_year)) %>% 
  mutate(Sex = ifelse(Gdr_Cd == "M", 1, ifelse(Gdr_Cd == "F", 2, NA))) %>% 
  mutate(Race = ifelse(Race == "U"|Race == "", NA, Race)) # two kind of missing value in Race

# change White to baseline
table2 = mutate(table2, Race = ifelse(Race == "W", 1, ifelse(Race == "A",2, ifelse(Race == "B", 3, ifelse(Race == "H", 4, Race)))))



# cox model #############

covariates <- c("Age", "Sex", "score", "AC", "Hepatitis_C", "Non_alcohol", "Ascites", "Varices", "HE", "HCC", "APP", "Gastro_only", "Hepatology", "Shared_visit", "Transplant_Evaluation", "SBP", "TIPS",
"APP_Gastro", "APP_Hepatology","Pneumonia","Sepsis","Urinary_tract_infection","Cellulitis","Bacteremia","Clostridium","Cholangitis","Paracentesis","Dialysis")
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
write.csv(temp, file = "tempfile.csv")

# Race univariate
res.cox <- coxph(Surv(time, Status) ~ Race, data = table2)
summary(res.cox)

# multivariate 
res.cox <- coxph(Surv(time, Status) ~ Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+APP+Gastro_only+Hepatology+SBP+TIPS+Race+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit,
                 data = table2)
x = data.frame(summary(res.cox)$conf.int)
x$exp..coef. = NULL 
x = mutate(x, lower..95 = round(lower..95, digits = 3), upper..95 = round(upper..95, digits = 3), exp.coef. = round(exp.coef., digits = 3)) 
x = mutate(x, result = paste0(exp.coef., " ", "(", lower..95, " ", upper..95,")")) 

write.csv(x, file = "tempfile.csv")
rm(x, temp)

# competing risk 
#devtools::install_github('raredd/cmprsk2')
table2 = table2 %>%
  mutate(Status = ifelse(Death_date != Lst_Date| is.na(Death_date), 1, 2)) %>% 
  mutate(Status = ifelse(is.na(Death_date) == F & Death_date <= Lst_Date + 90, 2, Status)) %>% 
  mutate(Status = ifelse(is.na(Trans_Dt)==F, 3, Status))


library(cmprsk2)
output1 <- crr2(Surv(time, Status(1)== 2) ~ Age+ Sex+ score + AC+ Hepatitis_C+ Non_alcohol+ Ascites+ Varices+ HE+ HCC+ APP+Gastro_only+Hepatology+SBP+TIPS+Race+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit+Shared_visit,
           data = table2)

x = summary(output1)
write.csv(x$`CRR: 2`, file = "tempfile death group.csv")
write.csv(x$`CRR: 3`, file = "tempfile tranplant group.csv")
rm(x,res,output)


# No shared visit 
output2 <- crr2(Surv(time, Status(1)== 2) ~ Age+ Sex+ score + AC+ Hepatitis_C+ Non_alcohol+ Ascites+ Varices+ HE+ HCC+ APP+ Gastro_only+ Hepatology+SBP+TIPS+Race+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis,
               data = table2, variance = F)

x = summary(output2)
write.csv(x$`CRR: 2`, file = "tempfile death group.csv")
write.csv(x$`CRR: 3`, file = "tempfile tranplant group.csv")
rm(x,res,output)











