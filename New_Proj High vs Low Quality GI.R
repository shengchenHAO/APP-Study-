setwd("X:/Shengchen Hao/Tapper Liver/R file/new/GI_proj") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 

load("Cox model.RData")
rm(res, output1, Gastro_unique_ID, High_quality)

temp = select(Person_year, Prov_Unique, Quality_measure, Patient_Num, Exp_level) %>% distinct()
summary(temp$Quality_measure)
rm(temp)
Person_year = mutate(Person_year, High_Quality = 1*(Quality_measure >= 0.333)) # decided by the IQR

AB_vaccine_date = select(filter(data_total, A_vaccine == 1 | B_vaccine == 1), Patid, Fst_Dt) %>% arrange(Patid, Fst_Dt) %>% group_by(Patid) %>% summarise(AB_date = first(Fst_Dt))
Screen_cancer_date = select(filter(data_total, CT == 1 | MRI == 1 | Ultrasound == 1), Patid, Fst_Dt) %>% arrange(Patid, Fst_Dt) %>% group_by(Patid) %>% summarise(Screen_date = first(Fst_Dt))
Endoscopy_date = select(filter(data_total, Endoscopy == 1), Patid, Fst_Dt) %>% arrange(Patid, Fst_Dt) %>% group_by(Patid) %>% summarise(Endoscopy_date = first(Fst_Dt))
Influenza_date = select(filter(data_total, Influenza_vaccine == 1), Patid, Fst_Dt) %>% arrange(Patid, Fst_Dt) %>% group_by(Patid) %>% summarise(Influenza_Date = first(Fst_Dt))


Person_year = mutate(Person_year, AB_vaccine = 1*(Patid %in% AB_vaccine_patid), 
                     Influenza_vaccine = 1*(Patid %in% Influenza_date$Patid), 
                     Screen_cancer = 1*(Patid %in% Screen_cancer_patid),
                     Endoscopy = 1*(Patid %in% Endoscopy_patid)) 

#  merge the dates into data_total -----------------------------------------------------------------------------------------------------------------------
# run in flux 
save(list = c("data_total", "AB_vaccine_date", "Screen_cancer_date", "Endoscopy_date", "Influenza_date"), file = "flux_temp.RData")
data_total = data_total %>% 
  merge(y = AB_vaccine_date, all.x = T, by = "Patid") %>% 
  merge(y = Screen_cancer_date, all.x = T, by = "Patid") %>% 
  merge(y = Endoscopy_date, all.x = T, by = "Patid") %>% 
  merge(y = Influenza_date, all.x = T, by = "Patid")  

save(data_total, file = "data merged dates.RData")
#  merge the dates into data_total -----------------------------------------------------------------------------------------------------------------------
load("data merged dates.RData")
rm(AB_vaccine_date,Endoscopy_date,Influenza_date, Screen_cancer_date)
# Get the patids based on different outcome date/ AB vaccine
AC_AB = unique(filter(data_total,(Alcoholic_cirrhosis == 1 | Alcohol_use == 1 | Alcoholic_liver_disease == 1) & (Fst_Dt <= AB_date | is.na(AB_date)))$Patid)
Hepatitis_C_AB = unique(filter(data_total, Hepatitis_C == 1 & (Fst_Dt <= AB_date | is.na(AB_date)))$Patid) 
Non_alcohol_AB = unique(filter(data_total, Non_alcohol == 1 & (Fst_Dt <= AB_date | is.na(AB_date)))$Patid) 
Ascites_AB = unique(filter(data_total, Ascites == 1 & (Fst_Dt <= AB_date | is.na(AB_date)))$Patid) 
Varices_AB = unique(filter(data_total, Varices == 1 & (Fst_Dt <= AB_date | is.na(AB_date)))$Patid)
HE_AB = unique(filter(data_total, HE == 1 & (Fst_Dt <= AB_date | is.na(AB_date)))$Patid) 
HCC_AB = unique(filter(data_total, HCC == 1 & (Fst_Dt <= AB_date | is.na(AB_date)))$Patid)
Hepatology_AB = unique(filter(data_total, Hepatology == 1 & (Fst_Dt <= AB_date | is.na(AB_date)))$Patid)

Person_year_AB = mutate(Person_year, AC = 1*(Patid %in% AC_AB), 
                     Hepatitis_C = 1*(Patid %in% Hepatitis_C_AB),
                     Non_alcohol = 1*(Patid %in% Non_alcohol_AB), 
                     HE = 1*(Patid %in% HE_AB), 
                     Ascites = 1*(Patid %in% Ascites_AB), 
                     Varices = 1*(Patid %in% Varices_AB), 
                     HCC = 1*(Patid %in% HCC_AB), 
                     Hepatology = 1*(Patid %in% Hepatology_AB))
rm(AC_AB, Hepatitis_C_AB, Non_alcohol_AB, Ascites_AB, Varices_AB, HE_AB, HCC_AB, Hepatology_AB)

# logistic regression -------------------------------------------------------------------------------------------------------------------------------------
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

output <- glm(AB_vaccine ~ High_Quality, data=Person_year_AB, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(AB_vaccine ~ High_Quality+Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+Hepatology+Exp_level+score+Race, data=Person_year_AB, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

table(Person_year_AB$AB_vaccine, Person_year_AB$High_Quality)


#  Screen for liver cancer -----------------------------------------------------------------------------------------------------------------------
AC_Screen = unique(filter(data_total,(Alcoholic_cirrhosis == 1 | Alcohol_use == 1 | Alcoholic_liver_disease == 1) & (Fst_Dt <= Screen_date | is.na(Screen_date)))$Patid)
Hepatitis_C_Screen = unique(filter(data_total, Hepatitis_C == 1 & (Fst_Dt <= Screen_date | is.na(Screen_date)))$Patid) 
Non_alcohol_Screen = unique(filter(data_total, Non_alcohol == 1 & (Fst_Dt <= Screen_date | is.na(Screen_date)))$Patid) 
Ascites_Screen = unique(filter(data_total, Ascites == 1 & (Fst_Dt <= Screen_date | is.na(Screen_date)))$Patid) 
Varices_Screen = unique(filter(data_total, Varices == 1 & (Fst_Dt <= Screen_date | is.na(Screen_date)))$Patid)
HE_Screen = unique(filter(data_total, HE == 1 & (Fst_Dt <= Screen_date | is.na(Screen_date)))$Patid) 
HCC_Screen = unique(filter(data_total, HCC == 1 & (Fst_Dt <= Screen_date | is.na(Screen_date)))$Patid)
Hepatology_Screen = unique(filter(data_total, Hepatology == 1 & (Fst_Dt <= Screen_date | is.na(Screen_date)))$Patid)

Person_year_Screen = mutate(Person_year, AC = 1*(Patid %in% AC_Screen), 
                        Hepatitis_C = 1*(Patid %in% Hepatitis_C_Screen),
                        Non_alcohol = 1*(Patid %in% Non_alcohol_Screen), 
                        HE = 1*(Patid %in% HE_Screen), 
                        Ascites = 1*(Patid %in% Ascites_Screen), 
                        Varices = 1*(Patid %in% Varices_Screen), 
                        HCC = 1*(Patid %in% HCC_Screen), 
                        Hepatology = 1*(Patid %in% Hepatology_Screen))
rm(AC_Screen, Hepatitis_C_Screen, Non_alcohol_Screen, Ascites_Screen, Varices_Screen, HE_Screen, HCC_Screen, Hepatology_Screen)

output <- glm(Screen_cancer ~ High_Quality, data=Person_year_Screen, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Screen_cancer ~ High_Quality+Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+Hepatology+Exp_level+score+Race, data=Person_year_Screen, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

table(Person_year_Screen$Screen_cancer, Person_year_Screen$High_Quality)

# Influenza vaccine --------------------------------------------------------------------------------------------------------------------------------------------------- 

AC_Influenza = unique(filter(data_total,(Alcoholic_cirrhosis == 1 | Alcohol_use == 1 | Alcoholic_liver_disease == 1) & (Fst_Dt <= Influenza_Date | is.na(Influenza_Date)))$Patid)
Hepatitis_C_Influenza = unique(filter(data_total, Hepatitis_C == 1 & (Fst_Dt <= Influenza_Date | is.na(Influenza_Date)))$Patid) 
Non_alcohol_Influenza = unique(filter(data_total, Non_alcohol == 1 & (Fst_Dt <= Influenza_Date | is.na(Influenza_Date)))$Patid) 
Ascites_Influenza = unique(filter(data_total, Ascites == 1 & (Fst_Dt <= Influenza_Date | is.na(Influenza_Date)))$Patid) 
Varices_Influenza = unique(filter(data_total, Varices == 1 & (Fst_Dt <= Influenza_Date | is.na(Influenza_Date)))$Patid)
HE_Influenza = unique(filter(data_total, HE == 1 & (Fst_Dt <= Influenza_Date | is.na(Influenza_Date)))$Patid) 
HCC_Influenza = unique(filter(data_total, HCC == 1 & (Fst_Dt <= Influenza_Date | is.na(Influenza_Date)))$Patid)
Hepatology_Influenza = unique(filter(data_total, Hepatology == 1 & (Fst_Dt <= Influenza_Date | is.na(Influenza_Date)))$Patid)

Person_year_Influenza = mutate(Person_year, AC = 1*(Patid %in% AC_Influenza), 
                            Hepatitis_C = 1*(Patid %in% Hepatitis_C_Influenza),
                            Non_alcohol = 1*(Patid %in% Non_alcohol_Influenza), 
                            HE = 1*(Patid %in% HE_Influenza), 
                            Ascites = 1*(Patid %in% Ascites_Influenza), 
                            Varices = 1*(Patid %in% Varices_Influenza), 
                            HCC = 1*(Patid %in% HCC_Influenza), 
                            Hepatology = 1*(Patid %in% Hepatology_Influenza))
rm(AC_Influenza, Hepatitis_C_Influenza, Non_alcohol_Influenza, Ascites_Influenza, Varices_Influenza, HE_Influenza, HCC_Influenza, Hepatology_Influenza)

output <- glm(Influenza_vaccine ~ High_Quality, data=Person_year_Influenza, family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Influenza_vaccine ~ High_Quality+Age+Sex+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+Hepatology+Exp_level+score+Race, data=Person_year_Influenza, family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

table(Person_year_Influenza$Influenza_vaccine, Person_year_Influenza$High_Quality)

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Endoscopy --------------------------------------------------------------------------------------------------------------------------------------------------- 
'%!in%' = Negate('%in%')
load("censor_Endoscopy.RData")

AC_Endoscopy = unique(filter(data_total,(Alcoholic_cirrhosis == 1 | Alcohol_use == 1 | Alcoholic_liver_disease == 1) & (Fst_Dt <= Endoscopy_date | is.na(Endoscopy_date)))$Patid)
Hepatitis_C_Endoscopy = unique(filter(data_total, Hepatitis_C == 1 & (Fst_Dt <= Endoscopy_date | is.na(Endoscopy_date)))$Patid) 
Non_alcohol_Endoscopy = unique(filter(data_total, Non_alcohol == 1 & (Fst_Dt <= Endoscopy_date | is.na(Endoscopy_date)))$Patid) 
Ascites_Endoscopy = unique(filter(data_total, Ascites == 1 & (Fst_Dt <= Endoscopy_date | is.na(Endoscopy_date)))$Patid) 
Varices_Endoscopy = unique(filter(data_total, Varices == 1 & (Fst_Dt <= Endoscopy_date | is.na(Endoscopy_date)))$Patid)
HE_Endoscopy = unique(filter(data_total, HE == 1 & (Fst_Dt <= Endoscopy_date | is.na(Endoscopy_date)))$Patid) 
HCC_Endoscopy = unique(filter(data_total, HCC == 1 & (Fst_Dt <= Endoscopy_date | is.na(Endoscopy_date)))$Patid)
Hepatology_Endoscopy = unique(filter(data_total, Hepatology == 1 & (Fst_Dt <= Endoscopy_date | is.na(Endoscopy_date)))$Patid)

Person_year_Endoscopy = mutate(Person_year, AC = 1*(Patid %in% AC_Endoscopy), 
                               Hepatitis_C = 1*(Patid %in% Hepatitis_C_Endoscopy),
                               Non_alcohol = 1*(Patid %in% Non_alcohol_Endoscopy), 
                               HE = 1*(Patid %in% HE_Endoscopy), 
                               Ascites = 1*(Patid %in% Ascites_Endoscopy), 
                               Varices = 1*(Patid %in% Varices_Endoscopy), 
                               HCC = 1*(Patid %in% HCC_Endoscopy), 
                               Hepatology = 1*(Patid %in% Hepatology_Endoscopy))
rm(AC_Endoscopy, Hepatitis_C_Endoscopy, Non_alcohol_Endoscopy, Ascites_Endoscopy, Varices_Endoscopy, HE_Endoscopy, HCC_Endoscopy, Hepatology_Endoscopy)

output <- glm(Endoscopy ~ High_Quality, data=filter(Person_year_Endoscopy, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR 
logistic.regression.or.ci(output)$OR.ci

output <- glm(Endoscopy ~ High_Quality+Age+Sex+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+Hepatology+Exp_level+score+Race, data=filter(Person_year_Endoscopy, Patid %!in% Censor_endoscopy_patid), family=binomial)
logistic.regression.or.ci(output)$OR[1]
logistic.regression.or.ci(output)$OR.ci[1,]

table(Person_year_Endoscopy$Endoscopy, Person_year_Endoscopy$High_Quality)

# Hospital data --------------------------------------------------------------------------------------------------------------------------------------------------- 
load("liver_conf.RData")
temp = Person_year$Patid
liver_conf = dplyr::filter(liver_conf, Patid %in% temp)
rm(temp)
temp = select(Person_year, Patid, GI_first_date)
liver_conf = merge(liver_conf, temp, all.x = T, by = "Patid")

rm(temp)
liver_conf = filter(liver_conf, Admit_Date >= GI_first_date & Admit_Date <= Lst_Date)

readmit_patid = unique(filter(liver_conf, time_diff <= 30 & grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date <= Lst_Date)$Patid)
readmit_N = unique(filter(liver_conf, grepl("^01$|^06$|^07$|^10$|^11$|^12$|^13$|^14$|^15$|^16$|^17$|^18$|^19$",last_disc) & Admit_Date <= Lst_Date)$Patid)













