setwd("X:/Shengchen Hao/Tapper Liver/R file/new/GI_proj") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 
library(survival)
library(survminer)

load("data prepare.RData")
rm(GI_provider, GI_providerID, High_Std_num, GI_first_date,Mean_survival, Patient_included_num)


# Person year construct ---------------------------------------------------------------------------------------------------------
Person_year = merge(Person_year, High_quality, by = "Prov_Unique", all.x = T)

liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 
temp = select(liver_member_fixed, Patid, Race, Gdr_Cd)
temp = distinct(temp) %>% 
  arrange(Patid, Gdr_Cd, Race) %>% 
  group_by(Patid) %>% 
  summarise(Race = first(Race), 
            Sex = first(Gdr_Cd))

Person_year = merge(Person_year, temp, by = "Patid", all.x = T)
rm(temp, liver_member_fixed)

# Charlson Index 
load("charlson index.RData")
index = dplyr::select(charlson9, Patid, index)
Person_year = merge(Person_year, index, by = "Patid", all.x = T)
rm(index, charlson9)

load("table1_patid.RData")
Person_year = mutate(Person_year, AC = 1*(Patid %in% AC_patid), 
                     Hepatitis_C = 1*(Patid %in% Hepatitis_C_patid),
                     Hepatitis_B = 1*(Patid %in% Hepatitis_B_patid),
                     Non_alcohol = 1*(Patid %in% Non_alcohol_patid), 
                     Cirrhosis_complication = 1*(Patid %in% Cirrhosis_complication_patid), 
                     HE = 1*(Patid %in% HE_patid), 
                     Ascites = 1*(Patid %in% Ascites_patid), 
                     Varices = 1*(Patid %in% Varices_patid), 
                     HCC = 1*(Patid %in% HCC_patid), 
                     Hepatology = 1*(Patid %in% Hepatology_patid))

Person_year = Person_year %>%
  mutate(Status = ifelse(Death_date != Lst_Date| is.na(Death_date), 1, 2)) %>% 
  mutate(Status = ifelse(is.na(Death_date) == F & Death_date <= Lst_Date + 90, 2, Status)) %>% 
  mutate(time = Lst_Date - GI_first_date) %>% 
  mutate(Race = ifelse(Race == "U"|Race == "", NA, Race)) # two kind of missing value in Race
  
Person_year = mutate(Person_year, Sex = ifelse(Sex == "M", 1, ifelse(Sex == "F", 2, NA)))
Person_year = mutate(Person_year, Race = ifelse(Race == "W", 1, ifelse(Race == "A",2, ifelse(Race == "B", 3, ifelse(Race == "H", 4, Race)))))
Person_year$YEAR_OF_DEATH = NULL 
Person_year$MONTH_OF_DEATH = NULL

# Charlson score
load("charlson index.RData")
temp = select(charlson9, Patid, wscore)
colnames(temp) = c("Patid", "score")
Person_year = merge(Person_year, temp, by = "Patid", all.x = T)
rm(temp, charlson9)

# Experienced level 
Person_year = mutate(Person_year, Exp_level = ifelse(Patient_Num >=25 & Patient_Num <=36, 1, 
                                                     ifelse(Patient_Num >=37 & Patient_Num <=55,2,
                                                            ifelse(Patient_Num >=56 & Patient_Num <=94,3,
                                                                   ifelse(Patient_Num >=95, 4, 0)))))

Person_year = mutate(Person_year, Quality_measure = signif(Quality_measure, digits = 3))

Person_year = mutate(Person_year, Treatment = as.factor(Treatment), 
                     Exp_level = as.factor(Exp_level))

# Singel var cox model ---------------------------------------------------------------------------------------------------------
covariates <- c("Age", "Sex", "score", "AC", "Hepatitis_C", "Non_alcohol", "Ascites", "Varices", "HE", "HCC", "Hepatology")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, Status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = Person_year)})
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
rm(temp, univ_formulas, univ_models, univ_results)

# Race univariate
res.cox <- coxph(Surv(time, Status) ~ Race, data = Person_year)
summary(res.cox)

rm(res.cox, res)

# Treatment as univariate
res.cox <- coxph(Surv(time, Status) ~ Treatment, data = Person_year)
summary(res.cox)
rm(res.cox)
# Experience level as univariate
res.cox <- coxph(Surv(time, Status) ~ Exp_level, data = Person_year)
summary(res.cox)
rm(res.cox)

# Experience level as univariate
res.cox <- coxph(Surv(time, Status) ~ Quality_measure, data = Person_year)
summary(res.cox)
rm(res.cox)
# ----------------------------------------------------------------------------------------------------------------------------


# Multivariate Cox-----------------------------------------------------------------------------------------------------------
res.cox <- coxph(Surv(time, Status) ~ Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+Hepatology+score+Race+Treatment+Exp_level+Quality_measure,
                 data = Person_year)

x = data.frame(summary(res.cox)$conf.int)
x$exp..coef. = NULL 
x$Var = rownames(x)
x = mutate(x, lower..95 = round(lower..95, digits = 3), upper..95 = round(upper..95, digits = 3), exp.coef. = round(exp.coef., digits = 3)) 
x = mutate(x, result = paste0(exp.coef., " ", "(", lower..95, " ", upper..95,")")) 

write.csv(x, file = "multivariate cox.csv")
rm(x, temp, res.cox)

# ----------------------------------------------------------------------------------------------------------------------------

# competing risk -------------------------------------------------------------------------------------------------------------
library(cmprsk2)
Person_year = Person_year %>%
  mutate(Status = ifelse(Death_date != Lst_Date| is.na(Death_date), 1, 2)) %>% 
  mutate(Status = ifelse(is.na(Death_date) == F & Death_date <= Lst_Date + 90, 2, Status)) %>% 
  mutate(Status = ifelse(is.na(Trans_Dt)==F, 3, Status))

output1 <- crr2(Surv(time, Status(1)== 2) ~ Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+Hepatology+score+Race+Treatment+Exp_level+Quality_measure,
                data = Person_year)

# death group
x = summary.crr(output1$`CRR: 2`)
x = data.frame(x$conf.int)
x$exp..coef. = NULL 
x$Var = rownames(x)
x = mutate(x, lower..95 = round(X2.5., digits = 3), upper..95 = round(X97.5., digits = 3), exp.coef. = round(exp.coef., digits = 3)) 
x = mutate(x, result = paste0(exp.coef., " ", "(", lower..95, " ", upper..95,")")) 
x[,1:3] = NULL 
x[, 2:3] = NULL
write.csv(x, file = "competing risk.csv")

# transplant group 
x = summary.crr(output1$`CRR: 3`)
x = data.frame(x$conf.int)
x$exp..coef. = NULL 
x$Var = rownames(x)
x = mutate(x, lower..95 = round(X2.5., digits = 3), upper..95 = round(X97.5., digits = 3), exp.coef. = round(exp.coef., digits = 3)) 
x = mutate(x, result = paste0(exp.coef., " ", "(", lower..95, " ", upper..95,")")) 
x[,1:3] = NULL 
x[, 2:3] = NULL
write.csv(x, file = "competing risk.csv")

# ----------------------------------------------------------------------------------------------------------------------------
save.image("Cox model.RData")
