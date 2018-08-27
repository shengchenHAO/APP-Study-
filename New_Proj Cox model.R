setwd("X:/Shengchen Hao/Tapper Liver/R file/new/GI_proj") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 
library(survival)
library(survminer)

load("Table1_complete.RData")
rm(data_total)

Person_year = Person_year %>%
  mutate(Status = ifelse(Death_date != Lst_Date| is.na(Death_date), 1, 2)) %>% 
  mutate(Status = ifelse(is.na(Death_date) == F & Death_date <= Lst_Date + 90, 2, Status)) %>% 
  mutate(time = Lst_Date - GI_first_date) %>% 
  mutate(Race = ifelse(Race == "U"|Race == "", NA, Race)) # two kind of missing value in Race
  
Person_year = mutate(Person_year, Sex = ifelse(Sex == "M", 1, ifelse(Sex == "F", 2, NA)))
Person_year = mutate(Person_year, Race = ifelse(Race == "W", 1, ifelse(Race == "A",2, ifelse(Race == "B", 3, ifelse(Race == "H", 4, Race)))))
Person_year$YEAR_OF_DEATH = NULL 
Person_year$MONTH_OF_DEATH = NULL

load("charlson index.RData")
temp = select(charlson9, Patid, wscore)
colnames(temp) = c("Patid", "score")
Person_year = merge(Person_year, temp, by = "Patid", all.x = T)
rm(temp, charlson9)

temp = select(GI_patient_num, Prov_Unique, Experienced)
Person_year = merge(Person_year, temp, by = "Prov_Unique", all.x = T)
rm(temp)

# multi-level treatment variable (0/3, 1/3, 2/3, 3/3)
load("data prepare.RData")
rm(list=setdiff(ls(), "data_total"))
data_total = data_total %>% 
  mutate(Treatment = GI_vaccine+GI_Screen+GI_Endoscopy) 
temp = data_total %>% 
  select(Patid, Treatment) %>% 
  distinct()
rm(data_total)

Person_year = merge(Person_year, temp, all.x = T, by = "Patid") 
Person_year = mutate(Person_year, Treatment = as.factor(Treatment))
rm(temp)
# Singel var cox model ---------------------------------------------------------------------------------------------------------
covariates <- c("Age", "Sex", "score", "AC", "Hepatitis_C", "Non_alcohol", "Ascites", "Varices", "HE", "HCC", "Hepatology", "Experienced")
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
# ----------------------------------------------------------------------------------------------------------------------------


# Multivariate Cox-----------------------------------------------------------------------------------------------------------
res.cox <- coxph(Surv(time, Status) ~ Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+Hepatology+Experienced+score+Race+Treatment,
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

output1 <- crr2(Surv(time, Status(1)== 2) ~ Age+Sex+score+AC+Hepatitis_C+Non_alcohol+Ascites+Varices+HE+HCC+Hepatology+Experienced+score+Race+Treatment,
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
