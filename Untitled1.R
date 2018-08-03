library(cmprsk2)
load("data_competing_risk.RData")

output2 <- crr2(Surv(time, Status(1)== 2) ~ Age+ Sex+ score + AC+ Hepatitis_C+ Non_alcohol+ Ascites+ Varices+ HE+ HCC+ APP+ Gastro_only+ Hepatology+SBP+TIPS+Race+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit,
                data = table2)
save.image("no_shared_visit.RData")