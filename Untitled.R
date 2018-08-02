library(cmprsk2)
output1 <- crr2(Surv(time, Status(1)== 2) ~ Age+ Sex+ score + AC+ Hepatitis_C+ Non_alcohol+ Ascites+ Varices+ HE+ HCC+ APP+Gastro_only+Hepatology+SBP+TIPS+Race+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit+Shared_visit,
                data = table2)



# No shared visit 
output2 <- crr2(Surv(time, Status(1)== 2) ~ Age+ Sex+ score + AC+ Hepatitis_C+ Non_alcohol+ Ascites+ Varices+ HE+ HCC+ APP+ Gastro_only+ Hepatology+SBP+TIPS+Race+APP_Gastro+APP_Hepatology+Pneumonia+Sepsis+Urinary_tract_infection+Cellulitis+Bacteremia+Clostridium+Cholangitis+Paracentesis+Dialysis+max_visit,
                data = table2)

save.image("output of competing risk.RData")