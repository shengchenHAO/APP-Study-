# Charlson Comorbidity Index 
setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(medicalrisk)
library(plyr)
library(foreach) 
library(parallel)
library(doParallel)
library(stringr)
library(comorbidity)

# Charlson index  
liver_diag$Code = NULL
data = dplyr::distinct(liver_diag)
data$Diag = str_replace_all(data$Diag, fixed(" "), "")
charlson9 <- comorbidity(x = data, id = "Patid", code = "Diag", score = "charlson_icd9")
save(charlson9, file = "charlson index.RData")

# check for outliers 
test = dplyr::filter(liver_diag, grepl("#", Diag))
test1 = dplyr::filter(liver_diag, grepl("-", Diag))
test2 = dplyr::filter(liver_diag, grepl("\\+", Diag)) 
test3 = dplyr::filter(liver_diag, grepl("-", Diag))

# 
index = dplyr::select(charlson9, Patid, index)
summary(index$index)
APP_patid = unique(APP_patid)

index = index %>% 
  mutate(APP = 1*(Patid %in% APP_patid))
table(index$index,index$APP)
