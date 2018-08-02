# Charlson Comorbidity Index 
setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 





# table 2 
# Endoscopy
temp_func = function(data){
  cat(length(unique(data)), ",") 
  cat(sum(unique(data) %in% APP_patid))
}
temp_func(Endoscopy_patid)

# mortality
liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 

length(unique(filter(liver_member_fixed, DIED == 1)$Patid))
temp_id = unique(filter(liver_member_fixed, DIED == 1)$Patid)

temp_func(temp_id)

# Hav HBV  
temp_id = c(A_vaccine_patid, B_vaccine_patid)
temp_id = unique(temp_id)
temp_func(temp_id)

# liver cancer screen 
temp_id = c(CT_patid, MRI_patid, Ultrasound_patid) 
temp_id = unique(temp_id)
temp_func(temp_id)

# rifaximin
temp = filter(liver_pharm, grepl("RIFAXIMIN", Brnd_Nm) | grepl("RIFAXIMIN", Gnrc_Nm))
length(unique(temp$Patid))
temp_func(unique(temp$Patid))

# readmission within 30 days 
## using discharge status 1 6 7 10 11 12 13 14 15 16 17 18 19
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


temp_func(temp_id)
save(liver_conf, file = "liver_conf.RData")

# logistic regression test 

Patids = unique(liver_member_fixed$Patid)
temp = as.data.frame(Patids) 
colnames(temp) = c("Patid")
temp = temp %>% 
  mutate(Endoscopy = 1*(Patid %in% Endoscopy_patid), 
         APP = 1*(Patid %in% APP_patid))


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

output <- glm(Endoscopy ~ APP, data=temp, family=binomial)
logistic.regression.or.ci(output)

