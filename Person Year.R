# Charlson Comorbidity Index 
setwd("X:/Shengchen Hao/Tapper Liver/R file/new") 
library(haven)  
library(dplyr) 
library(stringr) 
library(tidyr) 

##
# death date based on member data 
liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 
death_date =distinct(select(liver_member_fixed, Patid, YEAR_OF_DEATH, MONTH_OF_DEATH)) 

# eligeff 
Eligeff_date = distinct(select(liver_member_fixed, Patid, Eligeff))
Eligeff_date = Eligeff_date %>%  
  group_by(Patid) %>% # some patients have different Eligeff date, we take the last date 
  mutate(Eligeff = as.Date(Eligeff, "%Y-%m-%d")) %>%
  dplyr::arrange(Patid, Eligeff) %>% 
  dplyr::summarise(Eligeff = first(Eligeff))

# Eligend date 
Eligend_date = distinct(select(liver_member_fixed, Patid, Eligend)) 
Eligend_date = Eligend_date %>%  
  group_by(Patid) %>% # some patients have different eligend date, we take the last date 
  mutate(Eligend = as.Date(Eligend, "%Y-%m-%d")) %>%
  dplyr::arrange(Patid, Eligend) %>% 
  dplyr::summarise(Eligend = last(Eligend))


# liver transplant   
## load the "transplant.Rdata"  
## constructing see the Data construct.R 
load("transplant.Rdata")
temp_trans = liver_trans %>%  
  mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>%
  group_by(Patid) %>% 
  arrange(Patid, Fst_Dt) %>%
  dplyr::summarise(Trans_Dt = first(Fst_Dt)) %>% 
  ungroup()


# first claim date 
load("First_Claim.RData")
First_claim_date = temp %>% 
  dplyr::mutate(Fst_Dt = as.Date(Fst_Dt, "%Y-%m-%d")) %>% 
  dplyr::group_by(Patid) %>% 
  dplyr::arrange(Patid, Fst_Dt) %>% 
  dplyr::summarise(Fst_Dt = first(Fst_Dt))
save(First_claim_date, file = "First_claim_date.RData")

# liver cancer date 
## constructing see the Data construct.R 
load("liver_cancer.RData")

 

# Birth date 
Birth_date = dplyr::select(liver_member_fixed, Patid, Yrdob) 
Birth_date = distinct(Birth_date)
Birth_date = Birth_date %>% 
  group_by(Patid) %>% 
  mutate(Yrdob = as.numeric(Yrdob)) %>% 
  arrange(Patid, Yrdob) %>% 
  summarise(Yrdob = first(Yrdob))


save.image("Person Year Prepare.RData")

##################################### Personal Year ##############################
liver_member_fixed = read_sas("X:/Tapper Liver DOD/Member Files/liver_member_fixed.sas7bdat") 
load("TIPS bleed date.RData")

Person_year = data.frame(unique(liver_member_fixed$Patid))  
colnames(Person_year) = c("Patid") 

Person_year = merge(Person_year, temp_trans, all.x = T, by = "Patid")  
Person_year = merge(Person_year, death_date, all.x = T, by = "Patid")  
Person_year = merge(Person_year, Eligeff_date, all.x = T, by = "Patid")
Person_year = merge(Person_year, Eligend_date, all.x = T, by = "Patid") 
Person_year = merge(Person_year, liver_cancer, all.x = T, by = "Patid") 
Person_year = merge(Person_year, First_claim_date, all.x = T, by = "Patid")
Person_year = merge(Person_year, temp_bleed, all.x = T, by = "Patid") 
Person_year = merge(Person_year, temp_TIPS, all.x = T, by = "Patid") 
Person_year = merge(Person_year, Birth_date, all.x = T, by = "Patid")
colnames(Person_year) = c("Patid","Trans_Dt","YEAR_OF_DEATH","MONTH_OF_DEATH", "Eligeff", "Eligend", "Cancer_Diag_Date", "First_Claim_Date", "Bleed_date", "TIPS_date", "Birth_date")

Person_year = Person_year %>% 
  mutate(Death_date = ifelse(MONTH_OF_DEATH == 1 | MONTH_OF_DEATH == 3 | MONTH_OF_DEATH == 5 | MONTH_OF_DEATH == 7 | MONTH_OF_DEATH == 8 | MONTH_OF_DEATH == 10 | MONTH_OF_DEATH == 12, paste(YEAR_OF_DEATH, MONTH_OF_DEATH, "31", sep = "-"),
                             ifelse(MONTH_OF_DEATH == 4 | MONTH_OF_DEATH == 6 | MONTH_OF_DEATH == 9 | MONTH_OF_DEATH == 11, paste(YEAR_OF_DEATH, MONTH_OF_DEATH, "30", sep = "-"), 
                                    ifelse(MONTH_OF_DEATH == 2, paste(YEAR_OF_DEATH, MONTH_OF_DEATH, "28", sep = "-"), NA)))) %>% 
  mutate(Death_date = as.Date(Death_date, "%Y-%m-%d")) %>% 
  mutate(First_Claim_Date = as.Date(First_Claim_Date,  "%Y-%m-%d"), 
         Eligend = as.Date(Eligend, "%Y-%m-%d"), 
         Trans_Dt = as.Date(Trans_Dt, "%Y-%m-%d"), 
         Eligeff = as.Date(Eligeff, "%Y-%m-%d")) %>% 
  rowwise() %>%
  mutate(Lst_Date = as.Date(min(Death_date, Trans_Dt, Eligend, na.rm = T), "%Y-%m-%d")) %>% 
  ungroup() %>%
  mutate(Lst_Date = if_else(Lst_Date > as.Date("2015-12-31", "%Y-%m-%d"), as.Date("2015-12-31", "%Y-%m-%d"), Lst_Date)) %>% 
  mutate(Person_year = Lst_Date - Eligeff)
  
Person_year = Person_year %>% 
  mutate(Person_year = as.numeric(Person_year)/365)

Person_year = separate(Person_year, First_Claim_Date, sep = "-", into = "Year", extra = "drop", remove = F) 
Person_year = mutate(Person_year, Year = as.numeric(Year), 
                     Birth_date = as.numeric(Birth_date),
                     Age = Year - Birth_date) 

Person_year$Year = NULL
Person_year$Birth_date = NULL

save(Person_year, file = "person_year.RData")

# Because some patient's coverage are not continous 
# complicated, not use for now
temp_person = dplyr::select(Person_year, Patid, Trans_Dt, Eligeff, Eligend, Death_date, Bleed_date, TIPS_date, Cancer_Diag_Date)
temp_member = dplyr::select(liver_member_fixed, Patid, Eligeff, Eligend)
temp_member = merge(temp_member, temp_person, by = "Patid", all.x = T)

temp_member = temp_member %>% 
  mutate(Eligeff.x = as.Date(Eligeff.x, "%Y-%m-%d"), 
         Eligend.x = as.Date(Eligend.x, "%Y-%m-%d")) %>% 
  rowwise() %>% 
  mutate(END = min(Death_date, Trans_Dt, as.Date("2015-12-31", "%Y-%m-%d"), na.rm = T)) %>% 
  ungroup() %>%
  filter(Eligeff.x <= END) %>% 
  rowwise() %>%
  mutate(Coverage = if_else(Eligend.x < END, min(Eligend.x, as.Date("2015-12-31", "%Y-%m-%d"), na.rm = T) - Eligeff.x, 
                            if_else(Eligend.x >= END, END - Eligeff.x, NA_real_))
         ) %>%
  ungroup() %>%
  group_by(Patid) %>% 
  mutate(Person_year = sum(Coverage)/365) 

person_year_test = temp_member %>% 
  group_by(Patid) %>% 
  arrange(Patid, Eligeff.x) %>% 
  summarise(Person_year = first(Person_year))

rm(temp_person, temp_member) 
gc()

###################################################




save(Person_year, file = "person_year.RData")



