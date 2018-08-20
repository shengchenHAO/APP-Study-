library(dplyr) 
library(stringr) 
library(tidyr) 


for (year in 2001:2015){
  name = paste0("data_def_", year, ".RData")
  load(name) 
  
  data_complete = rbind(data_complete, temp_data)
  
  rm(temp_data, name, year) 
  gc()
  
}

save.image("data_def_together.RData")