library(tidyverse)

fd <- read.csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data.csv"), na = "", stringsAsFactors = F)


fish_data_cleaned <- fd %>% select(-c(Fish_Abundance_ID)) %>% 
  group_by(PU_Gap_Code, Reach_Name, Event_Date, Fish_Species_Code) %>% 
  summarise(Fish_Species_Count = sum(Fish_Species_Count))

write_csv(fish_data_cleaned, path = paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data_CREP_2013-2019.csv"), na = "")
