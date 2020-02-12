library(tidyverse)

# Run clean basin data until ~ line 93 to pull in all basin data

bdf <- basin

bdf$dupe <- duplicated(bdf)

sample_sum_bdf <- summarise(group_by(bdf, survey, survey_year, survey_type, gear, sample_type, reach_name, date), observations = length(date))

duplicate_sum_bdf <- bdf %>% group_by(survey, survey_year, survey_type, gear, sample_type, reach_name, date) %>% count(dupe) %>% filter(dupe == TRUE)

bdf_summary <- duplicate_sum_bdf %>% 
  select(-c(dupe)) %>% 
  rename(duplicates = n) %>% 
  right_join(sample_sum_bdf) %>% 
  tidyr::replace_na(list(duplicates = 0))

bdf_summary$fate <- if_else(bdf_summary$duplicates > 0.5*(bdf_summary$observations), "drop_check", 
                            if_else(bdf_summary$duplicates == 0.5*(bdf_summary$observations), "drop_double", "keep"))

tdf <- full_join(bdf_summary, basin)

# dataframe with no erroneous duplications detected
basin_ready_ef <- tdf %>% filter(gear_type == "Electro Fishing", fate == "keep")
# dateframe with all records that have questionable duplications
basin_unfit_ef <- tdf %>% filter(gear_type == "Electro Fishing", fate != "keep")


sampling_event_ready_list <- basin_ready_ef %>% 
  select(c("reach_name","date")) %>% 
  unique()

sampling_event_unfit_list <- basin_unfit_ef %>% 
  ungroup() %>% 
  select(c("reach_name","date")) %>% 
  unique()


sampling_site_list <- basin %>% 
  filter(gear_type == "Electro Fishing") %>% 
  select("reach_name") %>% 
  unique()

sampling_event_list <- basin %>% 
  filter(gear_type == "Electro Fishing") %>% 
  select("reach_name", "date") %>% 
  unique()

sampling_site_full <- basin %>% 
  select("reach_name") %>% 
  unique()

crep_event_list <- crep_data %>% 
  select("reach_name") %>% 
  unique()

write_csv(basin_ready_ef, "Data/idnr_kaskaskia_basin_survey_data_1997-2012_READY_ef.csv", na = "")
write_csv(basin_unfit_ef, "Data/idnr_kaskaskia_basin_survey_data_1997-2012_DUPLICATES_ef.csv", na = "")

#### BELOW THERE BE DRAGONS ####
summarise(bdf_summary)
