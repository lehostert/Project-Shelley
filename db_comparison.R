db <- basin %>% 
  filter(gear_type == "Electro Fishing") %>% 
  select(c("pugap_code", "date", "reach_name", "fish_species_code", "count"))

### count to consolidate fish counts like in tibble fixer
list_db <- db %>% 
  group_by(pugap_code, date, reach_name) %>% 
  unique()

drake <- readxl::read_xlsx("~/CREP/Analysis/Project-Shelley/Data/Kaskaskia_basin_survey_fish_samples_LeviDrake.xlsx", sheet = 1)