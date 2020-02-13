db <- basin %>% 
  filter(gear_type == "Electro Fishing") %>% 
  select(c("pugap_code", "date", "reach_name", "fish_species_code", "count")) %>% 
  group_by(pugap_code, date, reach_name, fish_species_code) %>% 
  summarise(fish_species_count = sum(count)) %>% 
  ungroup()
  
### count to consolidate fish counts like in tibble fixer
list_db <- db %>% 
  select(pugap_code, date, reach_name) %>% 
  unique()

drake <- readxl::read_xlsx("~/CREP/Analysis/Project-Shelley/Data/Kaskaskia_basin_survey_fish_samples_LeviDrake.xlsx", sheet = 1)

drake$MaxOfDate <- as.Date(drake$MaxOfDate)

list_drake <- drake %>% 
  select(pugap_code = PUGAP_CODE, date = MaxOfDate, reach_name = CODE) %>% 
  unique()

miss_list <- setdiff(list_db, list_drake) 
miss_list2 <- setdiff(list_drake, list_db) 
int_list  <- intersect(list_db,list_drake)

write_csv(list_drake, "~/CREP/Analysis/Project-Shelley/Data/unique_list_drake.csv")
write_csv(list_db, "~/CREP/Analysis/Project-Shelley/Data/unique_list_basin.csv")

### Matching station codes
list_drake_codes <- drake %>% 
  select(reach_name = CODE) %>% 
  unique()

list_db_codes <- db%>% 
  select(reach_name) %>% 
  unique()

int_list_code  <- intersect(list_drake_codes, list_db_codes)

### Matching station codes
list_drake_pugap <- drake %>% 
  select(pugap_code = PUGAP_CODE) %>% 
  unique()

list_db_pugap <- db%>% 
  select(pugap_code) %>% 
  unique()

int_list_pugap  <- intersect(list_drake_codes, list_db_codes)