library(tidyverse)

df <- readxl::read_excel("~/Stack_Overflow/duplication_example/Example_fish_sample_duplication.xlsx", sheet = 1, na = ".", col_names = T)
df <- df %>% drop_na(survey_year)

# If # duplicates == # of records in a group  then remove duplicates(# This is the part I am unsure of ) if not then do nothing. 

# If # of duplicates is 1/2 the number in teh group then remove every other record if not do nothing. 

#### Count number of observations(ie. rows) per site####
sample_sum <- summarise(group_by(df, survey_year, site, date), observations = length(date))

#### Count number of duplicate rows per site####

df$dupe <- duplicated(df)
duplicate_sum <- df %>% group_by(survey_year,site, date) %>% count(dupe) %>% filter(dupe == TRUE)

sample_summary <- duplicate_sum %>% select(-c(dupe)) %>% 
  rename(duplicates = n) %>% 
  right_join(sample_sum)
  
#### IF then ####

sample_summary$fate <- if_else(sample_summary$duplicates >= 0.5*(sample_summary$observations), "drop", "keep")

#### Now do it for basin data ####
bdf <- basin

sample_sum_bdf <- summarise(group_by(bdf, survey, survey_year, survey_type, gear, sample_type, reach_name, date), observations = length(date))

bdf$dupe <- duplicated(bdf)

duplicate_sum_bdf <- bdf %>% group_by(survey, survey_year, survey_type, gear, sample_type, reach_name, date) %>% count(dupe) %>% filter(dupe == TRUE)

sample_summary_bdf <- duplicate_sum_bdf %>% 
  select(-c(dupe)) %>% 
  rename(duplicates = n) %>% 
  right_join(sample_sum_bdf) %>% 
  tidyr::replace_na(list(duplicates = 0))

sample_summary_bdf$fate <- if_else(sample_summary_bdf$duplicates >= 0.5*(sample_summary_bdf$observations), "drop", "keep")

basin$fate <- sample_summary_bdf %>% 
  dplyr::select(x = count, nm = Site_ID) %>% 
  purrr::pmap(set_names) %>% 
  unlist

tdf <- full_join(sample_summary_bdf, basin)

# dataframe with no erroneous duplications detected
basin_ready <- tdf %>% filter(fate == "keep")
# dateframe with all records that have questionable duplications
basin_unfit <- tdf %>% filter(fate == "drop")

#### BELOW THERE BE DRAGONS ####
summarise(sample_summary_bdf)
