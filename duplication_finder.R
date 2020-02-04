library(tidyverse)

df <- readxl::read_excel("~/Stack_Overflow/duplication_example/Example_fish_sample_duplication.xlsx", sheet = 1, na = ".", col_names = T)
df <- df %>% drop_na(survey_year)

# If # duplicates == # of records in a group  then remove duplicates(# This is the part I am unsure of ) if not then do nothing. 

# If # of duplicates is 1/2 the number in teh group then remove every other record if not do nothing. 

#### Count number of rows per site####
summarise(group_by(df,survey_year,site, date),length(date))



#### Count number of duplicates ####

df$dupe <- duplicated(df)
df %>% group_by(survey_year,site, date) %>% count(dupe) %>% filter(dupe == TRUE)



#### IF then ####

if_else( )



#### BELOW THERE BE DRAGONS ####

nrow(df)
nrow(unique(df))
nrow(duplicated(df) == FALSE)
count(df[duplicated(df),])
anyDuplicated(df)

df %>% group_by(df,survey_year,site, date) %>% count(duplicated(df))


require(dplyr)
df1 %>% count(Year, Month)



dupe <- NULL
data.frame(dupe)

dupe[]<- duplicated(df)



df[duplicated(df)]




aggregate(list(numdup=rep(1,nrow(df))), df, length)

aggregate()

DT <- data.table(df)