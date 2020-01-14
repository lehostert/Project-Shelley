library(tidyverse)

# Goal is this script is to
# 1- read in the basin survey data from Jeff Stein  and append the data to each other (they are currently Separate excel worksheets in one excel file )
# 2- Format data in the same style as CREP fish data
# 3- Add CREP + Basin data together
# 4- histogram stream size/order/link per data source
# 
# I will need to locate a file that has the stream size for all of the CREP sites
# I will also need to compile a file that ties the basin surveys to PU_Gap codes in kasky.
## See waterbody_stations_2010 csv from ArcGIS Attribute table atrribute table
# Then join the table of size info with that of the basin + CREP location info via PU_Gap code

## Read in basin data from IDNR database and combine into one df
basin_1 <- readxl::read_excel(path = "Data/basin_survey_data_kaskaskia_il.xlsx", sheet = 1)
basin_2 <- readxl::read_excel(path = "Data/basin_survey_data_kaskaskia_il.xlsx", sheet = 2)
basin_3 <- readxl::read_excel(path = "Data/basin_survey_data_kaskaskia_il.xlsx", sheet = 3)
basin_4 <- readxl::read_excel(path = "Data/basin_survey_data_kaskaskia_il.xlsx", sheet = 4)
basin <- bind_rows(basin_1, basin_2, basin_3, basin_4)
names(basin) <- str_replace_all(names(basin),"[:space:]+", "_")
names(basin) <- str_to_lower(names(basin))


## Read in waterbody station data from GIS layer
stations <- read_csv(file = "Data/waterbody_stations_20101104.csv", col_names = T)
kasky_stations <- stations %>% filter(PU_CODE == "kasky")
names(kasky_stations) <- str_to_lower(names(kasky_stations))
kasky_stations<- rename(kasky_stations, station_code = station_co)


#### Pull Waterbody Code from Waterbody Station:
basin$dupe <- stringr::str_extract(basin$waterbody, "(?<=[:space:]\\([:space:])[:graph:]+")
basin$dupe <- stringr::str_replace(basin$dupe, "\\)", "-")
basin$station_location <- stringr::str_extract(basin$waterbody_station, "(?<=:).*")  
basin$station_code_wrong <- stringr::str_extract(basin$waterbody_station, ".*(?=:)")

### The following code includes a quoting mechanism \Q...\E that allows you to exactly matach user input b/c all the characters in the ... are exact matches
### See the regular expressions stringr vinettes for more info <https://stringr.tidyverse.org/articles/regular-expressions.html>
basin$station_code <- stringr::str_extract(basin$station_code_wrong,paste0("(?<=\\Q",basin$dupe,"\\E).*"))

basin <- -c(station_code_wrong, dupe)



# TODO: Get Basin stations matched with locality info from kasky_stations
# TODO: Get basin survey data in form with only PU_Gap, Reach, Date, Species, Count