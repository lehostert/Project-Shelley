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
#Godspeed

### Read in basin data from IDNR database and combine into one df

# on Mac Bison location is "smb://INHS-bison.inhs.illinois.edu/"
# on Windows Bison Location is "//INHS-Bison/"
# TODO how can I remedy running this in both Windows and Mac? the Bison connection between Windows and Mac is different.
# TODO change data file paths to pull from server folder for example:
# basin_1 <- readxl::read_excel(path = "//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Project_Shelley/Data/basin_survey_data_kaskaskia_il.xlsx", sheet = 1)
basin_1 <- readxl::read_excel(path = "Data/basin_survey_data_kaskaskia_il.xlsx", sheet = 1)
basin_2 <- readxl::read_excel(path = "Data/basin_survey_data_kaskaskia_il.xlsx", sheet = 2)
basin_3 <- readxl::read_excel(path = "Data/basin_survey_data_kaskaskia_il.xlsx", sheet = 3)
basin_4 <- readxl::read_excel(path = "Data/basin_survey_data_kaskaskia_il.xlsx", sheet = 4)
basin <- bind_rows(basin_1, basin_2, basin_3, basin_4)
names(basin) <- str_replace_all(names(basin),"[:space:]+", "_")
names(basin) <- str_to_lower(names(basin))
basin <- rename(basin, weight = "wt(g)")
basin <- rename(basin, waterbody_code = waterbody)


### Read in waterbody station data from GIS layer Stream Station Data. Stations_Nov8 and Streams_November4 
stations <- read_csv(file = "Data/waterbody_stations_20101104.csv", col_names = T)
kasky_stations <- stations %>% filter(PU_CODE == "kasky")
names(kasky_stations) <- str_to_lower(names(kasky_stations))

kasky_stations$waterbody <- str_to_title(kasky_stations$waterbody)
kasky_stations$new_loc <- str_to_title(kasky_stations$new_loc)
kasky_stations$county <- str_to_title(kasky_stations$county)

kasky_stations<- rename(kasky_stations, station_code = station_co)
kasky_stations<- rename(kasky_stations, length_segment = length)
kasky_stations<- rename(kasky_stations, section = section_)
kasky_stations<- rename(kasky_stations, note = note_)

### Pull Waterbody Code from Waterbody Station:
basin$dupe <- stringr::str_extract(basin$waterbody_code, "(?<=[:space:]\\([:space:])[:graph:]+")
basin$dupe <- stringr::str_replace(basin$dupe, "\\)", "-")
basin$station_location <- stringr::str_extract(basin$waterbody_station, "(?<=:).*")
basin$station_code_wrong <- stringr::str_extract(basin$waterbody_station, ".*(?=:)")

### The following code includes a quoting mechanism \Q...\E that allows you to exactly match user input b/c it treats all the characters in the ... are exact matches
### See the regular expressions stringr vinettes for more info <https://stringr.tidyverse.org/articles/regular-expressions.html>
basin$station_code <- stringr::str_extract(basin$station_code_wrong,paste0("(?<=\\Q",basin$dupe,"\\E).*"))
basin$reach_name <- basin$station_code
basin <- basin %>% select(-c(station_code_wrong, dupe))

### Combine kasky station info with basin data and simplify full basin survey dataset, clean up data, and write to .csv

basin <- dplyr::left_join(basin, kasky_stations, by = "station_code")
basin$gear <- if_else(is.na(basin$gear_used), basin$gear_type, basin$gear_used)
basin$date <- as.Date(basin$sample_start_date, format = "%m/%d/%Y")
basin <- rename(basin, fish_species_common = species)
basin$fish_species_common <- stringr::str_to_lower(basin$fish_species_common)

### Read in IL fish traits. 
il_fish_traits <- read.csv("//INHS-Bison/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Illinois_fish_traits_complete.csv", na = "", stringsAsFactors = F)
il_fish_list <- il_fish_traits %>% select(c(Fish_Species_Code, Fish_Species_Common, Fish_Species_Scientific))
names(il_fish_list) <- str_to_lower(names(il_fish_list))


# TODO consult Jeff regarding silvery minnow (western v. Mississippi) and silverjaw minnow x bigmouth shiner hybrid. 
basin <- basin %>% mutate(fish_species_common = replace(fish_species_common, fish_species_common == 'mosquitofish', 'western mosquitofish'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'bluegill x green sunfish', 'bluegill x green sunfish hybrid'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'carp', 'common carp'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'longear sunfish x green sunfish', 'longear sunfish x green sunfish hybrid'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'scp - silver carp', 'silver carp'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'longear sunfish x bluegill', 'longear sunfish x bluegill hybrid'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'carpsucker spp.', 'unidentified carpsucker'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'gar spp.', 'unidentified gar spp.'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'inland silversides', 'inland silverside'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'sunfish hybrid', 'unidentified sunfish hybrid'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'silvery minnow', 'non-carp minnow spp.'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'bufffalo spp.', 'unidentified buffalo'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'silverjaw minnow x bigmouth shiner', 'non-carp minnow spp.'),
                          fish_species_common = replace(fish_species_common, fish_species_common == 'carp x goldfish', 'common carp x goldfish hybrid')
                          )

# Add fish species codes and sci names
basin <- left_join(basin, il_fish_list, by = "fish_species_common")

#TODO consider changing full join to left? join so that you do not end up with more spp. than basin info. 

#### TEST IF all fish on list are matched with something in the IL fish list.  
# TODO delete this section if you no longer need it
# compare_fish_list <- basin %>% select(reach_name, gear, date, fish_species_common, fish_species_code, fish_species_scientific)
# NA_fish_list <- compare_fish_list %>% filter(is.na(fish_species_code))
# NA_fish_list <- as.data.frame(unique(NA_fish_list$fish_species_common))

#### County mismatch list ####
# county_check$same <- identical(basin$county.x, basin$county.y)
# county_check$same <- if_else(basin$county.x==basin$county.y, "TRUE", "FALSE")
# county_mismatch <- county_check %>% filter(same == "FALSE")
# View(county_mismatch)
# 
# county_mismatch_list <- basin %>% select(c("survey", "survey_year","survey_type","county.x","waterbody_code","waterbody_station",
#                                            "ibi_region","ibi_slope","gear_type","gear_used","sample_type","sample_start_date",
#                                            "waterbody","new_loc","county.y","station_code","reach_name","lat_d","long_d","pugap_code"))
# 
# county_mismatch_list <- rename(county_mismatch_list, county.basin_data = county.x)
# county_mismatch_list <- rename(county_mismatch_list, county.GIS_data = county.y)
# 
# county_mismatch_list <- county_mismatch_list %>% select(c("survey", "survey_year","survey_type","county.basin_data", "county.GIS_data","waterbody_code","waterbody_station",
#                                                           "ibi_region","ibi_slope","gear_type","gear_used","sample_type","sample_start_date",
#                                                           "waterbody","new_loc","station_code","reach_name","lat_d","long_d","pugap_code"))
# 
# county_mismatch_list <- unique(county_mismatch_list)
# county_mismatch_list <- county_mismatch_list %>% filter(county.basin_data != county.GIS_data)


# Clean it up drop NA fish species and drop duplicate data

#### Check for duplicate data ####
# 
# basin$duplicated <- duplicated(basin)
# 
# duplicate_list <- basin %>% filter(duplicated == 'TRUE') 
# 
# duplicate_site_list <- basin %>% filter(duplicated == 'TRUE') %>% 
#   select(c(survey_year, reach_name, waterbody_station ,sample_start_date, fish_species_common, fish_species_code, count, length, weight)) %>% 
#   dplyr::distinct()
# 
# 
# basin_cleaned <- basin %>% drop_na(reach_name) %>% dplyr::distinct()
# TODO before you export this bet sure to change count to "Fish_Species_Count" and string to ADA style. 
# basin <- rename(basin, Fish_Species_Count == count)
# basin <- rename(basin, Event_Date == date)
# basin$date <- as.Date(basin$event_date, format = "%m/%d/%Y")

#### output a file that contains the combined kaskaskia basin survey data. 
# write_csv(basin_cleaned, "Data/idnr_kaskaskia_basin_survey_data_1997-2012.csv", na = "")

#### Continue With Analysis ####

### Read in CREP fish data 2013-2019
network_prefix <- "//INHS-Bison"
crep <- read.csv(paste0(network_prefix,"/ResearchData/Groups/Kaskaskia_CREP/Analysis/Fish/Data/Fish_Abundance_Data_CREP_2013-2019.csv"), na = "", stringsAsFactors = F)
names(crep) <- str_to_lower(names(crep))
crep$date <- as.Date(crep$event_date, format = "%m/%d/%Y")
crep <- rename(crep, "pugap_code" = "pu_gap_code")
crep_data  <- crep %>% select(c(pugap_code, reach_name, date))

# kasky_data_final %>% group_by(data_source) %>% purrr::map(summary)


### Combine kasky station info with CREP data
basin_data <- basin_ready %>% select(c(pugap_code, reach_name, date))

kasky_data_combined <- bind_rows("crep_monitoring" = crep_data, "IDNR_basin_surveys" = basin_data, .id = "data_source")
kasky_data_combined <- unique(kasky_data_combined[c("data_source","pugap_code", "reach_name", "date")])

### Combine Kasky stream attributes with combined basin data
stream_attributes_table <- readxl::read_excel("Data/kasky_stream_lines_attributes_table.xlsx", sheet = 1)
names(stream_attributes_table) <- str_to_lower(names(stream_attributes_table))
stream_attributes_table <- rename(stream_attributes_table, "pugap_code" = "pu_gapcode")
stream_attributes_table <- rename(stream_attributes_table, "order" = "order_")
stream_attributes_table <- rename(stream_attributes_table, "arc" = "arc_")

kasky_data_final <- dplyr::left_join(kasky_data_combined, stream_attributes_table, by = "pugap_code")
# kasky_data_final$data_source <- stringr::str_replace(kasky_data_final$data_source, "crep_monitoring", "CREP Monitoring")
# kasky_data_final$data_source <- stringr::str_replace(kasky_data_final$data_source, "IDNR_basin_surveys", "IDNR Basin Surveys")
kasky_data_final <- kasky_data_final %>% mutate(data_source = as_factor(data_source))

### Get some summary stats
kasky_data_final %>% group_by(data_source) %>% purrr::map(summary)

###Plot it as a histogram
## Stream Link

# ggplot2::ggplot(kasky_data_final, aes(x = link, fill=data_source)) +
#   geom_histogram(breaks = c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,100,200), position="identity", alpha=0.5) +
#   theme(legend.position="top") +
#   labs(title="Kaskaskia Basin Community Sampling Locations",x="Stream Link", y = "Count", fill = "Survey Type")

## Stream Order

ggplot2::ggplot(kasky_data_final, aes(x = order, fill= data_source)) +
  geom_histogram(binwidth = 1, position="identity", alpha=0.5) +
  theme(legend.position="top",plot.title = element_text(hjust=0.5) ,text = element_text(size=18, hjust=0.5)) +
  labs(title="Kaskaskia Basin Fish Community Sampling Locations",x="Stream Order", y = "Count", fill = "Survey Type") +
  scale_color_manual(values=c("darkorchid1", "grey43", "#56B4E9")) +
  scale_fill_manual(values=c("darkorchid1", "grey43", "#56B4E9"))

ggplot2::ggsave("Kaskaskia_Basin_Community_Sampling_by_stream_order", device = "tiff")

###Box Plots
qplot(data_source, order, data = kasky_data_final, 
      geom= "boxplot", fill = data_source)

#### Additional Summary StatS
kasky_data_final %>% group_by(data_source) %>% 
  summarise(mean_order = mean(kasky_data_final$order),
            mean_link = mean(kasky_data_final$link)
  )


# NOTE: Get basin survey data in form with only PU_Gap, Reach, Date, Species, Count






