library(readr)
library(dplyr)
library(stringr)

setwd("C:/Users/dkvale/Desktop/aqi-watch")

states <- data.frame(State = c('MN', 'IA', 'WI', 'SD', 'ND', 'CAN', 'CAN'),
                     FIPS  = c('27', '19', '55', '46', '38', 'th', '00'),
                     stringsAsFactors = F)

border_sites <- c('380171004', '271370034', '550630012')

canada_sites <- c('000070118', '000070119', '000070203', '000064001')

#cat(paste0(sort(unique(filter(aqi, grepl("Minnesota", Agency))$`Site Name`)), collapse="'\n'"))
mn_sites <- c('Apple Valley',
              'Blaine',
              'Brainerd',
              'Detroit Lakes',
              'Duluth-WDSE',
              'East Bethel',
              'Ely',
              'FARGO NW',
              'Fond du Lac',
              'Grand Portage',
              'LACROSSE DOT',
              'Lakeville - Near Roa',
              'Marshall',
              'Mille Lacs',
              'Minneapolis-Near Roa',
              'Minneapolis-Phillips',
              'Red Lake Nation',
              'Rochester',
              'Shakopee',
              'St. Anthony Park',
              'St. Cloud',
              'St. Michael',
              'St. Paul - Downtown',
              'Stanton',
              'Virginia',
              'Voyageurs NP',
              'West Duluth')

site_names <- c('APPLETON AAL',
                'BAD RIVER TRIBAL SCH',
                'Badlands',
                'BAYSIDE',
                'Beloit-Converse',
                'Black Hawk',
                'Brandon',
                'Buffalo',
                'CARPENTER',
                'CHIWAUKEE',
                'Clinton, Chancy Park',
                'Clinton, Rainbow Par',
                'COLUMBUS',
                'Credit Union',
                'Davenport, 10th & Vi',
                'Davenport, Hayes Sch',
                'Devils Lake',
                'Eau Claire',
                'Emmetsburg, ILCC',
                'Experimental Lakes',
                'FOND DU LAC',
                'GRAFTON',
                'GREEN BAY E HIGH',
                'GREEN BAY UW',
                'HARRINGTON BCH',
                'Horicon Wildlife Area',
                'Iowa City, Hoover Sc',
                'Jefferson-Laatsch',
                'Kenosha-Water Tower',
                'KEWAUNEE',
                'Lake Ahquabi',
                'LAKE DUBAY',
                'LAKE GENEVA',
                'Lake Sugema',
                'MADISON EAST',
                'MANITOWOC',
                'MILWAUKEE 16TH ST',
                'Milwaukee College Av',
                'MILWAUKEE SER DNR',
                'Muscatine, High Scho',
                'NEWPORT',
                'PERKINSTOWN',
                'Pierre',
                'POTAWATOMI',
                'Potosi',
                'PUBLIC HEALTH',
                'PUBLIC HEALTH 4',
                'Racine-Payne and Dol',
                'Research Farm',
                'Scott County Park',
                'SHEBOYGAN',
                'Sheboygan-Haven',
                'Sioux Falls',
                'THUNDER-BAY',
                'TROUT LAKE',
                'UC1',
                'Viking Lake State Pa',
                'Waterloo, Water Towe',
                'Watertown',
                'WAUKESHA - CLEVELAND',
                'Waverly, Airport',
                "Williston",
                'WIND CAVE',
                'Winnipeg_Ellens',
                'Winnipeg_Scotia')

# Load credentials
credentials <- read_csv("C:\\Users\\dkvale\\Desktop\\credentials.csv")

user <- credentials$user
pwd  <- credentials$pwd
email <- credentials$email
token <- credentials$issue_token

airNow_link <- paste0('ftp://', user, ':', pwd, '@ftp.airnowapi.org/Locations/monitoring_site_locations.dat')
locations <-  try(read_delim(airNow_link, "|", col_names=F), silent=T)
closeAllConnections()

locations <- sites

locations <- locations[ , c(1:2,4:5,7,9:10,12:13,16:21)]

names(locations) <- c("AqsID","Parameter","Site Name","Active","Agency","Lat","Long","Local_Time","Country","City_Code","City","State_FIPS","State","FIPS","County")

# Filter to Ozone and PM
locations <- filter(locations, grepl('PM2.5', Parameter) |
                      grepl('PM25', Parameter) |
                      grepl('O3', Parameter) |
                      grepl('OZONE', Parameter) |
                      grepl('PM10', Parameter))

# Filter to Minnesota region
locations$StateID <- substring(locations$AqsID, 1, 2) 

locations <- filter(locations, StateID %in% c('27', '19', '55', '38', '46') |
                               AqsID %in% c(border_sites, canada_sites))

locations <- filter(locations, Active == 'Active')

# QC
sort(unique(locations$`Site Name`[!locations$`Site Name` %in% c(mn_sites, site_names)]))

c(mn_sites, site_names)[!c(mn_sites, site_names) %in% locations$`Site Name`]

length(c(mn_sites, site_names))

# Join Thunder Bay
thunder <- data.frame('AqsID' = 'thunder-bay',
                      'Site Name' = 'Thunder Bay',
                      'Lat' = '48.3794',
                      'Long'= '-89.29',
                      'Parameter' = 'O3',
                      stringsAsFactors = F,
                      check.names=F)
                      
locations <- rbind(locations[ , c(1,2,3,6:7)], thunder)

thunder$Parameter <- 'PM2.5'

locations <- rbind(locations, thunder) 

# Join missing Winnipeg parameters
winnipeg <- data.frame('AqsID' = c('000070119', '000070118'),
                       'Site Name' = c('winnipeg-ellen-st.', 'winnipeg-scotia-st.'),
                       'Lat' = c('49.9489','49.9319'),
                       'Long'= c('-97.3964', '-96.8631'),
                       'Parameter' = c('PM2.5', 'PM2.5'),
                       stringsAsFactors = F,
                       check.names=F)

locations <- rbind(locations, winnipeg) 

# Save locations
write.csv(locations[!duplicated(locations$AqsID), -2], "data-raw/locations.csv", row.names=F)

# Save Site-Params
locations$Parameter <- gsub("O3", "OZONE", locations$Parameter) 

locations$Parameter <- gsub("PM2.5", "PM25", locations$Parameter) 

locations$Site_Param <- paste(locations$AqsID, locations$Parameter, sep="_")

# Join state code
locations$FIPS <- substring(locations$AqsID, 1, 2)

locations <- left_join(locations, states)

write.csv(locations[ , -c(4:5,7)][, c(5,1:4)], "data-raw/site_params.csv", row.names=F)




