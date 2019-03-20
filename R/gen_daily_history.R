#########################################################
# Generate a daily history file for the previous seven
# days.  This is needed for testing.
#########################################################
library(stringr)

source("R/aqi_convert.R")

# Fargo, Lacrosse, Voyageurs
border_sites <- c('380171004', '271370034', '550630012')

# Sioux Falls, Emmetsburg
extra_sites <- c('191471002', '460990008')

site_params <- read.csv('data-raw/site_params.csv', stringsAsFactors = F,  check.names=F, colClasses = 'character')  

mn_sites <- filter(site_params, substring(AqsID, 1, 2) == '27' | AqsID %in% border_sites)

daily_history <- data.frame()

today_date <- as.Date(Sys.time(), tz = "America/Chicago")

for (i in 7:1) {
  get_date <- today_date - i
  
  file_date <- format(get_date, "%Y%m%d")
  file_year <- format(get_date, "%Y")
  
  airnow_link <- paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/",
                        file_year,"/",
                        file_date,
                        "/daily_data.dat")
  
  aqi_daily <- try(read_delim(airnow_link, "|", 
                              col_names = F, 
                              col_types = c('cccccdic')), 
                   silent = T)
  
  names(aqi_daily) <- c("Date", "AqsID", "Site Name", "Parameter", "Units", "Concentration","Averaging Period", "Agency")
  
  aqi_daily$Parameter <- toupper(aqi_daily$Parameter)
  
  aqi_daily <- filter(aqi_daily, AqsID %in% c(mn_sites$AqsID, border_sites, extra_sites), Parameter %in% c("OZONE-8HR", "PM2.5-24HR"))
  
  aqi_daily <- mutate(aqi_daily, Parameter = str_replace_all(Parameter, c("PM2.5.*" = "PM25", "OZONE.*" = "OZONE")))

  aqi_daily <- mutate(aqi_daily, AQI_Value = conc2aqi(Concentration,Parameter))
  
  daily_history <- rbind(daily_history, aqi_daily)

}

saveRDS(data.frame(daily_history, stringsAsFactors = F, check.names = F), "data/daily_history.Rdata" )

