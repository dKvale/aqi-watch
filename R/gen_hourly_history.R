#########################################################
# Generate an hourly history file for the previous day
# day and current day up to the previous hour.  This is needed for testing.
#########################################################
source("R/aqi_convert.R")
source("R/get_nowcast.R")
source("R/aqi_colors.R")
source("R/col_format.R")

#For first hour of data, join to data frame containing list of MN sites.
locations <- read.csv('data-raw/locations.csv', stringsAsFactors = F,  check.names=F, colClasses = 'character')  

site_params <- read.csv('data-raw/site_check.csv', stringsAsFactors = F,  check.names=F, colClasses = 'character')  

site_params$Local_Time <- as.numeric(site_params$Local_Time)

# Fargo, Lacrosse, Voyageurs
border_sites <- c('380171004', '271370034', '550630012')

# Sioux Falls, Emmetsburg
extra_sites <- c('191471002', '460990008')

canada_sites <- c('000070118', '000070119', '000070203', '000064001')

mn_sites <- filter(site_params, substring(AqsID, 1, 2) == '27' | AqsID %in% border_sites)


year <- format(Sys.Date(), "%Y")

daylight_savings <- Sys.Date() > as.Date(paste0(year, "-03-12")) & Sys.Date() < as.Date(paste0(year, "-10-6"))

# Load credentials
#credentials <- read_csv("../credentials.csv")

gmt_time <-  (as.numeric(format(Sys.time() - 195, tz="GMT", "%H")) - 1) %% 24
#gmt_time <- paste0("0", gmt_time) %>% substring(nchar(.) - 1, nchar(.))

#######################################################################
# Hourly AQI data -- obtain the first hour from the previous day and
# join to mn_sites to ensure any obs that are missing in the first hour
# are included in the list.
#######################################################################
aqi_all <- data.frame()


time <- paste0("0", (gmt_time) %% 24) %>% substring(nchar(.) - 1, nchar(.))
  
date_time <- paste0(format(Sys.time(), tz = "GMT", "%Y%m%d"), time)
  
if (daylight_savings){
  prev_day_init <- paste0(format(as.Date(date_time, format = "%Y%m%d%H")-1, format = "%Y%m%d"), "05")
} else {
  prev_day_init <- paste0(format(as.Date(date_time, format = "%Y%m%d%H")-1, format = "%Y%m%d"), "06")
}

start_date <- as.POSIXct(paste(as.Date(Sys.time()-24*60*60),"00", tz = "America/Chicago")) %>%
              format(tz = "GMT") %>% 
              as.POSIXct(tz = "GMT")

end_date <- as.POSIXct(date_time, tz = "GMT", format = "%Y%m%d%H") - 3600

all_dates <- seq(start_date,end_date,3600)
  
for (i in seq(1:length(all_dates))) {
  
  ind_date <- format(all_dates[i], format = "%Y%m%d%H")
  
  ind_mdy <- format(all_dates[i], format = "%m/%d/%y")
  
  ind_hr <- format(all_dates[i], format = "%H:%M")
  
  airnow_link <- paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/",
                        substring(ind_date, 1, 4), "/", 
                        substring(ind_date, 1, 8), "/", 
                        "HourlyData_", ind_date, ".dat")
  
  aqi <- try(read_delim(airnow_link, "|", 
                        col_names = F, 
                        col_types = c('ccccdccdc')), 
             silent = T)
  
  closeAllConnections()
  

  names(aqi) <- c("Date", "Time", "AqsID", "Site Name", "Local_Time" , "Parameter", "Units", "Concentration","Agency")
    
  aqi$Parameter <- gsub("[.]", "", aqi$Parameter)
    
  aqi <- filter(aqi, 
                AqsID %in% c(mn_sites$AqsID, border_sites, extra_sites), 
                Parameter %in% c("OZONE", "PM25"))
    
  # Keep all criteria pollutants
  aqi <- filter(aqi, toupper(Parameter) %in% c("CO", "NO2", "O3", "OZONE", "PM10", "PM25", "SO2"))
    
  aqi$Site_Param <- paste(aqi$AqsID, aqi$Parameter, sep = "_")
    
  aqi$Agency <- ifelse(grepl("Wisconsin", aqi$Agency), "Wisconsin DNR", aqi$Agency)
  
  aqi$Agency <- ifelse(grepl("South Dakota", aqi$Agency), "South Dakota", aqi$Agency)
    
  aqi$Agency <- ifelse(grepl("North Dakota", aqi$Agency), "North Dakota Health", aqi$Agency)
    
  missing <- filter(mn_sites, !Site_Param %in% aqi$Site_Param)
    
  missing <- mutate(missing,
                    Date = ind_mdy,
                    Time = ind_hr)
    
  aqi <- bind_rows(aqi, missing)
    
  aqi_all <- bind_rows(aqi, aqi_all)
}    
  

#--------------------------------------------------------#
# Check for results
#--------------------------------------------------------#

if (nrow(aqi_all) < 1) return()

#--------------------------------------------------------#


aqi <- aqi_all[ , 1:9]

# Adjust time to Central daylight time CDT
aqi$local <- as.POSIXct(paste(aqi$Date, aqi$Time), tz = "GMT", "%m/%d/%y %H:%M") %>% format(tz = "America/Chicago", usetz = TRUE)

#aqi$Time <- (as.numeric(gsub(":00", "", aqi$Time)) - 6 + daylight_savings) %% 24

aqi$Time <- as.POSIXlt(aqi$local, tz = "America/Chicago") %>% format(tz = "America/Chicago", format = "%H") %>% as.numeric()

aqi$Time <- paste0(aqi$Time, ":00")

#aqi$Date <- format(Sys.Date() - ifelse(gmt_time == 5, 1, 0), "%m/%d/%Y")
aqi$Date <-  as.POSIXlt(aqi$local, tz = "America/Chicago") %>% as.Date() %>% format("%m/%d/%Y")

aqi$local <- NULL


# Calculate AQI value using EPA breakpoints 
# [www.pca.state.mn.us/index.php/air/air-quality-and-pollutants/general-air-quality/air-quality-index/air-quality-about-the-data.html]
# PM10 is here [http://www3.epa.gov/ttn/oarpg/t1/memoranda/rg701.pdf]

# Load breakpoints
#breaks_aqi <- read_csv("data-raw/aqi_breakpoints.csv", col_types = c('cccccccc'))

#names(breaks_aqi) <- c("Rating", "Breakpoints", "OZONE", 
#                      "PM25", "SO2", "CO", "NO2", "PM10")

aqi <- group_by(aqi, AqsID, Parameter) %>% mutate(AQI_Value = round(conc2aqi(Concentration, Parameter)))

aqi$Time_CST   <- as.character(format(Sys.time() + 10, tz="America/Chicago"))
names(aqi)[11] <- as.character(format(Sys.time() + 10, tz="America/Chicago"))

# Drop negative AQIs below 30
aqi <- filter(aqi, AQI_Value > -29)[ , -5]

# Set negative AQIs & concentrations to zero
aqi$AQI_Value     <- ifelse(aqi$AQI_Value < 0, 0, aqi$AQI_Value)

aqi$Concentration <- ifelse(aqi$Concentration < -5, 0, aqi$Concentration)

aqi <- filter(aqi, 
               AqsID %in% c(mn_sites$AqsID, border_sites, extra_sites), 
               Parameter %in% c("OZONE", "PM25"))


  
aqi <- aqi[ , c(1:4,8,5,7,9)]
  
# Convert time to numeric
aqi$Time <- strptime(aqi$Time, "%H") %>% format("%H") %>% as.numeric()
  
# Convert date column to date
aqi$Date <- as.Date(aqi$Date, format = '%m/%d/%Y')

# Save hourly history file.
saveRDS(aqi, "data/aqi_history.Rdata")
  
  
