#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(stringr)
library(rmarkdown)
library(pander)

options(rstudio.markdownToHTML = 
          function(inputFile, outputFile) {      
            require(markdown)
            markdownToHTML(inputFile, outputFile, stylesheet = 'R/flat_table.css')   
          })

#setwd("../")
setwd("aqi-watch")

source("R/aqi_convert.R")

email_trigger <- 91
pm10_trigger  <- 130

# Email alert subscribers
subscribers <- try(read_csv("https://raw.githubusercontent.com/dKvale/aqi-watch/master/data/subscribers.csv"))

# Fargo, Lacrosse, Voyageurs
border_sites <- c('380171004', '271370034', '550630012')

# Sioux Falls, Emmetsburg, Aberdeen
extra_sites  <- c('191471002', '460990008', '840460990009', '840460130004')

canada_sites <- c('000070118', '000070119', '000070203', '000064001')

year <- format(Sys.Date(), "%Y")

daylight_savings <- Sys.Date() > as.Date(paste0(year, "-03-12")) & Sys.Date() < as.Date(paste0(year, "-10-6"))

# Load credentials
credentials <- read_csv("../credentials.csv")

gmt_time <-  (as.numeric(format(Sys.time() - 195, tz="GMT", "%H")) - 1) %% 24

#######################################################################
# Hourly AQI data -- obtain the most recent hour of data
#######################################################################
aqi_all <- data.frame()

# Loop through 3 hours of records and keep most recent
for (i in 0:2) {
  
  time <- paste0("0", (gmt_time - i) %% 24) %>% substring(nchar(.) - 1, nchar(.))
  
  # Adjust date when searching back to previous day's results
  if(((gmt_time < 2) && (time > 20)) | gmt_time == 23) {  
    date_time <- paste0(format(Sys.time() - (60 * 60 * 24), tz = "GMT", "%Y%m%d"), time)
  } else {
    date_time <- paste0(format(Sys.time(), tz = "GMT", "%Y%m%d"), time)
  }
  
  
  airnow_link <- paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/",
                        substring(date_time, 1, 4), "/", 
                        substring(date_time, 1, 8), "/", 
                        "HourlyData_", date_time, ".dat")
  
  aqi <- try(read_delim(airnow_link, "|", 
                        col_names = F, 
                        col_types = c('ccccdccdc')), 
             silent = T)
  
  closeAllConnections()
  
  # If blank, try again in 5 minutes
  if(!is.data.frame(aqi) || (nrow(aqi) < 1)) {
    if(i == 0) {
      
      Sys.sleep(60 * 4)  # Pause for 5 minutes        
      
      aqi <- try(read_delim(airnow_link, "|", 
                            col_names = F, 
                            col_types = c('ccccdccdc')), 
                 silent = T)         
    }
  }
  
  # Write to error log if AirNow data missing
  if (!is.data.frame(aqi) || (nrow(aqi) < 1)) {
    
    errs <- read.csv("log/error_log.csv", stringsAsFactors = F)
    
    errs$File <- as.character(errs$File)
    
    err_time <- as.character(format(Sys.time(), tz = "America/Chicago"))
    
    errs <- bind_rows(errs, data.frame(File    = date_time, 
                                       Time    = err_time, 
                                       Status  = "Failed", 
                                       Message = paste0(aqi, collapse = ""), stringsAsFactors = F))
    
    write.csv(errs, "log/error_log.csv", row.names=F)
    
  } else {
    
    names(aqi) <- c("Date", "Time", "AqsID", "Site Name", "Local_Time" , "Parameter", "Units", "Concentration","Agency")
    
    aqi$Parameter <- gsub("[.]", "", aqi$Parameter)
    
    aqi$StateID <- substring(aqi$AqsID, 1, 2) 
    
    # Filter to local results
    aqi <- filter(aqi, StateID %in% c('27', '19', '55', '38', '46', '84') |
                    AqsID %in% c(border_sites, canada_sites))
    
    # Keep all criteria pollutants
    aqi <- filter(aqi, toupper(Parameter) %in% c("CO", "NO2", "O3", "OZONE", "PM10", "PM25", "SO2"))
    
    aqi$Site_Param <- paste(aqi$AqsID, aqi$Parameter, sep = "_")
    
    aqi <- filter(aqi, !Site_Param %in% aqi_all$Site_Param)
    
    aqi_all <- bind_rows(aqi, aqi_all)
    
  }
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


#-- Get missing sites from China Air Quality site - aqicn.org
source("R/get_aqicn.R")

#-- Fargo
## fargo <- get_aqicn(country="usa", state="north-dakota", city="fargo-nw", param="pm25")
#-- Red Lake
## red_lake <- get_aqicn(country="usa", state="minnesota", city="red-lake-nation", param="pm25")

#-- Canada
winnipeg_ellen_pm25 <- tryCatch({get_aqicn(country="canada", state="manitoba", city="winnipeg-ellen-st.", param="pm25")}, error = function(e) {aqi[0, ]})
winnipeg_ellen_o3  <- tryCatch({get_aqicn(country="canada", state="manitoba", city="winnipeg-ellen-st.", param="o3")}, error = function(e) {aqi[0, ]})

winnipeg_scotia_pm25 <- tryCatch({get_aqicn(country="canada", state="manitoba", city="winnipeg-scotia-st.", param="pm25")}, error = function(e) {aqi[0, ]})
#winnipeg_scotia_o3   <- tryCatch({get_aqicn(country="canada", state="manitoba", city="winnipeg-scotia-st.", param="o3")}, error = function(e) {aqi[0, ]})

brandon_pm25 <- tryCatch({get_aqicn(country="canada", state="manitoba", city="brandon", param="pm25")}, error = function(e) {aqi[0, ]})
brandon_o3   <- tryCatch({get_aqicn(country="canada", state="manitoba", city="brandon", param="o3")}, error = function(e) {aqi[0, ]})

thunder_pm25 <-  tryCatch({get_aqicn(country="canada", state="ontario", city="thunder-bay", param="pm25")}, error = function(e) {aqi[0, ]})
thunder_o3   <-  tryCatch({get_aqicn(country="canada", state="ontario", city="thunder-bay", param="o3")}, error = function(e) {aqi[0, ]})

# Combine all
aqi <- bind_rows(aqi, 
                 winnipeg_ellen_pm25, winnipeg_ellen_o3, 
                 winnipeg_scotia_pm25, #winnipeg_scotia_o3, 
                 brandon_pm25, brandon_o3,
                 thunder_pm25, thunder_o3)

# Add current time
aqi$Time_CST   <- as.character(format(Sys.time() + 10, tz = "America/Chicago"))

names(aqi)[11] <- as.character(format(Sys.time() + 10, tz = "America/Chicago"))

# Drop negative AQIs below 30
aqi <- filter(aqi, AQI_Value > -29)[ , -5]

# Set negative AQIs & concentrations to zero
aqi$AQI_Value     <- ifelse(aqi$AQI_Value < 0, 0, aqi$AQI_Value)

aqi$Concentration <- ifelse(aqi$Concentration < -5, 0, aqi$Concentration)


# Arrange from high to low
aqi <- arrange(ungroup(aqi), -AQI_Value)

# Load previous aqi table
aqi_prev <- read_csv("data/aqi_previous.csv", col_types = c("ccccccdcdTT")) %>% 
            filter(!is.na(AQI_Value))

# Attach last AQI watch notification time
aqi$last_notification <- NA

names(aqi)[11] <- names(aqi_prev)[11]

locations <- read.csv('data-raw/locations.csv', stringsAsFactors = F,  check.names=F, colClasses = 'character')  

#new_locations <- read_delim("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/2021/20210716/monitoring_site_locations.dat",
#                            "|", 
#                            col_names = F)

# Update Sioux Falls & Aberdeen, Milwaukee, Madison
locations <- locations %>%
             bind_rows(data.frame(AqsID = c("840380250004",
                                            "840460990009",
                                            "840550250047",
                                            "840550790068",
                                            "840551270006",
                                            "840460130004"
                                            
                                            ), 
                                  "Site Name" = c("Lake Ilo",
                                                  "SF-USD",
                                                  "Madison University Ave",
                                                  "Milwaukee-UWM UPark", 
                                                  "Elkhorn",
                                                  "Aberdeen"
                                                  
                                                  ),
                                  Lat   = c("47.34259",
                                            "43.59901",
                                            "43.07378",
                                            "43.09455",
                                            "42.66218",
                                            "45.4686"
                                            ),
                                  Long  = c("-102.646",
                                            " -96.78331",
                                            "-89.43595",
                                            " -87.90145",
                                            "-88.48703",
                                            "-98.49406"
                                             ),
                                  stringsAsFactors = F,
                                  check.names = F))

# Get MN site info
site_params <- read.csv('data-raw/site_params.csv', stringsAsFactors = F, check.names = F, colClasses = 'character')  

mn_sites <- filter(site_params, substring(AqsID, 1, 2) == '27' | AqsID %in% border_sites)

# List of forecast sites for plotting model performance only includes sites with a monitor for that pollutant.
forecast_sites_pm25 <- filter(mn_sites, Parameter == "PM25")

forecast_sites_ozone <- filter(mn_sites, Parameter == "OZONE", AqsID != "271370034")

########################################################################
#  Daily AQI data -- Obtain the previous day's data once per day and
#  add to daily history.
########################################################################

# Load daily history
daily_history <- readRDS("data/daily_history.Rdata")

#If it's after 7 AM, check the most recent day and download yesterday's data if not in the history.
local_hr <- as.numeric(format(Sys.time(), tz = "America/Chicago", "%H"))

today_date <- as.Date(Sys.time(), tz = "America/Chicago")

if (local_hr > 7) {
  
  yesterday_date <- today_date - 1
  
  max_date <- max(as.Date(daily_history$Date, format = "%m/%d/%y"))
  
  if (max_date < yesterday_date) {
    
    file_date <- format(yesterday_date, "%Y%m%d")
    file_year <- format(yesterday_date, "%Y")
    
    airnow_link <- paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/",
                          file_year,"/",
                          file_date,
                          "/daily_data.dat")
    
    aqi_daily <- try(read_delim(airnow_link, "|", 
                                col_names = F, 
                                col_types = c('cccccdic')), 
                     silent = T)
    
    if ( !is.data.frame(aqi_daily) || nrow(aqi_daily) < 1 ) {
      errs <- read.csv("log/error_log.csv", stringsAsFactors = F)
      
      err_time <- as.character(format(Sys.time(), tz = "America/Chicago"))
      
      errs <- bind_rows(errs, data.frame(File    = paste0(file_date,"/daily_data.dat"), 
                                         Time    = err_time, 
                                         Status  = "Failed", 
                                         Message = paste0(aqi, collapse = "")))
      
      write.csv(errs, "log/error_log.csv", row.names=F)
      
    } else {
      
      names(aqi_daily) <- c("Date", "AqsID", "Site Name", "Parameter", "Units", "Concentration","Averaging Period", "Agency")
      
      aqi_daily$Parameter <- toupper(aqi_daily$Parameter)
      
      data <- filter(aqi, 
                     AqsID %in% c(mn_sites$AqsID, border_sites, extra_sites), 
                     Parameter %in% c("OZONE", "PM25"))
      
      aqi_daily <- filter(aqi_daily, AqsID %in% c(mn_sites$AqsID, border_sites, extra_sites), Parameter %in% c("OZONE-8HR", "PM2.5-24HR"))
      
      aqi_daily <- mutate(aqi_daily, Parameter = str_replace_all(Parameter, c("PM2.5.*" = "PM25", "OZONE.*" = "OZONE")))
      
      aqi_daily <- mutate(aqi_daily, AQI_Value = conc2aqi(Concentration,Parameter))
      
      daily_history <- rbind(daily_history, aqi_daily)
      
    }
    
    # Remove any data older than one week.
    weekold_date <- today_date - 7
    
    daily_history <- daily_history[as.Date(daily_history$Date, format = "%m/%d/%y") >= weekold_date,]
    
    saveRDS(data.frame(daily_history, stringsAsFactors = F, check.names = F), "data/daily_history.Rdata" )
    
  }
}


########################################################################
#  AQI model performance -- Obtain AQI model performance data.  Updated
#  once per day.
########################################################################
aqi_models <- read.csv("https://raw.githubusercontent.com/dKvale/aircast/master/data/model_performance.csv",
                       stringsAsFactors = FALSE)

aqi_models$forecast_date <- as.Date(aqi_models$forecast_date)

# Put category forecasts in new column and change numeric forecasts to integers.
aqi_cat_colors <- c("green",
                    "yellow",
                    "orange",
                    "red",
                    "purple")

aqi_models <- mutate(aqi_models, fcst_pm25_aqi_cats = ifelse(as.character(fcst_pm25_aqi) %in% aqi_cat_colors, as.character(fcst_pm25_aqi), NA), 
                     fcst_ozone_aqi_cats = ifelse(as.character(fcst_ozone_aqi) %in% aqi_cat_colors, as.character(fcst_ozone_aqi), NA),
                     fcst_pm25_aqi_cats_val = cat2aqimax(fcst_pm25_aqi_cats),
                     fcst_ozone_aqi_cats_val = cat2aqimax(fcst_ozone_aqi_cats),
                     fcst_pm25_aqi_cats_text = ifelse(!is.na(fcst_pm25_aqi_cats), paste0("Day ",forecast_day," fcst\n",fcst_pm25_aqi_cats), NA),
                     fcst_ozone_aqi_cats_text = ifelse(!is.na(fcst_ozone_aqi_cats), paste0("Day ",forecast_day," fcst\n",fcst_ozone_aqi_cats), NA))

aqi_models$fcst_pm25_aqi <- as.integer(aqi_models$fcst_pm25_aqi)

aqi_models$fcst_ozone_aqi <- as.integer(aqi_models$fcst_ozone_aqi)

aqi_models <- mutate(aqi_models, site_catid = gsub('-','',site_catid))

mn_sites_uniq <- mn_sites[,c("AqsID","Site Name")] %>% unique()

aqi_models <- left_join(aqi_models, mn_sites_uniq, by = c("site_catid"="AqsID"))


#--------------------------------------------------------#
# Send Alert                                             #
#--------------------------------------------------------#
# Create issue if exceedances found for a critieria pollutant
## And a new site has been added to the list
## or if it has been over 2 hours since the last issued alert

# Set issue notification to sleep from 10 pm to 4 am
watch_time <- (as.numeric(format(Sys.time(), "%H")) < 22) && (as.numeric(format(Sys.time(), "%H")) > 4)


if(watch_time) { 
  
  # Remove: low concentrations, and outstate monitors
  watch <- filter(aqi, AQI_Value > email_trigger) 
  
  watch <- filter(watch, (AQI_Value > pm10_trigger) | (Parameter %in% c("OZONE", "PM25")))
  
  watch <- filter(watch, grepl('Minnesota', Agency) | AqsID %in% c(border_sites, extra_sites))
  
  
  ## Drop PM10 from previous alert list to 
  ## ensure PM2.5 and Ozone alerts go out?
  #watch <- filter(watch, Parameter != "PM10")
  #aqi_prev <- filter(aqi_prev, Parameter != "PM10")
  if (nrow(watch) > 0) {
    
    if (as.numeric(difftime(names(watch)[10], names(aqi_prev)[11], units = "hours")) > .9) {
      
      if ((sum(!watch$AqsID %in% aqi_prev$AqsID) > 0) || 
          as.numeric(difftime(names(watch)[10], names(aqi_prev)[11], units = "hours")) > 1.9) {
        
        
        watch$Agency <- gsub("Minnesota Pollution Control Agency", "MPCA", watch$Agency)
        
        max_site <- filter(watch, AQI_Value == max(watch$AQI_Value, na.rm = T))[1, ]
        
        # Commit to github 
        git <- "cd ~/aqi-watch & git "
        
        system(paste0(git, "config --global user.name dkvale"))
        system(paste0(git, "config --global user.email ", credentials$email))          
        
        if (sum(unique(watch$AqsID) %in% mn_sites$AqsID) < 1) {
          VIP_list <- ""
        } else {
          VIP_list <- paste("Attention:",  paste0("&#64;", subscribers$git_name, collapse = " "))
        }
        
        message_title <- paste0("1-hr AQI at ", max_site$AQI_Value, " for ", max_site$Parameter)
        
        message_text <- paste0("**AQI Watch** </br>",
                               length(unique(watch$AqsID)), 
                               ifelse(length(unique(watch$AqsID)) > 1, " monitors are ", " monitor is "),
                               "reporting a 1-hr AQI above 90&#46; ", 
                               "A value of **", max_site$AQI_Value,
                               "** for ", gsub("25","2&#46;5", max_site$Parameter), 
                               " was reported at **", max_site$'Site Name',
                               "** (", max_site$Agency, 
                               ")&#46; For more details visit the <a href=http://mpca-air&#46;github&#46;io/aqi-watch> AQI Watch</a>&#46; </br>",
                               "_", format(Sys.time(), "%h %d, %Y at %H:%M"), " CDT_ </br> </br>",
                               VIP_list)
        
        issue <- paste0('{\n\t"title": "', message_title, 
                        '", \n\t"body": "', message_text,
                        '", \n\t"labels": ["watch alerts"]\n}')
        
        # Save issue to JSON file
        cat(issue, file = "issue.json") 
        
        # Create batch file
        send_issue <- paste0('cd ~/aqi-watch; curl ', 
                             '-i -H "Authorization: token ', 
                             credentials$issue_token,
                             '\" -d @issue.json https://api.github.com/repos/dKvale/aqi-watch/issues')
        
        system(send_issue)
        
        #Save alert time
        names(watch)[11] <- as.character(Sys.time() + 61)
        
        write.csv(watch, "data/aqi_previous.csv", row.names = F)
      }   # Sites added to list or 2 hours has lapsed
    }   # 1 hour has lapsed
  }   # High sites check
}   # Sleep time check


#--------------------------------------------------------#
# Update web map and tables                              #
#--------------------------------------------------------#

# Clean outstate names
aqi$Agency <- ifelse(grepl("Wisconsin", aqi$Agency), "Wisconsin DNR", aqi$Agency)

aqi$Agency <- ifelse(grepl("South Dakota", aqi$Agency), "South Dakota", aqi$Agency)

aqi$Agency <- ifelse(grepl("North Dakota", aqi$Agency), "North Dakota Health", aqi$Agency)

aqi_rank <- group_by(aqi, AqsID) %>% arrange(-AQI_Value) %>% mutate(rank = 1:n())

aqi_rank <- filter(ungroup(aqi_rank), rank == 1) %>% arrange(-AQI_Value)

# Create high sites table
setwd("web")

rmarkdown::render_site("index.Rmd")
rmarkdown::render_site("todays_obs.Rmd")
rmarkdown::render_site("daily_history.Rmd")
rmarkdown::render_site("week_review.Rmd")

if (watch_time) {
  rmarkdown::render_site("model_perf.Rmd")
}

if (FALSE) {
  rmarkdown::render_site("airnow_map.Rmd")
  rmarkdown::render_site("smogwatch.Rmd")
}

setwd("../")

system("sudo cp -a ~/aqi-watch/web/_site/.  ../../../../usr/share/nginx/html/")

# Save high sites table to test for changes on next cycle
write.csv(aqi, "data/aqi_previous.csv", row.names = F)

# Clean house
rm(aqi)
rm(aqi_rank)
