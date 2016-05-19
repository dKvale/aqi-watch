#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(stringr)
library(rmarkdown)
library(pander)

setwd("C:/Users/dkvale/Desktop/aqi-watch")

options(rstudio.markdownToHTML = 
          function(inputFile, outputFile) {      
            require(markdown)
            markdownToHTML(inputFile, outputFile, stylesheet='C:\\Users\\dkvale\\Desktop\\aqi-watch\\R\\flat_table.css')   
          })

border_sites <- c('380171004', '271370034', '550630012')

canada_sites <- c('000070118', '000070119', '000070203', '000064001')

year <- format(Sys.Date(), "%Y")

daylight_savings <- Sys.Date() > as.Date(paste0(year, "-03-12")) & Sys.Date() < as.Date(paste0(year, "-10-6"))
  
# Load credentials
credentials <- read_csv("C:\\Users\\dkvale\\Desktop\\credentials.csv")

user <- credentials$user
pwd  <- credentials$pwd
email <- credentials$email
token <- credentials$issue_token

gmt_time <-  (as.numeric(format(Sys.time() - 195, tz="GMT", "%H")) - 1) %% 24
#gmt_time <- paste0("0", gmt_time) %>% substring(nchar(.) - 1, nchar(.))

aqi_all <- data.frame()

# Loop through 3 hours of records and keep most recent
for(i in 0:2) {

time <- paste0("0", (gmt_time - i) %% 24) %>% substring(nchar(.) - 1, nchar(.))
  
date_time <- paste0(format(Sys.time() - (60 * 60 + 195), tz="GMT", "%Y%m%d"), time)

airNow_link <- paste0('ftp://', user, ':', pwd, 
                      '@ftp.airnowapi.org/HourlyData/', date_time,'.dat')
  
aqi <- try(read_delim(airNow_link, "|", 
                      col_names=F, 
                      col_types=c('ccccdccdc')), 
           silent=T)

closeAllConnections()

# Write to error log if AirNow site is down
if(!is.data.frame(aqi) || (nrow(aqi) < 1)) {
  errs <- read.csv("log/error_log.csv", stringsAsFactors=F)

  Time <- as.character(format(Sys.time(), tz="America/Chicago"))
  
  errs <- rbind(errs, data.frame(File = date_time, 
                                 Time = Time, 
                                 Status="Failed", 
                                 Message = paste0(aqi, collapse="")))
  
  write.csv(errs, "log/error_log.csv", row.names=F)
  
} else {

names(aqi) <- c("Date", "Time", "AqsID", "Site Name", "Local_Time" , "Parameter", "Units", "Concentration","Agency")

aqi$Parameter <- gsub("[.]", "", aqi$Parameter)

aqi$StateID <- substring(aqi$AqsID, 1, 2) 

# Filter to local results
aqi <- filter(aqi, StateID %in% c('27', '19', '55', '38', '46') |
                AqsID %in% c(border_sites, canada_sites))

# Filter to Ozone and PM
aqi <- filter(aqi, grepl('PM25', Parameter) |
                   grepl('OZONE', Parameter) |
                   grepl('PM10', Parameter))

aqi$Site_Param <- paste(aqi$AqsID, aqi$Parameter, sep="_")

aqi <- filter(aqi, !Site_Param %in% aqi_all$Site_Param)

aqi_all <- rbind(aqi, aqi_all)

}
}

if(nrow(aqi_all) < 1) return()

aqi <- aqi_all[ , 1:9]

# Adjust time to Central time
aqi$Time <- (as.numeric(gsub(":00", "", aqi$Time)) - 6 + daylight_savings) %% 24

aqi$Time <- paste0(aqi$Time, ":00")

aqi$Date <- format(Sys.Date() - ifelse(gmt_time == 5, 1, 0), "%m/%d/%Y")

# Calculate AQI value using EPA breakpoints 
# [www.pca.state.mn.us/index.php/air/air-quality-and-pollutants/general-air-quality/air-quality-index/air-quality-about-the-data.html]
# PM10 is here [http://www3.epa.gov/ttn/oarpg/t1/memoranda/rg701.pdf]

# Load breakpoints
breaks <- read_csv("data-raw/aqi_breakpoints.csv", col_types=c('cccccccc'))

names(breaks) <- c("Rating", "Breakpoints", "OZONE", 
                   "PM25", "SO2", "CO", "NO2", "PM10")

# Define concentration to AQI function
conc2aqi <- function(conc, param){
  
  aqi_value <- breaks[ , c(param, "Breakpoints", "Rating")]
  
  names(aqi_value)[1] <- "Conc_cutoffs"
  
  aqi_value <- mutate(aqi_value, 
                      Breakpoints = str_split(Breakpoints, ","),
                      Conc_cutoffs = str_split(Conc_cutoffs, ","))
  
  aqi_value <- group_by(aqi_value, Rating) %>%
               mutate(Low_break  = as.numeric(unlist(Breakpoints)[1]),
                      High_break = as.numeric(unlist(Breakpoints)[2]),
                      Low_conc   = as.numeric(unlist(Conc_cutoffs)[1]),
                      High_conc  = as.numeric(unlist(Conc_cutoffs)[2]))
  
  aqi_value <- filter(aqi_value, High_conc > conc)[1, ]

  with(aqi_value, 
       Low_break + (conc - Low_conc) * (High_break - Low_break)/(High_conc - Low_conc))

}

# Define AQI to concentration function
aqi2conc <- function(aqi, param){
  
  aqi_value <- breaks[ , c(param, "Breakpoints", "Rating")]
  
  names(aqi_value)[1] <- "Conc_cutoffs"
  
  aqi_value <- mutate(aqi_value, 
                      Breakpoints = str_split(Breakpoints, ","),
                      Conc_cutoffs = str_split(Conc_cutoffs, ","))
  
  aqi_value <- group_by(aqi_value, Rating) %>%
    mutate(Low_break  = as.numeric(unlist(Breakpoints)[1]),
           High_break = as.numeric(unlist(Breakpoints)[2]),
           Low_conc   = as.numeric(unlist(Conc_cutoffs)[1]),
           High_conc  = as.numeric(unlist(Conc_cutoffs)[2]))
  
  aqi_value <- filter(aqi_value, High_break > aqi)[1, ]
  
  with(aqi_value, 
       Low_conc + (aqi - Low_break) * (High_conc - Low_conc) / (High_break - Low_break)) 
  
}


aqi <- group_by(aqi, AqsID, Parameter) %>% mutate(AQI_Value = round(conc2aqi(Concentration, Parameter)))


# Define function to grab concentrations for Canada sites from China's aqicn.org site
grab_aqicn <- function(country="usa", state="north-dakota", city="fargo-nw", param="pm25") {
  
  data <- try(readLines(paste0("http://aqicn.org/city/", 
                               country,"/", 
                               state, "/", 
                               city, "/")),
                               silent=TRUE)
  
  if(class(data) == "try-error" || (length(data) < 5)) return(data_frame())
  
  data <- data[grep(paste0("id='cur_", param, "'"), data)]
  
  data_aqi <- str_split(data, paste0("id='cur_", param, "'"))[[1]][2]
    
  data_aqi <- str_split(data_aqi, "align=center>")[[1]][2] %>% 
              substring(1, 2)
  
  data_aqi <- gsub("<", "", data_aqi)
  
  data <- str_split(data, "Updated on ")[[1]][2]
  
  data_time <- str_split(data, " ")[[1]][2]
  
  data_time <- as.numeric(gsub(":", "", substring(data_time, 1, 2))) %% 24
  
  data_time <- ifelse(state == "ontario", data_time - 2 + daylight_savings, data_time)
  
  data_time <- ifelse(state == "north-dakota", data_time - 1 + daylight_savings, data_time)
  
  data_time <- ifelse(state == "minnesota", data_time - 1 + daylight_savings, data_time)
  
  data_day <- str_split(data, " ")[[1]][1] %>% substring(1, 3)
  
  data_date <- c(format(Sys.Date() - as.numeric(!identical(data_day, format(Sys.Date(), "%a"))), "%m/%d/%Y"))
  
  aqsid <- switch(city, "fargo-nw" = "380171004", 
                        "red-lake-nation" = "Red Lake", 
                        "winnipeg-ellen-st." = '000070119',
                        "winnipeg-scotia-st." = '000070118',
                        "thunder-bay" = "thunder-bay"
                           )
  
  units <- ifelse(param=="o3", "PPB", "UG/M3")
  
  param <- ifelse(param=="o3", "OZONE", toupper(param))
  
  data_conc <- round(aqi2conc(as.numeric(data_aqi), param), 1)

  data <- data.frame(data_date, paste0(data_time, ":00"), aqsid, toupper(city), NaN, param, units, data_conc, x=paste(toupper(state), "Department of Health"), as.numeric(data_aqi), stringsAsFactors = F)
  
  names(data) <- names(aqi)
  
  return(data)
}

# Grab Fargo
## fargo <- grab_aqicn(country="usa", state="north-dakota", city="fargo-nw", param="pm25")

# Grab Red Lake
## red_lake <- grab_aqicn(country="usa", state="minnesota", city="red-lake-nation", param="pm25")

# Grab Canada
winnipeg_ellen_pm25 <- grab_aqicn(country="canada", state="manitoba", city="winnipeg-ellen-st.", param="pm25")
#winnipeg_ellen_03 <- grab_aqicn(country="canada", state="manitoba", city="winnipeg-ellen-st.", param="o3")

winnipeg_scotia_pm25 <- grab_aqicn(country="canada", state="manitoba", city="winnipeg-scotia-st.", param="pm25")
##winnipeg_scotia_03 <- grab_aqicn(country="canada", state="manitoba", city="winnipeg-scotia-st.", param="o3")

##brandon_pm25 <- grab_aqicn(country="canada", state="manitoba", city="brandon", param="pm25")
##brandon_o3 <- grab_aqicn(country="canada", state="manitoba", city="brandon", param="o3")

thunder_pm25 <- grab_aqicn(country="canada", state="ontario", city="thunder-bay", param="pm25")
thunder_o3 <- grab_aqicn(country="canada", state="ontario", city="thunder-bay", param="o3")

# Combine all
aqi <- rbind_list(aqi, 
                 winnipeg_ellen_pm25, 
                 winnipeg_scotia_pm25, 
                 thunder_pm25, 
                 thunder_o3)

# Add current time
aqi$Time_CST   <- as.character(format(Sys.time() + 10, tz="America/Chicago"))
names(aqi)[11] <- as.character(format(Sys.time() + 10, tz="America/Chicago"))

# Drop negative AQIs
aqi <- filter(aqi, AQI_Value >= 0)[ , -5]

# Arrange from high to low
aqi <- arrange(ungroup(aqi), -AQI_Value)

# Load previous aqi table
aqi_prev <- read_csv("data/aqi_previous.csv", col_types = c("ccccccdcdTT")) %>% 
              filter(!is.na(AQI_Value))

# Attach last AQI watch notification time
aqi$last_notification <- NA
names(aqi)[11] <- names(aqi_prev)[11]

# If high sites table has changed update github repo
if(!identical(aqi$AQI_Value, aqi_prev$AQI_Value) || 
   !identical(aqi$AqsID, aqi_prev$AqsID) || 
   as.numeric(difftime(names(aqi)[10], names(aqi_prev)[10], units="hours")) > 0.9) {

locations <- read.csv('data-raw/locations.csv', stringsAsFactors = F,  check.names=F, colClasses = 'character')  
  
site_params <- read.csv('data-raw/site_params.csv', stringsAsFactors = F,  check.names=F, colClasses = 'character')  

aqi_rank <- group_by(aqi, AqsID) %>% arrange(-AQI_Value) %>% mutate(rank = 1:n())

aqi_rank <- filter(ungroup(aqi_rank), rank == 1) %>% arrange(-AQI_Value)

# Create high sites table
#rmarkdown::render("R/high_sites2.Rmd", output_file="../index.html")  
setwd("C:/Users/dkvale/Desktop/aqi-watch/web")

rmarkdown::render_site()

setwd("C:/Users/dkvale/Desktop/aqi-watch")

# Commit to github 
git <- "C: & cd C:/Users/dkvale/Desktop/aqi-watch & C:/Users/dkvale/AppData/Local/Programs/Git/bin/git.exe "

shell(paste0(git, "checkout --orphan new_branch"))

shell(paste0(git, "add -A"))

commit <- paste0(git, 'commit -am ', '"update high sites"')
shell(commit)

shell(paste0(git, "branch -D master"))
shell(paste0(git, "branch -m master"))

shell(paste0(git, "config --global user.name dkvale"))
shell(paste0(git, "config --global user.email ", email))
shell(paste0(git, "config credential.helper store"))

push <- paste0(git, "push -f origin master")
shell(push)

shell(paste0(git, "push origin --delete gh-pages"))

push <- paste0(git, "subtree push --prefix web/_site origin gh-pages")
shell(push)

# Save high sites table to test for changes on next cycle
write.csv(aqi, "data/aqi_previous.csv", row.names = F)
}


# Create issue if exceedances found for PM25 or Ozone
## And a new site has been added to the list
## or if it has been over 2 hours since the last issued alert

# Set issue notification to sleep from 10 pm to 4 am
if((as.numeric(format(Sys.time(), "%H")) < 22) && (as.numeric(format(Sys.time(), "%H")) > 3)) { 
  
# Remove PM10 and low concentrations
aqi <- filter(aqi, AQI_Value >= 85) 
  
aqi_all <- aqi
aqi <- filter(aqi, Parameter != "PM10")
aqi_prev <- filter(aqi_prev, Parameter != "PM10")

if(nrow(aqi) > 0) {
  
  if(((sum(!aqi$AqsID %in% aqi_prev$AqsID) > 0) || 
     (sum(!aqi$Parameter %in% aqi_prev$Parameter) > 0)) ||
      as.numeric(difftime(names(aqi)[10], names(aqi_prev)[11], units="hours")) > 1.05) {
  
  # Commit to github 
  git <- "C: & cd C:/Users/dkvale/Desktop/aqi-watch & C:/Users/dkvale/AppData/Local/Programs/Git/bin/git.exe "
    
  shell(paste0(git, "config --global user.name dkvale"))
  shell(paste0(git, "config --global user.email ", email))
    
  max_site <- filter(aqi, AQI_Value == max(aqi$AQI_Value, na.rm=T))[1, ]
    
  if(nrow(filter(aqi, grepl('Minnesota', Agency) | AqsID %in% c(border_sites),
                 AQI_Value > 91)) > 0) {
    VIP_list <- "Attention:  &#64;monikav21 &#64;rrobers &#64;Mr-Frank &#64;Rstrass &#64;krspalmer "
  } else {
    VIP_list <- " "
  }
  
  message <- paste0("__Update for ", format(Sys.time(), "%h %d, %Y at %H:%M"), " CST&#46;__", 
                    "  There ", 
                    ifelse(length(unique(aqi$AqsID)) > 1, "are ", "is "),
                    length(unique(aqi$AqsID)), 
                    " monitoring ", 
                    ifelse(length(unique(aqi$AqsID)) > 1, "sites ", "site "),
                    "reporting a 1-hr AQI above 85&#46; ", 
                    "The maximum 1-hr AQI of ", max_site$AQI_Value,
                    " (", gsub("25","2&#46;5", max_site$Parameter), 
                    ") was reported at ", max_site$'Site Name', " [AQS ID: ", max_site$AqsID,
                    "] by the **", max_site$Agency, 
                    "**&#46; Learn more at <a href=http://dkvale&#46;github&#46;io/aqi-watch> AQI Watch</a>&#46; </br> </br> ",
                    VIP_list)
  
   issue <- paste0('{\n\t"title": " High concentrations reported at ', 
                   aqi$Time[1], ', ', aqi$Date[1], 
                   '", \n\t"body": "', message,
                   '", \n\t"labels": ["watch alerts"]\n}')
   
  # Save issue to JSON file
  cat(issue, file = "issue.json") 
  
  # Create batch file
  cat(paste0('C: & CD C:/Users/dkvale/Desktop/aqi-watch & 
              C:/Users/dkvale/AppData/Local/Programs/Git/bin/curl.exe ', 
             '-i -H "Authorization: token ', token,
             '\" -d @issue.json https://api.github.com/repos/dKvale/aqi-watch/issues'), file = "create_issue.bat") 
  
  shell("C: & CD C:/Users/dkvale/Desktop/aqi-watch & create_issue.bat")
  
  #Save alert time
  names(aqi_all)[11] <- as.character(Sys.time() + 61)
  write.csv(aqi_all, "data/aqi_previous.csv", row.names = F)
  }
}

}
