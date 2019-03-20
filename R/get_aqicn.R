# Define function to download current concentrations for Canada sites 
# from China's aqicn.org site

get_aqicn <- function(country="usa", state="north-dakota", city="fargo-nw", param="pm25") {
  
  data <- try(readLines(paste0("http://aqicn.org/city/", 
                               country,"/", 
                               state, "/", 
                               city, "/")),
              silent = TRUE)
  
  if(class(data) == "try-error" || (length(data) < 5)) stop("No observations returned.")
  
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
                  "thunder-bay" = "thunder-bay")
  
  units <- ifelse(param=="o3", "PPB", "UG/M3")
  
  param <- ifelse(param=="o3", "OZONE", toupper(param))
  
  data_conc <- round(aqi2conc(as.numeric(data_aqi), param), 1)
  
  data <- data.frame(data_date, 
                     paste0(data_time, ":00"), 
                     aqsid, 
                     toupper(city), 
                     NaN,
                     param, 
                     units, 
                     data_conc, 
                     x=paste(toupper(state), "Department of Health"), 
                     as.numeric(data_aqi), 
                     stringsAsFactors = F)
  
  names(data) <- names(aqi)[1:10]
  
  if(nrow(data) < 1) stop("No observations returned.")
  
  return(data)
}