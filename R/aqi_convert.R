# Calculate AQI value using EPA breakpoints 
# [www.pca.state.mn.us/index.php/air/air-quality-and-pollutants/general-air-quality/air-quality-index/air-quality-about-the-data.html]
# PM10 is here [http://www3.epa.gov/ttn/oarpg/t1/memoranda/rg701.pdf]

library(readr)
library(dplyr)


# Load breakpoints
breaks_aqi <- read_csv("https://raw.githubusercontent.com/dKvale/aqi-watch/master/data-raw/aqi_breakpoints.csv", 
                       col_types = c('cccccccc'))

names(breaks_aqi) <- c("rating", "breakpoints", "ozone", 
                       "pm25", "so2", "co", "no2", "pm10")


# Color functions

# AQI label to AQI category
label2cat <- function(label = NULL) {
  
  aqi_cats   <- c("white",
                  "green",
                  "yellow",
                  "orange",
                  "red",
                  "purple")
  
  get_cat <- function(x) {
    ifelse(is.null(x), aqi_cats[1],
           ifelse(is.na(x), aqi_cats[1],
             ifelse(x == "Good",  aqi_cats[2], 
                    ifelse(x == "Moderate", aqi_cats[3],
                           ifelse(x == "Unsafe for Sensitive Groups", aqi_cats[4], 
                                  ifelse(x == "Unhealthy", aqi_cats[5], 
                                         aqi_cats[6]))))))
  }
  
  cats <- sapply(label, FUN = function(x) get_cat(x))
  
  names(cats) <- NULL
  
  return(cats)
}


# AQI category to color
cat2color <- function(cat = NULL) {
  
  cat <- tolower(cat)
  
  aqi_colors <- c("#FFF",     # White
                  "#53BF33",  #"#9BF59B",  # Green
                  "#FFEE00",  #"#ffff00",  # Yellow 
                  "#DB6B1A",  #"#ff7e00",  # Orange
                  "#ff0000",  # Red 
                  "#99004c")  # Purple
  
  get_color <- function(x) {
    ifelse(is.null(x), aqi_colors[1],
           ifelse(is.na(x), aqi_colors[1],
                  ifelse(x == "green", aqi_colors[2], 
                         ifelse(x == "yellow", aqi_colors[3],
                                ifelse(x == "orange", aqi_colors[4], 
                                       ifelse(x == "red", aqi_colors[5], 
                                              aqi_colors[6]))))))
  }
  
  colors <- sapply(cat, FUN = function(x) get_color(x))
  
  names(colors) <- NULL
  
  return(colors)
  
}

# AQI category to value
cat2aqi <- function(cat = NULL) {
  
  cat <- tolower(cat)

  aqi_cats   <- c("white",
                  "green",
                  "yellow",
                  "orange",
                  "red",
                  "purple")
  
  aqi_cuts   <- c(NA,35,70,120,200,300)
  
  get_conc <- function(x) {
    ifelse(is.null(x), aqi_cuts[1],
           ifelse(is.na(x), aqi_cuts[1],
                  ifelse(x == "green", aqi_cuts[2], 
                         ifelse(x == "yellow", aqi_cuts[3],
                                ifelse(x == "orange", aqi_cuts[4], 
                                       ifelse(x == "red", aqi_cuts[5], 
                                              aqi_cuts[6]))))))
  }
  
  concs <- sapply(cat, FUN = function(x) get_conc(x))
  
  names(concs) <- NULL
  
  return(concs)
}

# AQI category to value
cat2aqimax <- function(cat = NULL) {
  
  cat <- tolower(cat)
  
  aqi_cats   <- c("white",
                  "green",
                  "yellow",
                  "orange",
                  "red",
                  "purple")
  
  aqi_cuts   <- c(NA,50,100,150,200,300)
  
  get_conc <- function(x) {
    ifelse(is.null(x), aqi_cuts[1],
           ifelse(is.na(x), aqi_cuts[1],
                  ifelse(x == "green", aqi_cuts[2], 
                         ifelse(x == "yellow", aqi_cuts[3],
                                ifelse(x == "orange", aqi_cuts[4], 
                                       ifelse(x == "red", aqi_cuts[5], 
                                              aqi_cuts[6]))))))
  }
  
  concs <- sapply(cat, FUN = function(x) get_conc(x))
  
  names(concs) <- NULL
  
  return(concs)
}


# AQI value to AQI color
aqi2color <- function(aqi) {
  
  if(is.null(aqi)) return(NA)
  
  aqi_colors <- c("#FFF",     # White
                  "#53BF33",  #"#9BF59B",  # Green
                  "#FFEE00",  #"#ffff00",  # Yellow 
                  "#DB6B1A",  #"#ff7e00",  # Orange
                  "#ff0000",  # Red 
                  "#99004c")  # Purple
  
  aqi <- as.numeric(aqi)
  
  get_color <- function(aqi) {
    
  ifelse(is.na(aqi), "white",
         ifelse(aqi <= 50,  aqi_colors[2], 
                ifelse(aqi <= 100, aqi_colors[3],
                       ifelse(aqi <= 150, aqi_colors[4], 
                              ifelse(aqi <= 200, aqi_colors[5], 
                                     aqi_colors[6])))))
  }
  
  colors <- sapply(aqi, FUN = function(x) get_color(x))
  
  names(colors) <- NULL
  
  return(colors)
  
}

# AQI value to AQI color index (for plotting)
aqi2colind <- function(aqi) {
  
  if(is.null(aqi)) return(NA)
  
  aqi <- as.numeric(aqi)
  
  get_ind <- function(aqi) {
    
    ifelse(is.na(aqi), 1,
           ifelse(aqi <= 50,  1, 
                  ifelse(aqi <= 100, 2,
                         ifelse(aqi <= 150, 3, 
                                ifelse(aqi <= 200, 4, 
                                       5)))))
  }
  
  ind <- sapply(aqi, FUN = function(x) get_ind(x))
  
  names(ind) <- NULL
  
  return(ind)
  
}


# Concentration to AQI color
conc2color <- function(conc, param) {
  
  conc <- as.numeric(conc)
  
  aqi <- conc2aqi(conc, param)
  
  aqi2color(aqi)
  
}


# Define concentration to AQI function
conc2aqi <- function(conc, param) {
  
  conc <- as.numeric(conc)
  
  if (sum(!is.na(as.numeric(conc))) < 1 | is.na(param)) return(NA)
  
  #-- Clean param name
  param <- tolower(param)
  param[param == "fine particles"] <- "pm25"
  param[param == "pm"]             <- "pm25"
  param <- gsub("[.]", "", param)
  
  #-- Repeat param name if vector shorter than concentration list
  if(length(param) < length(conc)) param <- rep(param[1], length(conc))
  
  #-- Take floor of concentration
  conc <- ifelse(param %in% "pm25", floor(10 * as.numeric(conc)) / 10, floor(as.numeric(conc)))
  
  #-- Loop through concentration vector
  aqi_values <- c()
  
  for(i in 1:length(conc)) {
    
    if(is.na(conc[i]) | is.na(param[i])) {
      
      aqi_values <- c(aqi_values, NA)
      
    } else {
      
      #-- Find relevant break points
      aqi_value <- breaks_aqi[ , c(param[i], "breakpoints", "rating")]
      
      names(aqi_value)[1] <- "conc_cutoffs"
      
      aqi_value <- mutate(aqi_value, 
                          breakpoints  = strsplit(breakpoints, ","),
                          conc_cutoffs = strsplit(conc_cutoffs, ","))
      
      aqi_value <- group_by(aqi_value, rating) %>%
                    mutate(low_break  = as.numeric(unlist(breakpoints)[1]),
                           high_break = as.numeric(unlist(breakpoints)[2]),
                           low_conc   = as.numeric(unlist(conc_cutoffs)[1]),
                           high_conc  = as.numeric(unlist(conc_cutoffs)[2]))
      
      aqi_value <- filter(aqi_value, high_conc >= conc[i])[1, ]
      
      #-- Calculate AQI value
      aqi_values <- c(aqi_values,
                      with(aqi_value, low_break + (conc[i] - low_conc) * (high_break - low_break) / (high_conc - low_conc)))
    }
    
  }
  
  return(round(aqi_values))
  
}

# Define AQI to concentration function
aqi2conc <- function(aqi, param){
  
  aqi <- as.numeric(aqi)
  
  if(sum(!is.na(as.numeric(aqi))) < 1 | is.na(param)) return(NA)
  
  #-- Clean param name
  param <- tolower(param)
  param[param == "fine particles"] <- "pm25"
  param[param == "pm"]             <- "pm25"
  param <- gsub("[.]", "", param)
  
  
  #-- Repeat param name if vector shorter than concentration list
  if (length(param) < length(aqi)) param <- rep(param[1], length(aqi))
  
  #-- Loop through aqi vector
  conc_values <- c()
  
  for (i in 1:length(aqi)) {
    
    if (is.na(aqi[i]) | is.na(param[i])) {
      
      conc_values <- c(conc_values, NA)
      
    } else {
      
      #-- Find relevant break points
      aqi_value <- breaks_aqi[ , c(param[i], "breakpoints", "rating")]
      
      names(aqi_value)[1] <- "conc_cutoffs"
      
      aqi_value <- mutate(aqi_value, 
                          breakpoints = strsplit(breakpoints, ","),
                          conc_cutoffs = strsplit(conc_cutoffs, ","))
      
      aqi_value <- group_by(aqi_value, rating) %>%
                   mutate(low_break  = as.numeric(unlist(breakpoints)[1]),
                          high_break = as.numeric(unlist(breakpoints)[2]),
                          low_conc   = as.numeric(unlist(conc_cutoffs)[1]),
                          high_conc  = as.numeric(unlist(conc_cutoffs)[2]))
      
      conc_value <- filter(aqi_value, high_break >= aqi[i])[1, ]
      
      conc_values <- c(conc_values, 
                       with(conc_value, low_conc + (aqi[i] - low_break) * (high_conc - low_conc) / (high_break - low_break)))
    }
  }
  
  return(round(conc_values, 1))
}

##
