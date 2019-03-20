# Define concentration to AQI function

conc2aqi <- function(conc, param) {
  
  if(is.na(conc) || is.na(param)) return(NA)
  
#  conc      <- floor(conc)
  
  aqi_value <- breaks_aqi[ , c(param, "Breakpoints", "Rating")]
  
  names(aqi_value)[1] <- "Conc_cutoffs"
  
  aqi_value <- mutate(aqi_value, 
                      Breakpoints  = str_split(Breakpoints, ","),
                      Conc_cutoffs = str_split(Conc_cutoffs, ","))
  
  aqi_value <- group_by(aqi_value, Rating) %>%
    mutate(Low_break  = as.numeric(unlist(Breakpoints)[1]),
           High_break = as.numeric(unlist(Breakpoints)[2]),
           Low_conc   = as.numeric(unlist(Conc_cutoffs)[1]),
           High_conc  = as.numeric(unlist(Conc_cutoffs)[2]))
  
  aqi_value <- filter(aqi_value, High_conc >= conc)[1, ]
  
  aqi_value <- with(aqi_value, 
                    Low_break + (conc - Low_conc) * (High_break - Low_break)/(High_conc - Low_conc))
  
  return(round(aqi_value))
  
}