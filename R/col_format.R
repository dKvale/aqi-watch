# Color formatting for AQI table

col_format <- function(i) {
  
  formatter("span", 
            style = x ~ style("padding-left"   = "12px",
                              "padding-right"  = "12px",
                              "padding-top"    = "4px",
                              "padding-bottom" = "4px",
                              #"margin-right"  = paste0(i, "px"),
                              "font-weight"   = 400,
                              "background-color" = 
                                ifelse(x == " " | is.na(x), NULL,
                                       ifelse(as.numeric(x) <= 50,  aqi_colors[2], 
                                              ifelse(as.numeric(x) <= 100, aqi_colors[3],
                                                     ifelse(as.numeric(x) <= 150, aqi_colors[4], 
                                                            ifelse(as.numeric(x) <= 500, aqi_colors[5], NULL)))))))
}