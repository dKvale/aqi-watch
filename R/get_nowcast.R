# NowCast function

get_nowcast <- function(x = data) {
  
  x[x == "" | x == " "] <- NA
  
  x[x < 0 & !is.na(x)] <- 0
  
  x <- as.numeric(x)
  
  is_value <- !is.na(x) 
  
  if(sum(is_value[1:3], na.rm = T) < 2) return(NA)
  
  a_range  <- max(x, na.rm=T) - min(x, na.rm=T)
  
  a_max    <- max(x, na.rm=T)
  
  a_factor <- 1 - (a_range / a_max)
  
  a_factor <- max(0.5, a_factor, na.rm = T)
  
  wtd_sum  <- sum(x * a_factor**(0:11), na.rm = T) / sum((a_factor**(0:11)) * is_value, na.rm = T)
  
  # Test
  #test_x <- rev(c(50,80,75,90,82,53,64,74,21,10,16,13))
  #NowCast <- 17.4
  #invisible(print(wtd_sum))
}