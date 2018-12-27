CHIRPm_correction <-function(CHIRPm,ratios,date) { 
  coR <- ratios[[lubridate::month(date)]]
  return(CHIRPm*coR)
}

CHIRPd_correction <-function(CHIRPd,ratios,date) { 
  coR <- ratios[[lubridate::month(date)]]
  plot(coR)
  dayR <- list.files(ratios,"\\.tif$",full.names = T)
  dated <- strptime(dayR %>% basename, "ratio.%Y-%m-%d.tif")
  date_modified <- sprintf("%s-01",substr(date,1,7)) %>% as.Date
  positionR <- as.Date(dated)  %in% date_modified %>% which
  coR <- raster(dayR[positionR])*CHIRPd
  return(coR)
}