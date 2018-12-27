download_CHIRPm<-function(date,
                          set_dir=getwd(),
                          BBlonMin = -86,
                          BBlonMax = -66,
                          BBlatMin = -19.25,
                          BBlatMax = 1.25
){
  ftp <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/monthly/"  #ftp data
  filenames <- getURL(ftp, ftp.use.epsv = FALSE, ftplistonly = TRUE, 
                      crlf = TRUE)
  filePaths <- paste(ftp, strsplit(filenames, "\r*\n")[[1]], sep = "")  #obsolete
  filePaths <- grep("\\.tif$", filePaths, value = T)  #obsolete
  fechas <- sprintf("%s.01", substr(basename(filePaths), 7, 13)) %>% 
    as.Date(., format = "%Y.%m.%d")  #obsolete
  selects <- fechas %in% (date %>% as.Date) %>% which
  selectedfilePaths <- filePaths[selects]
  nameT<-sprintf("%s/%s",set_dir,basename(selectedfilePaths))
  download.file(selectedfilePaths,nameT,mode="wb")
  chirp_crop <- crop(raster(nameT),c(BBlonMin, BBlonMax, BBlatMin, BBlatMax) )
  writeRaster(chirp_crop, nameT, overwrite = T)
  chirp_crop
}



download_CHIRPd<-function(date,
                          set_dir = sprintf("%s/inputs/chirps/chirps_d_0.05",database_directory),
                          BBlonMin = -86,
                          BBlonMax = -66,
                          BBlatMin = -19.25,
                          BBlatMax = 1.25
){
  ftp <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05"  #ftp data
  ftp2 <- sprintf("%s/%s/",ftp,lubridate::year(date))[1]
  filenames <- getURL(ftp2, ftp.use.epsv = FALSE, ftplistonly = TRUE, 
                      crlf = TRUE)
  filePaths <- paste(ftp2, strsplit(filenames, "\r*\n")[[1]], sep = "")  #obsolete
  filePaths <- grep("\\.gz$", filePaths, value = T)  #obsolete
  fechas <- substr(basename(filePaths), 13, 22) %>% 
    as.Date(., format = "%Y.%m.%d")  #obsolete
  selects <- fechas %in% (date %>% as.Date) %>% which
  selectedfilePaths <- filePaths[selects]
  nameT <- sprintf("%s/%s",set_dir,sprintf("CHIRPS.%s.tif.gz",fechas[selects]))
  nameT2 <- gsub("\\.gz","",nameT)
  if(sum(file.exists(nameT2))>0) file.remove(nameT2)
  mapply(function(i){
    download.file(selectedfilePaths[i],nameT[i],mode="wb")
    gunzip(nameT[i])
    chirp_crop <- crop(raster(nameT2[i]),c(BBlonMin, BBlonMax, BBlatMin, BBlatMax) )
    chirp_crop[chirp_crop<0]=0
    writeRaster(chirp_crop, nameT2[i], overwrite = T)  
  }, 1:length(selectedfilePaths))
}
