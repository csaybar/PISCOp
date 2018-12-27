run_me_d <- function(gauge,date,add_data=F) {
  # setmonth_num numero del mes empezando desde 81
  date <- paste0(substr(date,1,7),"-01")
  setmonth_num <- which(seq(as.Date("1981-01-01"),Sys.Date(),"month")  %in% as.Date(date))
  initDay <- sprintf("%s-01",substr(date,1,7)) %>% as.Date
  finalDay <- sprintf("%s-01",substr(date,1,7)) %>% as.Date + monthDays(date) - 1
  serieDay <- seq.Date(initDay,finalDay,"day") #fechas de datos a generar
  setday_num <- which(seq(as.Date("1981-01-01"),Sys.Date(),"day")  %in% serieDay) #posición tomando como base 1981-01-01
  
  listCHIRP <- sprintf("%s/inputs/chirps/chirps_d_0.05/CHIRPS.%s.tif",database_directory,serieDay)
  chirpx <- try(stack(listCHIRP))
  if (class(chirpx)=="try-error") {
    message(sprintf("Descargando CHIRP del %s al %s",initDay,finalDay))
    chirpx <- download_CHIRPd(date = serieDay)
    chirpx <- stack(listCHIRP)
  }
  # Calculando  R_CHIRP si es que no existe

  listCHIRPd_01 <- sprintf("%s/inputs/chirps/chirps_d_0.1/CHIRPS.%s.tif",database_directory,serieDay)
  listCHIRPMd <- sprintf("%s/PISCOpV2.1/Daily_Products/unstable/CHIRPMd/CHIRPMd.%s.tif",database_directory,serieDay)
  
  chirpx_R_stk <- try(stack(listCHIRP))
  chirpMdx_R_stk <- try(stack(listCHIRPMd))
  if(class(chirpx_R_stk)=="try-error"||class(chirpMdx_R_stk)=="try-error"){
    message("Creando CHIRPMd")
    for(i in 1:length(serieDay)) {
      chirpx_R<-resampleR(R1 = chirpx[[i]]*1,R2 = base)
      chirpx_R[chirpx_R<0]=0
      Dname <- sprintf("%s/inputs/chirps/chirps_d_0.1/CHIRPS.%s.tif",database_directory,serieDay[i])
      writeRaster(chirpx_R,Dname,overwrite=T)
      CHIRPMd <- CHIRPm_correction(CHIRPm = chirpx_R,
                                   ratios = ratio_Pd,
                                   date = serieDay[i])
    CHIRPMd[CHIRPMd<0]=0
    CHIRPMd_name <- sprintf("%s/PISCOpV2.1/Daily_Products/unstable/CHIRPMd/CHIRPMd.%s.tif",database_directory,serieDay[i])
    writeRaster(CHIRPMd,CHIRPMd_name,overwrite=T)
    }
  }
  
  if (add_data == T) {
    new_base_m <- add_daily_base(dailypp = gauge,day_date =  serieDay)  
  } else { 
    new_base_m <- PISCOp_raingauge_database
  }
  
  chirpMm_lista <- list.files(sprintf("%s/PISCOpV2.1/Daily_Products/unstable/CHIRPMd/",database_directory),"\\.tif$",full.names = T)
  #for(i in 1:length(setday_num)) {
  
  foreach(i=1:length(setday_num)) %dopar% {
    gauge_sp <- new_base_m$spatial[1:441,]
    gauge_sp$gauge <- as.numeric(new_base_m$unstable$Qm$daily[setday_num[i],])[1:441]
    CHIRPMd <- raster(grep(serieDay[i],chirpMm_lista,value = T))
    namesat <- names(CHIRPMd)
    names(CHIRPMd) <- "sat"
    gauge_sp <- gauge_sp[2]
    gauge_f <- mean_double_Station(gauge_sp,CHIRPMd,longlat = T) #eliminar det 0
    names(gauge_f) <- namesat
    kd <- RIDW(gauge = gauge_f,cov = CHIRPMd,formula = as.formula(paste0(names(gauge_f),"~sat")))
    kd[kd<0]=0
    writeRaster(kd,sprintf("%s/PISCOpV2.1/Daily_Products/unstable/P-PISCOpd/P-PISCOpd_V2.1.%s.tif",database_directory,serieDay[i]),overwrite=T)
  }
  
  ll_m_total <- list.files(sprintf("%s/PISCOpV2.1/Monthly_Products/unstable/PISCOpm/",database_directory),"\\.tif$",full.names = T)
  ll_d_total <- list.files(sprintf("%s/PISCOpV2.1/Daily_Products/unstable/P-PISCOpd/",database_directory),"\\.tif$",full.names = T)
  l_d_total_select <- ll_d_total[setday_num] %>% stack 
  l_d_total_sum <- l_d_total_select %>% sum
  l_m_total_select <- ll_m_total[setmonth_num] %>% raster
  ratio_C <- (l_m_total_select+1)/(l_d_total_sum+1)
  PISCOpd <- l_d_total_select*ratio_C
  mapply(function(i) writeRaster(PISCOpd[[i]],
    sprintf("%s/PISCOpV2.1/Daily_Products/unstable/PISCOpd/PISCOpd_V2.1.%s.tif",database_directory,serieDay[i]),overwrite=T),1:length(serieDay))
  if (add_data) {
    save(PISCOp_raingauge_database,file = sprintf("%s/base-inputs/PISCOp_raingauge_database_backup_%s.Rdata",database_directory,Sys.time())) #create backup
    PISCOp_raingauge_database <- new_base_m
    save(PISCOp_raingauge_database,file=sprintf("%s/PISCOpV2.1/PISCOp_raingauge_database.Rdata",database_directory))
    name <- colnames(new_base_m$unstable$CUTOFF$daily)[1:441]
    coord_gauge <- data.frame(name,coordinates(new_base_m$spatial[1:441,])) %>% tbl_df
    
    #COMPLETE DATA
    data_join2 <- cbind(coord_gauge,(new_base_m$unstable$Qm$daily[1:441] %>% t%>% tbl_df)) %>% tbl_df
    colnames(data_join2) <- c("name","x","y",substr(seq(as.Date("1981-01-01"),as.Date(date),"month"),1,7))
    write.csv(data_join2,sprintf("%s/PISCOpV2.1/Daily_Products/unstable/Unstable_Daily_gaugecomplete_Dataset.csv",database_directory),row.names = F)
    
    #RAW DATA
    data_join2 <- cbind(coord_gauge,(new_base_m$unstable$brute$daily[1:441] %>% t%>% tbl_df)) %>% tbl_df
    colnames(data_join2) <- c("name","x","y",substr(seq(as.Date("1981-01-01"),as.Date(date),"month"),1,7))
    write.csv(data_join2,sprintf("%s/PISCOpV2.1/Daily_Products/unstable/Unstable_Daily_gaugeraw_Dataset.csv",database_directory),row.names = F)
  }
  PISCOpd 
}
