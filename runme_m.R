run_me_m <- function(gauge,date,add_data=F) {
  date <- paste0(substr(date,1,7),"-01")
  setmonth_num <- which(seq(as.Date("1981-01-01"),Sys.Date(),"month")  %in% as.Date(date))
  
  # Calculando  R_CHIRP si es que no existe || busca dentro database_directory (definelo en setup_m.R)
  listCHIRP <- sprintf("%s/inputs/chirps/chirps_m_0.05/CHIRPm.%s.tif",database_directory,date)
  chirpx <- try(raster(listCHIRP))
  if (class(chirpx) == "try-error") {
    message(sprintf("Descargando CHIRP %s",date))
    chirpx <- download_CHIRPm(date)
    chirpx <- crop(chirpx,base)
    chirpx[chirpx<0]=0
    Mname <- sprintf("%s/inputs/chirps/chirps_m_0.05/CHIRPm.%s.tif",database_directory,date)
    writeRaster(chirpx,Mname,overwrite=T)
    message("Done!")  
  }
  
  chirpx_R <- resampleR(R1 = chirpx,R2 = base)*base
  chirpx_R[chirpx_R<0]=0
  message("Creando CHIRPMm")
  Mname <- sprintf("%s/inputs/chirps/chirps_m_0.1/CHIRPm.%s.tif",database_directory,date)
  writeRaster(chirpx_R,Mname,overwrite=T)
  CHIRPMm <<- CHIRPm_correction(CHIRPm = chirpx_R,
                                ratios = ratio_Pm,
                                date = date)
  CHIRPMm[CHIRPMm<0] = 0
  CHIRPMm_name <- sprintf("%s/PISCOpV2.1/Monthly_Products/unstable/CHIRPMm/CHIRPMm.%s.tif",database_directory,date)
  writeRaster(CHIRPMm,CHIRPMm_name,overwrite=T)
  
  if (add_data == T) {
    message("Completando los datos faltantes .... Espere")
    names(gauge) <- "pp"
    new_base_m <- add_monthly_base(monthlypp = gauge,month_date = date)
  } else {
    new_base_m <- PISCOp_raingauge_database
  }
  
  gauge_sp <- new_base_m$spatial
  data_month_f <- new_base_m$unstable$plausible[setmonth_num,] %>% as.numeric
  gauge_sp$gauge <- data_month_f
  gauge_sp <- gauge_sp[2]
  gauge_f <- mean_double_Station(gauge_sp,CHIRPMm,longlat = T) #eliminar det 0
  CHIRPMm_log <- log1p(CHIRPMm)
  names(CHIRPMm_log) <- "sat"
  names(gauge_f) <- "gauge"
  gauge_f$gauge <- log1p(gauge_f$gauge)
  names(gauge_f) <- names(chirpx)
  
  kd <- ROK(gauge = gauge_f,cov = CHIRPMm_log)
  
  if(sum(getValues(kd)>10000,na.rm = T) > 0) {
    kd <- RIDW(gauge = sp_piscop_f,cov = CHIRPMm_log,formula = as.formula(sprintf("month%05d ~ 1",i)))  
    kd <- expm1(kd)
  }
  
  pbias_p <- pbias(raster::extract(kd,gauge),gauge[[1]])
  cor_p <- cor(raster::extract(kd,gauge),gauge[[1]],use = "pairwise.complete.obs")
  message(sprintf("PBIAS: %s && PEARSON: %s",pbias_p,round(cor_p,2)))
  if (pbias_p < -50 || pbias_p > 50 || cor_p < 0.7) stop("Alta disparidad en las predicciones espaciales finales...")
  else writeRaster(kd,sprintf("%s/PISCOpV2.1/Monthly_Products/unstable/PISCOpm/PISCOpm_V2.1.%s.tif",database_directory,date),overwrite=T)
  if (add_data) {
    save(PISCOp_raingauge_database,file = sprintf("%s/base-inputs/PISCOp_raingauge_database_backup_%s.Rdata",database_directory,Sys.time())) #create backup
    PISCOp_raingauge_database <- new_base_m
    file.remove(sprintf("%s/PISCOpV2.1/PISCOp_raingauge_database.Rdata",database_directory))
    message("data guardada ...")
    save(PISCOp_raingauge_database,file=sprintf("%s/PISCOpV2.1/PISCOp_raingauge_database.Rdata",database_directory))
    name <- colnames(new_base_m$unstable$CUTOFF$monthly)[1:441]
    coord_gauge <- data.frame(name,coordinates(new_base_m$spatial[1:441,])) %>% tbl_df
    
    #COMPLETE DATA
    data_join2 <- cbind(coord_gauge,(new_base_m$unstable$plausible[1:441] %>% t%>% tbl_df)) %>% tbl_df
    colnames(data_join2) <- c("name","x","y",substr(seq(as.Date("1981-01-01"),as.Date(date),"month"),1,7))
    write.csv(data_join2,sprintf("%s/PISCOpV2.1/Monthly_Products/unstable/Unstable_Monthly_gaugecomplete_Dataset.csv",database_directory),row.names = F)
    
    #RAW DATA
    data_join2 <- cbind(coord_gauge,(new_base_m$unstable$brute$monthly[1:441] %>% t%>% tbl_df)) %>% tbl_df
    colnames(data_join2) <- c("name","x","y",substr(seq(as.Date("1981-01-01"),as.Date(date),"month"),1,7))
    write.csv(data_join2,sprintf("%s/PISCOpV2.1/Monthly_Products/unstable/Unstable_Monthly_gaugeraw_Dataset.csv",database_directory),row.names = F)
  }
   kd
}
