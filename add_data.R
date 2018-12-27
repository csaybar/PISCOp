add_daily_base <- function(dailypp,day_date){
  # initDay <- sprintf("%s-01",substr(date,1,7)) %>% as.Date
  # finalDay <- sprintf("%s-01",substr(date,1,7)) %>% as.Date + monthDays(date) - 1
  # serieDay <- seq.Date(initDay,finalDay,"day")
  serieDay <- day_date
  setday_num <- which(seq(as.Date("1981-01-01"),Sys.Date(),"day")  %in% serieDay)
  
  dailypp2 <- dailypp
  dailypp <- dailypp@data %>% t
  
  # 1._ Generar la base bruta -----------------------------------------------
  Sofia_Stations <- collocated_daily$collocated_Sofia
  Piscop_Stations <- collocated_daily$collocated_PISCOp
  Piscopd <- PISCOp_raingauge_database$unstable$brute$daily %>% as.matrix
  
  if(nrow(Piscopd) <= max(setday_num)){
    lost_row <- (setday_num-nrow(Piscopd)) %>% length
    Piscopd <- rbind(Piscopd,matrix(NA,lost_row,496)) 
  }
  
  for (i in 1:length(setday_num)) {
    Piscopd[setday_num[i],Piscop_Stations] <- dailypp[i,Sofia_Stations] %>% as.numeric  
  }

  spp <- PISCOp_raingauge_database$spatial
  PISCOp_raingauge_database$unstable$brute$daily <- Piscopd %>% tbl_df
  message("Base bruta diaria ... OK")
  
  # 2._ Generar la base CUTOFF -----------------------------------------------
  
  PISCOpd_CUTOFF <- PISCOp_raingauge_database$unstable$CUTOFF$daily %>% as.matrix
  if(nrow(PISCOpd_CUTOFF) <= max(setday_num)){
    lost_row <- (setday_num-nrow(PISCOpd_CUTOFF)) %>% length
    PISCOpd_CUTOFF <- rbind(PISCOpd_CUTOFF,matrix(NA,lost_row,496)) 
  }  
  
  for(i in 1:length(setday_num)) { 
    serie <- PISCOp_raingauge_database$unstable$brute$daily[setday_num[i],] %>% as.numeric()
    fill_serie_d_CUTOFF <- mapply(fill_serie_day,1:496,MoreArgs = list(date=serieDay,serie=serie,CUTOFFR_PISCOp=CUTOFFR_PISCOp_day))
    fill_serie_d_CUTOFF[is.nan(fill_serie_d_CUTOFF)]=NA
    PISCOpd_CUTOFF[setday_num[i],] <- fill_serie_d_CUTOFF
  }
  
  PISCOp_raingauge_database$unstable$CUTOFF$daily <- PISCOpd_CUTOFF %>% tbl_df
  message("Base CUTOFF diaria ... OK")
  

  # 3._ Generar la base Qm -----------------------------------------------
  listCHIRPMd <- list.files(sprintf("%s/inputs/chirps/chirps_d_0.1/",database_directory),"\\.tif$",full.names = T)
  R_d <- stack(listCHIRPMd[setday_num]) %>% brick
  sp <- PISCOp_raingauge_database$spatial
  values_d <- raster::extract(R_d,sp) %>% t %>% tbl_df
  
  
  list_daily_Qm2<-list()
  for (j in 1:length(setday_num)) {
    list_daily_Qm<-list()
    for(i in 1:496){ 
      sx = PISCOp_raingauge_database$unstable$CUTOFF$daily[[i]][setday_num[j]]
      list_daily_Qm[[i]] <- complete_qm_daily_data(i,serie_na = sx,Qm_model = Qm_model,values_d = values_d[j,])
    }
    serie_daily_Serially_complete <- do.call("c",list_daily_Qm)
    list_daily_Qm2[[j]] <- serie_daily_Serially_complete
  }
  
  serie_daily_Serially_complete2 <- do.call("rbind",list_daily_Qm2)
  PISCOpd_Qm <- PISCOp_raingauge_database$unstable$Qm$daily %>% as.matrix
  
  if(nrow(PISCOpd_Qm) <= max(setday_num)){
    lost_row <- (setday_num-nrow(PISCOpd_CUTOFF)) %>% length
    PISCOpd_Qm <- rbind(PISCOpd_Qm,matrix(NA,lost_row,496)) 
  }
  
  PISCOpd_Qm[setday_num,] <- serie_daily_Serially_complete2
  PISCOp_raingauge_database$unstable$Qm$daily <- PISCOpd_Qm %>% tbl_df
  message("Base Qm diaria ... OK")
  PISCOp_raingauge_database
}

plausibility_checks <- function(station,station_comp) { 
  val_1<-station
  val_comp<-station_comp
  min_dif <- min(abs(as.numeric(val_comp) - as.numeric(val_1)))
  max_valu <- max(c(as.numeric(val_comp)))
  indicator <- (min_dif+1)/(max_valu+1)
  if (indicator>2)  new_value <- mean(station_comp)
  else new_value <- val_1
  new_value
}

add_monthly_base <-  function(monthlypp,month_date) {
  month_date <- paste0(substr(month_date,1,7),"-01")
  setmonth_num <- which(seq(as.Date("1981-01-01"),Sys.Date(),"month")  %in% as.Date(month_date))
  # 1._ Generar la base bruta -----------------------------------------------
  Sofia_Stations <- collocated_monthly$collocated_Sofia
  Piscop_Stations <- collocated_monthly$collocated_PISCOp
  Piscopm <- PISCOp_raingauge_database$unstable$brute$monthly %>% as.matrix
  
  if(nrow(Piscopm) <= setmonth_num){
    lost_row <- setmonth_num-nrow(Piscopm)
    Piscopm <- rbind(Piscopm,matrix(NA,lost_row,496)) 
  }
  Piscopm[setmonth_num,Piscop_Stations] <- monthlypp$pp[Sofia_Stations]
  spp <- PISCOp_raingauge_database$spatial
  PISCOp_raingauge_database$unstable$brute$monthly <- Piscopm %>% tbl_df()
  message("Base bruta mensual ... OK")
  
  # 2._ Generar la base CUTOFF -----------------------------------------------
  PISCOpm_CUTOFF <- PISCOp_raingauge_database$unstable$CUTOFF$monthly %>% as.matrix
  serie <- PISCOp_raingauge_database$unstable$brute$monthly[setmonth_num,] %>% as.numeric()
  fill_serie_m_CUTOFF <- mapply(fill_serie_month,1:496,MoreArgs = list(date=month_date,serie=serie,CUTOFFR_PISCOp=CUTOFFR_PISCOp))
  fill_serie_m_CUTOFF[is.nan(fill_serie_m_CUTOFF)]=NA
  if(nrow(PISCOpm_CUTOFF) <= setmonth_num){
    lost_row <- setmonth_num-nrow(PISCOpm_CUTOFF)
    PISCOpm_CUTOFF <- rbind(PISCOpm_CUTOFF,matrix(NA,lost_row,496)) 
  }  
  PISCOpm_CUTOFF[setmonth_num,] <- fill_serie_m_CUTOFF
  PISCOp_raingauge_database$unstable$CUTOFF$monthly <- PISCOpm_CUTOFF %>% tbl_df
  message("Base CUTOFF mensual ... OK")
  
  # 3._ Generar la base Qm -----------------------------------------------
  list_chirp <- list.files(sprintf("%s/inputs/chirps/chirps_m_0.05/",database_directory),"\\.tif$",full.names = T)
  R_m <- raster(list_chirp[setmonth_num])
  sp <- PISCOp_raingauge_database$spatial
  values_m <- raster::extract(R_m,sp) %>% t %>% tbl_df
  
  
  list_monthly_Qm<-list()
  for(i in 1:496){ 
    sx = PISCOp_raingauge_database$unstable$CUTOFF$monthly[[i]][setmonth_num]
    list_monthly_Qm[[i]] <- complete_qm_monthly_data(i,sx,Qm_model,values_m)
  }
  serie_monthly_Serially_complete<-do.call("c",list_monthly_Qm)
  PISCOpm_QM <- PISCOp_raingauge_database$unstable$Qm$monthly %>% as.matrix  
  if(nrow(PISCOpm_QM) <= setmonth_num){
    lost_row <- setmonth_num-nrow(PISCOpm_QM)
    PISCOpm_QM <- rbind(PISCOpm_QM,matrix(NA,lost_row,496)) 
  } 
  PISCOpm_QM[setmonth_num,] <- serie_monthly_Serially_complete
  PISCOp_raingauge_database$unstable$Qm$monthly <- PISCOpm_QM %>% tbl_df
  PISCOpm_QM <- PISCOpm_QM %>% tbl_df
  message("Base Qm mensual ... OK")
  
  ###Plausible checking
  plausible_serie <- rep(NA,496)
  for(i in 1:496){
    if (is.na(new_groups[[i]])|| sum(new_groups[[i]])==0||i==293||i==400||i==151||i==290||i==149||i==397) {
      plausible_serie[i] <- PISCOp_raingauge_database$unstable$Qm$monthly[[i]][setmonth_num]
    } else { 
      station <- PISCOp_raingauge_database$unstable$Qm$monthly[i][setmonth_num,] %>% as.numeric
      station_comp <- PISCOp_raingauge_database$unstable$Qm$monthly[new_groups[[i]]][setmonth_num,] %>% as.numeric
      plausible_serie[i] <- plausibility_checks(station,station_comp)
    }
  }
  plausible <- PISCOp_raingauge_database$unstable$plausible %>% as.matrix
  if(nrow(plausible) <= setmonth_num){
    lost_row <- setmonth_num-nrow(plausible)
    plausible <- rbind(plausible,matrix(NA,lost_row,496)) 
  } 
  plausible[setmonth_num,] <- plausible_serie    
  PISCOp_raingauge_database$unstable$plausible <- plausible %>% tbl_df
  message("Base Plausible mensual  ... OK")  
  PISCOp_raingauge_database
}