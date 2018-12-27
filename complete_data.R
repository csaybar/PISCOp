library(xts)
library(qmap)

cutoff_cesar<- function(val,cutoff_group,serie, Rm, Cm, date){
  monthx <- lubridate::month(date)
  #si no tiene grupo y el valor es un NA retorna NA 
  if((length(cutoff_group)==0)&&is.na(val)) NA
  else { 
    if (is.na(val)) {
      R <- serie[cutoff_group] %>% mean(.,na.rm=T)
      na_position <- serie[cutoff_group] %>% is.na 
      na_position <- (!na_position) %>% which
      R_m <-Rm[-1][na_position][monthx,] %>% as.numeric %>% mean(.,na.rm=T)
      C_m <- Cm[monthx,2] %>% as.numeric
      return(R*(C_m/R_m))
    } else {
      #si el valor no es NA retorna el mismo valor
      return(val)
    }
  }
}

#' fill_serie_month: Es el iterador mensual.
fill_serie_month <- function(i,date,serie,CUTOFFR_PISCOp) {
  val<-serie[i]
  cutoff_group <- CUTOFFR_PISCOp$group[[i]]
  Rm <- CUTOFFR_PISCOp$R[[i]]
  Cm  <- CUTOFFR_PISCOp$C[[i]]
  cutoff_cesar(val,cutoff_group,serie,Rm,Cm,date)
}

#' fill_serie_day: Es el iterador diario.
fill_serie_day <- function(i,date,serie,CUTOFFR_PISCOp) {
  val<-serie[i]
  cutoff_group <- CUTOFFR_PISCOp_day$group[[i]]
  Rm <- CUTOFFR_PISCOp_day$R[[i]]
  Cm  <- CUTOFFR_PISCOp_day$C[[i]]
  cutoff_cesar(val,cutoff_group,serie,Rm,Cm,date = date[i])
}


#' qmap_get: Permite generar el BIAS CORRECTED CHIRP
#'               - sat: Serie de tiempo de CHIRP.
#'               - model: Modelo de ajuste del bias (obtenido mediante qmap_set)
qmap_get_m <- function(sat,model) {
  doQmapQUANT(sat, model, type="tricub") %>% as.numeric() %>% round(., 2)
}

qmap_get_d <- function(sat,model,date,i) {
    Ser_Sat <- try(doQmapQUANT(sat, model$case2, type="tricub") %>% as.numeric() %>% round(., 2))
    if(class(Ser_Sat)=="try-error") {
      spselect<-sapply(1:length(CUTOFFR_PISCOp_day$group[[i]]),function(j) cor(PISCOp_raingauge_database$stable$Qm$daily[[i]],PISCOp_raingauge_database$stable$Qm$daily[CUTOFFR_PISCOp_day$group[[i]]][[j]]))
      optional<-CUTOFFR_PISCOp_day$group[[i]][which.max(spselect)]
      Ser_Sat <- doQmapQUANT(sat, Qm_model$daily_model[[optional]]$case2, type="tricub") %>% as.numeric() %>% round(., 2)
    }
    if (length(Ser_Sat %in% modal(Ser_Sat))>100) {
      Ser_Sat <- Ser_Sat-modal(Ser_Sat)*1.5
      Ser_Sat[Ser_Sat<0]=0
    }
    Ser_Sat
}
#' qmap_get: Permite generar el modelo de corrección del bias
#'               - obs_ts: Serie de tiempo del pluviometro.
#'               - gridded_ts: Serie de tiempo del CHIRP.
#'               - model: Modelo de ajuste del bias (obtenido mediante qmap_set)
qmap_set_m <- function(obs_ts, gridded_ts,date) {
  obs_ts<-xts(obs_ts,date)
  gridded_ts<-xts(gridded_ts,date)
  qm_fit <- fitQmapQUANT(coredata(obs_ts), 
                         coredata(gridded_ts) ,
                         qstep = 0.01, nboot = 1, wet.day = T, type = "tricube")
  qm_fit
}


qmap_set_d <- function(obs_ts, gridded_ts,date) {
  obs_ts<-xts(obs_ts,date)
  gridded_ts<-xts(gridded_ts,date)
  groups<- list(c(12,1,2),c(3,4,5),c(6,7,8),c(9,10,11))
  qm_model_stat <- function(j){
    obs_d_select <- obs_ts[month(obs_ts) %in%  groups[[j]] %>% which]
    plot(obs_d_select)
    gridded_d_select <- gridded_ts[month(gridded_ts) %in%  j %>% which]
    qm_fit <- fitQmapQUANT(coredata(obs_d_select), 
                           coredata(gridded_d_select) ,
                           qstep = 0.01, nboot = 1, wet.day = T, type = "tricube")
    qm_fit
  }
  case1 <- lapply(1:4, qm_model_stat)
  qm_fit <- fitQmapQUANT(coredata(obs_ts), 
                         coredata(gridded_ts) ,
                         qstep = 0.01, nboot = 1, wet.day = T, type = "tricube")
  list(case1=case1,case2=qm_fit)
}


# 
#   special_case <- stable$brute$daily[[228]]  %>% tbl_df %>% mutate(date=substr(sqD,1,7))
#   special_caseM <- special_case %>% group_by(date) %>% summarise_all(funs(sum(.,na.rm=T)))
#   special_caseMd <- c(special_caseM[[2]][1:100],rep(NA,332))
#   stable$brute$monthly[[228]] <- special_caseMd
#   stable$CUTOFF$monthly[[228]] <- stable$brute$monthly[[228]]
# 
#   special_case <- stable$brute$daily[[230]]  %>% tbl_df %>% mutate(date=substr(sqD,1,7))
#   special_caseM <- special_case %>% group_by(date) %>% summarise_all(funs(sum(.,na.rm=T)))
#   special_caseMd <- c(rep(NA,249),special_caseM[[2]][250:360],rep(NA,72))
#   stable$brute$monthly[[230]] <- special_caseMd
#   stable$CUTOFF$monthly[[230]] <-  stable$brute$monthly[[230]]

#' fill_serie_month: Es el iterador mensual
qmap_set_iterator_m <- function(i) { 
  gridded_ts <- values_m[[i]]
  obs_ts <- stable$brute$monthly[[i]]
  qmap_set_m(obs_ts, gridded_ts, sqM)
}

#' fill_serie_day: Es el iterador diario.
qmap_set_iterator_d <- function(i) { 
  gridded_ts <- values_d[[i]]
  obs_ts <- stable$brute$daily[[i]]
  qmap_set_d(obs_ts, gridded_ts, sqD)
}

complete_qm_monthly_data <- function(i,serie_na,Qm_model,values_m) { 
  serie_na <- serie_na
  lost_data<-(is.na(serie_na)) %>% which
  new_value <- qmap_get_m(sat = values_m[[i]],model = Qm_model$monthly_model[[i]])
  serie_na[lost_data] <- new_value[lost_data]
  serie_na
}


complete_qm_daily_data <- function(i,serie_na,Qm_model,values_d) { 
  #serie_na <- NA
  lost_data <- (is.na(serie_na)) %>% which
  if (length(lost_data)==0) serie_na
  else { 
    new_value <- qmap_get_d(sat = values_d[[i]],model = Qm_model$daily_model[[i]],date=date,i=i)
    serie_na[lost_data] <- new_value[lost_data]
    serie_na
  }
}
