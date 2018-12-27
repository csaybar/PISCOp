library(raster)
library(dplyr)
library(RCurl)
library(lubridate)
library(stringr)

refresh_ftp = function(){
  ftp = "ftp://publi_dgh2:123456@ftp.senamhi.gob.pe/PISCOp_V2.1_beta/"
  
  #Copy the R.data
  
  name = "/home/senamhi-cesar/PISCO/PISCOpV2.1/PISCOp_raingauge_database.Rdata"
  ftp_name = sprintf("%s/%s",ftp,basename(name))
  ftpUpload(name,ftp_name)
  
  # Copy monthly
  
  name_com = "/home/senamhi-cesar/PISCO/PISCOpV2.1/Monthly_Products/unstable/Unstable_Monthly_gaugecomplete_Dataset.csv"
  ftp_name_com = sprintf("%s/Monthly_Products/unstable/%s",ftp,basename(name_com))
  ftpUpload(name_com,ftp_name_com)
  
  name_raw = "/home/senamhi-cesar/PISCO/PISCOpV2.1/Monthly_Products/unstable/Unstable_Monthly_gaugeraw_Dataset.csv"
  ftp_name_raw = sprintf("%s/Monthly_Products/unstable/%s",ftp,basename(name_raw))
  ftpUpload(name_raw,ftp_name_raw)
  
  # Copy daily  
  name_com = "/home/senamhi-cesar/PISCO/PISCOpV2.1/Daily_Products/unstable/Unstable_Daily_gaugecomplete_Dataset.csv"
  ftp_name_com = sprintf("%s/Daily_Products/unstable/%s",ftp,basename(name_com))
  ftpUpload(name_com,ftp_name_com)
  
  name_raw = "/home/senamhi-cesar/PISCO/PISCOpV2.1/Daily_Products/unstable/Unstable_Daily_gaugeraw_Dataset.csv"
  ftp_name_raw = sprintf("%s/Daily_Products/unstable/%s",ftp,basename(name_raw))
  ftpUpload(name_raw,ftp_name_raw)
  
  ### CHIRPMm
  CHIRPm = "/home/senamhi-cesar/PISCO/PISCOpV2.1/Monthly_Products/unstable/CHIRPMm"
  dd = list.files(CHIRPm,full.names = T)
  dates_home = gsub("CHIRPMm.","",basename(dd)) %>% gsub(".tif","",.) %>% as.Date()
  
  chirpMm = "ftp://publi_dgh2:123456@ftp.senamhi.gob.pe/PISCOp_V2.1_beta/Monthly_Products/unstable/CHIRPMm/"
  filenames <- getURL(chirpMm, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE)
  filePaths <- strsplit(filenames, "*\n")[[1]]
  dates_server = filePaths %>% sub("CHIRPMm.","",.) %>% sub(".tif","",.) %>% as.Date()
  stdff = setdiff(dates_home,dates_server) %>% as.Date(.,origin=as.Date("1970-01-01"))
  lost_files = which(dates_home %in% stdff)
  
  if(length(lost_files)>0){
    message("Completando el ftp ... CHIRPMm")
    basename_CHIRPMm = dd[lost_files]
    ftpname_CHIRPMm = sprintf("%s/Monthly_Products/unstable/CHIRPMm/%s",ftp,basename(dd[lost_files]))
    mapply(function(i) ftpUpload(basename_CHIRPMm[i],ftpname_CHIRPMm[i]), 1:length(basename_CHIRPMm))
  }
  
  ### PISCOpm
  CHIRPm = "/home/senamhi-cesar/PISCO/PISCOpV2.1/Monthly_Products/unstable/PISCOpm/"
  dd = list.files(CHIRPm,full.names = T)
  dates_home = gsub("PISCOpm_V2.1.","",basename(dd)) %>% gsub(".tif","",.) %>% as.Date()
  
  chirpMm = "ftp://publi_dgh2:123456@ftp.senamhi.gob.pe/PISCOp_V2.1_beta/Monthly_Products/unstable/PISCOpm/"
  filenames <- getURL(chirpMm, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE)
  filePaths <- strsplit(filenames, "*\n")[[1]]
  dates_server = filePaths %>% sub("PISCOpm_V2.1.","",.) %>% sub(".tif","",.) %>% as.Date()
  stdff = setdiff(dates_home,dates_server) %>% as.Date(.,origin=as.Date("1970-01-01"))
  lost_files = which(dates_home %in% stdff)
  
  if(length(lost_files)>0){
    message("Completando el ftp ... PISCOpm")
    basename_CHIRPMm = dd[lost_files]
    ftpname_CHIRPMm = sprintf("%s/Monthly_Products/unstable/PISCOpm/%s",ftp,basename(dd[lost_files]))
    mapply(function(i) ftpUpload(basename_CHIRPMm[i],ftpname_CHIRPMm[i]), 1:length(basename_CHIRPMm))
  }
  
  ### CHIRPMd
  
  CHIRPm = "/home/senamhi-cesar/PISCO/PISCOpV2.1/Daily_Products/unstable/CHIRPMd/"
  dd = list.files(CHIRPm,full.names = T)
  dates_home = gsub("CHIRPMd.","",basename(dd)) %>% gsub(".tif","",.) %>% as.Date()
  
  chirpMm = "ftp://publi_dgh2:123456@ftp.senamhi.gob.pe/PISCOp_V2.1_beta/Daily_Products/unstable/CHIRPMd/"
  filenames <- getURL(chirpMm, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE)
  filePaths <- strsplit(filenames, "*\n")[[1]]
  dates_server = filePaths %>% sub("CHIRPMd.","",.) %>% sub(".tif","",.) %>% as.Date()
  stdff = setdiff(dates_home,dates_server) %>% as.Date(.,origin=as.Date("1970-01-01"))
  lost_files = which(dates_home %in% stdff)
  if(length(lost_files)>0){
    message("Completando el ftp ... CHIRPMd")
    basename_CHIRPMm = dd[lost_files]
    ftpname_CHIRPMm = sprintf("%s/Daily_Products/unstable/CHIRPMd/%s",ftp,basename(dd[lost_files]))
    mapply(function(i) ftpUpload(basename_CHIRPMm[i],ftpname_CHIRPMm[i]), 1:length(basename_CHIRPMm))
  } 
  ### PISCOpd
  
  CHIRPm = "/home/senamhi-cesar/PISCO/PISCOpV2.1/Daily_Products/unstable/PISCOpd/"
  dd = list.files(CHIRPm,full.names = T)
  dates_home = gsub("PISCOpd_V2.1.","",basename(dd)) %>% gsub(".tif","",.) %>% as.Date()
  
  chirpMm = "ftp://publi_dgh2:123456@ftp.senamhi.gob.pe/PISCOp_V2.1_beta/Daily_Products/unstable/PISCOpd/"
  filenames <- getURL(chirpMm, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE)
  filePaths <- strsplit(filenames, "*\n")[[1]]
  dates_server = filePaths %>% sub("PISCOpd_V2.1.","",.) %>% sub(".tif","",.) %>% as.Date()
  stdff = setdiff(dates_home,dates_server) %>% as.Date(.,origin=as.Date("1970-01-01"))
  lost_files = which(dates_home %in% stdff) 
 
  if(length(lost_files)>0){
    message("Completando el ftp ... PISCOpd")
    basename_CHIRPMm = dd[lost_files]
    ftpname_CHIRPMm = sprintf("%s/Daily_Products/unstable/PISCOpd/%s",ftp,basename(dd[lost_files]))
    mapply(function(i) ftpUpload(basename_CHIRPMm[i],ftpname_CHIRPMm[i]), 1:length(basename_CHIRPMm))
  }
  ### P-PISCOpd
  
  CHIRPm = "/home/senamhi-cesar/PISCO/PISCOpV2.1/Daily_Products/unstable/P-PISCOpd/"
  dd = list.files(CHIRPm,full.names = T)
  dates_home = gsub("P-PISCOpd_V2.1.","",basename(dd)) %>% gsub(".tif","",.) %>% as.Date()
  
  chirpMm = "ftp://publi_dgh2:123456@ftp.senamhi.gob.pe/PISCOp_V2.1_beta/Daily_Products/unstable/P-PISCOpd/"
  filenames <- getURL(chirpMm, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE)
  filePaths <- strsplit(filenames, "*\n")[[1]]
  dates_server = filePaths %>% sub("P-PISCOpd_V2.1.","",.) %>% sub(".tif","",.) %>% as.Date()
  stdff = setdiff(dates_home,dates_server) %>% as.Date(.,origin=as.Date("1970-01-01"))
  lost_files = which(dates_home %in% stdff) 
  
  if(length(lost_files)>0){
    message("Completando el ftp ... P-PISCOpd")
    basename_CHIRPMm = dd[lost_files]
    ftpname_CHIRPMm = sprintf("%s/Daily_Products/unstable/P-PISCOpd/%s",ftp,basename(dd[lost_files]))
    mapply(function(i) ftpUpload(basename_CHIRPMm[i],ftpname_CHIRPMm[i]), 1:length(basename_CHIRPMm))
  }
}


