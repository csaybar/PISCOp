rm(list = ls())
#' Ruta de la carpeta interpolacion
working_directory <- "/home/senamhi-cesar/Github/Intp/"
database_directory <- "/home/senamhi-cesar/PISCO/"
#' Instalacion de librerias necesarias
#need R > 3.4.0
if (!require(ncdf4))  install.packages("ncdf4") ##
if (!require(xts))  install.packages("xts")
if (!require(qmap))  install.packages("qmap")
if (!require(tidyverse))  install.packages("tidyverse")
if (!require(rgdal))  install.packages("rgdal") #need GDAL>1.1
if (!require(raster))  install.packages("raster")
if (!require(rgeos))  install.packages("rgeos")
if (!require(velox))  install.packages("velox") #need GDAL>2.0 ###
if (!require(lubridate))install.packages("lubridate")
if (!require(RCurl))install.packages("RCurl")
if (!require(gstat))  install.packages("gstat")
if (!require(R.utils))  install.packages("R.utils")
if (!require(gdalUtils))  install.packages("gdalUtils")
if (!require(devtools)) install.packages("devtools") 
if (!require(prioritizr))devtools::install_github("prioritizr/prioritizr") ##
if (!require(Hmisc)) install.packages("Hmisc") 
if (!require(hydroGOF)) install.packages("hydroGOF") 

#Inicializadores
source(sprintf("%s/code/complete_data.R",working_directory))
source(sprintf("%s/code/download_CHIRP.R",working_directory))
source(sprintf("%s/code/CHIRP_correction.R",working_directory))
source(sprintf("%s/code/interpol.R",working_directory))
source(sprintf("%s/code/runme_m.R",working_directory))
source(sprintf("%s/code/resample.R",working_directory))
source(sprintf("%s/code/add_data.R",working_directory))
source(sprintf("%s/code/update_ftp.R",working_directory))


base<-raster(sprintf("%s/base-inputs/base.tif",database_directory)) #base son las extensiones que el dr. Waldo propuso a 0.1°.
load(sprintf("%s/example.Rdata",database_directory)) #example
load(sprintf("%s/PISCOpV2.1/PISCOp_raingauge_database.Rdata",database_directory)) # gauge base
load(sprintf("%s/base-inputs/monthly/collocated_monthly.Rdata",database_directory)) #integracion base pisco y sofia
load(sprintf("%s/base-inputs/monthly/cutoff_month.Rdata",database_directory)) #grupos e inputs para realizar cutoff
load(sprintf("%s/base-inputs/model_Qm.Rdata",database_directory)) #Modelos de distribucion empirica de cada estacion
load(sprintf("%s/base-inputs/monthly/plausible_groups.Rdata",database_directory)) #Modelos de distribucion empirica de cada estacion

ratio_Pm <- brick(sprintf("%s/inputs/Ratios/ratios_PISCOp.nc",database_directory))
