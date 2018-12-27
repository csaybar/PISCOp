resampleR <- function (R1 = NULL, R2 = NULL, t_res = NULL, t_ext = NULL, r = "cubicspline") {
  Rb <- R1
  if(inMemory(R1)){
    nameR1 <- sprintf("%s/%s.tif",getwd(),names(R1))
    R1 <- writeRaster(R1,nameR1,overwrite=T)
  } 
  if (!missing(R2)) {
    t1 <- c(xmin(R2), ymin(R2), xmax(R2), ymax(R2))
    t2 <- c(res(R2)[1], res(R2)[2])
  } else {
    t1 <-  t_ext
    t2 <-  t_res
  }
  
  Routput2 <- sprintf("%s/R_%s.tif",getwd(),names(R1))
  Rf <- gdalwarp(srcfile = R1@file@name, dstfile = Routput2, 
                 tr = t2, te = t1, output_Raster = T, overwrite = T, 
                 verbose = T,r=r) * 1
  
  file.remove(Routput2)   
  if (inMemory(Rb)) file.remove(nameR1)
  Rf
}