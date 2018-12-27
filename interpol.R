nscore <- function(x) {
  nscore <- qqnorm(x, plot.it = FALSE)$x  # normal score
  trn.table <- data.frame(x=sort(x),nscore=sort(nscore))
  return (list(nscore=nscore, trn.table=trn.table))
}

backtr <- function(scores, nscore, tails='none', draw=FALSE) {
  if(tails=='none') {   # No extrapolation
    min.x <- min(nscore$trn.table$x)
    max.x <- max(nscore$trn.table$x)
  }
  min.sc <- min(scores)
  max.sc <- max(scores)
  x <- c(min.x, nscore$trn.table$x, max.x)
  nsc <- c(min.sc, nscore$trn.table$nscore, max.sc)
  back.xf <- approxfun(nsc,x) # Develop the back transform function
  val <- back.xf(scores)
  return(val)
}


mean_double_Station = function (gauge = NULL, sat = NULL, longlat = TRUE){
  #----------------------------------------------------------------------
  projection(sat) <- projection(gauge)
  point <- data.frame(rasterToPoints(sat))
  colnames(point) <- c("x","y","sat")
  coordinates(point) <-  ~ x + y
  projection(point) <- projection(sat)
  distances <- function(x,ptsat = point) which.min(spDists(ptsat,gauge[x,],longlat = T))
  #----------------------------------------------------------------------
  loc <- do.call("c",lapply(1:length(gauge), distances))
  duplicates <- loc[which(duplicated(loc))]
  gauge2 <- cbind(coordinates(gauge),gauge@data)
  colnames(gauge2) <- c("x","y",colnames(gauge2)[3:length(gauge2)])
  gauge2p <- gauge2 %>% tbl_df 
  list <- lapply(1:length(duplicates), function(i){
    dupliStation <- which(loc==duplicates[i])
    gaugeD <- gauge2p[dupliStation,]
    PromStation <- colMeans(gaugeD,na.rm=T)
    list(Prom=PromStation,position=dupliStation)
  })
  stat<-do.call("rbind",lapply(1:length(duplicates),function(x) list[[x]]$position))
  stat2<-do.call("rbind",lapply(1:length(duplicates),function(x) list[[x]]$Prom))
  gauge2p[stat[,1],]<-stat2
  newG<-gauge2p[-stat[,2],]
  coordinates(newG)<-~x+y
  projection(newG) <- projection(sat)
  return(newG)
}
var_fit <- function (gauge, cov, formula, ...){
  ext <- raster::extract(cov, gauge, cellnumber = F, sp = T)
  ext2<-data.frame(coordinates(ext),ext@data) %>% na.omit
  coordinates(ext2)<-~x+y
  projection(ext2)<-projection(gauge)
  list(ftvariogram = FitVariogram(formula, ext2, ...), ext = ext2)
}

RIDW <- function (gauge, cov, formula, idpR = seq(0.8, 3.5, 0.1), ...) {
  sav_name <- names(gauge)
  ext <- raster::extract(cov, gauge, cellnumber = F, sp = T)
  gauge <- gauge[which(!is.na(ext$sat)),]
  station <- gauge
  linear <- na.omit(ext@data) %>% tbl_df %>% mutate_all(as.character) %>% 
    mutate_all(as.numeric)
  #llm <- lm(formula, linear)
  #if(anyNA(coefficients(llm))){ 
  station$residuals <- linear[[1]]-linear[[2]]
  #} else {station$residuals <- llm$residuals}
  point <- rasterToPoints(cov) %>% data.frame
  coordinates(point) <- ~x + y
  projection(point) <- projection(cov)
  idpRange <- idpR
  mse <- rep(NA, length(idpRange))
  for (i in 1:length(idpRange)) {
    mse[i] <- mean(krige.cv(residuals ~ 1, station, nfold = 10, 
                            set = list(idp = idpRange[i]), verbose = F, ...)$residual^2)
  }
  poss <- which(mse %in% min(mse))
  bestparam <- idpRange[poss]
  # residual.best <- krige.cv(residuals ~ 1, station, nfold = nrow(station), 
  #                           set = list(idp = idpRange[poss]), verbose = F, ...)$residual
  idwError <- idw(residuals ~ 1, station, point, idp = bestparam)
  idwError <- idwError["var1.pred"]
  gridded(idwError) <- TRUE
  mapa <- raster(idwError)
  Ridw <- mapa+cov
  Ridw
}

KED <- function (gauge, cov, formula, model, crossval = F,nfold, ...) 
{
  projection(cov)<-projection(gauge)
  if (missing(formula)) {
    formula <- sprintf("%s~%s",names(gauge),names(cov)) %>% as.formula
  }
  
  if (missing(model)) {
    vm.fit <- var_fit(gauge, cov, formula)
    ext <- vm.fit$ext
    model <- vm.fit$ftvariogram
  }
  
  point <- rasterToPoints(cov) %>% data.frame
  coordinates(point) <- ~x + y
  projection(point) <- projection(gauge)
  Zs <- krige(formula, locations = ext, newdata = point, model = model$var_model)
  map <- as(Zs[1], "SpatialPixelsDataFrame")
  gridded(map) <- TRUE
  mapa <- raster(map)
  mapa<-expm1(mapa)
  mapa[mapa<0]=0
  
  PISCO_valu <- raster::extract(mapa,gauge)
  nwdf<-data.frame(obs=expm1(gauge$gauge),sim=PISCO_valu)
  coeficients<-lm(obs~sim,nwdf)$coefficients
  PISCO_climatology <- mapa*coeficients[2]+coeficients[1]
  PISCO_climatology2<-resampleR(PISCO_climatology,base,r="near")
  PISCO_climatology2  
    
  if (crossval == T) {
    if (missing(nfold)) {
      Zs.cv <- krige.cv(formula, ext, model$var_model, nfold = nrow(ext))
    }
    else Zs.cv <- krige.cv(formula, ext, model$var_model, nfold = nfold)
    Zs.cvresidual <- Zs.cv["residual"]
    list(Interpol = mapa, params = list(residual = Zs.cvresidual, 
                                        rmse = sqrt(mean(Zs.cvresidual$residual^2)), var = vm.fit))
  }
  else mapa
}


ROK <- function (gauge, cov, formula, model, crossval = F,nfold, ...) {
  projection(cov) <- projection(gauge)
  sav_name <- names(gauge)
  names(gauge) <- "gauge"
  sat_cor <- raster::extract(cov,gauge) #extraido del sat
  dTF <- data.frame(obs=gauge$gauge,sat=sat_cor) #creo el df
  #llm<-lm(obs~sat,dTF) # construyo el modelo lineal global
  gauge$diff <- round(dTF$obs-dTF$sat,1)
  fv <- FitVariogram(diff~1,gauge,fix.values = c(0,NA,NA)) #cov_lm <- cov*llm$coefficients[2]+llm$coefficients[1] #corrigo el sat
  cov_lm <- cov
  kd <- krige(diff~1,gauge,rasterToPoints(cov_lm,sp=T),model=fv$var_model)
  rF<-kd[1]
  gridded(rF)=T
  rFF <- raster(rF) + cov_lm
  rFF <- expm1(rFF)
  rFF[rFF<0]=0
  # PISCO_valu <- raster::extract(rFF,gauge)
  # nwdf<-data.frame(obs=expm1(gauge$gauge),sim=PISCO_valu)
  # coeficients<-lm(obs~sim,nwdf)$coefficients
  # PISCO_climatology <- rFF*coeficients[2]+coeficients[1]
  # names(PISCO_climatology) <- sav_name
  # PISCO_climatology2<-resampleR(,base,r="near")
  # PISCO_climatology2
  rFF
}

FitVariogram <- function (formula, input_data, model = c("Sph", "Exp", "Gau", 
                                         "Ste"), kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), fix.values = c(NA, 
                                                                                                          NA, NA), verbose = FALSE, GLS.model = NA, start_vals = c(NA, 
                                                                                                                                                                   NA, NA), miscFitOptions = list(), boundaries, ...) 
{
  if ("alpha" %in% names(list(...))) 
    warning("Anisotropic variogram model fitting not supported, see the documentation of autofitVariogram for more details.")
  miscFitOptionsDefaults = list(merge.small.bins = TRUE, min.np.bin = 5)
  miscFitOptions = modifyList(miscFitOptionsDefaults, miscFitOptions)
  longlat = !is.projected(input_data)
  if (is.na(longlat)) 
    longlat = FALSE
  diagonal = spDists(t(bbox(input_data)), longlat = longlat)[1, 
                                                             2]
  if (!missing(boundaries)) 
    boundaries = boundaries
  else {
    boundaries = c(2, 4, 6, 9, 12, 15, 25, 35, 50, 65, 80, 
                   100) * diagonal * 0.35/100
  }
  if (!is(GLS.model, "variogramModel")) {
    experimental_variogram = variogram(formula, input_data, 
                                       boundaries = boundaries, ...)
  }
  else {
    if (verbose) 
      cat("Calculating GLS sample variogram\n")
    g = gstat(NULL, "bla", formula, input_data, model = GLS.model, 
              set = list(gls = 1))
    experimental_variogram = variogram(g, boundaries = boundaries, 
                                       ...)
  }
  if (miscFitOptions[["merge.small.bins"]]) {
    if (verbose) 
      cat("Checking if any bins have less than 5 points, merging bins when necessary...\n\n")
    while (TRUE) {
      if (length(experimental_variogram$np[experimental_variogram$np < 
                                           miscFitOptions[["min.np.bin"]]]) == 0 | length(boundaries) == 
          1) 
        break
      boundaries = boundaries[2:length(boundaries)]
      if (!is(GLS.model, "variogramModel")) {
        experimental_variogram = variogram(formula, 
                                           input_data, boundaries = boundaries, ...)
      }
      else {
        experimental_variogram = variogram(g, boundaries = boundaries, 
                                           ...)
      }
    }
  }
  if (is.na(start_vals[1])) {
    initial_nugget = min(experimental_variogram$gamma)
  }
  else {
    initial_nugget = start_vals[1]
  }
  if (is.na(start_vals[2])) {
    initial_range = 0.1 * diagonal
  }
  else {
    initial_range = start_vals[2]
  }
  if (is.na(start_vals[3])) {
    initial_sill = mean(c(max(experimental_variogram$gamma), 
                          median(experimental_variogram$gamma)))
  }
  else {
    initial_sill = start_vals[3]
  }
  if (!is.na(fix.values[1])) {
    fit_nugget = FALSE
    initial_nugget = fix.values[1]
  }
  else fit_nugget = TRUE
  if (!is.na(fix.values[2])) {
    fit_range = FALSE
    initial_range = fix.values[2]
  }
  else fit_range = TRUE
  if (!is.na(fix.values[3])) {
    fit_sill = FALSE
    initial_sill = fix.values[3]
  }
  else fit_sill = TRUE
  getModel = function(psill, model, range, kappa, nugget, 
                      fit_range, fit_sill, fit_nugget, verbose) {
    if (verbose) 
      debug.level = 1
    else debug.level = 0
    if (model == "Pow") {
      warning("Using the power model is at your own risk, read the docs of autofitVariogram for more details.")
      if (is.na(start_vals[1])) 
        nugget = 0
      if (is.na(start_vals[2])) 
        range = 1
      if (is.na(start_vals[3])) 
        sill = 1
    }
    obj = try(fit.variogram(experimental_variogram, model = vgm(psill = psill, 
                                                                model = model, range = range, nugget = nugget, kappa = kappa), 
                            fit.ranges = c(fit_range), fit.sills = c(fit_nugget, 
                                                                     fit_sill), debug.level = 0), TRUE)
    if ("try-error" %in% class(obj)) {
      warning("An error has occured during variogram fitting. Used:\n", 
              "\tnugget:\t", nugget, "\n\tmodel:\t", model, 
              "\n\tpsill:\t", psill, "\n\trange:\t", range, 
              "\n\tkappa:\t", ifelse(kappa == 0, NA, kappa), 
              "\n  as initial guess. This particular variogram fit is not taken into account. \nGstat error:\n", 
              obj)
      return(NULL)
    }
    else return(obj)
  }
  test_models = model
  SSerr_list = c()
  vgm_list = list()
  counter = 1
  for (m in test_models) {
    if (m != "Mat" && m != "Ste") {
      model_fit = getModel(initial_sill - initial_nugget, 
                           m, initial_range, kappa = 0, initial_nugget, 
                           fit_range, fit_sill, fit_nugget, verbose = verbose)
      if (!is.null(model_fit)) {
        vgm_list[[counter]] = model_fit
        SSerr_list = c(SSerr_list, attr(model_fit, "SSErr"))
      }
      counter = counter + 1
    }
    else {
      for (k in kappa) {
        model_fit = getModel(initial_sill - initial_nugget, 
                             m, initial_range, k, initial_nugget, fit_range, 
                             fit_sill, fit_nugget, verbose = verbose)
        if (!is.null(model_fit)) {
          vgm_list[[counter]] = model_fit
          SSerr_list = c(SSerr_list, attr(model_fit, 
                                          "SSErr"))
        }
        counter = counter + 1
      }
    }
  }
  strange_entries = sapply(vgm_list, function(v) any(c(v$psill, 
                                                       v$range) < 0) | is.null(v))
  if (any(strange_entries)) {
    if (verbose) {
      print(vgm_list[strange_entries])
      cat("^^^ ABOVE MODELS WERE REMOVED ^^^\n\n")
    }
    warning("Some models where removed for being either NULL or having a negative sill/range/nugget, \n\tset verbose == TRUE for more information")
    SSerr_list = SSerr_list[!strange_entries]
    vgm_list = vgm_list[!strange_entries]
  }
  if (verbose) {
    cat("Selected:\n")
    print(vgm_list[[which.min(SSerr_list)]])
    cat("\nTested models, best first:\n")
    tested = data.frame(`Tested models` = sapply(vgm_list, 
                                                 function(x) as.character(x[2, 1])), kappa = sapply(vgm_list, 
                                                                                                    function(x) as.character(x[2, 4])), SSerror = SSerr_list)
    tested = tested[order(tested$SSerror), ]
    print(tested)
  }
  result = list(exp_var = experimental_variogram, var_model = vgm_list[[which.min(SSerr_list)]], 
                sserr = min(SSerr_list))
  class(result) = c("autofitVariogram", "list")
  return(result)
}
