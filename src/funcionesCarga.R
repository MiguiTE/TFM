library(reshape2)
colores = c("blue", "orange", "green", "cyan", "deepskyblue", "darkgreen", "red", "violet")
regiones = c("Bretaña", "Iberia", "Francia", "Europa Central", "Escandinavia", "Alpes", "Mediterráneo", "Europa del Este")
n_regions = length(regiones)
ruta = "/home/doctor/workspace/master/TFM/"

loadGLM = function(estacion){
  if(missing(estacion)){
    load(paste0(ruta, "data//valueAnual/resultados/GLM-KNN/GLMAn.rda", collapse=""), envir = .GlobalEnv)
  }else{
    load(paste0(ruta, "data/resultados/",estacion,"/precip/GLM-KNN/GLMEst.rda", collapse=""), envir = .GlobalEnv)  
  }
}

loadGLM2 = function(estacion){
  if(missing(estacion)){
    load(paste0(ruta, "data//valueAnual/resultados/GLM-KNN/GLM2An.rda", collapse=""), envir = .GlobalEnv)
  }else{
    load(paste0(ruta, "data/resultados/",estacion,"/precip/GLM-KNN/GLM2Est.rda", collapse=""), envir = .GlobalEnv)  
  }
}

loadKNN = function(estacion){
  if(missing(estacion)){
    load(paste0(ruta, "data//valueAnual/resultados/GLM-KNN/KNNAn.rda", collapse=""), envir = .GlobalEnv)
  }else{
    load(paste0(ruta, "data/resultados/",estacion,"/precip/GLM-KNN/KNNEst.rda", collapse=""), envir = .GlobalEnv)  
  }
  yOccPredKNN <<- lapply(yRegPredKNN, function(region){
    matrix(as.numeric(region > 1), nrow = dim(region)[1], ncol = dim(region)[2])
  })
  yOccRealKNN <<- lapply(yRegRealKNN, function(region){
    matrix(as.numeric(region > 1), nrow = dim(region)[1], ncol = dim(region)[2])
  })
}

loadKNN2 = function(estacion){
  if(missing(estacion)){
    load(paste0(ruta, "data//valueAnual/resultados/GLM-KNN/KNN2An.rda", collapse=""), envir = .GlobalEnv)
  }else{
    load(paste0(ruta, "data/resultados/",estacion,"/precip/GLM-KNN/KNN2Est.rda", collapse=""), envir = .GlobalEnv)  
  }
  yOccPredKNN <<- lapply(yRegPredKNN, function(region){
    matrix(as.numeric(region > 1), nrow = dim(region)[1], ncol = dim(region)[2])
  })
  yOccRealKNN <<- lapply(yRegRealKNN, function(region){
    matrix(as.numeric(region > 1), nrow = dim(region)[1], ncol = dim(region)[2])
  })
}

loadRF = function(estacion){
  if(missing(estacion)){
    load(paste0(ruta, "data/valueAnual/resultados/RF/RFAn.rda", collapse=""), envir = .GlobalEnv)
  }else{
    load(paste0(ruta, "data/resultados/",estacion,"/precip/RF/RF.rda", collapse=""), envir = .GlobalEnv)  
  }
}

loadRFPCs = function(estacion){
  if(missing(estacion)){
    load(paste0(ruta, "data/valueAnual/resultados/RF/RFPCsAn.rda", collapse=""), envir = .GlobalEnv)
  }else{
    load(paste0(ruta, "data/resultados/",estacion,"/precip/RF/RFPCsEst.rda", collapse=""), envir = .GlobalEnv)  
  }
}

loadRF2 = function(estacion){
  if(missing(estacion)){
    load(paste0(ruta, "data/valueAnual/resultados/RF/RF2An.rda", collapse=""), envir = .GlobalEnv)
  }else{
    load(paste0(ruta, "data/resultados/",estacion,"/precip/RF/RF2Est.rda", collapse=""), envir = .GlobalEnv)  
  }
}

loadRF2PCs = function(estacion){
  if(missing(estacion)){
    load(paste0(ruta, "data/valueAnual/resultados/RF/RF2PCsAn.rda", collapse=""), envir = .GlobalEnv)
  }else{
    load(paste0(ruta, "data/resultados/",estacion,"/precip/RF/RF2PCsEst.rda", collapse=""), envir = .GlobalEnv)  
  }
}

loadNNRF = function(estacion,modelo){
  if(missing(estacion)){
    load(paste0(ruta, "data/valueAnual/resultados/RF/NNRFnvecinos", modelo, "Est.rda", collapse=""), envir = .GlobalEnv)
  }else{
    load(paste0(ruta, "data/resultados/",estacion,"/precip/RF/NNRFnvecinos", modelo, "Est.rda", collapse=""), envir = .GlobalEnv)
  }
}
loadNNRFComp = function(estacion){
  load(paste0(ruta, "data/resultados/",estacion,"/precip/RF/NNRFnvecinos2EstComp.rda", collapse=""), envir = .GlobalEnv)
}
estaciones = c("primavera", "verano", "otoño", "invierno")