# Paquetes climate4R
library(loadeR)
library(transformeR)
library(downscaleR)

ruta = "/home/doctor/workspace/master/TFM/"
GUARDA = T

codigos = matrix(c(sprintf("%06i", 272), sprintf("%06i", 350), sprintf("%06i", 3946), sprintf("%06i", 232), sprintf("%06i", 355), sprintf("%06i", 32), sprintf("%06i", 3991), sprintf("%06i", 2006), sprintf("%06i", 708), sprintf("%06i", 191), sprintf("%06i", 4002), sprintf("%06i", 58), sprintf("%06i", 1686), sprintf("%06i", 62), sprintf("%06i", 450), sprintf("%06i", 333)), nrow=2, ncol=8)


load(paste0(ruta, "data/yRegFinal.rda", collapse = ""))
yValueReg = lapply(1:length(yValueReg), function(i){
  subsetStation(yValueReg[[i]], station.id = codigos[,i])
})

source(paste0(ruta, "src/funcionesCarga.R", collapse = ""))
estaciones = list()
estaciones[["primavera"]] = c(3, 4, 5)
estaciones[["invierno"]] = c(12, 1, 2)
estaciones[["verano"]] = c(6, 7, 8)
estaciones[["otoño"]] = c(9, 10, 11)

modelos = c("GLM", "KNN", "RF")
loadGLM2()
GLM = yValueReg
loadKNN2()
KNN = yValueReg
loadRF2PCs()
RF = yValueReg
for(i in 1:length(GLM)){
  GLM[[i]][["Data"]] = yRegPredGLM[[i]]
  attr(GLM[[i]][["Data"]], "dimensions") <- c("time", "loc")
  KNN[[i]][["Data"]] = yRegPredKNN[[i]]
  attr(KNN[[i]][["Data"]], "dimensions") <- c("time", "loc")
  RF[[i]][["Data"]] = yRegPredRF[[i]]
  attr(RF[[i]][["Data"]], "dimensions") <- c("time", "loc")
}
rm(list = c(ls()[grepl(pattern = "yOcc", ls())], ls()[grepl(pattern = "yReg", ls())]))
#######################################################################################
for(estacion in names(estaciones)){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/Spearman", estacion, "An.pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    do.call("<-",list("yRegPred", eval(parse(text = modelo))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = subsetGrid(yRegPred[[i]], season = estaciones[[estacion]])[["Data"]]
      real = subsetGrid(yValueReg[[i]], season = estaciones[[estacion]])[["Data"]]
      lapply(1:dim(pred)[2], function(j) cor(pred[,j],real[,j], method = "spearman"))
    })
    return(tmp2)
  })
  df = melt(tmp)
  boxplot(value ~ L1, data = df, pos = 1:13, ylim = c(0,1), main=paste("Correlación Spearman", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2, ylab = "Correlacion", par(mar=c(6,4,2,4)))
  abline(h=0, col = "grey", lty=3, lwd=2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  if(GUARDA){
    dev.off()  
  }
}
#######################################################################################
for(estacion in names(estaciones)){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/R01", estacion, "An.pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    do.call("<-",list("yRegPred", eval(parse(text = modelo))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = binaryGrid(subsetGrid(yRegPred[[i]], season = estaciones[[estacion]]), condition = "GT", threshold = 1)[["Data"]]
      real = binaryGrid(subsetGrid(yValueReg[[i]], season = estaciones[[estacion]]), condition = "GT", threshold = 1)[["Data"]]
      lapply(1:dim(pred)[2], function(j) sum(pred[,j]) / sum(real[,j]))
    })
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0, 1.2), main=paste("R01", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  if(GUARDA){
    dev.off()  
  }
}
#######################################################################################
for(estacion in names(estaciones)){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/SDII", estacion, "An.pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    do.call("<-",list("yRegPred", eval(parse(text = modelo))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = subsetGrid(yRegPred[[i]], season = estaciones[[estacion]])[["Data"]]
      real = subsetGrid(yValueReg[[i]], season = estaciones[[estacion]])[["Data"]]
      lapply(1:dim(pred)[2], function(j) mean(pred[,j][pred[,j] > 1]) / mean(real[,j][real[,j] > 1]))
    })
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0.8, 1.7), main=paste("SDII", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  if(GUARDA){
    dev.off()  
  }
}
#######################################################################################
for(estacion in names(estaciones)){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/Spearman", estacion, "Est.pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2PCs(estacion)
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) cor(pred[,j],real[,j], method = "spearman"))
    })
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df, pos = 1:13, ylim = c(0,1), main=paste("Correlación Spearman", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2, ylab = "Correlacion", par(mar=c(6,4,2,4)))
  abline(h=0, col = "grey", lty=3, lwd=2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  if(GUARDA){
    dev.off()  
  }
}
#######################################################################################
for(estacion in names(estaciones)){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/R01", estacion, "Est.pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2(estacion)
    }
    do.call("<-",list("yOccPred", eval(parse(text = paste0("yOccPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yOccReal", eval(parse(text = paste0("yOccReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yOccPred), function(i){
      pred = yOccPred[[i]]
      real = yOccReal[[i]]
      lapply(1:dim(pred)[2], function(j) sum(pred[,j]) / sum(real[,j]))
    })
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0, 1.2), main=paste("R01", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  if(GUARDA){
    dev.off()  
  }
}
#######################################################################################
for(estacion in names(estaciones)){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/SDII", estacion, "Est.pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2(estacion)
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) mean(pred[,j][pred[,j] > 1]) / mean(real[,j][real[,j] > 1]))
    })
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0.8, 1.7), main=paste("SDII", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  if(GUARDA){
    dev.off()  
  }
}
