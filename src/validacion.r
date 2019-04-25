library(reshape2)
colores = c("blue", "orange", "green", "cyan", "deepskyblue", "darkgreen", "red", "violet")
regiones = c("Bretaña", "Iberia", "Francia", "Europa Central", "Escandinavia", "Alpes", "Mediterráneo", "Europa del Este")
n_regions = length(regiones)

loadGLM = function(estacion){
  load(paste0("data/resultados/",estacion,"/precip/GLM-KNN/GLM2.rda", collapse=""), envir = .GlobalEnv)
}

loadKNN = function(estacion){
  load(paste0("data/resultados/",estacion,"/precip/GLM-KNN/KNN2.rda", collapse=""), envir = .GlobalEnv)
  yOccPredKNN <<- lapply(yRegPredKNN, function(region){
    matrix(as.numeric(region > 1), nrow = dim(region)[1], ncol = dim(region)[2])
  })
  yOccRealKNN <<- lapply(yRegRealKNN, function(region){
    matrix(as.numeric(region > 1), nrow = dim(region)[1], ncol = dim(region)[2])
  })
}

loadNNRF = function(estacion,modelo){
  load(paste0("data/resultados/",estacion,"/precip/NNRF/", modelo, ".rda", collapse=""), envir = .GlobalEnv)
}

modelos = c("KNN", "GLM", paste("NNRF", seq(2,21, 2), sep = ""))
estaciones = c("primavera", "verano", "otoño", "invierno")

#pdf("imagenes/Spearman.pdf")
#par(mfrow=c(2,2))
for(estacion in estaciones){
  pdf(paste0("imagenes/Spearman", estacion, ".pdf", collapse = ""))
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM(estacion)
    }else if (modelo == "KNN"){
      loadKNN(estacion)
    }else if (modelo == "RF"){
      
    }else{
      loadNNRF(estacion, modelo)
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) cor(pred[,j],real[,j], method = "spearman"))
    })
    tmp2[[9]] = timeElapsed
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df, pos = 1:12, ylim = c(0,1), names = modelos, main=paste("Correlación Spearman", estacion))
  abline(h=0, col = "grey", lty=3, lwd=2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  for(j in 1:length(tmp)){
    for(i in 1:n_regions){
      lines(c(j-0.4,j+0.4), rep(mean(unlist(tmp[[j]][[i]])),2), col = colores[i], lw = 2)
    }
  }
  par(new = TRUE)
  plot(seq(1.45,11.05, length.out = 12), tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "")
  dev.off()
}
#dev.off()
for(modelo in modelos){
  rm(list = c(paste0("yRegPred",substr(modelo, 1, 4)), paste0("yRegReal",substr(modelo, 1, 4)), 
              paste0("yOccPred",substr(modelo, 1, 4)), paste0("yOccReal",substr(modelo, 1, 4))))
}

######################################################################################3
#pdf("imagenes/R01.pdf")
#par(mfrow=c(2,2))
for(estacion in estaciones){
  pdf(paste0("imagenes/R01", estacion, ".pdf", collapse = ""))
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM(estacion)
    }else if (modelo == "KNN"){
      loadKNN(estacion)
    }else if (modelo == "RF"){
      
    }else{
      loadNNRF(estacion, modelo)
    }
    do.call("<-",list("yOccPred", eval(parse(text = paste0("yOccPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yOccReal", eval(parse(text = paste0("yOccReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yOccPred), function(i){
      pred = yOccPred[[i]]
      real = yOccReal[[i]]
      lapply(1:dim(pred)[2], function(j) sum(pred[,j]) / sum(real[,j]))
    })
    tmp2[[9]] = timeElapsed
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0.4,1.2), names = modelos, main=paste("R01", estacion))
  abline(h=1, col = "grey", lty=3, lwd=2)
  for(j in 1:length(tmp)){
    for(i in 1:n_regions){
      lines(c(j-0.4,j+0.4), rep(mean(unlist(tmp[[j]][[i]])),2), col = colores[i])
    }
  }
  par(new = TRUE)
  plot(tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "")
  dev.off()
}
#dev.off()
for(modelo in modelos){
  rm(list = c(paste0("yRegPred",substr(modelo, 1, 4)), paste0("yRegReal",substr(modelo, 1, 4)), 
              paste0("yOccPred",substr(modelo, 1, 4)), paste0("yOccReal",substr(modelo, 1, 4))))
}


######################################################################################3
#pdf("imagenes/SDII.pdf")
#par(mfrow=c(2,2))
for(estacion in estaciones){
  pdf(paste0("imagenes/SDII", estacion, ".pdf", collapse = ""))
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM(estacion)
    }else if (modelo == "KNN"){
      loadKNN(estacion)
    }else if (modelo == "RF"){
      
    }else{
      loadNNRF(estacion, modelo)
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) mean(pred[,j][pred[,j] > 1]) / mean(real[,j][real[,j] > 1]))
    })
    tmp2[[9]] = timeElapsed
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0.8,1.6), names = modelos, main=paste("SDII", estacion))
  abline(h=1, col = "grey", lty=3, lwd=2)
  for(j in 1:length(tmp)){
    for(i in 1:n_regions){
      lines(c(j-0.4,j+0.4), rep(mean(unlist(tmp[[j]][[i]])),2), col = colores[i])
    }
  }
  par(new = TRUE)
  plot(seq(4,10, length.out = 12), tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "")
  dev.off()
}
#dev.off()
for(modelo in modelos){
  rm(list = c(paste0("yRegPred",substr(modelo, 1, 4)), paste0("yRegReal",substr(modelo, 1, 4)), 
              paste0("yOccPred",substr(modelo, 1, 4)), paste0("yOccReal",substr(modelo, 1, 4))))
}
