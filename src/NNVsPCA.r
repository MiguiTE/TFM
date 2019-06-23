ruta = "/home/doctor/workspace/master/TFM/"
source(paste0(ruta, "src/funcionesCarga.R", collapse = ""))
GUARDA = T

######################################################################################################
#NN VS PCs
modelos = c(paste("NNRF", seq(2,21, 2), sep = ""), "RF")


#pdf("imagenes/Spearman.pdf")
#par(mfrow=c(2,2), mar=c(5, 4, 4, 3))
#par(mfrow=c(2,1))
for(estacion in estaciones){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/Spearman", estacion, ".pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2PCs(estacion)
    }else if (grepl(x = modelo, pattern = "NNRF")){
      if(nchar(modelo) == 5){
        loadNNRF(estacion, substr(modelo, 5, 5))
      }else{
        loadNNRF(estacion, substr(modelo, 5, 6))
      }
      yOccPredNNRF = yOccPredRF
      yOccRealNNRF = yOccRealRF
      yRegPredNNRF = yRegPredRF
      yRegRealNNRF = yRegRealRF
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) cor(pred[,j],real[,j], method = "spearman"))
    })
    tmp2[[9]] = as.numeric(timeElapsed, units = "mins")
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df, pos = 1:13, ylim = c(0,1), main=paste("Correlación Spearman", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2, ylab = "Correlacion", par(mar=c(6,4,2,4)))
  abline(h=0, col = "grey", lty=3, lwd=2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  par(new = TRUE)
  plot(seq(0.55,13.5, length.out = length(modelos)), tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "", xlim = c(0,14))
  axis(4)
  mtext("Tiempo", side=4, line = 2, cex = 1.2)
  if(GUARDA){
    dev.off()  
  }
}
#dev.off()
######################################################################################3
#pdf("imagenes/R01.pdf")
#par(mfrow=c(2,2), mar=c(5, 4, 4, 3))
#par(mfrow=c(2,1))
for(estacion in estaciones){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/R01", estacion, ".pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2(estacion)
    }else if (grepl(x = modelo, pattern = "NNRF")){
      if(nchar(modelo) == 5){
        loadNNRF(estacion, substr(modelo, 5, 5))
      }else{
        loadNNRF(estacion, substr(modelo, 5, 6))
      }
      yOccPredNNRF = yOccPredRF
      yOccRealNNRF = yOccRealRF
      yRegPredNNRF = yRegPredRF
      yRegRealNNRF = yRegRealRF
    }
    do.call("<-",list("yOccPred", eval(parse(text = paste0("yOccPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yOccReal", eval(parse(text = paste0("yOccReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yOccPred), function(i){
      pred = yOccPred[[i]]
      real = yOccReal[[i]]
      lapply(1:dim(pred)[2], function(j) sum(pred[,j]) / sum(real[,j]))
    })
    tmp2[[9]] = as.numeric(timeElapsed, units = "mins")
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0, 1.2), main=paste("R01", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  par(new = TRUE)
  plot(seq(0.55,13.5, length.out = length(modelos)), tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "", xlim = c(0,14))
  axis(4)
  mtext("Tiempo", side=4, line = 2, cex = 1.2)
  if(GUARDA){
    dev.off()  
  }
}
#dev.off()
#######################################################################################
for(estacion in estaciones){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/SDII", estacion, ".pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2(estacion)
    }else if (grepl(x = modelo, pattern = "NNRF")){
      if(nchar(modelo) == 5){
        loadNNRF(estacion, substr(modelo, 5, 5))
      }else{
        loadNNRF(estacion, substr(modelo, 5, 6))
      }
      yOccPredNNRF = yOccPredRF
      yOccRealNNRF = yOccRealRF
      yRegPredNNRF = yRegPredRF
      yRegRealNNRF = yRegRealRF
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) mean(pred[,j][pred[,j] > 1]) / mean(real[,j][real[,j] > 1]))
    })
    tmp2[[9]] = as.numeric(timeElapsed, units = "mins")
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0.8, 1.6), main=paste("SDII", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  par(new = TRUE)
  plot(seq(0.55,13.5, length.out = length(modelos)), tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "", xlim = c(0,14))
  axis(4)
  mtext("Tiempo", side=4, line = 2, cex = 1.2)
  if(GUARDA){
    dev.off()  
  }
}
######################################################################################3
#pdf("imagenes/RV.pdf")
#par(mfrow=c(2,2), mar=c(5, 4, 4, 3))
#par(mfrow=c(2,1))
for(estacion in estaciones){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/RV", estacion, ".pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2(estacion)
    }else if (grepl(x = modelo, pattern = "NNRF")){
      if(nchar(modelo) == 5){
        loadNNRF(estacion, substr(modelo, 5, 5))
      }else{
        loadNNRF(estacion, substr(modelo, 5, 6))
      }
      yOccPredNNRF = yOccPredRF
      yOccRealNNRF = yOccRealRF
      yRegPredNNRF = yRegPredRF
      yRegRealNNRF = yRegRealRF
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) sd(pred[,j][pred[,j] > 1]) / sd(real[,j][real[,j] > 1]))
    })
    tmp2[[9]] = as.numeric(timeElapsed, units = "mins")
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0,1), main=paste("Ratio Varianzas", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  abline(h=0, col = "grey", lty=3, lwd=2)
  par(new = TRUE)
  plot(seq(0.55,13.5, length.out = length(modelos)), tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "", xlim = c(0,14))
  axis(4)
  mtext("Tiempo", side=4, line = 1.75, cex = 0.8)
  if(GUARDA){
    dev.off()  
  }
}
#dev.off()
######################################################################################3
#pdf("imagenes/R95P.pdf")
#par(mfrow=c(2,2), mar=c(5, 4, 4, 3))
#par(mfrow=c(2,1))
for(estacion in estaciones){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/R95P", estacion, ".pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2(estacion)
    }else if (grepl(x = modelo, pattern = "NNRF")){
      if(nchar(modelo) == 5){
        loadNNRF(estacion, substr(modelo, 5, 5))
      }else{
        loadNNRF(estacion, substr(modelo, 5, 6))
      }
      yOccPredNNRF = yOccPredRF
      yOccRealNNRF = yOccRealRF
      yRegPredNNRF = yRegPredRF
      yRegRealNNRF = yRegRealRF
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) quantile(pred[,j][pred[,j] > 1], 0.95) / quantile(real[,j][real[,j] > 1], 0.95))
    })
    tmp2[[9]] = as.numeric(timeElapsed, units = "mins")
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(0.5,1.5), main=paste("Ratio Percentil 95", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  #abline(h=0, col = "grey", lty=3, lwd=2)
  par(new = TRUE)
  plot(seq(0.55,13.5, length.out = length(modelos)), tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "", xlim = c(0,14))
  axis(4)
  mtext("Tiempo", side=4, line = 1.75, cex = 0.8)
  if(GUARDA){
    dev.off()  
  }
}
for(estacion in estaciones){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/R05P", estacion, ".pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2(estacion)
    }else if (grepl(x = modelo, pattern = "NNRF")){
      if(nchar(modelo) == 5){
        loadNNRF(estacion, substr(modelo, 5, 5))
      }else{
        loadNNRF(estacion, substr(modelo, 5, 6))
      }
      yOccPredNNRF = yOccPredRF
      yOccRealNNRF = yOccRealRF
      yRegPredNNRF = yRegPredRF
      yRegRealNNRF = yRegRealRF
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) quantile(pred[,j][pred[,j] > 1], 0.05) / quantile(real[,j][real[,j] > 1], 0.05))
    })
    tmp2[[9]] = as.numeric(timeElapsed, units = "mins")
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df,ylim = c(1.5,5.5), main=paste("Ratio Percentil 05", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  #abline(h=0, col = "grey", lty=3, lwd=2)
  par(new = TRUE)
  plot(seq(0.55,13.5, length.out = length(modelos)), tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "", xlim = c(0,14))
  axis(4)
  mtext("Tiempo", side=4, line = 1.75, cex = 0.8)
  if(GUARDA){
    dev.off()  
  }
}
#######################################################################################
for(estacion in estaciones){
  if(GUARDA){
    pdf(paste0(ruta, "imagenes/Sesgo", estacion, ".pdf", collapse = ""))  
  }
  tmp = lapply(modelos, function(modelo){
    if (modelo == "GLM"){
      loadGLM2(estacion)
    }else if (modelo == "KNN"){
      loadKNN2(estacion)
    }else if (modelo == "RF"){
      loadRF2(estacion)
    }else if (grepl(x = modelo, pattern = "NNRF")){
      if(nchar(modelo) == 5){
        loadNNRF(estacion, substr(modelo, 5, 5))
      }else{
        loadNNRF(estacion, substr(modelo, 5, 6))
      }
      yOccPredNNRF = yOccPredRF
      yOccRealNNRF = yOccRealRF
      yRegPredNNRF = yRegPredRF
      yRegRealNNRF = yRegRealRF
    }
    do.call("<-",list("yRegPred", eval(parse(text = paste0("yRegPred",substr(modelo, 1, 4))))))
    do.call("<-",list("yRegReal", eval(parse(text = paste0("yRegReal",substr(modelo, 1, 4))))))
    tmp2 = lapply(1:length(yRegPred), function(i){
      pred = yRegPred[[i]]
      real = yRegReal[[i]]
      lapply(1:dim(pred)[2], function(j) (mean(pred[,j][pred[,j] > 1]) - mean(real[,j][real[,j] > 1])) / mean(real[,j][real[,j] > 1]))
    })
    tmp2[[9]] = as.numeric(timeElapsed, units = "mins")
    return(tmp2)
  })
  df = melt(tmp)
  tiempos = df[is.na(df$L3),]$value
  df = df[!is.na(df$L3),]
  boxplot(value ~ L1, data = df, pos = 1:13, ylim = c(-1,1), main=paste("Sesgo", estacion), at = seq(1, length(modelos), by = 1), names = modelos, las = 2, ylab = "Sesgo")
  abline(h=0, col = "grey", lty=3, lwd=2)
  abline(h=1, col = "grey", lty=3, lwd=2)
  abline(h=-1, col = "grey", lty=3, lwd=2)
  par(new = TRUE)
  plot(seq(0.55,13.5, length.out = length(modelos)), tiempos, type = "l", col="lightgrey", axes = FALSE, bty = "n", xlab = "", ylab = "", xlim = c(0,14))
  axis(4)
  mtext("Tiempo", side=4, line = 2, cex = 1.2)
  if(GUARDA){
    dev.off()  
  }
}