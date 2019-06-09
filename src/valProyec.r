library(transformeR)
ruta = "/home/doctor/workspace/master/TFM/"

#gcms = c("Canes", "Cnrm", "Gfdl", "Miroc", "MpiLr", "MpiMr", "Noresm")
gcms = c("Canes")
#gcms = c("Cnrm")
hist = list()
hist[["Canes"]] = "canesm2.historical"
hist[["Cnrm"]] = "cnrm.historial"
hist[["Gfdl"]] = "gfdl.historical"
hist[["Miroc"]] = "miroc.historical"
hist[["MpiLr"]] = "mpi.lr.historical"
hist[["MpiMr"]] = "mpi.mr.historical"
hist[["Noresm"]] = "noresm.historical"

rcp85 = list()
rcp85[["Canes"]] = "canesm2.rcp85"
rcp85[["Cnrm"]] = "cnrm.rcp85"
rcp85[["Gfdl"]] = "gfdl.rcp85"
rcp85[["Miroc"]] = "miroc.rcp85"
rcp85[["MpiLr"]] = "mpi.lr.rcp85"
rcp85[["MpiMr"]] = "mpi.mr.rcp85"
rcp85[["Noresm"]] = "noresm.rcp85"

patrones = list()
patrones[["P1"]] = c("psl", "ta@85000")
patrones[["P2"]] = c("psl", "hus@85000")
patrones[["P3"]] = c("psl", "ta@85000", "hus@85000")
patrones[["P4"]] = c("psl", "ta@85000", "ua@25000","hus@85000")
patrones[["P5"]] = c("psl", "hur@85000")
patrones[["P6"]] = c("psl", "hur@85000", "ua@25000")
patrones[["P7"]] = c("psl", "ta@85000", "ua@25000","hus@85000", "hur@85000")
patrones[["P8"]] = c("hur@25000", "hur@50000", "hur@85000", "hus@25000", "hus@50000", "hus@85000", "ta@25000", "ta@50000", "ta@85000", "ua@25000", "ua@50000", "ua@85000", "va@25000", "va@50000", "va@85000", "psl")
patronesLegend = list()
patronesLegend[["P1"]] = c("PSL", "T850")
patronesLegend[["P2"]] = c("PSL", "Q850")
patronesLegend[["P3"]] = c("PSL", "T850", "Q850")
patronesLegend[["P4"]] = c("PSL", "T850", "U250","Q850")
patronesLegend[["P5"]] = c("PSL", "R850")
patronesLegend[["P6"]] = c("PSL", "R850", "U250")
patronesLegend[["P7"]] = c("PSL", "T850", "U250","Q850", "R850")
patronesLegend[["P8"]] = c("muchas")
patronesLegend[["P7PCs"]] = paste0("PCA(",paste0(patronesLegend[["P7"]], collapse = ", "), ")", collapse = "")
patronesLegend[["P8PCs"]] = paste0("PCA(",paste0(patronesLegend[["P8"]], collapse = ", "), ")", collapse = "")
opcionPatrones = c("P2", "P5")

colores = c("blue", "orange")
GUARDA = F
for (gcm in gcms) {
  i = 1
  if (GUARDA){
    pdf(paste0(ruta, "imagenes/proyeccionGLMgcm", gcm, ".pdf", collaspe = ""))
  }
  plot(1, type="n", xlab="Años", ylab="Pr", xlim=c(2006, 2100), ylim=c(10, 4500), main = gcm)
  for (patron in opcionPatrones){
    load(paste0(ruta, "data/proyeccion/resultados/pred", gcm, "patron", patron, "GLM.rda"))
    tmp = aggregateGrid(prediccionFinal, aggr.m = list(FUN = "sum"), aggr.y = list(FUN = "sum"))
    serie = rowMeans(tmp$Data)
    lines(2006:2100, serie, col = colores[i])
    i = i + 1
  }
  legend("topleft", lty = 1, col = colores, legend = c(paste(patronesLegend[["P2"]], collapse = ", "), paste(patronesLegend[["P5"]], collapse = ", ")))
  if (GUARDA){
    dev.off()
  }
}
gcms = c("Canes", "Cnrm", "Gfdl", "Miroc", "MpiLr", "MpiMr")
opcionPatrones = c("P2", "P5", "P7", "P7PCs", "P8PCs")
colores = c("blue", "green","orange", "purple", "red")
anios = getYearsAsINDEX(prediccionFinal)
aniomin = min(anios)
aniomax = max(anios)
aniosT = aniomin:aniomax

vecinos = c(1, 25)
GUARDA = F
for(n in vecinos){
  for (gcm in gcms){
    i = 1
    if(GUARDA){
      pdf(paste0(ruta, "imagenes/proyeccionRFNvecinos", n, "gcm", gcm, ".pdf", collaspe = ""))   
    }
    plot(1, type="n", xlab="Años", ylab="Pr", xlim=c(2006, 2100), ylim=c(10, 4500), main = paste(gcm, "n vecinos", n))
    for (patron in opcionPatrones){
      if(grepl("PCs", patron)){
        load(paste0(ruta, "data/proyeccion/resultados/pred", gcm, "patron", substr(patron, 1, 2),"RFPCs.rda"))
      }else{
        load(paste0(ruta, "data/proyeccion/resultados/pred", gcm, "nvecions", n, "patron", patron, "RF.rda")) 
      }
      serie = c()
      for(j in 1:length(aniosT)){
        anio = aniosT[j]
        indices = which(anios == anio)
        serie = rbind(serie, colSums(prediccionFinal[indices,]))
      }
      serie = rowMeans(serie)
      lines(2006:2100, serie, col = colores[i])
      i = i + 1
    }
    legend("topleft", lty = 1, col = colores, legend = c(paste(patronesLegend[["P2"]], collapse = ", "), paste(patronesLegend[["P5"]], collapse = ", "), 
                                                         paste(patronesLegend[["P7"]], collapse = ", "), paste(patronesLegend[["P7PCs"]], collapse = ", "), paste(patronesLegend[["P8PCs"]], collapse = ", ")))
    if(GUARDA){
      dev.off()
    }
  }
}
