library(transformeR)
ruta = "/home/doctor/workspace/master/TFM/data/proyeccion/resultados/"

#gcms = c("Canes", "Cnrm", "Gfdl", "Miroc", "MpiLr", "MpiMr", "Noresm")
gcms = c("Cnrm")
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
patrones[["P4"]] = c("psl", "ta@85000", "ua@85000","hus@85000")
patrones[["P5"]] = c("psl", "hur@85000")
patrones[["P6"]] = c("psl", "hur@85000", "ua@85000")
opcionPatrones = c("P2", "P5")

colores = c("blue", "orange")

for (gcm in gcms) {
  plot(1, type="n", xlab="Años", ylab="Pr", xlim=c(2006, 2100), ylim=c(10, 4500), main = gcm)
  i = 1
  for (patron in opcionPatrones){
    load(paste0(ruta, "pred", gcm, "patron", patron, "GLM.rda"))
    tmp = aggregateGrid(prediccionFinal, aggr.m = list(FUN = "sum"), aggr.y = list(FUN = "sum"))
    serie = rowMeans(tmp$Data)
    lines(2006:2100, serie, col = colores[i])
    i = i + 1
  }
  legend("topleft", lty = 1, col = colores, legend = c(paste(patrones[["P2"]], collapse = ", "), paste(patrones[["P5"]], collapse = ", ")))
}

anios = getYearsAsINDEX(prediccionFinal)
aniomin = min(anios)
aniomax = max(anios)
aniosT = aniomin:aniomax
for (gcm in gcms) {
  plot(1, type="n", xlab="Años", ylab="Pr", xlim=c(2006, 2100), ylim=c(10, 4500), main = gcm)
  i = 1
  for (patron in opcionPatrones){
    load(paste0(ruta, "pred", gcm, "patron", patron, "RF.rda"))
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
  legend("topleft", lty = 1, col = colores, legend = c(paste(patrones[["P2"]], collapse = ", "), paste(patrones[["P5"]], collapse = ", ")))
}
