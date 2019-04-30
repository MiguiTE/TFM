library(magrittr)
library(loadeR)
library(visualizeR)
library(transformeR)
library(downscaleR)

ruta = "/home/jovyan/TFM/TFM/"

ruta.obs = paste0(ruta, "data/VALUE_ECA_86_v2.zip", collapse = "")
n_regions <- 8
longitudes_x <- matrix(c(-10,2,-10,4,-6,6,2,16,4,30,4,16,2,26,16,30), nrow = 2, ncol = 8)
latitudes_x <- matrix(c(50,60,36,44,44,50,48,56,54,70,44,48,36,44,44,56), nrow = 2, ncol = 8)
longitudes_y <- matrix(c(-10,2,-10,3,-5,5,3,16,3,32,5,16,3,25,16,30), nrow = 2, ncol = 8)
latitudes_y <- matrix(c(50,59,36,44,44,50,48,55,55,72,44,48,36,44,44,55), nrow = 2, ncol = 8)
todas = loadStationData(ruta.obs, var = "precip", years = 1979:2008)
orden = sort.list(todas[["xyCoords"]][["y"]])
coorOrdenadas = matrix(unlist(lapply(orden, function(i){
  x = todas[["xyCoords"]][["x"]][[i]]
  y = todas[["xyCoords"]][["y"]][[i]]
  list("x"=x, "y"=y)
})),nrow=2,ncol=length(orden))
ordRegion = c()
for (i in 1:dim(coorOrdenadas)[2]) {
  for(j in 1:dim(longitudes_y)[2]){
    if(coorOrdenadas[1,i] > longitudes_y[1,j] && coorOrdenadas[1,i] < longitudes_y[2,j] && coorOrdenadas[2,i] > latitudes_y[1,j] 
       && coorOrdenadas[2,i] < latitudes_y[2,j]){
      ordRegion = c(ordRegion, j)
      break
    }
  }
}
tmp = rep(1,8)
ordenLat = c()
for (i in 1:length(ordRegion)) {
  ordenLat = rbind(ordenLat, c(ordRegion[i], tmp[ordRegion[i]]))
  tmp[ordRegion[i]] = tmp[ordRegion[i]] + 1
}
ordenLatNombres = todas[["Metadata"]][["name"]][orden]
save(ordenLat, ordenLatNombres, file=paste0(ruta, "data/orden.zip", collapse = ""))



