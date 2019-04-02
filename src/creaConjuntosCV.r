# Paquetes climate4R
library(magrittr)
library(loadeR)
library(visualizeR)
library(transformeR)
library(downscaleR)

ruta = "/home/jovyan/TFM/TFM/"

# Limites regiones VALUE
n_regions <- 8
longitudes_x <- matrix(c(-10,2,-10,4,-6,6,2,16,4,30,4,16,2,26,16,30), nrow = 2, ncol = 8)
latitudes_x <- matrix(c(50,60,36,44,44,50,48,56,54,70,44,48,36,44,44,56), nrow = 2, ncol = 8)
longitudes_y <- matrix(c(-10,2,-10,3,-5,5,3,16,3,32,5,16,3,25,16,30), nrow = 2, ncol = 8)
latitudes_y <- matrix(c(50,59,36,44,44,50,48,55,55,72,44,48,36,44,44,55), nrow = 2, ncol = 8)

#Crear los años con los folds igual que en el artículo
anios = seq(1979,2009, by = 6)
foldsAnios = lapply(1:(length(anios)-1), function(z){
     seq(anios[z],anios[z+1]-1, by=1)
 })
nFolds = length(foldsAnios)

#Crear las estaciones
meses = lapply(1:12, function(i){
  (i %% 12 + 3) %/% 3
})
estaciones = list("primavera" = c(), "verano" = c(), "otoño" = c(), "invierno" = c())
for (i in 1:12) {
  if (meses[[i]] == 1){
    estaciones[["invierno"]] = c(estaciones[["invierno"]], i)
  }
  if (meses[[i]] == 2){
    estaciones[["primavera"]] = c(estaciones[["primavera"]], i)
  }
  if (meses[[i]] == 3){
    estaciones[["verano"]] = c(estaciones[["verano"]], i)
  }
  if (meses[[i]] == 4){
    estaciones[["otoño"]] = c(estaciones[["otoño"]], i)
  }
}
rm(meses)



# Crear conjuntos de Validacion Cruzada para GLM-KNN
# Observaciones
ruta.obs = paste0(ruta, "data/VALUE_ECA_86_v2.zip", collapse = "")
# Predictores
load(paste0(ruta,"data/x.rda", collapse = ""))

predictandos = c("precip", "tmax", "tmin")

for(var in predictandos){
    for(estacion in names(estaciones)){
        xValue = lapply(1:n_regions, function(i){
            tmp = subsetGrid(x, lonLim = longitudes_x[,i], latLim=latitudes_x[,i], season=estaciones[[estacion]])
            if(anyNA(tmp$Data)){
                tmp = filterNA(tmp)
            }
            return(tmp)
        })
        yValueReg = lapply(1:n_regions, function(i){
            tmp = loadStationData(ruta.obs, var = var, season = estaciones[[estacion]], years = 1979:2008, lonLim =longitudes_y[,i], latLim = latitudes_y[,i])
            if(anyNA(tmp$Data)){
                tmp = filterNA(tmp)
            }
            getTemporalIntersection(xValue[[i]], tmp, which.retun = "prd")
        })

        xValue = lapply(1:n_regions, function(i){
            getTemporalIntersection(xValue[[i]], tmp, which.retun = "obs")
        })
        yValueOcc =lapply(yValueReg, function(y), binaryGrid(y, condition = "GT", threshold = 1))

        dataOccCV = lapply(1:n_regions, function(i) dataSplit(xValue[[i]], yValueOcc[[i]], foldsAnios, type="chronological"))
        dataRegCV = lapply(1:n_regions, function(i) dataSplit(xValue[[i]], yValueReg[[i]], foldsAnios, type="chronological"))

        save(dataOccCV, dataRegCV, file=paste0(ruta, "data/datosEstaciones/", estacion, "/", var, "/GLM-KNN/datos.rda", collapse=""))
    }
}
# Liberar un poco de memoria
rm(x, xValue, yValueOcc, yValueReg, dataOccCV, dataRegCV)

# Crear conjuntos de Validacion Cruzada para GLM-KNN
# Observaciones
ruta.obs = paste0(ruta, "data/VALUE_ECA_86_v2.zip", collapse = "")
# Predictores
load(paste0(ruta,"data/xRF.rda", collapse = ""))

predictandos = c("precip", "tmax", "tmin")

for(var in predictandos){
    for(estacion in names(estaciones)){
        xValue = lapply(1:n_regions, function(i){
            tmp = subsetGrid(xRF, lonLim = longitudes_x[,i], latLim=latitudes_x[,i], season=estaciones[[estacion]])
            if(anyNA(tmp$Data)){
                tmp = filterNA(tmp)
            }
            return(tmp)
        })
        yValueReg = lapply(1:n_regions, function(i){
            tmp = loadStationData(ruta.obs, var = var, season = estaciones[[estacion]], years = 1979:2008, lonLim =longitudes_y[,i], latLim = latitudes_y[,i])
            if(anyNA(tmp$Data)){
                tmp = filterNA(tmp)
            }
            getTemporalIntersection(xValue[[i]], tmp, which.retun = "prd")
        })

        xValue = lapply(1:n_regions, function(i){
            getTemporalIntersection(xValue[[i]], tmp, which.retun = "obs")
        })
        yValueOcc =lapply(yValueReg, function(y), binaryGrid(y, condition = "GT", threshold = 1))

        dataOccCV = lapply(1:n_regions, function(i) dataSplit(xValue[[i]], yValueOcc[[i]], foldsAnios, type="chronological"))
        dataRegCV = lapply(1:n_regions, function(i) dataSplit(xValue[[i]], yValueReg[[i]], foldsAnios, type="chronological"))

        save(dataOccCV, dataRegCV, file=paste0(ruta, "data/datosEstaciones/", estacion, "/", var, "/RF/datos.rda", collapse=""))
    }
}