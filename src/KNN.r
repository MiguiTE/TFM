# Paquetes climate4R
library(loadeR)
library(transformeR)
library(downscaleR)

# variables utiles
# Se pueden cambiar
ruta = "/home/jovyan/TFM/TFM/"
nComp = 20 #Numero de componentes para PCA

ESTACIONES = T
ANUAL = T


regionaliza = function(dataRegCV){
    n_regions = length(dataRegCV)
    modelos = list()
    yRegPredKNN = list()
    yRegRealKNN = list()
    for(region in 1:n_regions){
        print(paste("Region:", region))
        modelos[[region]] = list()
        realidadReg = c()
        prediccionReg = c()
        for(fold in names(dataRegCV[[region]])){
            print(paste("Fold:", fold))
            #Fase preparar datos train
            trainReg = prepareData(dataRegCV[[region]][[fold]][["train"]][["x"]], dataRegCV[[region]][[fold]][["train"]][["y"]],
            spatial.predictors = list("n" = nComp, "which.combine"=getVarNames(dataRegCV[[region]][[fold]][["train"]][["x"]])))

            #Fase preparar datos test
            testReg = prepareNewData(dataRegCV[[region]][[fold]][["test"]][["x"]], trainReg)
            realidadReg = rbind(realidadReg, dataRegCV[[region]][[fold]][["test"]][["y"]][["Data"]])

            #Fase entrenamiento modelos
            modelos[[region]][[fold]] = list()
    
            modelos[[region]][[fold]][["Reg"]] = downscale.train(trainReg, method = "analogs", n.analogs=1, condition = "GE", threshold = 0)

            #Predecir
            prediccionReg = rbind(prediccionReg, downscale.predict(testReg, modelos[[region]][[fold]][["Reg"]])[["Data"]])
        }
        yRegPredKNN[[region]] = prediccionReg
        yRegRealKNN[[region]] = realidadReg
    }
    return(list(modelos = modelos, yRegPredKNN = yRegPredKNN, yRegRealKNN = yRegRealKNN))
}

if (ESTACIONES) {
    estaciones = c("primavera", "verano", "otoño", "invierno")
    for(estacion in estaciones){
        print(paste("Estacion:", estacion))
        load(paste0(ruta, "data/datosEstaciones/",estacion, "/precip/GLM-KNN/datos.rda", collapse = ""))
        resultado = regionaliza(dataRegCV)
        modelos = resultado[["modelos"]]
        yRegPredKNN = resultado[["yRegPredKNN"]]
        yRegRealKNN = resultado[["yRegRealKNN"]]

        save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/GLM-KNN/KNNEst.rda", collapse = ""))
        save(yRegPredKNN, yRegRealKNN, file = paste0(ruta, "data/resultados/", estacion, "/precip/GLM-KNN/KNNEst.rda", collapse = ""))
    }
}

if (ANUAL) {
    load(paste0(ruta, "data/valueAnual/datos/GLM-KNN/datosAnual.rda", collapse = ""))
    resultado = regionaliza(dataRegCV)
    modelos = resultado[["modelos"]]
    yRegPredKNN = resultado[["yRegPredKNN"]]
    yRegRealKNN = resultado[["yRegRealKNN"]]

    save(modelos, file = paste0(ruta, "data/valueAnual/modelos/GLM-KNN/KNNAn.rda", collapse = ""))
    save(yRegPredKNN, yRegRealKNN, file = paste0(ruta, "data/valueAnual/resultados/GLM-KNN/KNNAn.rda", collapse = ""))
}
