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
codigos = matrix(c(sprintf("%06i", 272), sprintf("%06i", 350), sprintf("%06i", 3946), sprintf("%06i", 232), sprintf("%06i", 355), sprintf("%06i", 32), sprintf("%06i", 3991), sprintf("%06i", 2006), sprintf("%06i", 708), sprintf("%06i", 191), sprintf("%06i", 4002), sprintf("%06i", 58), sprintf("%06i", 1686), sprintf("%06i", 62), sprintf("%06i", 450), sprintf("%06i", 333)), nrow=2, ncol=8)

regionaliza = function(dataRegCV){
    n_regions = length(dataRegCV)
    modelos = list()
    yRegPredKNN = list()
    yRegRealKNN = list()
    start = Sys.time()
    for(region in 1:n_regions){
        print(paste("Region:", region))
        modelos[[region]] = list()
        realidadReg = c()
        prediccionReg = c()
        for(fold in names(dataRegCV[[region]])){
            print(paste("Fold:", fold))
            dataRegCV[[region]][[fold]][["train"]][["y"]] = subsetStation(dataRegCV[[region]][[fold]][["train"]][["y"]], station.id = codigos[,region])
            dataRegCV[[region]][[fold]][["test"]][["y"]] = subsetStation(dataRegCV[[region]][[fold]][["test"]][["y"]], station.id = codigos[,region])
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
    timeElapsed = Sys.time() - start 
    return(list(modelos = modelos, yRegPredKNN = yRegPredKNN, yRegRealKNN = yRegRealKNN, timeElapsed = timeElapsed))
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
        timeElapsed = resultado[["timeElapsed"]]

        save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/GLM-KNN/KNN2Est.rda", collapse = ""))
        save(yRegPredKNN, yRegRealKNN, timeElapsed, file = paste0(ruta, "data/resultados/", estacion, "/precip/GLM-KNN/KNN2Est.rda", collapse = ""))
    }
}

if (ANUAL) {
    load(paste0(ruta, "data/valueAnual/datos/GLM-KNN/datosAnual.rda", collapse = ""))
    resultado = regionaliza(dataRegCV)
    modelos = resultado[["modelos"]]
    yRegPredKNN = resultado[["yRegPredKNN"]]
    yRegRealKNN = resultado[["yRegRealKNN"]]
    timeElapsed = resultado[["timeElapsed"]]

    save(modelos, file = paste0(ruta, "data/valueAnual/modelos/GLM-KNN/KNN2An.rda", collapse = ""))
    save(yRegPredKNN, yRegRealKNN, timeElapsed, file = paste0(ruta, "data/valueAnual/resultados/GLM-KNN/KNN2An.rda", collapse = ""))
}
