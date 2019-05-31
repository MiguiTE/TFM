# Paquetes climate4R
library(loadeR)
library(transformeR)
library(downscaleR)
library(randomForest)

# variables utiles
# Se pueden cambiar
ruta = "/home/jovyan/TFM/TFM/"
codigos = matrix(c(sprintf("%06i", 272), sprintf("%06i", 350), sprintf("%06i", 3946), sprintf("%06i", 232), sprintf("%06i", 355), sprintf("%06i", 32), sprintf("%06i", 3991), sprintf("%06i", 2006), sprintf("%06i", 708), sprintf("%06i", 191), sprintf("%06i", 4002), sprintf("%06i", 58), sprintf("%06i", 1686), sprintf("%06i", 62), sprintf("%06i", 450), sprintf("%06i", 333)), nrow=2, ncol=8)
estaciones = c("primavera", "verano", "otoño", "invierno")

ESTACIONES = T
ANUAL = T

regionaliza = function(dataOccCV, dataRegCV, n_vecinos){
    n_regions = length(dataOccCV)
    modelos = list()
    yOccPredRF = list()
    yOccRealRF = list()
    yRegPredRF = list()
    yRegRealRF = list()
    start = Sys.time()
    for(region in 1:n_regions){
        print(paste("Estacion:", estacion, "Region:", region))
        modelos[[region]] = list()
        prediccionFoldOcc = c()
        prediccionFoldReg = c()
        realFoldOcc = c()
        realFoldReg = c()
        for(fold in names(dataOccCV[[region]])){
            print(paste("Fold:", fold))
            dataOccCV[[region]][[fold]][["train"]][["y"]] = subsetStation(dataOccCV[[region]][[fold]][["train"]][["y"]], station.id = codigos[,region])
            dataOccCV[[region]][[fold]][["test"]][["y"]] = subsetStation(dataOccCV[[region]][[fold]][["test"]][["y"]], station.id = codigos[,region])
            dataRegCV[[region]][[fold]][["train"]][["y"]] = subsetStation(dataRegCV[[region]][[fold]][["train"]][["y"]], station.id = codigos[,region])
            dataRegCV[[region]][[fold]][["test"]][["y"]] = subsetStation(dataRegCV[[region]][[fold]][["test"]][["y"]], station.id = codigos[,region])
            #Fase preparar datos train
            trainOcc = prepareData(dataOccCV[[region]][[fold]][["train"]][["x"]], dataOccCV[[region]][[fold]][["train"]][["y"]], local.predictors = list("n" = n_vecinos, vars = getVarNames(dataOccCV[[region]][[fold]][["train"]][["x"]])))

            trainReg = prepareData(dataRegCV[[region]][[fold]][["train"]][["x"]], dataRegCV[[region]][[fold]][["train"]][["y"]], local.predictors = list("n" = n_vecinos, vars = getVarNames(dataRegCV[[region]][[fold]][["train"]][["x"]])))

            #Fase preparar datos test
            testOcc = prepareNewData(dataOccCV[[region]][[fold]][["test"]][["x"]], trainOcc)
            realFoldOcc = rbind(realFoldOcc, dataOccCV[[region]][[fold]][["test"]][["y"]][["Data"]])

            testReg = prepareNewData(dataRegCV[[region]][[fold]][["test"]][["x"]], trainReg)
            realFoldReg = rbind(realFoldReg, dataRegCV[[region]][[fold]][["test"]][["y"]][["Data"]])

            #Fase entrenamiento modelos
            modelos[[region]][[fold]] = list()
            modelos[[region]][[fold]][["Occ"]] = list()
            modelos[[region]][[fold]][["Reg"]] = list()
            numeroEstaciones = dim(trainOcc[["y"]][["Data"]])[2]
            for(k in 1:numeroEstaciones){
                dfTrainReg = data.frame("x"=trainReg[["x.local"]][[k]][["member_1"]])
                dfTrainReg["y"] = trainReg[["y"]][["Data"]][,k]
                diasLluvia = which(dfTrainReg["y"] > 1)
                modelos[[region]][[fold]][["Reg"]][[k]] = randomForest(y ~ ., data=dfTrainReg, subset = diasLluvia, ntree = 100)
                dfTrainOcc = data.frame("x"=trainOcc[["x.local"]][[k]][["member_1"]])
                dfTrainOcc["y"] = as.factor(trainOcc[["y"]][["Data"]][,k])
                modelos[[region]][[fold]][["Occ"]][[k]] = randomForest(y ~ ., data=dfTrainOcc, ntree = 100)
            }

            #Liberar un poco de memoria
            rm(dfTrainOcc, dfTrainReg)

            #Predecir
            prediccionesOcc = c()
            prediccionesReg = c()
            dfTestOcc = data.frame("x" = testOcc[["x.local"]][[k]][["member_1"]])
            dfTestReg = data.frame("x" = testReg[["x.local"]][[k]][["member_1"]])
            for(k in 1:numeroEstaciones){
                prediccionOcc = as.numeric(predict(modelos[[region]][[fold]][["Occ"]][[k]], dfTestOcc, type = "prob")[,2] > 0.5)
                prediccionesOcc = cbind(prediccionesOcc, prediccionOcc)
                prediccionReg = predict(modelos[[region]][[fold]][["Reg"]][[k]], dfTestReg)
                prediccionesReg = cbind(prediccionesReg, prediccionReg)
            }
            prediccionFoldOcc = rbind(prediccionFoldOcc, prediccionesOcc)
            prediccionFoldReg = rbind(prediccionFoldReg, prediccionesOcc * prediccionesReg)

            #Liberar un poco de memoria
            rm(dfTestOcc, dfTestReg, prediccionOcc, prediccionReg, prediccionesOcc, prediccionesReg)
        }
        yOccPredRF[[region]] = prediccionFoldOcc
        yOccRealRF[[region]] = realFoldOcc
        yRegPredRF[[region]] = prediccionFoldReg
        yRegRealRF[[region]] = realFoldReg
    }
    timeElapsed = Sys.time() - start 
    return(list(modelos = modelos, yOccPredRF = yOccPredRF, yOccRealRF = yOccRealRF, yRegPredRF = yRegPredRF, yRegRealRF = yRegRealRF, timeElapsed = timeElapsed))
}


if (ESTACIONES) {
    estaciones = c("primavera", "verano", "otoño", "invierno")
    for(estacion in estaciones){
        #print(paste("Estacion:", estacion))
        load(paste0(ruta, "data/datosEstaciones/",estacion, "/precip/RF/datos.rda", collapse = ""))
        for (n_vecinos in seq(2,21,2)) {
            resultado = regionaliza(dataOccCV, dataRegCV, n_vecinos)
            modelos = resultado[[modelos]]
            yOccPredRF = resultado[[yOccPredRF]]
            yOccRealRF = resultado[[yOccRealRF]]
            yRegPredRF = resultado[[yRegPredRF]]
            yRegRealRF = resultado[[yRegRealRF]]
            timeElapsed = resultado[[timeElapsed]]

            save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/RF/RFnvecinos",n_vecinos, "Est.rda", collapse = ""))
            save(yOccPredRF, yOccRealRF, yRegPredRF, yRegRealRF, timeElapsed, file = paste0(ruta, "data/resultados/", estacion, "/precip/RF/RFRFnvecinos",n_vecinos, "Est.rda", collapse = ""))
        }
    }
}
if (ANUAL) {
    load(paste0(ruta, "data/valueAnual/datos/precip/RF/datosAnual.rda", collapse = ""))
    for (n_vecinos in seq(2,21,2)) {
        resultado = regionaliza(dataOccCV, dataRegCV)
        modelos = resultado[[modelos]]
        yOccPredRF = resultado[[yOccPredRF]]
        yOccRealRF = resultado[[yOccRealRF]]
        yRegPredRF = resultado[[yRegPredRF]]
        yRegRealRF = resultado[[yRegRealRF]]
        timeElapsed = resultado[[timeElapsed]]

        save(modelos, file = paste0(ruta, "data/valueAnual/modelos/RF/RFnvecinos",n_vecinos, "An.rda", collapse = ""))
        save(yOccPredRF, yOccRealRF, yRegPredRF, yRegRealRF, timeElapsed, file = paste0(ruta, "data/valueAnual/resultados/RF/RFnvecinos",n_vecinos, "An.rda", collapse = ""))
    }
}
