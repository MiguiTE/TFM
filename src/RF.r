# Paquetes climate4R

library(loadeR)
library(transformeR)
library(downscaleR)
library(randomForest)

# variables utiles
# Se pueden cambiar
ruta = "/home/jovyan/TFM/TFM/"

PCS = F
ESTACIONES = F
ANUAL = T

regionaliza = function(dataOccCV, dataRegCV){
    n_regions = length(dataOccCV)
    modelos = list()
    yOccPredRF = list()
    yOccRealRF = list()
    yRegPredRF = list()
    yRegRealRF = list()
    for(region in 1:n_regions){
        print(paste("Estacion:", estacion, "Region:", region))
        modelos[[region]] = list()
        prediccionFoldOcc = c()
        prediccionFoldReg = c()
        realFoldOcc = c()
        realFoldReg = c()
        for(fold in names(dataOccCV[[region]])){
            print(paste("Fold:", fold))
            #Fase preparar datos train
            trainOcc = prepareData(dataOccCV[[region]][[fold]][["train"]][["x"]], dataOccCV[[region]][[fold]][["train"]][["y"]])

            trainReg = prepareData(dataRegCV[[region]][[fold]][["train"]][["x"]], dataRegCV[[region]][[fold]][["train"]][["y"]])

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
                dfTrainReg = data.frame(trainReg[["x.global"]])
                dfTrainReg["y"] = trainReg[["y"]][["Data"]][,k]
                diasLluvia = which(dfTrainReg["y"] > 1)
                modelos[[region]][[fold]][["Reg"]][[k]] = randomForest(y ~ ., data=dfTrainReg, subset = diasLluvia, ntree = 100)
                dfTrainOcc = data.frame(trainOcc[["x.global"]])
                dfTrainOcc["y"] = as.factor(trainOcc[["y"]][["Data"]][,k])
                modelos[[region]][[fold]][["Occ"]][[k]] = randomForest(y ~ ., data=dfTrainOcc, ntree = 100)
            }

            #Liberar un poco de memoria
            rm(dfTrainOcc, dfTrainReg)

            #Predecir
            prediccionesOcc = c()
            prediccionesReg = c()
            dfTestOcc = data.frame(testOcc[["x.global"]][["member_1"]])
            dfTestReg = data.frame(testReg[["x.global"]][["member_1"]])
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
    return(list(modelos = modelos, yOccPredRF = yOccPredRF, yOccRealRF = yOccRealRF, yRegPredRF = yRegPredRF, yRegRealRF = yRegRealRF))
}

regionalizaPCs = function(dataOccCV, dataRegCV){
    n_regions = length(dataOccCV)
    modelos = list()
    yOccPredRF = list()
    yOccRealRF = list()
    yRegPredRF = list()
    yRegRealRF = list()
    for(region in 1:n_regions){
        print(paste("Estacion:", estacion, "Region:", region))
        modelos[[region]] = list()
        prediccionFoldOcc = c()
        prediccionFoldReg = c()
        realFoldOcc = c()
        realFoldReg = c()
        for(fold in names(dataOccCV[[region]])){
            print(paste("Fold:", fold))
            #Fase preparar datos train
            trainOcc = prepareData(dataOccCV[[region]][[fold]][["train"]][["x"]], dataOccCV[[region]][[fold]][["train"]][["y"]], spatial.predictors = list(which.combine = getVarNames(dataOccCV[[region]][[fold]][["train"]][["x"]]), v.exp = 0.95))

            trainReg = prepareData(dataRegCV[[region]][[fold]][["train"]][["x"]], dataRegCV[[region]][[fold]][["train"]][["y"]], spatial.predictors = list(which.combine = getVarNames(dataRegCV[[region]][[fold]][["train"]][["x"]]), v.exp = 0.95))
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
                dfTrainReg = data.frame("x"=trainReg[["pca"]][["COMBINED"]][[1]][["PCs"]])
                dfTrainReg["y"] = trainReg[["y"]][["Data"]][,k]
                diasLluvia = which(dfTrainReg["y"] > 1)
                modelos[[region]][[fold]][["Reg"]][[k]] = randomForest(y ~ ., data=dfTrainReg, subset = diasLluvia, ntree = 100)
                
                dfTrainOcc = data.frame("x"=trainOcc[["pca"]][["COMBINED"]][[1]][["PCs"]])
                dfTrainOcc["y"] = as.factor(trainOcc[["y"]][["Data"]][,k])
                modelos[[region]][[fold]][["Occ"]][[k]] = randomForest(y ~ ., data=dfTrainOcc, ntree = 100)
            }

            #Liberar un poco de memoria
            rm(dfTrainOcc, dfTrainReg)

            #Predecir
            prediccionesOcc = c()
            prediccionesReg = c()
            dfTestOcc = data.frame("x" = testOcc[["x.global"]][["member_1"]])
            dfTestReg = data.frame("x" = testReg[["x.global"]][["member_1"]])
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
    return(list(modelos = modelos, yOccPredRF = yOccPredRF, yOccRealRF = yOccRealRF, yRegPredRF = yRegPredRF, yRegRealRF = yRegRealRF))
}


if (PCS){
    if (ESTACIONES) {
        estaciones = c("primavera", "verano", "otoño", "invierno")
        for(estacion in estaciones){
            #print(paste("Estacion:", estacion))
            load(paste0(ruta, "data/datosEstaciones/",estacion, "/precip/RF/datos.rda", collapse = ""))
            resultado = regionalizaPCs(dataOccCV, dataRegCV)
            modelos = resultado[["modelos"]]
            yOccPredRF = resultado[["yOccPredRF"]]
            yOccRealRF = resultado[["yOccRealRF"]]
            yRegPredRF = resultado[["yRegPredRF"]]
            yRegRealRF = resultado[["yRegRealRF"]]

            save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/RF/RFPCsEst.rda", collapse = ""))
            save(yOccPredRF, yOccRealRF, yRegPredRF, yRegRealRF, file = paste0(ruta, "data/resultados/", estacion, "/precip/RF/RFPCsEst.rda", collapse = ""))
        }
    }
    if (ANUAL) {
        load(paste0(ruta, "data/valueAnual/datos/RF/datosAnual.rda", collapse = ""))
        resultado = regionalizaPCs(dataOccCV, dataRegCV)
        modelos = resultado[["modelos"]]
        yOccPredRF = resultado[["yOccPredRF"]]
        yOccRealRF = resultado[["yOccRealRF"]]
        yRegPredRF = resultado[["yRegPredRF"]]
        yRegRealRF = resultado[["yRegRealRF"]]

        save(modelos, file = paste0(ruta, "data/valueAnual/modelos/RF/RFPCsAn.rda", collapse = ""))
        save(yOccPredRF, yOccRealRF, yRegPredRF, yRegRealRF, file = paste0(ruta, "data/valueAnual/resultados/RF/RFPCsAn.rda", collapse = ""))
    }
} else {
    if (ESTACIONES) {
        estaciones = c("primavera", "verano", "otoño", "invierno")
        for(estacion in estaciones){
            #print(paste("Estacion:", estacion))
            load(paste0(ruta, "data/datosEstaciones/",estacion, "/precip/RF/datos.rda", collapse = ""))
            resultado = regionaliza(dataOccCV, dataRegCV)
            modelos = resultado[["modelos"]]
            yOccPredRF = resultado[["yOccPredRF"]]
            yOccRealRF = resultado[["yOccRealRF"]]
            yRegPredRF = resultado[["yRegPredRF"]]
            yRegRealRF = resultado[["yRegRealRF"]]

            save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/RF/RFEst.rda", collapse = ""))
            save(yOccPredRF, yOccRealRF, yRegPredRF, yRegRealRF, file = paste0(ruta, "data/resultados/", estacion, "/precip/RF/RFEst.rda", collapse = ""))
        }
    }
    if (ANUAL) {
        load(paste0(ruta, "data/valueAnual/datos/RF/datosAnual.rda", collapse = ""))
        resultado = regionaliza(dataOccCV, dataRegCV)
        modelos = resultado[["modelos"]]
        yOccPredRF = resultado[["yOccPredRF"]]
        yOccRealRF = resultado[["yOccRealRF"]]
        yRegPredRF = resultado[["yRegPredRF"]]
        yRegRealRF = resultado[["yRegRealRF"]]

        save(modelos, file = paste0(ruta, "data/valueAnual/modelos/RF/RFAn.rda", collapse = ""))
        save(yOccPredRF, yOccRealRF, yRegPredRF, yRegRealRF, file = paste0(ruta, "data/valueAnual/resultados/RF/RFAn.rda", collapse = ""))
    }
}