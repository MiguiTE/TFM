# Paquetes climate4R

library(loadeR)
library(transformeR)
library(downscaleR)
library(randomForest)

# variables utiles
# Se pueden cambiar
ruta = "/home/jovyan/TFM/TFM/"

#No cambiar nada
estaciones = c("primavera", "verano", "otoño", "invierno")
for(estacion in estaciones){
    print(paste("Estacion:", estacion))
    load(paste0(ruta, "data/datosEstaciones/",estacion, "/precip/RF/datos.rda", collapse = ""))
    n_regions = length(dataOccCV)
    modelos = list()
    yOccPredRF = list()
    yOccRealRF = list()
    yRegPredRF = list()
    yRegRealRF = list()
    for(region in 1:n_regions){
        print(paste("Region:", region))
        modelos[[region]] = list()
        yOccPredRF[[region]] = list()
        yOccRealRF[[region]] = list()
        yRegPredRF[[region]] = list()
        yRegRealRF[[region]] = list()
        for(fold in names(dataOccCV[[region]])){
            print(paste("Fold:", fold))
            #Fase preparar datos train
            trainOcc = prepareData(dataOccCV[[region]][[fold]][["train"]][["x"]], dataOccCV[[region]][[fold]][["train"]][["y"]])

            trainReg = prepareData(dataRegCV[[region]][[fold]][["train"]][["x"]], dataRegCV[[region]][[fold]][["train"]][["y"]])

            #Fase preparar datos test
            testOcc = prepareNewData(dataOccCV[[region]][[fold]][["test"]][["x"]], trainOcc)
            yOccRealRF[[region]] = rbind(yOccRealRF[[region]], dataOccCV[[region]][[fold]][["test"]][["y"]])

            testReg = prepareNewData(dataRegCV[[region]][[fold]][["test"]][["x"]], trainReg)
            yRegRealRF[[region]] = rbind(yRegRealRF[[region]], dataRegCV[[region]][[fold]][["test"]][["y"]])

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
            for(k in 1:numeroEstaciones){
                dfTestOcc = data.frame(testOcc[["x.global"]][["member_1"]])
                prediccionOcc = as.numeric(predict(modelos[[region]][[fold]][["Occ"]][[k]], dfTestOcc, type = "prob")[,2] > 0.5)
                prediccionesOcc = cbind(prediccionesOcc, prediccionOcc)
                dfTestReg = data.frame(testReg[["x.global"]][["member_1"]])
                prediccionReg = predict(modelos[[region]][[fold]][["Reg"]][[k]], dfTestReg)
                prediccionesReg = cbind(prediccionReg, prediccionOcc * prediccionReg)
            }
            yOccPredRF[[region]] = rbind(yOccPredRF[[region]], prediccionesOcc)
            yRegPredRF[[region]] = rbind(yRegPredRF[[region]], prediccionesReg)

            #Liberar un poco de memoria
            rm(dfTestOcc, dfTestReg, prediccionOcc, prediccionReg, prediccionesOcc, prediccionesReg)
        }
        save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/RF/RF.rda", collapse = ""))
        save(yOccPredRF, yOccRealRF, yRegPredRF, yRegRealRF, file = paste0(ruta, "data/resultados/", estacion, "/precip/RF/RF.rda", collapse = ""))
    }
}