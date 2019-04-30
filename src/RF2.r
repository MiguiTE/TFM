# Paquetes climate4R

library(loadeR)
library(transformeR)
library(downscaleR)
library(randomForest)

# variables utiles
# Se pueden cambiar
ruta = "/home/jovyan/TFM/TFM/"

#No cambiar nada
codigos = matrix(c(sprintf("%06i", 272), sprintf("%06i", 350), sprintf("%06i", 3946), sprintf("%06i", 232), sprintf("%06i", 355), sprintf("%06i", 32), sprintf("%06i", 3991), sprintf("%06i", 2006), sprintf("%06i", 708), sprintf("%06i", 191), sprintf("%06i", 4002), sprintf("%06i", 58), sprintf("%06i", 1686), sprintf("%06i", 62), sprintf("%06i", 450), sprintf("%06i", 333)), nrow=2, ncol=8)
estaciones = c("primavera", "verano", "otoño", "invierno")
for(estacion in estaciones){
    #print(paste("Estacion:", estacion))
    load(paste0(ruta, "data/datosEstaciones/",estacion, "/precip/RF/datos.rda", collapse = ""))
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
    timeElapsed = Sys.time() - start
    save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/RF/RF2.rda", collapse = ""))
    save(yOccPredRF, yOccRealRF, yRegPredRF, yRegRealRF, timeElapsed, file = paste0(ruta, "data/resultados/", estacion, "/precip/RF/RF2.rda", collapse = ""))
}