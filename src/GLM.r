# Paquetes climate4R

library(loadeR)
library(transformeR)
library(downscaleR)

# variables utiles
# Se pueden cambiar
ruta = "/home/jovyan/TFM/TFM/"
nComp = 20 #Numero de componentes para PCA

#No cambiar nada
estaciones = c("primavera", "verano", "otoño", "invierno")
for(estacion in estaciones){
    print(paste("Estacion:", estacion))
    load(paste0(ruta, "data/datosEstaciones/",estacion, "/precip/GLM-KNN/datos.rda", collapse = ""))
    n_regions = length(dataOccCV)
    modelos = list()
    yOccPredGLM = list()
    yOccRealGLM = list()
    yRegPredGLM = list()
    yRegRealGLM = list()
    for(region in 1:n_regions){
        print(paste("Region:", region))
        modelos[[region]] = list()
        yOccPredGLM[[region]] = list()
        yOccRealGLM[[region]] = list()
        yRegPredGLM[[region]] = list()
        yRegRealGLM[[region]] = list()
        for(fold in names(dataOccCV[[region]])){
            print(paste("Fold:", fold))
            #Fase preparar datos train
            trainOcc = prepareData(dataOccCV[[region]][[fold]][["train"]][["x"]], dataOccCV[[region]][[fold]][["train"]][["y"]],
            spatial.predictors = list("n" = nComp, "which.combine" = getVarNames(dataOccCV[[region]][[fold]][["train"]][["x"]])))

            trainReg = prepareData(dataRegCV[[region]][[fold]][["train"]][["x"]], dataRegCV[[region]][[fold]][["train"]][["y"]],
            spatial.predictors = list("n" = nComp, "which.combine" = getVarNames(dataRegCV[[region]][[fold]][["train"]][["x"]])))

            #Fase preparar datos test
            testOcc = prepareNewData(dataOccCV[[region]][[fold]][["test"]][["x"]], trainOcc)
            yOccRealGLM[[region]] = rbind(yOccRealGLM[[region]], dataOccCV[[region]][[fold]][["test"]][["y"]])

            testReg = prepareNewData(dataRegCV[[region]][[fold]][["test"]][["x"]], trainReg)
            yRegRealGLM[[region]] = rbind(yRegRealGLM[[region]], dataRegCV[[region]][[fold]][["test"]][["y"]])

            #Fase entrenamiento modelos
            modelos[[region]][[fold]] = list()
            modelos[[region]][[fold]][["Occ"]] = downscale.train(trainOcc, method = "GLM", family = binomial(link = "logit"), condition = "GE", threshold = 0)
            modelos[[region]][[fold]][["Reg"]] = downscale.train(trainReg, method = "GLM", family = Gamma(link = "log"), condition = "GT", threshold = 1)

            #Predecir
            prediccionOcc = downscale.predict(testOcc, modelos[[region]][[fold]][["Occ"]])
            tmpOcc = binaryGrid(prediccionOcc, condition = "GT", threshold = 0.5)
            yOccPredGLM[[region]] = rbind(yOccPredGLM[[region]], tmpOcc[["Data"]])

            prediccionReg = downscale.predict(testReg, modelos[[region]][[fold]][["Reg"]])
            tmpReg = gridArithmetics(tmpOcc, prediccionReg, operator = "*")
            yRegPredGLM[[region]] = rbind(yRegPredGLM[[region]], tmpReg[["Data"]])
        }
        save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/GLM-KNN/GLM.rda", collapse = ""))
        save(yOccPredGLM, yOccRealGLM, yRegPredGLM, yRegRealGLM, file = paste0(ruta, "data/modelos/", estacion, "/precip/GLM-KNN/GLM.rda", collapse = ""))
    }
}