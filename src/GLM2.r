# Paquetes climate4R
library(loadeR)
library(transformeR)
library(downscaleR)

# variables utiles
# Se pueden cambiar
ruta = "/home/jovyan/TFM/TFM/"
nComp = 20 #Numero de componentes para PCA

#No cambiar nada
codigos = matrix(c(sprintf("%06i", 272), sprintf("%06i", 350), sprintf("%06i", 3946), sprintf("%06i", 232), sprintf("%06i", 355), sprintf("%06i", 32), sprintf("%06i", 3991), sprintf("%06i", 2006), sprintf("%06i", 708), sprintf("%06i", 191), sprintf("%06i", 4002), sprintf("%06i", 58), sprintf("%06i", 1686), sprintf("%06i", 62), sprintf("%06i", 450), sprintf("%06i", 333)), nrow=2, ncol=8)
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
    start = Sys.time()
    for(region in 1:n_regions){
        print(paste("Region:", region))
        modelos[[region]] = list()
        realidadOcc = c()
        realidadReg = c()
        prediccionOcc = c()
        prediccionReg = c()
        for(fold in names(dataOccCV[[region]])){
            print(paste("Fold:", fold))
            dataOccCV[[region]][[fold]][["train"]][["y"]] = subsetStation(dataOccCV[[region]][[fold]][["train"]][["y"]], station.id = codigos[,region])
            dataOccCV[[region]][[fold]][["test"]][["y"]] = subsetStation(dataOccCV[[region]][[fold]][["test"]][["y"]], station.id = codigos[,region])
            dataRegCV[[region]][[fold]][["train"]][["y"]] = subsetStation(dataRegCV[[region]][[fold]][["train"]][["y"]], station.id = codigos[,region])
            dataRegCV[[region]][[fold]][["test"]][["y"]] = subsetStation(dataRegCV[[region]][[fold]][["test"]][["y"]], station.id = codigos[,region])
            #Fase preparar datos train
            trainOcc = prepareData(dataOccCV[[region]][[fold]][["train"]][["x"]], dataOccCV[[region]][[fold]][["train"]][["y"]],
            spatial.predictors = list("n" = nComp, "which.combine" = getVarNames(dataOccCV[[region]][[fold]][["train"]][["x"]])))

            trainReg = prepareData(dataRegCV[[region]][[fold]][["train"]][["x"]], dataRegCV[[region]][[fold]][["train"]][["y"]],
            spatial.predictors = list("n" = nComp, "which.combine" = getVarNames(dataRegCV[[region]][[fold]][["train"]][["x"]])))

            #Fase preparar datos test
            testOcc = prepareNewData(dataOccCV[[region]][[fold]][["test"]][["x"]], trainOcc)
            realidadOcc = rbind(realidadOcc, dataOccCV[[region]][[fold]][["test"]][["y"]][["Data"]])

            testReg = prepareNewData(dataRegCV[[region]][[fold]][["test"]][["x"]], trainReg)
            realidadReg = rbind(realidadReg, dataRegCV[[region]][[fold]][["test"]][["y"]][["Data"]])

            #Fase entrenamiento modelos
            modelos[[region]][[fold]] = list()
            modelos[[region]][[fold]][["Occ"]] = downscale.train(trainOcc, method = "GLM", family = binomial(link = "logit"), condition = "GE", threshold = 0)
            modelos[[region]][[fold]][["Reg"]] = downscale.train(trainReg, method = "GLM", family = Gamma(link = "log"), condition = "GT", threshold = 1)

            #Predecir
            prediccionesOcc = downscale.predict(testOcc, modelos[[region]][[fold]][["Occ"]])
            tmpOcc = binaryGrid(prediccionesOcc, condition = "GT", threshold = 0.5)
            prediccionOcc = rbind(prediccionOcc, tmpOcc[["Data"]])

            prediccionesReg = downscale.predict(testReg, modelos[[region]][[fold]][["Reg"]])
            tmpReg = gridArithmetics(tmpOcc, prediccionesReg, operator = "*")
            prediccionReg = rbind(prediccionReg, tmpReg[["Data"]])
        }
        yOccPredGLM[[region]] = prediccionOcc
        yOccRealGLM[[region]] = realidadOcc
        yRegPredGLM[[region]] = prediccionReg
        yRegRealGLM[[region]] = realidadReg
    }
    timeElapsed = Sys.time() - start
    save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/GLM-KNN/GLM2.rda", collapse = ""))
    save(yOccPredGLM, yOccRealGLM, yRegPredGLM, yRegRealGLM, timeElapsed, file = paste0(ruta, "data/resultados/", estacion, "/precip/GLM-KNN/GLM2.rda", collapse = ""))
}