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
    n_regions = length(dataRegCV)
    modelos = list()
    yRegPredGLM = list()
    yRegRealGLM = list()
    for(region in 1:n_regions){
        print(paste("Region:", region))
        modelos[[region]] = list()
        yRegPredGLM[[region]] = list()
        yRegRealGLM[[region]] = list()
        for(fold in names(dataRegCV[[region]])){
            print(paste("Fold:", fold))
            #Fase preparar datos train
            trainReg = prepareData(dataRegCV[[region]][[fold]][["train"]][["x"]], dataRegCV[[region]][[fold]][["train"]][["y"]],
            spatial.predictors = list("n" = nComp, "which.combine"=getVarNames(dataRegCV[[region]][[fold]][["train"]][["x"]])))

            #Fase preparar datos test
            testReg = prepareNewData(dataRegCV[[region]][[fold]][["test"]][["x"]], trainReg)
            yRegRealGLM[[region]] = rbind(yRegRealGLM[[region]], dataRegCV[[region]][[fold]][["test"]][["y"]])

            #Fase entrenamiento modelos
            modelos[[region]][[fold]] = list()
    
            modelos[[region]][[fold]][["Reg"]] = downscale.train(trainReg, method = "analogs", n.analogs=1, condition = "GE", threshold = 0)

            #Predecir
            yRegPredGLM[[region]] = rbind(yRegPredGLM[[region]], downscale.predict(testReg, modelos[[region]][[fold]][["Reg"]])[["Data"]])
        }
        save(modelos, file = paste0(ruta, "data/modelos/", estacion, "/precip/GLM-KNN/KNN.rda", collapse = ""))
        save(yRegPredGLM, yRegRealGLM, file = paste0(ruta, "data/resultados/", estacion, "/precip/GLM-KNN/KNN.rda", collapse = ""))
    }
}