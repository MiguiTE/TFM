library(loadeR)
library(transformeR)
library(downscaleR)
library(randomForest)

ruta = "/home/jovyan/TFM/TFM/"

print("Carga observaciones y Reanalisis")
obs = loadStationData(dataset = paste0(ruta, "data/DCCMS_obs.zip",collapse = ""), var = "pr")
load(paste0(ruta, "data/proyeccionInterim.rda", collapse = ""))
#interim[["Variable"]][["varName"]] = c("hur@85000", "hus@85000", "ta@85000", "psl", "ua@25000")
#interim[["Variable"]][["level"]] = c(85000, 85000, 85000,    NA, 25000)
interim[["Variable"]][["varName"]] = c("hur@25000", "hur@50000", "hur@85000", "hus@25000", "hus@50000", "hus@85000", "ta@25000", "ta@50000", "ta@85000", "ua@25000", "ua@50000", "ua@85000", "va@25000", "va@50000", "va@85000", "psl")
interim[["Variable"]][["level"]] = c(rep(c(25000, 50000, 85000),5), NA)


#gcms = c("Canes", "Cnrm", "Gfdl", "Miroc", "MpiLr", "MpiMr", "Noresm")
gcms = c("Canes", "Cnrm", "Gfdl", "Miroc", "MpiLr", "MpiMr")


hist = list()
hist[["Canes"]] = "canesm2.historical"
hist[["Cnrm"]] = "cnrm.historial"
hist[["Gfdl"]] = "gfdl.historical"
hist[["Miroc"]] = "miroc.historical"
hist[["MpiLr"]] = "mpi.lr.historical"
hist[["MpiMr"]] = "mpi.mr.historical"
hist[["Noresm"]] = "noresm.historical"

rcp85 = list()
rcp85[["Canes"]] = "canesm2.rcp85"
rcp85[["Cnrm"]] = "cnrm.rcp85"
rcp85[["Gfdl"]] = "gfdl.rcp85"
rcp85[["Miroc"]] = "miroc.rcp85"
rcp85[["MpiLr"]] = "mpi.lr.rcp85"
rcp85[["MpiMr"]] = "mpi.mr.rcp85"
rcp85[["Noresm"]] = "noresm.rcp85"

patrones = list()
patrones[["P1"]] = c("psl", "ta@85000")
patrones[["P2"]] = c("psl", "hus@85000")
patrones[["P3"]] = c("psl", "ta@85000", "hus@85000")
patrones[["P4"]] = c("psl", "ta@85000", "ua@25000","hus@85000")
patrones[["P5"]] = c("psl", "hur@85000")
patrones[["P6"]] = c("psl", "hur@85000", "ua@25000")
patrones[["P7"]] = c("psl", "ta@85000", "ua@25000","hus@85000", "hur@85000")
patrones[["P8"]] = c("hur@25000", "hur@50000", "hur@85000", "hus@25000", "hus@50000", "hus@85000", "ta@25000", "ta@50000", "ta@85000", "ua@25000", "ua@50000", "ua@85000", "va@25000", "va@50000", "va@85000", "psl")
opcionPatrones = c("P8")

nvecinos = c(1, 16, 25)

modelos = list()


for (patron in opcionPatrones){
    for(gcm in gcms){
        #Cargar datos
        print(paste0("Patron ", patron, ", Gcm: ", gcm, collapse = ""))
        load(paste0(ruta,"data/proyeccion",gcm,".rda", collapse = ""))

        #Misma escala temporal predictores
        #historico = intersectGrid.time(get(hist[[gcm]]), interim, which.return = 1)
        historico = get(hist[[gcm]])
        #escalado
        centrado = scaleGrid(historico, base = historico, ref = interim, time.frame = "monthly", type = "center")
        final = scaleGrid(centrado, base = interim, time.frame = "none", type = "standardize")

        # Tener la misma escala temporal de predicando y predictor
        prd = getTemporalIntersection(obs, final, which.return = "prd")
        obs.reg = getTemporalIntersection(obs, final, which.return = "obs")
        obs.occ = binaryGrid(obs.reg, condition = "GT", threshold = 1)

        #preparar datos
        trainReg = prepareData(prd, obs.reg, spatial.predictors = list(which.combine = patrones[[patron]], v.exp = 0.95))
        trainOcc = prepareData(prd, obs.occ, spatial.predictors = list(which.combine = patrones[[patron]], v.exp = 0.95))
        #prediccion para rcp85
        #escalado
        centrado = scaleGrid(get(rcp85[[gcm]]), base = historico, ref = interim, time.frame = "monthly", type = "center")
        final = scaleGrid(centrado, base = interim, time.frame = "none", type = "standardize")

        #preparar datos
        test = prepareNewData(final, trainReg)

        print("Comienza entrenamiento")
        modelos[["Reg"]] = list()
        modelos[["Occ"]] = list()
        prediccionOcc = c()
        prediccionReg = c()
        modelos = list()
        modelos[["Occ"]] = list()
        modelos[["Reg"]] = list()

        numeroEstaciones = dim(trainReg[["y"]][["Data"]])[2]
        for(k in 1:numeroEstaciones){
            dfTrainOcc = data.frame("x"=trainOcc[["pca"]][["COMBINED"]][[1]][["PCs"]])
            dfTrainOcc["y"] = as.factor(trainOcc[["y"]][["Data"]][,k])
            modelos[["Occ"]][[k]] = randomForest(y ~ ., data = dfTrainOcc, ntree = 100)

            dfTest = data.frame("x" = test[["x.global"]][["member_1"]])
            prediccionOcc = cbind(prediccionOcc, as.numeric(predict(modelos[["Occ"]][[k]], dfTest, type="prob")[,2]>0.5))

            dfTrainReg = data.frame("x"=trainReg[["pca"]][["COMBINED"]][[1]][["PCs"]])
            dfTrainReg["y"] = trainReg[["y"]][["Data"]][,k]
            diasLluvia = which(dfTrainReg["y"] > 1)
            modelos[["Reg"]][[k]] = randomForest(y ~ ., data = dfTrainReg, subset = diasLluvia, ntree = 100)

            prediccionReg = cbind(prediccionReg, predict(modelos[["Reg"]][[k]], dfTest))
        }
        prediccionFinal = prediccionOcc * prediccionReg
        save(modelos, file = paste0(ruta, "data/proyeccion/modelos/modelos", gcm, "patron", patron, "RFPCs.rda"))
        save(prediccionReg, prediccionOcc, prediccionFinal, file = paste0(ruta, "data/proyeccion/resultados/pred", gcm, "patron", patron, "RFPCs.rda"))
        rm(list = c(hist[[gcm]], rcp85[[gcm]]))
    }
}