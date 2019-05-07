library(loadeR)
library(transformeR)
library(downscaleR)

ruta = "/home/jovyan/TFM/TFM/"

print("Carga observaciones y Reanalisis")
obs = loadStationData(dataset = paste0(ruta, "data/DCCMS_obs.zip",collapse = ""), var = "pr")
load(paste0(ruta, "data/proyeccionInterim.rda", collapse = ""))
interim[["Variable"]][["varName"]] = c("hur@85000", "hus@85000", "ta@85000", "psl", "ua@25000")
interim[["Variable"]][["level"]] = c(85000, 85000, 85000,    NA, 25000)

gcms = c("Canes", "Cnrm", "Gfdl", "Miroc", "MpiLr", "MpiMr", "Noresm")

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
opcionPatrones = c("P2", "P5")

for (patron in opcionPatrones){
  for(gcm in gcms){
    #Cargar datos
    print(paste0("Patron ", patron, ", Gcm: ", gcm, collapse = ""))
    load(paste0(ruta,"data/proyeccion",gcm,".rda", collapse = ""))
    
    #Misma escala temporal predictores
    historico = get(hist[[gcm]])
    #historico = intersectGrid.time(get(hist[[gcm]]), interim, which.return = 1)
    #escalado
    centrado = scaleGrid(historico, base = historico, ref = interim, time.frame = "monthly", type = "center")
    final = scaleGrid(centrado, base = interim, time.frame = "none", type = "standardize")
    
    # Tener la misma escala temporal de predicando y predictor
    prd = getTemporalIntersection(obs, final, which.return = "prd")
    obs.reg = getTemporalIntersection(obs, final, which.return = "obs")
    obs.occ = binaryGrid(obs.reg, condition = "GT", threshold = 1)

    #preparar datos
    trainReg = prepareData(prd, obs.reg, local.predictors = list(vars = patrones[[patron]], n = 1))
    trainOcc = prepareData(prd, obs.occ, local.predictors = list(vars = patrones[[patron]], n = 1))
    nvars = dim(trainReg[["y"]][["Data"]])[2]
    
    print("Comienza entrenamiento")
    modelos = list()
    modelos[["Reg"]] = downscale.train(trainReg, method = "GLM", family = Gamma(link = "log"), condition = "GT", threshold = 1)
    modelos[["Occ"]] = downscale.train(trainOcc, method = "GLM", family = binomial(link = "logit"), condition = "GE", threshold = 0)
    save(modelos, file = paste0(ruta, "data/proyeccion/modelos/modelos", gcm, "patron", patron, "GLM.rda"))
    print("Finaliza entrenamiento")
    print("Comienza prediccion")
    #prediccion para rcp85
    #escalado
    centrado = scaleGrid(get(rcp85[[gcm]]), base = historico, ref = interim, time.frame = "monthly", type = "center")
    final = scaleGrid(centrado, base = interim, time.frame = "none", type = "standardize")

    #preparar datos
    test = prepareNewData(final, trainReg)

    #prediccion
    prediccionReg = downscale.predict(test, modelos[["Reg"]])
    prediccionOcc = binaryGrid(downscale.predict(test, modelos[["Occ"]]), condition = "GT", threshold = 0.5)
    prediccionFinal = gridArithmetics(prediccionReg, prediccionOcc, operator = "*")
    save(prediccionReg, prediccionOcc, prediccionFinal, file = paste0(ruta, "data/proyeccion/resultados/pred", gcm, "patron", patron, "GLM.rda"))
    rm(list = c(hist[[gcm]], rcp85[[gcm]]))
  }
}