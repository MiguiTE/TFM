# Importar paquetes

#Para descargar los datos necesitamos el paquete `loadeR` y transformeR del conjunto de paquetes `climate4R` del **Grupo de meteorología de Santander**.
library(loadeR)
library(transformeR)

# Gestión de directorios
ruta = "/home/jovyan/TFM/TFM/"


#Iniciar sesión en [UDG-TAP](http://www.meteo.unican.es/udg-tap/home). Además guarda la ruta en donde se encuentra el conjunto de datos.
name = readline(prompt = "Nombre de usuario UDG: ")
pass = readline(prompt = "Contraseña UDG: ")
loginUDG(username = name, password = pass)


## OBS (DCCMS)
#obs = loadStationData(dataset = "data/DCCMS_obs.zip", var = "pr")

## REA (ERA-Interim)
dataset = "http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml"

variables = c("hur850", "hus850", "ta850", "psl", "ua250")
nvars = length(variables)
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6), time = "DD", aggr.d = "mean") #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
interim = makeMultiGrid(grid.list)
grid = getGrid(grid.list[[1]])

save(interim, file = paste0(ruta, "data/proyeccionInterim.rda"))
rm(interim)

variables = c("hur@85000", "hus@85000", "ta@85000", "ua@25000", "psl")

# GCM (CANESM2, historical)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CCCMA/CANESM2/historical/day/cccma_canesm2_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(22, 46), latLim = c(-22, 0)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
canesm2.historical = makeMultiGrid(grid.list)


## GCM (CANESM2, RCP85)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CCCMA/CANESM2/rcp85/day/cccma_canesm2_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(22, 46), latLim = c(-22, 0)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
canesm2.rcp85 = makeMultiGrid(grid.list)

save(canesm2.historical, canesm2.rcp85, file =paste0(ruta, "data/proyeccionCanes.rda"))
rm(canesm2.rcp85, canesm2.historical)
## GCM(CNRM, historical)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CNRM-CERFACS/CNRM-CM5/historical/day/cnrm-cerfacs_cnrm-cm5_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
cnrm.historial = makeMultiGrid(grid.list)


## GCM (CNRM, RCP85)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CNRM-CERFACS/CNRM-CM5/rcp85/day/cnrm-cerfacs_cnrm-cm5_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
cnrm.rcp85 = makeMultiGrid(grid.list)

save(cnrm.historial, cnrm.rcp85, file =paste0(ruta, "data/proyeccionCnrm.rda"))
rm(cnrm.historial, cnrm.rcp85)

## GCM (GFDL, historical)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/NOAA-GFDL/GFDL-ESM2M/historical/day/noaa-gfdl_gfdl-esm2m_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
gfdl.historical = makeMultiGrid(grid.list)


## GCM (GFDL, RCP85)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/NOAA-GFDL/GFDL-ESM2M/rcp85/day/noaa-gfdl_gfdl-esm2m_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
gfdl.rcp85 = makeMultiGrid(grid.list)

save(gfdl.historical, gfdl.rcp85, file = paste0(ruta, "data/proyeccionGfdl.rda"))
rm(gfdl.historical, gfdl.rcp85)

## GCM (MIROC, historical)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/MIROC/MIROC-ESM/historical/day/miroc_miroc-esm_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
miroc.historical = makeMultiGrid(grid.list)


## GCM (MIROC, RCP85)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/MIROC/MIROC-ESM/rcp85/day/miroc_miroc-esm_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
miroc.rcp85 = makeMultiGrid(grid.list)

save(miroc.historical, miroc.rcp85, file = paste0(ruta, "data/proyeccionMiroc.rda"))
rm(miroc.historical, miroc.rcp85)

## GCM (MPI-LR, historical)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/MPI-M/MPI-ESM-LR/historical/day/mpi-m_mpi-esm-lr_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
mpi.lr.historical = makeMultiGrid(grid.list)


## GCM (MPI-LR, RCP85)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/MPI-M/MPI-ESM-LR/rcp85/day/mpi-m_mpi-esm-lr_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
mpi.lr.rcp85 = makeMultiGrid(grid.list)

save(mpi.lr.historical, mpi.lr.rcp85, file = paste0(ruta, "data/proyeccionMpiLr.rda"))
rm(mpi.lr.historical, mpi.lr.rcp85)

## GCM (MPI-MR, historical)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/MPI-M/MPI-ESM-MR/historical/day/mpi-m_mpi-esm-mr_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
mpi.mr.historical = makeMultiGrid(grid.list)


## GCM (MPI-MR, RCP85)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/MPI-M/MPI-ESM-MR/rcp85/day/mpi-m_mpi-esm-mr_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
mpi.mr.rcp85 = makeMultiGrid(grid.list)

save(mpi.mr.historical, mpi.mr.rcp85, file = paste0(ruta, "data/proyeccionMpiMr.rda"))
rm(mpi.mr.historical, mpi.mr.rcp85)


## GCM (NORESM, historical)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/NCC/NORESM1-M/historical/day/ncc_noresm1-m_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
noresm.historical = makeMultiGrid(grid.list)


## GCM (NORESM, RCP85)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/NCC/NORESM1-M/rcp85/day/ncc_noresm1-m_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
grid.list = intersectGrid.time(grid.list, which.return = seq(1, nvars))
noresm.rcp85 = makeMultiGrid(grid.list)

save(noresm.historical, noresm.rcp85, file = paste0(ruta, "data/proyeccionNoresm.rda"))
rm(noresm.historical, noresm.rcp85)
