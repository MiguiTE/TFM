# Importar paquetes

#Para descargar los datos necesitamos el paquete `loadeR` y transformeR del conjunto de paquetes `climate4R` del **Grupo de meteorología de Santander**.

library(loadeR)
library(transformeR)

# Gestión de directorios
ruta = "/home/jovyan/TFM/TFM/"


#Iniciar sesión en [UDG-TAP](http://www.meteo.unican.es/udg-tap/home). Además guarda la ruta en donde se encuentra el conjunto de datos.
#name = readline(prompt = "Nombre de usuario UDG: ")
#pass = readline(prompt = "Contraseña UDG: ")
loginUDG(username = "Miguel", password = "sherpa")


## OBS (DCCMS)
#obs = loadStationData(dataset = "data/DCCMS_obs.zip", var = "pr")

## REA (ERA-Interim)
dataset = "http://meteo.unican.es/tds5/dodsC/interim/interim075.ncml"

variables = c("hur850", "hus850", "ta850", "psl", "ua250")
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6), time = "DD", aggr.d = "mean") #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
interim = makeMultiGrid(grid.list)

save(interim, paste0(ruta, "proyeccionInterim.rda"))
rm(interim)
variables = sapply(variables, function(var){
  if(grepl("0", var)){
    return(paste0(var, "00", collapse=""))
  }else{
    return(var)
  }
})
## GCM (CANESM2, historical)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CCCMA/CANESM2/historical/day/cccma_canesm2_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
canesm2.historical = makeMultiGrid(grid.list)


## GCM (CANESM2, RCP85)
dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CCCMA/CANESM2/rcp85/day/cccma_canesm2_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
canesm2.rcp85 = makeMultiGrid(grid.list)

save(canesm2.historical, canesm2.rcp85, paste0(ruta, "proyeccionCanes.rda"))
rm(canesm2.rcp85, canesm2.historical)
## GCM(CNRM, historical)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/CNRM-CERFACS/CNRM-CM5/historical/day/cnrm-cerfacs_cnrm-cm5_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
cnrm.historial = makeMultiGrid(grid.list)


## GCM (CANESM2, RCP85)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/CNRM-CERFACS/CNRM-CM5/rcp85/day/cnrm-cerfacs_cnrm-cm5_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
cnrm.rcp85 = makeMultiGrid(grid.list)

save(cnrm.historial, cnrm.rcp85, paste0(ruta, "proyeccionCnrm.rda"))
rm(cnrm.historial, cnrm.rcp85)

## GCM (GFDL, historical)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/NOAA-GFDL/GFDL-ESM2M/historical/day/noaa-gfdl_gfdl-esm2m_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
gfdl.historical = makeMultiGrid(grid.list)


## GCM (CANESM2, RCP85)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/NOAA-GFDL/GFDL-ESM2M/rcp85/day/noaa-gfdl_gfdl-esm2m_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
gfdl.rcp85 = makeMultiGrid(grid.list)

save(gfdl.historical, gfdl.rcp85, paste0(ruta, "proyeccionGfdl.rda"))
rm(gfdl.historical, gfdl.rcp85)

## GCM (MIROC, historical)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/MIROC/MIROC-ESM/historical/day/miroc_miroc-esm_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
miroc.historical = makeMultiGrid(grid.list)


## GCM (MIROC, RCP85)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/MIROC/MIROC-ESM/rcp85/day/miroc_miroc-esm_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
miroc.rcp85 = makeMultiGrid(grid.list)

save(miroc.historical, miroc.rcp85, paste0(ruta, "proyeccionMiroc.rda"))
rm(miroc.historical, miroc.rcp85)

## GCM (MPI-LR, historical)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/MPI-M/MPI-ESM-LR/historical/day/mpi-m_mpi-esm-lr_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
mpi.lr.historical = makeMultiGrid(grid.list)


## GCM (MPI-LR, RCP85)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/MPI-M/MPI-ESM-LR/rcp85/day/mpi-m_mpi-esm-lr_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
mpi.lr.rcp85 = makeMultiGrid(grid.list)

save(mpi.lr.historical, mpi.lr.rcp85, paste0(ruta, "proyeccionMpiLr.rda"))
rm(mpi.lr.historical, mpi.lr.rcp85)

## GCM (MPI-MR, historical)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/MPI-M/MPI-ESM-MR/historical/day/mpi-m_mpi-esm-mr_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
mpi.mr.historical = makeMultiGrid(grid.list)


## GCM (MPI-MR, RCP85)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/MPI-M/MPI-ESM-MR/rcp85/day/mpi-m_mpi-esm-mr_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
mpi.mr.rcp85 = makeMultiGrid(grid.list)

save(mpi.mr.historical, mpi.mr.rcp85, paste0(ruta, "proyeccionMpiMr.rda"))
rm(mpi.mr.historical, mpi.mr.rcp85)


## GCM (NORESM, historical)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/NCC/NORESM1-M/historical/day/ncc_noresm1-m_historical_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
noresm.historical = makeMultiGrid(grid.list)


## GCM (NORESM, RCP85)
dataset = "http://meteo.unican.es/tds5/catalogs/cmip5/cmip5ESMSubset.html?dataset=cmip5/NCC/NORESM1-M/rcp85/day/ncc_noresm1-m_rcp85_r1i1p1.ncml"
grid.list = lapply(variables, function (var) {
  tmp = loadGridData(dataset, var = var,  lonLim = c(28, 40), latLim = c(-20, -6)) #Descarga
  grid = getGrid(tmp) #Sacar grid
  interpGrid(tmp, new.coordinates = list(x = seq(grid$x[1], grid$x[2], 2),
                                         y = seq(grid$y[1], grid$y[2], 2))) # Interpolarlo a 2º
})
noresm.rcp85 = makeMultiGrid(grid.list)

save(noresm.historical, noresm.rcp85, paste0(ruta, "proyeccionNoresm.rda"))
rm(noresm.historical, noresm.rcp85)
