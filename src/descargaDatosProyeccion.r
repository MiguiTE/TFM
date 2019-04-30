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
cmip.canes.hist.url = "http://meteo.unican.es/tds5/dodsC/cmip5/CCCMA/CANESM2/historical/day/cccma_canesm2_historical_r1i1p1.ncml"
cmip.canes.fut.url = "http://meteo.unican.es/tds5/dodsC/cmip5/CCCMA/CANESM2/rcp85/day/cccma_canesm2_rcp85_r1i1p1.ncml"

cmip.mpi.hist.url = "http://meteo.unican.es/tds5/dodsC/cmip5/MPI-M/MPI-ESM-LR/historical/day/mpi-m_mpi-esm-lr_historical_r1i1p1.ncml"
cmip.mpi.fut.url = "http://meteo.unican.es/tds5/dodsC/cmip5/MPI-M/MPI-ESM-LR/rcp85/day/mpi-m_mpi-esm-lr_rcp85_r1i1p1.ncml"

#variables a utilizar
di = dataInventory(cmip.canes.hist.url)
presiones = di$hus$Dimensions$level$Values
variables = c(paste("hus", presiones, sep = "@"), paste("hur", presiones, sep = "@"), paste("ta", presiones, sep = "@"), 
paste("ua", presiones, sep = "@"), paste("va", presiones, sep = "@"), paste("zg", presiones, sep = "@"), "psl", "tas")


#Descarga los datos de las variables y guarda
grid.list <- lapply(variables, function(x) {
  loadGridData(dataset = cmip.canes.hist.url,
                var = x,
                lonLim = c(-10,32), # 22 puntos en total
                latLim = c(36,72),  # 19 puntos en total
                years = 1941:2005)
})
xcmip.canes.hist = makeMultiGrid(grid.list)
save(xcmip.canes.hist, file = paste0(ruta, "data/cmip5_canes_hist.rda", collapse = ""))


grid.list <- lapply(variables, function(x) {
  loadGridData(dataset = cmip.canes.fut.url,
                var = x,
                lonLim = c(-10,32), # 22 puntos en total
                latLim = c(36,72),  # 19 puntos en total
                years = 1941:2005)
})
xcmip.canes.fut = makeMultiGrid(grid.list)
save(xcmip.canes.fut, file = paste0(ruta, "data/cmip5_canes_fut.rda", collapse = ""))


grid.list <- lapply(variables, function(x) {
  loadGridData(dataset = cmip.mpi.hist.url,
                var = x,
                lonLim = c(-10,32), # 22 puntos en total
                latLim = c(36,72),  # 19 puntos en total
                years = 1941:2005)
})
xcmip.mpi.hist = makeMultiGrid(grid.list)
save(xcmip.mpi.hist, file = paste0(ruta, "data/cmip5_mpi_hist.rda", collapse = ""))


grid.list <- lapply(variables, function(x) {
  loadGridData(dataset = cmip.mpi.fut.url,
                var = x,
                lonLim = c(-10,32), # 22 puntos en total
                latLim = c(36,72),  # 19 puntos en total
                years = 1941:2005)
})
xcmip.mpi.fut = makeMultiGrid(grid.list)
save(xcmip.mpi.fut, file = paste0(ruta, "data/cmip5_mpi_fut.rda", collapse = ""))


