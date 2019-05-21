library(loadeR)
library(transformeR)
library(visualizeR)

ruta = "/home/doctor/workspace/master/TFM/"

#carga malawi
obs = loadStationData(dataset = paste0(ruta, "data/DCCMS_obs.zip",collapse = ""), var = "pr")

clim = climatology(obs)
pdf(paste0(ruta, "imagenes/malawi.pdf", collapse = ""))
spatialPlot(clim, backdrop.theme = "countries", main = "Estaciones Malawi")
dev.off()
