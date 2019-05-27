library(loadeR)
library(transformeR)
library(visualizeR)
library(RColorBrewer)
ruta = "/home/doctor/workspace/master/TFM/"

#carga malawi
obs = loadStationData(dataset = paste0(ruta, "data/DCCMS_obs.zip",collapse = ""), var = "pr")

clim = climatology(obs)
#pdf(paste0(ruta, "imagenes/malawi.pdf", collapse = ""))
#Observaciones
url = "http://meteo.unican.es/tds5/dodsC/eobs/e-obs_v17_0.25regular.ncml"
colores = c("#a4eff7", "#85c99c", "#28871f", "#616d0d", "#935f00", "#b77600", "#c88200", "#d79d33", "#f1debb")
orografia = loadGridData(dataset = url, var = "elevation", lonLim = c(28, 40), latLim = c(-20, -6))
orografia$Data[is.na(orografia$Data)] = 0
spatialPlot(orografia, backdrop.theme = "countries", col.regions  = colorRampPalette(colores)) + spatialPlot(climatology(obs))
#dev.off()