library(loadeR)
library(transformeR)
library(visualizeR)


ruta = "/home/doctor/workspace/master/TFM/"
#Predictores
load(paste0(ruta, "data/tmp.rda", collapse = ""))

vars = getVarNames(tmp)

clim = list()
for (var in vars){
  figura = subsetGrid(tmp, var = var)
  clim[[var]] = climatology(figura)
}

spatialPlot(clim[[vars[1]]], backdrop.theme = "countries", main = vars[1])
spatialPlot(clim[[vars[2]]], backdrop.theme = "countries", main = vars[2])
spatialPlot(clim[[vars[3]]], backdrop.theme = "countries", main = vars[3])
spatialPlot(clim[[vars[4]]], backdrop.theme = "countries", main = vars[4])
spatialPlot(clim[[vars[5]]], backdrop.theme = "countries", main = vars[5])
spatialPlot(clim[[vars[6]]], backdrop.theme = "countries", main = vars[6])
spatialPlot(clim[[vars[7]]], backdrop.theme = "countries", main = vars[7])


#Observaciones
library(RColorBrewer)
library(sp)
obs = loadStationData(paste0(ruta, "data/VALUE_ECA_86_v2.zip",collapse = ""), var = "precip")
sp = SpatialPoints(getCoordinates(obs))
url = "http://meteo.unican.es/tds5/dodsC/eobs/e-obs_v17_0.25regular.ncml"
colores = c("#a4eff7", "#85c99c", "#28871f", "#616d0d", "#935f00", "#b77600", "#c88200", "#d79d33", "#f1debb")
orografia = loadGridData(dataset = url, var = "elevation", lonLim = c(-10, 32), latLim = c(36,72))
orografia$Data[is.na(orografia$Data)] = 0
spatialPlot(orografia, backdrop.theme = "countries", col.regions  = colorRampPalette(colores)) + spatialPlot(climatology(obs))
