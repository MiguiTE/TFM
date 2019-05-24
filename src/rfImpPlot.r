library(tidyverse)

ruta = "/home/doctor/workspace/master/TFM/"

gcms = c("Canes", "Cnrm", "Gfdl", "Miroc", "MpiLr", "MpiMr", "Noresm")
variables = c("psl", "T850", "U250","Q850", "R850")
patron = "P7"
nvecinos = c(1, 25)

df = list()
for (n in nvecinos){
  df[[as.character(n)]] = data.frame()
    for(gcm in gcms){
      load(paste0(ruta, "data/proyeccion/modelos/importancia", gcm, "patron", patron, "nvecions",n, "RF.rda"))
      tmp = data.frame(cbind(x = variables, cat = rep(gcm, length(variables))), impFinalOcc, impFinalReg)
      df[[as.character(n)]] = rbind(df[[as.character(n)]], tmp)
    }
}


importancias = c("MeanDecreaseGini", "MeanDecreaseAccuracy", "X.IncMSE", "IncNodePurity")
GUARDA = F
for (n in nvecinos) {
  for (importancia in importancias) {
    if (GUARDA){
      pdf(paste0(ruta, "imagenes/importancia",importancia, "vecinos", n, ".pdf", collapse = "")) 
    }
    print(ggplot(df[[as.character(n)]], aes(x = x, y = get(importancia), color = cat)) + 
      geom_segment(aes(x=x, xend=x, y=0, yend=get(importancia)), color = "skyblue") +
      geom_point(size=4, alpha=0.6) +
      theme_light() +
      coord_flip() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      labs(x = "", y ="Importancia", title = paste(importancia, "vecinos", n), color = "ECM"))
    if (GUARDA){
      dev.off()
    }
  }
}
