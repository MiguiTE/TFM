ruta = "/home/jovyan/TFM/TFM/"
estaciones = c("primavera", "verano", "otoño", "invierno")
modelo = "RF2PCsEst.rda"
for (estacion in estaciones){
    ruta2 = paste0(ruta, "data/modelos/", estacion, "/precip/RF")
    load(paste0(ruta2, "/", modelo))
    for(region in 1:length(modelos)){
        for (fold in names(modelos[[region]])) {
            #Occurencia
            pdf(paste0(ruta, "imagenes/oob", substr(modelo,1,9), estacion, "region", region, "fold", fold, "occ.pdf"))
            tmp = modelos[[region]][[fold]][["Occ"]]
            par(mfrow = c(2,1))
            plot(tmp[[1]]$err.rate[, 1], type = "l", xlab = "no. trees", ylab = "OOB error", main = paste("Estación", estacion, "región", region, "estación 1"))
            plot(tmp[[2]]$err.rate[, 1], type = "l", xlab = "no. trees", ylab = "OOB error", main = paste("Estación", estacion, "región", region, "estación 2"))
            dev.off()
            #regresion
            pdf(paste0(ruta, "imagenes/oob", substr(modelo,1,9), estacion, "region", region, "fold", fold, "reg.pdf"))
            tmp = modelos[[region]][[fold]][["Reg"]]
            par(mfrow = c(2,1))
            plot(tmp[[1]]$mse, type = "l", xlab = "no. trees", ylab = "OOB error", main = paste("Estación", estacion, "región", region, "estación 1"))
            plot(tmp[[2]]$mse, type = "l", xlab = "no. trees", ylab = "OOB error", main = paste("Estación", estacion, "región", region, "estación 2"))
            dev.off()
        }
    }
}