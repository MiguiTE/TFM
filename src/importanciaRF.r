library(randomForest)

ruta = "/home/jovyan/TFM/TFM/"

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
patrones[["P7"]] = c("psl", "ta@85000", "ua@25000","hus@85000", "hur@85000")
patrones[["P8"]] = c("hur@25000", "hur@50000", "hur@85000", "hus@25000", "hus@50000", "hus@85000", "ta@25000", "ta@50000", "ta@85000", "ua@25000", "ua@50000", "ua@85000", "va@25000", "va@50000", "va@85000", "psl")
opcionPatrones = c("P7")

nvecinos = c(1, 25)


for (n in nvecinos){
    impFinalOcc = matrix(0, ncol = 2, nrow = 5)
    impFinalReg = matrix(0, ncol = 2, nrow = 5)
    impOcc = matrix(0, ncol = 2, nrow = 5)
    impReg = matrix(0, ncol = 2, nrow = 5)
    indices = seq(1, 5 * n + 1, n)
    for (patron in opcionPatrones){
        for(gcm in gcms){
            load(paste0(ruta, "data/proyeccion/modelos/modelos", gcm, "patron", patron, "nvecions",n, "RFImp.rda"))
            for (k in 1:length(modelos[["Occ"]])){
                tmpOcc = modelos[["Occ"]][[k]][["importance"]][,c(3,4)]
                tmpReg = modelos[["Reg"]][[k]][["importance"]][,]
                for (i in 1:(length(indices) - 1)){
                    start = indices[i]
                    stop = indices[i+1] - 1
                    impOcc[i,] = if (n == 1) tmpOcc[seq(start, stop, 1),] else colMeans(tmpOcc[seq(start, stop, 1),])
                    impReg[i,] = if (n == 1) tmpReg[seq(start, stop, 1),] else colMeans(tmpReg[seq(start, stop, 1),]) 
                }
                impFinalOcc = impFinalOcc + impOcc
                impFinalReg = impFinalReg + impReg
            }
            impFinalOcc = impFinalOcc / k
            impFinalReg = impFinalReg / k
            colnames(impFinalOcc) = colnames(modelos[["Occ"]][[k]][["importance"]])[c(3,4)]
            colnames(impFinalReg) = colnames(modelos[["Reg"]][[k]][["importance"]])
            save(impFinalOcc, impFinalReg, file = paste0(ruta, "data/proyeccion/modelos/importancia", gcm, "patron", patron, "nvecions",n, "RF.rda"))
        }
    }
}
