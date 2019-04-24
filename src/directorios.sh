mkdir imagenes
mkdir data
cd data
mkdir datosEstaciones modelos resultados
for tipo in $(ls)
do
    mkdir ${tipo}/primavera ${tipo}/verano ${tipo}/otoño ${tipo}/invierno

    for dir in $(ls ${tipo})
    do
        mkdir ${tipo}/${dir}/precip ${tipo}/${dir}/tmax ${tipo}/${dir}/tmin
        for subdir in $(ls ${tipo}/${dir})
        do
            mkdir ${tipo}/${dir}/${subdir}/GLM-KNN
            mkdir ${tipo}/${dir}/${subdir}/RF
            mkdir ${tipo}/${dir}/${subdir}/NNRF
        done
    done
done


