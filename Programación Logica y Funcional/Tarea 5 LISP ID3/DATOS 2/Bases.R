rm(list=ls())
# LIbrary #
library(foreign)
# Car-data
ruta1 = "car-data.csv"
datos1 = as.data.frame(read.csv(ruta1))
for(i in 1:ncol(datos1)){
  # Categorizar
  datos1[,i] = factor(datos1[,i])
}


# Exportar al formato ARFF
write.arff(datos1, file = "car-data.arff",
          relation = "class")


# Cargar el tic-tac-toe
ruta2 = "tic-tac-toe.csv"
datos2 = as.data.frame(read.csv(ruta2))
for(i in 1:ncol(datos2)){
  # Categorizar
  datos2[,i] = factor(datos2[,i])
}
# Exportar al formato ARFF
write.arff(datos2, file = "tic-tac-toe.arff",
           relation = "Class")

# Cargar el de car-data
ruta3 = "zoo2.csv"
datos3 = as.data.frame(read.csv(ruta3))
for(i in 1:ncol(datos3)){
  # Categorizar
  datos3[,i] = factor(datos3[,i])
}
str(datos3)

# Exportar al formato ARFF
write.arff(datos3, file = "zoo2.arff",
           relation = "Tipo")

