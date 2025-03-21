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

table(datos1$buying)
table(datos1$maint)
table(datos1$doors)
table(datos1$persons) 
table(datos1$lug_boot) 
table(datos1$safety) 
table(datos1$class)



# Exportar al formato ARFF
write.arff(datos1, file = "car-data.arff")


# Cargar el tic-tac-toe
ruta2 = "tic-tac-toe.csv"
datos2 = as.data.frame(read.csv(ruta2))
for(i in 1:ncol(datos2)){
  # Categorizar
  datos2[,i] = factor(datos2[,i])
}
# Exportar al formato ARFF
write.arff(datos2, file = "tic-tac-toe.arff")

# Cargar el de car-data
ruta3 = "zoo2.csv"
datos3 = as.data.frame(read.csv(ruta3))
for(i in 1:ncol(datos3)){
  # Categorizar
  datos3[,i] = factor(datos3[,i])
}
# Exportar al formato ARFF
write.arff(datos3, file = "zoo2.arff")

