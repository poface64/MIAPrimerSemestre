rm(list=ls())

#### IRIS ####
datos = iris[iris$Species!="setosa",]
# Nota que 0 = versicolor y 1 = virginica
datos$Species = as.numeric(datos$Species)-2
# Escribir como un data.frame
write.csv(datos,"irisvv.csv",row.names = F)

file.choose()

