rm(list=ls())
# Cargar las librerias
# Cargar la imagen en R
## Cargar librerias necesarias ##
library(png) # Libreria para leer PNG's
library(flextable)# LIbreria para hacer tablas bonitas
library(scatterplot3d) #gráficar en 3D
library(rsvg) # Manipular archivos SVG
library(jpeg) # Comprimir el SVG a JPEG
library(rgl) # OpenGL pero para los gráficos en 3D
library(corrplot) # Para gráficar las correlaciones
library(NbClust) # Correr el Kmedias
library(factoextra) # Obtener exploraciones para el kmedias
library(JuliaCall) # Para pasarme objetos a Julia
#### Función para leer imagenes y mapearlas ###
pixeles = function(imagen){
  # Extraer los datos
  datos = cbind.data.frame(R = as.vector(imagen[ , ,1]), # Extraer inf de R
                           G = as.vector(imagen[ , ,2]), # Extraer inf de G
                           B = as.vector(imagen[ , ,3])) # Extraer inf de B
  # Extraer las coordenadas de cada pixel para re-mapearlos
  #Filas
  nf = dim(imagen)[1]
  #Columnas
  nc = dim(imagen)[2]
  # Hacer el ordenamiento para remapearlos
  X = rep(nf:1,nc)
  Y = rep(1:nc,each = nf)
  datos$X = X
  datos$Y = Y
  # Añadir el vector de colores al conjunto de datos
  datos$color = rgb(datos$R,datos$G,datos$B)
  # Regresar el objeto transformado
  return(datos)
}

### Función para llamar el codigo fuente de Julia ###
#julia_source("Kmedias Angel Julia.jl")
julia_source("Kmedias julia julia.jl")
#### Imagen 1: Denisse Guerrero ####
### Cargando la imagen desde la ruta local
ruta1 = "imagenes/IM2.jpg" # Ruta local de la imagen
imagen1 = readJPEG(ruta1) # Descomponer la imagen en sus canales RGB
datos1 = pixeles(imagen1)
### Cargar las cosas de Julia
re = proc.time()
prueba1 = julia_call("kmedias",datos1[,1:3],10) |> as.vector()
proc.time()-re

re = proc.time()
prueba1 = kmeans(datos1[,1:3],10)
proc.time()-re


### Agegar las etiquetas
datos1$etiqueta = prueba1$cluster
#datos1$etiqueta = prueba1$Cluster
datos1$ecolor= ""

for(i in unique(datos1$etiqueta)){
  ### Calcular la media
  media = colMeans(datos1[datos1$etiqueta==i ,1:3])
  ### Calcular y agregar el nuevo color
  datos1$ecolor[datos1$etiqueta==i] = rgb(media[1],media[2],media[3])
}


plot3d(x = datos1$Y,
       y = 1,
       z = datos1$X,
       col = datos1$ecolor)




