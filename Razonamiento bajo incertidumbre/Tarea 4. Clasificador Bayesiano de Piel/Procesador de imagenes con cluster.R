# Experimento de las imagenes con cluster
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

#### Comprimir y operar los datos: ####

### Cargando la imagen desde la ruta local
ruta = "imagenes/IM3.1.jpg" # Ruta local de la imagen
imagen = readJPEG(ruta) # Descomponer la imagen en sus canales RGB
#imagen = readPNG(ruta)
str(imagen) # EL archivo cargado es un CUBO de información

  

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
  # Regresar el objeto transformado
  return(datos)
}

datos = pixeles(imagen)

# Hacer el cluster. con el K sugerido.
n = 20 #Numero de clusters
set.seed(2024)
km_clusters <- kmeans(x = datos[,1:3], centers = n, nstart = 50)
# Añadir a los datos el cluster
datos$etiqueta = km_clusters$cluster

# Obtener las medias del color
medias = cbind.data.frame(R = tapply(datos[,1],as.factor(datos$etiqueta),mean),
                          G = tapply(datos[,2],as.factor(datos$etiqueta),mean),
                          B = tapply(datos[,3],as.factor(datos$etiqueta),mean))
# Vector RGB de las medias
coloresmed = rgb(medias$R,medias$G,medias$B)
names(coloresmed) = 1:n
#### Usando el vector como diccionario para poner los colores
datos$etiqueta = coloresmed[km_clusters$cluster]


# Gráfico de la imagen reconstruida:
plot3d(x = datos$X,y = datos$Y,z = 1,
       col = datos$etiqueta,
       size = 10,
       xlim = c(0, dim(imagen)[1]),
       ylim = c(0, dim(imagen)[2]))


