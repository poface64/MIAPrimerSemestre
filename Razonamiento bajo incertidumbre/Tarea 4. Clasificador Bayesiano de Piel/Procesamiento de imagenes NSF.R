
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

### Función que extrae los pixeles
### Los separa en 3 canales de colores
### Calcula las posiciones de los pixeles para reconstruir la imagen
### Calcula el color de cada pixel a Hexadecimasl
### Comprime todo en un DataFrame

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

#### DATA DE ENTRENAMIENTO ####



#### Imagen 1 ####
### Cargando la imagen desde la ruta local
ruta1 = "imagenes/IM2.jpg" # Ruta local de la imagen
imagen1 = readJPEG(ruta1) # Descomponer la imagen en sus canales RGB
datos1 = pixeles(imagen1)
# Carita
subSI1 = datos1[(datos1$X>=610 &datos1$X<=815) &
                  (datos1$Y>=300 &datos1$Y<=490), ]
# Fondo
semilla = 2024
set.seed(semilla)
N = 100
mSI1 = subSI1[sample(1:nrow(subSI1),
                     size= N),]
mSI1$etiqueta = "Piel"
cond1 = (datos1$X >= 0 & datos1$X <= 350) & (datos1$Y >= 200 & datos1$Y <= 1152)
cond2 <- (datos1$X >= 900 & datos1$X <= 1152) & (datos1$Y >=  0 & datos1$Y <= 768 )
subNO1 = datos1[cond1|cond2, ]

# Extraigo una muestra random de los 10 puntos
set.seed(semilla)
mNO1 = subNO1[sample(1:nrow(subNO1),
                     size= N),]
mNO1$etiqueta = "Fondo"
### Conjunto de entrenamiento ###
X1 = rbind.data.frame(mSI1[,c(1:3,7)],
                      mNO1[,c(1:3,7)])

#### Imagen 2 ####
### Cargando la imagen desde la ruta local
ruta2 = "imagenes/IM3.1.jpg" # Ruta local de la imagen
imagen2 = readJPEG(ruta2) # Descomponer la imagen en sus canales RGB
datos2 = pixeles(imagen2)
# Carita
subSI2 = datos2[(datos2$X>=1200 &datos2$X<=1500) &
                  (datos2$Y>=480 &datos2$Y<=650), ]
### Mostrar la subregión de la carita

# Carita
set.seed(semilla)
mSI2 = subSI2[sample(1:nrow(subSI2),
                     size= N),]
mSI2$etiqueta = "Piel"
# Para el fondo
cond1 = (datos2$X >= 0 & datos2$X <= 1500) & (datos2$Y >= 0 & datos2$Y <= 200)
cond2 <- (datos2$X >= 0  & datos2$X <= 1500) & (datos2$Y >=  1100 & datos2$Y <= 1300 )
subNO2 = datos2[cond1|cond2, ]

# Extraigo una muestra random de los 10 puntos
set.seed(semilla)
mNO2 = subNO2[sample(1:nrow(subNO2),
                     size= N),]
mNO2$etiqueta = "Fondo"
### Conjunto de entrenamiento ###
X2 = rbind.data.frame(mSI2[,c(1:3,7)],
                      mNO2[,c(1:3,7)])

#### Imagen 3 ####
### Cargando la imagen desde la ruta local
ruta3 = "imagenes/S5.PNG" # Ruta local de la imagen
imagen3 = readPNG(ruta3) # Descomponer la imagen en sus canales RGB
datos3 = pixeles(imagen3)
# Carita
subSI3 = datos3[(datos3$X>=790 &datos3$X<=940) &
                  (datos3$Y>=440 &datos3$Y<=520), ]

### Mostrar la subregión de la carita

# Carita
set.seed(semilla)
mSI3 = subSI3[sample(1:nrow(subSI3),
                     size= N),]
mSI3$etiqueta = "Piel"

# Para el fondo
cond1 = (datos3$X >= 0 & datos3$X <= 1000) & (datos3$Y >= 230 & datos3$Y <= 640)
subNO3 = datos3[cond1, ]
subNO3 = datos3[setdiff(row.names(datos3),row.names(subNO3)),]

# Extraigo una muestra random de los 10 puntos
set.seed(semilla)
mNO3 = subNO3[sample(1:nrow(subNO3),
                     size= N),]
mNO3$etiqueta = "Fondo"
### Conjunto de entrenamiento ###
X3 = rbind.data.frame(mSI3[,c(1:3,7)],
                      mNO3[,c(1:3,7)])

#### Imagen 4 ####
### Cargando la imagen desde la ruta local
ruta4 = "imagenes/S6.PNG" # Ruta local de la imagen
imagen4 = readPNG(ruta4) # Descomponer la imagen en sus canales RGB
datos4 = pixeles(imagen4)
# Carita
subSI4 = datos4[(datos4$X>=800 &datos4$X<=1100) &
                  (datos4$Y>=240 &datos4$Y<=440), ]

### Mostrar la subregión de la carita

# Carita
set.seed(semilla)
mSI4 = subSI4[sample(1:nrow(subSI4),
                     size= N),]
mSI4$etiqueta = "Piel"

# Para el fondo
cond1 = (datos4$X >= 350 & datos4$X <= 500) & (datos4$Y >= 80 & datos4$Y <= 180)
cond2 <- (datos4$X >= 700  & datos4$X <= 1250) & (datos4$Y >=  0 & datos4$Y <= 180 )
subNO4 = datos4[cond1|cond2, ]

# Extraigo una muestra random de los 10 puntos
set.seed(semilla)
mNO4 = subNO4[sample(1:nrow(subNO4),
                     size= N),]
mNO4$etiqueta = "Fondo"
### Conjunto de entrenamiento ###
X4 = rbind.data.frame(mSI4[,c(1:3,7)],
                      mNO4[,c(1:3,7)])

#### Imagen 5 ####
### Cargando la imagen desde la ruta local
ruta5 = "imagenes/S4.PNG" # Ruta local de la imagen
imagen5 = readPNG(ruta5) # Descomponer la imagen en sus canales RGB
datos5 = pixeles(imagen5)
# Carita
cond1 = (datos5$X>=780 &datos5$X<=1000) & (datos5$Y>=520 &datos5$Y<=650)
cond2 = (datos5$X>=740 &datos5$X<=950) & (datos5$Y>=220 &datos5$Y<=356)

subSI5 = datos5[cond1|cond2, ]

### Mostrar la subregión de la carita

# Carita
set.seed(semilla)
mSI5 = subSI5[sample(1:nrow(subSI5),
                     size= N),]
mSI5$etiqueta = "Piel"

# Para el fondo
cond1 = (datos5$X >= 1050 & datos5$X <= 1250) & (datos5$Y >= 0 & datos5$Y <= 900)
subNO5 = datos5[cond1, ]

# Extraigo una muestra random de los 10 puntos
set.seed(semilla)
mNO5 = subNO5[sample(1:nrow(subNO5),
                     size= N),]
mNO5$etiqueta = "Fondo"
### Conjunto de entrenamiento ###
X5 = rbind.data.frame(mSI5[,c(1:3,7)],
                      mNO5[,c(1:3,7)])


##### 

M = rbind.data.frame(X1,X2,X3,X4,X5)

julia_source("BayesianoJulia.jl")

### Probar con la foto de Denisse ###
ruta6 = "imagenes/S3.PNG"
imagen6 = readPNG(ruta6) # Descomponer la imagen en sus canales RGB
#imagen6 = readJPEG(ruta6) # Descomponer la imagen en sus canales RGB
datos6 = pixeles(imagen6)

prueba1 =  julia_call("Bayesiano",M,datos6,"Piel",0.4 )

plot3d(x = prueba1[prueba1$Etiqueta=="Piel",]$Y,
       y = 1,
       z = prueba1[prueba1$Etiqueta=="Piel",]$X,
       col = prueba1[prueba1$Etiqueta=="Piel",]$color)

plot3d(x = prueba1[prueba1$Etiqueta!="Piel",]$Y,
       y = 1,
       z = prueba1[prueba1$Etiqueta!="Piel",]$X,
       col = prueba1[prueba1$Etiqueta!="Piel",]$color)




