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


#### Imagen 1: Denisse Guerrero ####
### Cargando la imagen desde la ruta local
ruta1 = "imagenes/IM2.jpg" # Ruta local de la imagen
imagen1 = readJPEG(ruta1) # Descomponer la imagen en sus canales RGB
datos1 = pixeles(imagen1)


# Selección de la Carita
subSI1 = datos1[(datos1$X>=610 &datos1$X<=815) &
                  (datos1$Y>=300 &datos1$Y<=490), ]
plot3d(x = subSI1$Y,
       y = 1,
       z = subSI1$X,
       col = rgb(subSI1$R,subSI1$G,subSI1$B),size = 10)# Fondo
rgl.snapshot("R1.1.png", fmt = "png")

semilla = 12
set.seed(semilla)
N = 10
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

### Pixeles de piel y Fondo en el espacio
plot3d(x = X1$R,y = X1$G,z = X1$B,
       col = rgb(X1$R,X1$G,X1$B),size = 30)
rgl.snapshot("R1.2.png", fmt = "png")

#### Evaluar con los datos de la foto 1 ####
prueba1 =  julia_call("Bayesiano",X1,datos1,"Piel",0.4)

# Ver los resultados de piel
plot3d(x = prueba1[prueba1$Etiqueta=="Piel",]$Y,
       y = 1,
       z = prueba1[prueba1$Etiqueta=="Piel",]$X,
       col = prueba1[prueba1$Etiqueta=="Piel",]$color)
rgl.snapshot("R1.3.png", fmt = "png")
# Ver los resultados de fondo
plot3d(x = prueba1[prueba1$Etiqueta!="Piel",]$Y,
       y = 1,
       z = prueba1[prueba1$Etiqueta!="Piel",]$X,
       col = prueba1[prueba1$Etiqueta!="Piel",]$color)
rgl.snapshot("R1.4.png", fmt = "png")

#### Imagen 2: Selena quintanilla ####
### Cargando la imagen desde la ruta local
ruta2 = "imagenes/IM3.jpg" # Ruta local de la imagen
imagen2 = readJPEG(ruta2) # Descomponer la imagen en sus canales RGB
datos2 = pixeles(imagen2)

# Carita
subSI2 = datos2[(datos2$X>=130 &datos2$X<=240) &
                  (datos2$Y>=150 &datos2$Y<=220), ]
plot3d(x = subSI2$Y,
       y = 1,
       z = subSI2$X,
       col = rgb(subSI2$R,subSI2$G,subSI2$B),size = 10)# Fondo
rgl.snapshot("R2.1.png", fmt = "png")


### Mostrar la subregión de la carita
# Carita
set.seed(semilla)
mSI2 = subSI2[sample(1:nrow(subSI2),
                     size= N),]
mSI2$etiqueta = "Piel"
# Para el fondo
cond1 = (datos2$X >= 50 & datos2$X <= 300) & (datos2$Y >= 0 & datos2$Y <= 100)
subNO2 = datos2[cond1, ]

# Extraigo una muestra random de los 10 puntos
set.seed(semilla)
mNO2 = subNO2[sample(1:nrow(subNO2),
                     size= N),]
mNO2$etiqueta = "Fondo"
### Conjunto de entrenamiento ###
X2 = rbind.data.frame(mSI2[,c(1:3,7)],
                      mNO2[,c(1:3,7)])
### Pixeles de piel y Fondo en el espacio
plot3d(x = X2$R,y = X2$G,z = X2$B,
       col = rgb(X2$R,X2$G,X2$B),size = 30)
rgl.snapshot("R2.2.png", fmt = "png")

#### Evaluar con los datos de la foto 2 ####
prueba2 =  julia_call("Bayesiano",X2,datos2,"Piel",0.4)

# Ver los resultados de piel
plot3d(x = prueba2[prueba2$Etiqueta=="Piel",]$Y,
       y = 1,
       z = prueba2[prueba2$Etiqueta=="Piel",]$X,
       col = prueba2[prueba2$Etiqueta=="Piel",]$color)
rgl.snapshot("R2.3.png", fmt = "png")
# Ver los resultados de fondo
plot3d(x = prueba2[prueba2$Etiqueta!="Piel",]$Y,
       y = 1,
       z = prueba2[prueba2$Etiqueta!="Piel",]$X,
       col = prueba2[prueba2$Etiqueta!="Piel",]$color)
rgl.snapshot("R2.4.png", fmt = "png")

#### Imagen 3: Ana Sofía ####
### Cargando la imagen desde la ruta local
ruta3 = "imagenes/IM4.jpg" # Ruta local de la imagen
imagen3 = readJPEG(ruta3) # Descomponer la imagen en sus canales RGB
datos3 = pixeles(imagen3)
# Carita


subSI3 = datos3[(datos3$X>=520 &datos3$X<=790) &
                  (datos3$Y>=270 &datos3$Y<=450), ]

### Mostrar la subregión de la carita
plot3d(x = subSI3$Y,
       y = 1,
       z = subSI3$X,
       col = rgb(subSI3$R,subSI3$G,subSI3$B),size = 10)# Fondo
rgl.snapshot("R3.1.png", fmt = "png")

# Carita
set.seed(semilla)
mSI3 = subSI3[sample(1:nrow(subSI3),
                     size= N),]
mSI3$etiqueta = "Piel"

# Para el fondo
con1 = (datos3$X >= 850 & datos3$X <= 1000) & (datos3$Y >= 0 & datos3$Y <= 768)
con2 = (datos3$X >= 0 & datos3$X <= 230) & (datos3$Y >= 0 & datos3$Y <= 768)
subNO3 = datos3[con1|con2, ]

# Extraigo una muestra random de los 10 puntos
set.seed(semilla)
mNO3 = subNO3[sample(1:nrow(subNO3),
                     size= N),]
mNO3$etiqueta = "Fondo"
### Conjunto de entrenamiento ###
X3 = rbind.data.frame(mSI3[,c(1:3,7)],
                      mNO3[,c(1:3,7)])
### Pixeles de piel y Fondo en el espacio
plot3d(x = X3$R,y = X3$G,z = X3$B,
       col = rgb(X3$R,X3$G,X3$B),size = 30)
rgl.snapshot("R3.2.png", fmt = "png")

#### Evaluar con los datos de la foto 3 ####
prueba3 =  julia_call("Bayesiano",X3,datos3,"Piel",0.3)

# Ver los resultados de piel
plot3d(x = prueba3[prueba3$Etiqueta=="Piel",]$Y,
       y = 1,
       z = prueba3[prueba3$Etiqueta=="Piel",]$X,
       col = prueba3[prueba3$Etiqueta=="Piel",]$color)
rgl.snapshot("R3.3.png", fmt = "png")
# Ver los resultados de fondo
plot3d(x = prueba3[prueba3$Etiqueta!="Piel",]$Y,
       y = 1,
       z = prueba3[prueba3$Etiqueta!="Piel",]$X,
       col = prueba3[prueba3$Etiqueta!="Piel",]$color)
rgl.snapshot("R3.4.png", fmt = "png")

#### Haciendo un modelo de todas las muestras ####
M = rbind.data.frame(X1,X2,X3)

### Pixeles de piel y Fondo en el espacio
plot3d(x = M$R,y = M$G,z = M$B,
       col = rgb(M$R,M$G,M$B),size = 30)
rgl.snapshot("R4.png", fmt = "png")


#### Evaluar la imagen 1 con el modelo general ####
prueba1 =  julia_call("Bayesiano",M,datos1,"Piel",0.4)

# Ver los resultados de piel
plot3d(x = prueba1[prueba1$Etiqueta=="Piel",]$Y,
       y = 1,
       z = prueba1[prueba1$Etiqueta=="Piel",]$X,
       col = prueba1[prueba1$Etiqueta=="Piel",]$color)
rgl.snapshot("R5.1.png", fmt = "png")
# Ver los resultados de fondo
plot3d(x = prueba1[prueba1$Etiqueta!="Piel",]$Y,
       y = 1,
       z = prueba1[prueba1$Etiqueta!="Piel",]$X,
       col = prueba1[prueba1$Etiqueta!="Piel",]$color)
rgl.snapshot("R5.2.png", fmt = "png")

#### Evaluar la imagen 2 con el modelo general ####
prueba2 =  julia_call("Bayesiano",M,datos2,"Piel",0.4)

# Ver los resultados de piel
plot3d(x = prueba2[prueba2$Etiqueta=="Piel",]$Y,
       y = 1,
       z = prueba2[prueba2$Etiqueta=="Piel",]$X,
       col = prueba2[prueba2$Etiqueta=="Piel",]$color)
rgl.snapshot("R6.1.png", fmt = "png")
# Ver los resultados de fondo
plot3d(x = prueba2[prueba2$Etiqueta!="Piel",]$Y,
       y = 1,
       z = prueba2[prueba2$Etiqueta!="Piel",]$X,
       col = prueba2[prueba2$Etiqueta!="Piel",]$color)
rgl.snapshot("R6.2.png", fmt = "png")

#### Evaluar la imagen 3 con el modelo general ####

prueba3 =  julia_call("Bayesiano",M,datos3,"Piel",0.2)

# Ver los resultados de piel
plot3d(x = prueba3[prueba3$Etiqueta=="Piel",]$Y,
       y = 1,
       z = prueba3[prueba3$Etiqueta=="Piel",]$X,
       col = prueba3[prueba3$Etiqueta=="Piel",]$color)
rgl.snapshot("R7.1.png", fmt = "png")
# Ver los resultados de fondo
plot3d(x = prueba3[prueba3$Etiqueta!="Piel",]$Y,
       y = 1,
       z = prueba3[prueba3$Etiqueta!="Piel",]$X,
       col = prueba3[prueba3$Etiqueta!="Piel",]$color)
rgl.snapshot("R7.2.png", fmt = "png")
