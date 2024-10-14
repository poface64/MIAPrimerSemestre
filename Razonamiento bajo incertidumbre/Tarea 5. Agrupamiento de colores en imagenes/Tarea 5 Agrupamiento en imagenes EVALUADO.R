rm(list=ls())
### Cargar las librerias
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
### Función para calcular la intravarianza ####
INTRA = function(X){
  # Definir el objeto que va a guardar la intravarianza
  IV = c()
  # Deginir del 1 al 10
  k = 10
  for(i in 1:k){
    # Calcular el Kmedias y extraer la intra varianza
    KM = kmeans(X,i)
    IV[i] = KM$tot.withinss
  }
  # Devolver el vector de las intra varianzas
  plot(IV,type = "o",
       pch = 16, xlab = "K",ylab = "Suma de errores al cuadrado")
}
### FUnción para sacar colores promedio 
promcolor = function(X){
  for(i in 1:length(unique(X[,7]))){
    # Calcular el vector media
    VM = colMeans(X[X[,7]==i ,1:3])  
    # Obtener el color promedio
    X[X[,7]==i ,8] = rgb(VM[1],VM[2],VM[3])
  }
  return(colores = X[,8])
}



#### Sección de K medias para imagen 1####
ruta1 = "IMAGENES/IM2.jpg"
imagen1 = readJPEG(ruta1) # Descomponer la imagen en sus canales RGB
datos1 = pixeles(imagen1)
dim(imagen1) # La imagen 1 tiene 1152 X 768  = 884,736

#### Definir el criterio de la intra varianza
set.seed(2024)
pdf(file="RESULTADOS/R1.pdf")
INTRA(datos1[,1:3])
dev.off()


#### Kmedias para la primer imagen con K  = 3 ####
k = 3
A = kmeans(datos1[,1:3],k)
datos1$etiqueta = A$cluster
# Agregar el color promedio #
datos1$ecolor =  promcolor(datos1)
### Proyectar la imagen ###
plot3d(x = datos1$Y,
       y = 1,
       z = datos1$X,
       col = datos1$ecolor)
rgl.snapshot("RESULTADOS/R1.2.png", fmt = "png")


#### Kmedias para la primer imagen con K  = 5 ####
k = 5
A = kmeans(datos1[,1:3],k)
datos1$etiqueta = A$cluster
# Agregar el color promedio #
datos1$ecolor =  promcolor(datos1)
### Proyectar la imagen ###
plot3d(x = datos1$Y,
       y = 1,
       z = datos1$X,
       col = datos1$ecolor)
rgl.snapshot("RESULTADOS/R1.3.png", fmt = "png")

#### Kmedias para la primer imagen con K  = 10 ####
k = 10
A = kmeans(datos1[,1:3],k)
datos1$etiqueta = A$cluster
# Agregar el color promedio #
datos1$ecolor =  promcolor(datos1)
### Proyectar la imagen ###
plot3d(x = datos1$Y,
       y = 1,
       z = datos1$X,
       col = datos1$ecolor)
rgl.snapshot("RESULTADOS/R1.4.png", fmt = "png")


#### Sección de K medias para imagen 2####
ruta2 = "IMAGENES/IM4.jpg"
imagen2 = readJPEG(ruta2) # Descomponer la imagen en sus canales RGB
datos2 = pixeles(imagen2)
dim(imagen2) # La imagen 1 tiene 1024 X 768  = 786,432

#### Definir el criterio de la intra varianza
#### Definir el criterio de la intra varianza
set.seed(2024)
pdf(file="RESULTADOS/R2.pdf")
INTRA(datos2[,1:3])
dev.off()

#### Kmedias para la segunda imagen con K  = 3 ####
k = 3
A = kmeans(datos2[,1:3],k)
datos2$etiqueta = A$cluster
# Agregar el color promedio #
datos2$ecolor =  promcolor(datos2)
### Proyectar la imagen ###
plot3d(x = datos2$Y,
       y = 1,
       z = datos2$X,
       col = datos2$ecolor)
rgl.snapshot("RESULTADOS/R2.2.png", fmt = "png")

#### Kmedias para la segunda imagen con K  = 5 ####
k = 5
A = kmeans(datos2[,1:3],k)
datos2$etiqueta = A$cluster
# Agregar el color promedio #
datos2$ecolor =  promcolor(datos2)
### Proyectar la imagen ###
plot3d(x = datos2$Y,
       y = 1,
       z = datos2$X,
       col = datos2$ecolor)
rgl.snapshot("RESULTADOS/R2.3.png", fmt = "png")

#### Kmedias para la segunda imagen con K  = 10 ####
k = 10
A = kmeans(datos2[,1:3],k)
datos2$etiqueta = A$cluster
# Agregar el color promedio #
datos2$ecolor =  promcolor(datos2)
### Proyectar la imagen ###
plot3d(x = datos2$Y,
       y = 1,
       z = datos2$X,
       col = datos2$ecolor)
rgl.snapshot("RESULTADOS/R2.4.png", fmt = "png")


#### Sección de K medias para imagen 3####
ruta3 = "IMAGENES/IM1.jpg"
imagen3 = readJPEG(ruta3) # Descomponer la imagen en sus canales RGB
datos3 = pixeles(imagen3)
dim(imagen3) # La imagen 1 tiene 1024 X 1024  = 1,048,576

#### Definir el criterio de la intra varianza
set.seed(2024)
pdf(file="RESULTADOS/R3.pdf")
INTRA(datos3[,1:3])
dev.off()

#### Kmedias para la tercera imagen con K  = 3 ####
k = 3
A = kmeans(datos3[,1:3],k)
datos3$etiqueta = A$cluster
# Agregar el color promedio #
datos3$ecolor =  promcolor(datos3)
### Proyectar la imagen ###
plot3d(x = datos3$Y,
       y = 1,
       z = datos3$X,
       col = datos3$ecolor)
rgl.snapshot("RESULTADOS/R3.2.png", fmt = "png")

#### Kmedias para la tercera imagen con K  = 5 ####
k = 5
A = kmeans(datos3[,1:3],k)
datos3$etiqueta = A$cluster
# Agregar el color promedio #
datos3$ecolor =  promcolor(datos3)
### Proyectar la imagen ###
plot3d(x = datos3$Y,
       y = 1,
       z = datos3$X,
       col = datos3$ecolor)
rgl.snapshot("RESULTADOS/R3.3.png", fmt = "png")

#### Kmedias para la segunda imagen con K  = 10 ####
k = 10
A = kmeans(datos3[,1:3],k)
datos3$etiqueta = A$cluster
# Agregar el color promedio #
datos3$ecolor =  promcolor(datos3)
### Proyectar la imagen ###
plot3d(x = datos3$Y,
       y = 1,
       z = datos3$X,
       col = datos3$ecolor)
rgl.snapshot("RESULTADOS/R3.4.png", fmt = "png")

#### Mezcla de Gaussianas para la primer imagen ####
### Función para llamar el codigo fuente de Julia ###
julia_source("GMM en julia.jl")

### Cargar la imagen 1 ###
imagen1 = readJPEG(ruta1) # Descomponer la imagen en sus canales RGB
datos1 = pixeles(imagen1)

#### GMM con K = 3 para la primer imagen ####
k = 3
set.seed(2024)
re = proc.time()
prueba1 = julia_call("gausiano",k,datos1[,1:3]) 
proc.time()-re
### Proyectar la imagen ###
datos1$etiqueta = prueba1$Etiqueta
datos1$ecolor =  promcolor(datos1)
plot3d(x = datos1$Y,
       y = 1,
       z = datos1$X,
       col = datos1$ecolor)
rgl.snapshot("RESULTADOS/R1.5.png", fmt = "png")

#### GMM con K = 5 para la primer imagen ####
k = 5
set.seed(2024)
re = proc.time()
prueba1 = julia_call("gausiano",k,datos1[,1:3]) 
proc.time()-re
### Proyectar la imagen ###
datos1$etiqueta = prueba1$Etiqueta
datos1$ecolor =  promcolor(datos1)
plot3d(x = datos1$Y,
       y = 1,
       z = datos1$X,
       col = datos1$ecolor)
rgl.snapshot("RESULTADOS/R1.6.png", fmt = "png")

#### GMM con K = 10 para la primer imagen ####
k = 10
set.seed(2024)
re = proc.time()
prueba1 = julia_call("gausiano",k,datos1[,1:3]) 
proc.time()-re
### Proyectar la imagen ###
datos1$etiqueta = prueba1$Etiqueta
datos1$ecolor =  promcolor(datos1)
plot3d(x = datos1$Y,
       y = 1,
       z = datos1$X,
       col = datos1$ecolor)
rgl.snapshot("RESULTADOS/R1.7.png", fmt = "png")

#### Mezcla de Gaussianas para la segunda imagen ####
imagen2 = readJPEG(ruta2) # Descomponer la imagen en sus canales RGB
datos2 = pixeles(imagen2)

#### GMM con K = 3 para la segunda imagen ####
k = 3
set.seed(2024)
re = proc.time()
prueba2 = julia_call("gausiano",k,datos2[,1:3]) 
proc.time()-re
### Proyectar la imagen ###
datos2$etiqueta = prueba2$Etiqueta
datos2$ecolor =  promcolor(datos2)
plot3d(x = datos2$Y,
       y = 1,
       z = datos2$X,
       col = datos2$ecolor)
rgl.snapshot("RESULTADOS/R2.5.png", fmt = "png")

#### GMM con K = 5 para la segunda imagen ####
k = 5
set.seed(2024)
re = proc.time()
prueba2 = julia_call("gausiano",k,datos2[,1:3]) 
proc.time()-re
### Proyectar la imagen ###
datos2$etiqueta = prueba2$Etiqueta
datos2$ecolor =  promcolor(datos2)
plot3d(x = datos2$Y,
       y = 1,
       z = datos2$X,
       col = datos2$ecolor)
rgl.snapshot("RESULTADOS/R2.6.png", fmt = "png")

#### GMM con K = 10 para la segunda imagen ####
k = 10
set.seed(2024)
re = proc.time()
prueba2 = julia_call("gausiano",k,datos2[,1:3]) 
proc.time()-re
### Proyectar la imagen ###
datos2$etiqueta = prueba2$Etiqueta
datos2$ecolor =  promcolor(datos2)
plot3d(x = datos2$Y,
       y = 1,
       z = datos2$X,
       col = datos2$ecolor)
rgl.snapshot("RESULTADOS/R2.7.png", fmt = "png")


#### Mezcla de Gaussianas para la tercera imagen ####
imagen3 = readJPEG(ruta3) # Descomponer la imagen en sus canales RGB
datos3 = pixeles(imagen3)

#### GMM con K = 3 para la tercera imagen ####
k = 3
set.seed(2024)
re = proc.time()
prueba3 = julia_call("gausiano",k,datos3[,1:3]) 
proc.time()-re
### Proyectar la imagen ###
datos3$etiqueta = prueba3$Etiqueta
datos3$ecolor =  promcolor(datos3)
plot3d(x = datos3$Y,
       y = 1,
       z = datos3$X,
       col = datos3$ecolor)
rgl.snapshot("RESULTADOS/R3.5.png", fmt = "png")

#### GMM con K = 5 para la tercera imagen ####
k = 5
set.seed(2024)
re = proc.time()
prueba3 = julia_call("gausiano",k,datos3[,1:3]) 
proc.time()-re
### Proyectar la imagen ###
datos3$etiqueta = prueba3$Etiqueta
datos3$ecolor =  promcolor(datos3)
plot3d(x = datos3$Y,
       y = 1,
       z = datos3$X,
       col = datos3$ecolor)
rgl.snapshot("RESULTADOS/R3.6.png", fmt = "png")

#### GMM con K = 10 para la tercera imagen ####
k = 10
set.seed(2024)
re = proc.time()
prueba3 = julia_call("gausiano",k,datos3[,1:3]) 
proc.time()-re
### Proyectar la imagen ###
datos3$etiqueta = prueba3$Etiqueta
datos3$ecolor =  promcolor(datos3)
plot3d(x = datos3$Y,
       y = 1,
       z = datos3$X,
       col = datos3$ecolor)
rgl.snapshot("RESULTADOS/R3.7.png", fmt = "png")
