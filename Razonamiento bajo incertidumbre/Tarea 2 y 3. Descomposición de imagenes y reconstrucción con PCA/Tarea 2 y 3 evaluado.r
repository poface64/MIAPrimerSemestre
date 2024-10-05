knitr::opts_chunk$set(echo = TRUE,
                      eval = T,
                      message = F,
                      warning = F)

rm(list=ls())
# Cargar la imagen en R
## Cargar librerias necesarias ##
library(png) # Libreria para leer PNG's
library(flextable)# LIbreria para hacer tablas bonitas
library(scatterplot3d) #gráficar en 3D
library(rsvg) # Manipular archivos SVG
library(jpeg) # Comprimir el SVG a JPEG
library(rgl) # OpenGL pero para los gráficos en 3D
library(corrplot) # Para gráficar las correlaciones
### Cargando la imagen desde la ruta local
ruta = "imagenes/im2.jpg" # Ruta local de la imagen
imagen = readJPEG(ruta) # Descomponer la imagen en sus canales RGB 
str(imagen) # EL archivo cargado es un CUBO de información

# Extraer la información de sus componentes
# Del cubo de información se extrae cada capa de color
rojo = imagen[ , ,1] # Extraer inf de  R
verde = imagen[ , ,2] # Extraer inf de G
azul = imagen[ , ,3] # Extraer inf de B

# Comprimir esa info en un dataframe

datos = cbind.data.frame(R = as.vector(rojo),
                         G = as.vector(verde),
                         B = as.vector(azul))
dim(datos)

# Obtener su vector de medias
medias = apply(datos,2,mean)
# Proyectarlo bonito
autofit(theme_box(flextable(as.data.frame(t(medias)))))

knitr::include_graphics("imagenes/C2.PNG")

# Obtener la matriz de varianzas y covarianzas
S = var(datos)
Si = as.data.frame(round(S,4)) 
autofit(theme_box(flextable(Si)))

# Obtener la matriz de correlaciones
R = cor(datos)
corrplot(R, method = 'square', addCoef.col = 'black')

# OBTENER LOS EIGENVALORES
# Obtener los eigen valores y los eigen vectores
A = eigen(var(datos))
eigenvalores = round(data.frame(eigenvalores = t(A$values)),4)
names(eigenvalores) = paste0("Eigen",1:3)
autofit(theme_box(flextable(eigenvalores)))

# Varianza explicada
autofit(theme_box(flextable(round(eigenvalores/sum(eigenvalores),4))))

### Eigenvectores
autofit(theme_box(flextable(as.data.frame(round(A$vectors,4)))))

## # Primer nuevo eje
## origen = c(0,0,0)
## eig1 = A$vectors[,1]
## eig2 = -eig1
## # Segundo nuevo eje
## eig3 = A$vectors[,2]
## eig4 = -eig3
## # Tercer nuevo eje
## eig5 = A$vectors[,3]
## eig6 = -eig5
## ### DIBUJAR EL GRÁFICO CON SUS EJES ###
## plot3d(datos,
##        col = rgb(datos[,1],datos[,2],datos[,3]))
## # Agregar las flechas
## segments3d(rbind(origen, 0.1*eig1), col = "black", lwd = 5)
## segments3d(rbind(origen, 2*eig2), col = "black", lwd = 5)
## segments3d(rbind(origen, 0.3*eig3), col = "black", lwd = 5)
## segments3d(rbind(origen, 0.3*eig4), col = "black", lwd = 5)
## segments3d(rbind(origen, 0.3*eig5), col = "black", lwd = 5)
## segments3d(rbind(origen, 0.3*eig6), col = "black", lwd = 5)
## # Añadir los ejes X, Y y Z
## axes3d(edges = c("x--", "y--", "z--"), col = "black", nticks = 10, cex = 1)
## # Añadir una rejilla en los ejes
## grid3d(c("x", "y", "z"))
## 

## ### DIBUJAR EL GRÁFICO CON SUS EJES en la media ###
## #datos1 = datos
## plot3d(datos,
##        col = rgb(datos[,1],datos[,2],datos[,3]))
## # Agregar las flechas
## segments3d(rbind(medias + eig1, medias - eig1), col = "black", lwd = 8)
## segments3d(rbind(medias + eig3, medias - eig3), col = "black", lwd = 8)
## segments3d(rbind(medias + eig5, medias - eig5), col = "black", lwd = 8)
## # Añadir los ejes X, Y y Z
## axes3d(edges = c("x--", "y--", "z--"), col = "black", nticks = 10, cex = 1)
## # Añadir una rejilla en los ejes
## grid3d(c("x", "y", "z"))

# Calculo de los escores con una multiplicación de matrices
scores <- round(as.matrix(datos) %*% A$vectors,3)
autofit(theme_box(flextable(as.data.frame(head(scores))))) 

## #### PROYECTAR LOS NUEVOS PUNTOS ####
## #datos1 = datos
## plot3d(SCORES,
##        col = rgb(datos[,1],datos[,2],datos[,3]),
##        xlab = "R", ylab = "G",zlab = "B")
## # Añadir los ejes X, Y y Z
## axes3d(edges = c("x--", "y--", "z--"), col = "black", nticks = 10, cex = 1)
## # Añadir una rejilla en los ejes
## grid3d(c("x", "y", "z"))
## rgl.snapshot("imagenes/TR4.png")

# Obtener su vector de medias
Smedias = apply(scores,2,mean)
# Proyectarlo bonito
autofit(theme_box(flextable(as.data.frame(t(Smedias)))))

# Obtener la matriz de varianzas y covarianzas
SS = var(scores)
SSi = as.data.frame(round(SS,4)) 
autofit(theme_box(flextable(SSi)))

# Obtener la matriz de correlaciones
SR = round(cor(scores),4)
corrplot(SR, method = 'square', addCoef.col = 'black')
