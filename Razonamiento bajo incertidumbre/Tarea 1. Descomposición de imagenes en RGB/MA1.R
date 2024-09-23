rm(list=ls())

# Cargar librerias necesarias
library(png) # Libreria para leer PNG's
library(reshape2) #?
library(flextable)# LIbreria para hacer tablas bonitas
library(scatterplot3d) #gráficar en 3D
library(rsvg) # Manipular archivos SVG
library(jpeg) # Comprimir el SVG a JPEG
library(rgl)


#### Cargar y descomponer la imagen 1. ####

ruta = "im1.png"
imagen = readPNG(ruta)
str(imagen) # EL archivo cargado es un CUBO de información

## Del cubo de información se extrae cada capa de color##
rojo = imagen[ , ,1] # Extraer inf de  R
verde = imagen[ , ,2] # Extraer inf de G
azul = imagen[ , ,3] # Extraer inf de B

# Compactar los vectores en un dataframe
datos = cbind.data.frame(R = as.vector(rojo),
                         G = as.vector(verde),
                         B = as.vector(azul))
# EL dataframe esta compuesto por 261,744 filas (pixeles) y 3 columnas (R,G y B)
autofit(theme_box(flextable(head(datos))))

# Obtener el vectorede medias:
media = data.frame(t(apply(datos,2,mean)))
datos1 = rbind.data.frame(datos,media)

## Crear el gráfico en SVG para no perder calidad
svg("r1.svg")

## Ejecutar el gráfico ##
scatterplot3d(datos1[,1:3], # Datos de R,G y B como X,Y y Z 
              color = rgb(datos1$R,datos1$G,datos1$B),# Colores acorde a su RGB
              pch = 16, # Forma rellenita de los puntos
              angle = 600, #Rotación del gráfico
              cex.symbols = 2) 
# Exportarlo
dev.off()
### Comprimir el gráfico a JPEG
sv = rsvg("r1.svg",
          height = 2000,
          width = 2000)
### Exportar en jpeg
writeJPEG(sv, "r1.jpg", quality = 10)

#### Cargar y descomponer la imagen 2. ####
rm(list=ls())
ruta = "im2.jpg"
imagen = readJPEG(ruta)
str(imagen) # EL archivo cargado es un CUBO de información

## Del cubo de información se extrae cada capa de color##
rojo = imagen[ , ,1] # Extraer inf de  R
verde = imagen[ , ,2] # Extraer inf de G
azul = imagen[ , ,3] # Extraer inf de B

# Compactar los vectores en un dataframe
datos = cbind.data.frame(R = as.vector(rojo),
                         G = as.vector(verde),
                         B = as.vector(azul))

dim(datos)

# EL dataframe esta compuesto por 2,250,000 filas (pixeles) y 3 columnas (R,G y B)
autofit(theme_box(flextable(head(datos))))

# Obtener el vectorede medias:
media = data.frame(t(apply(datos,2,mean)))
datos1 = rbind.data.frame(datos,media)

# Hacer el gráfico con RGL
plot3d(datos1$R,datos1$G,datos1$B,
       xlab = "Rojo",ylab ="Verde", zlab = "Azul",
       size = 1,
       col = rgb(datos$R,
                 datos$G,
                 datos$B))
bg3d("black")
rgl.snapshot("R21.jpg")


#### Cargar y descomponer la imagen 3. ####
rm(list=ls())
ruta = "im3.jpeg"
imagen = readJPEG(ruta)
str(imagen) # EL archivo cargado es un CUBO de información

## Del cubo de información se extrae cada capa de color##
rojo = imagen[ , ,1] # Extraer inf de  R
verde = imagen[ , ,2] # Extraer inf de G
azul = imagen[ , ,3] # Extraer inf de B

# Compactar los vectores en un dataframe
datos = cbind.data.frame(R = as.vector(rojo),
                         G = as.vector(verde),
                         B = as.vector(azul))

dim(datos)

# EL dataframe esta compuesto por 12,979,200 filas (pixeles) y 3 columnas (R,G y B)
autofit(theme_box(flextable(head(datos))))

# Obtener el vectore de medias:
media = data.frame(t(apply(datos,2,mean)))
datos1 = rbind.data.frame(datos,media)

# Hacer el gráfico con RGL
plot3d(datos1$R,datos1$G,datos1$B,
       xlab = "Rojo",ylab ="Verde", zlab = "Azul",
       size = 1,
       col = rgb(datos$R,
                 datos$G,
                 datos$B))
bg3d("black")
rgl.snapshot("R3.jpg")



