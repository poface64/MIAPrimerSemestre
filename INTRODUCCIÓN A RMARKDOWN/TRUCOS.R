rm(list=ls())

#### 1.- Hacer gráficos en ggplot2 y exportarlos en buena calidad ####
library(rsvg)
library(jpeg)
library(ggplot2)

# Contabilizar las especies de los pinwinos #
library(palmerpenguins)
datos = penguins
# Hacer una tabla de datos
T1 = table(Especie = datos$species);T1
# Convertir la tabla en un data frame
T1 = as.data.frame(T1);T1

# Hacer un gráfico de barras en ggplot2 de las especies
G1 = ggplot(T1,aes(x = Especie, y = Freq, fill = Especie)) + 
  geom_bar(stat = "identity") # Indicarle que sea una gráfica de barras
G1

### Sacar el gráfico
ggsave("G1.svg", G1, 
       width = 7, height = 7,
       dpi = 4000)
### Exportandolo
sv = rsvg("G1.svg",
          height = 2000,
          width = 2000)
### Exportar en jpeg
writeJPEG(sv, "G1.jpg", quality = 10)

#### 2.- Funciones  ####




library(knitr)
library(flextable)
cuesti = cbind.data.frame(Variable,Escala,Respuestas)
kable(cuesti)




# Partes de Rmarkdown

# Opciones del cabezal
# Opciones de los bloques de comando

# Como escribir texto plano, negritas, italica, listas con balazos
# Insertar imagenes 
# Como insertar links
# Como hacer ecuaciones en Rmarkdown con latex

# Como introducir tablas en Rmarkdown con ayuda de flextable









