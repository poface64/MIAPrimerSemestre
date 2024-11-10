#Libros de referencia
#R Markdown Cookbook
#https://bookdown.org/yihui/rmarkdown-cookbook/
#R Markdown: The Definitive Guide
#https://bookdown.org/yihui/rmarkdown/

rm(list=ls())

# Cargar los datos de los pinguinos
ruta = "pinguinos.csv"
datos = read.csv(ruta)
names(datos)

# Mostrar los primeros 6 datos para ver el contenido de los datos
head(datos)

## Versión mejorada
library(flextable) # Liberia para hacer tablas bonitas
flextable(head(datos))

## Versión aun mejor
autofit(theme_box(flextable(head(datos))))

## Revisar el tipo de variable que tiene
# Extrae los nombres de las variables
nombres =  names(datos)
# Extrae los tipos de las variables
tipos = sapply(datos, class)
# Crea un nuevo DataFrame con la información
tiposvariable =  data.frame(
  Nombre = nombres,
  Tipo = tipos,
  stringsAsFactors = FALSE)

# Reportar bonito
theme_box(flextable(tiposvariable)) 

# Obtener el conteo de cuantos pinguinos hay por cada especie
conteo = table(Especies = datos$species)
conteo
# Convertir la tabla de frecuencias en un data frame
conteo1 = as.data.frame(conteo)
conteo1
# Reportar bonito en una tabla
theme_box(flextable(conteo1)) 

# Obtener la media, la mediana, la varianza y la desviación estandar
medias1 = apply(datos[,-1],2,mean)
medianas1 = apply(datos[,-1],2,median)
varianza1 = apply(datos[,-1],2,var)
desvi1 = apply(datos[,-1],2,sd)
# Pegar los datos juntos y hacerlos una tabla
resumen1 = (rbind(medias1,medianas1,varianza1,desvi1))
resumen1
# Agregar el nombre del estadístico a la fila
resumen2 = cbind.data.frame(Estadístico = c("Media","Mediana","Varianza","Desviación"),resumen1)
resumen2
# Reportar bonito
autofit(theme_box(flextable(resumen2)))


### Automatizando para extraer las estadísticas por especie ###
# Lista de especies únicas en el dataset
especies <- unique(datos$species)
# Inicializar un data.frame vacío para guardar los resultados
resumen_total <- data.frame()
# Ciclo for para cada especie
for (especie in especies) {
  # Filtrar los datos para la especie actual
  datos_especie <- subset(datos, species == especie)[,-1]  # Quitamos la columna 'species'
  # Calcular los estadísticos para la especie actual
  medias <- apply(datos_especie, 2, mean)
  medianas <- apply(datos_especie, 2, median)
  varianza <- apply(datos_especie, 2, var)
  desviacion <- apply(datos_especie, 2, sd)
  
  # Crear el resumen de la especie actual en un data.frame
  resumen_especie <- rbind(medias, medianas, varianza, desviacion)
  resumen_especie <- cbind.data.frame(Estadístico = c("Media", "Mediana", "Varianza", "Desviación"),
                                      resumen_especie)
  
  # Agregar una columna para la especie
  resumen_especie$Especie <- especie
  
  # Añadir los resultados al resumen total
  resumen_total <- rbind(resumen_total, resumen_especie)
}

# Mostrar bonito el resumen
resumen_total = resumen_total[,c(1,6,2:5)]
autofit(theme_box(flextable(resumen_total)))

#### Exploración gráfica ####

# Gráfico de barras por especie
barplot(conteo1$Freq~conteo1$Especies,
        col = c("red","blue","green"),
        xlab = "Especies",
        ylab = "Frecuencias",
        main = "Gráfico de barras para las especies de pinguinos")

# Gráfico de pastel
pie(conteo1$Freq,labels = conteo1$Especies)


# Histograma de la distribución de la longitud del pico

hist(datos$bill_length_mm, col = "blue")

# Gráfico de cajas para la distribución del pico

boxplot(datos$bill_length_mm~datos$species,
        col = as.factor(datos$bill_length_mm),
        xlab = "Especies",
        ylab = "Longitud del pico",
        main = "Gráfico de cajas de la longitud del pico por especies")


# Gráfico de dispersión entre la altura y profundidad del pico
plot(datos$bill_length_mm,datos$bill_depth_mm,
     col= as.factor(datos$species),pch = 16,
     xlab = "Longitud del pico",
     ylab = "Profundidad del pico",
     main = "Gráfico de dispersión entre la longitud del pico y\n la profundidad del pico por especie")

