rm(list=ls())


#### Para gráficar la función ####
# Declarar la función y evaluarla
fx = function(x){(x^2) +  (54/x)}
# Definir la función de forma rápida
x = c(seq(-10,-0.1,by = 0.01),seq(0.1,10,by = 0.01))
y = fx(x)
# Graficar
plot(x = x,y = y,type = "l",
     col = "blue",lwd = 3,
     main = expression("Gráfica para " ~ x^2 + frac(54, x)),
     xlim = c(-20, 20), ylim = c(-20, 60))
abline(h =0 )
abline(v =0 )
# Lineal del mínimo
abline(h =27,col = "red",lwd = 3 )


#### Cargar el codigo fuente de la función del método ####
library(flextable)
# Llamo el código
source("Tarea 2. Eliminación de regiones MAIN.r")

#### Aplicar las evaluaciones ####

# Declaro la función que se desea minimizar
fx = function(x){(x^2) +  (54/x)}
#Aplicar el método con los parámetros mencionados
res1 = round(ElimReg(a = 0, b = 5,epsilon = 0.001,fx),6)
# Reportar los resultados bonitos
res1

#Aplicar el método con los parámetros mencionados
res2 = round(ElimReg(a = 0, b = 10,epsilon = 0.001,fx),6)
# Reportar los resultados bonitos
res2

#Aplicar el método con los parámetros mencionados
res3 = round(ElimReg(a = 2, b = 3,epsilon = 0.001,fx),6)
# Reportar los resultados bonitos
res3

#Aplicar el método con los parámetros mencionados
res4 = round(ElimReg(a = -5, b = 0,epsilon = 0.001,fx),6)
# Reportar los resultados bonitos
res4

