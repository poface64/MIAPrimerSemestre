##Cargar el script con el método de búsqueda exhaustiva ####

source("Tarea 5. Busqueda exhaustiva MAIN.R")

# Función a evaluar #
fx = function(x){(x^2) +  (54/x)}


#### Evaluación para el ejercicio de clase ####
# a = 0
# b = 5 
# n = 10
res1 = round(BE(fx,0,5,10),6)
res1


#### Evaluación propuesta de hacer más grande el n##
# a = 0
# b = 5 
# n = 50
res2 = round(BE(fx,0,5,50),6)
res2
