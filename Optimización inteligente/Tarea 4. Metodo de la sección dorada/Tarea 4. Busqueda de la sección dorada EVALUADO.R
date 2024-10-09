rm(list=ls())

##Cargar el script con el método de busqueda Fibonacci ####

source("Tarea 4. Busqueda la sección dorada MAIN.R")

#### Declaro la función que se desea minimizar ####
fx = function(x){(x^2) +  (54/x)}


#### Evaluación para el ejercicio de clase ####
# a = 0
# b = 5 
# epsilon = 0.01
res1 = round(Dorado(fx,0,5,0.01),6)
res1

#### Evaluación propuesta de hacer más pequeño el epsilon####
# a = 0
# b = 5 
# epsilon = 0.001
res2 = round(Dorado(fx,0,5,0.001),6)
res2

#### Evaluación propuesta de hacer más pequeño el epsilon####
# a = 0
# b = 5 
# epsilon = 0.001
res3 = round(Dorado(fx,0,5,0.0001),6)
res3


