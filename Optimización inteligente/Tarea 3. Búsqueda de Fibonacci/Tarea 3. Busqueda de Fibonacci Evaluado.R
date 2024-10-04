rm(list=ls())

##Cargar el script con el método de busqueda Fibonacci ####

source("Tarea 3. Busqueda de Fibonacci MAIN.R")

##Función que se desea acotar ####

fx = function(x){(x^2) +  (54/x)}


#### Declaro la función que se desea minimizar ####
fx = function(x){(x^2) +  (54/x)}


#### Evaluación para el ejercicio de clase ####
# a = 0
# b = 5 
# N = 3
res1 = round(fibonacci(fx,0,5,3),6)
res1

#### Evaluación propuesta de aumentar el rango de búsqueda ####
# a = 0
# b = 10
# N = 3
res2 = round(fibonacci(fx,0,10,3),6)
res2

#### Evaluación propuesta de clase y aumentar N = 10 ####
# a = 0
# b = 5
# N = 10
res3 = round(fibonacci(fx,0,5,10),6)
res3

#### Evaluación propuesta con a = 0, b = 10  y aumentar N = 20 ####
# a = 0
# b = 10
# N = 20
res4 = round(fibonacci(fx,0,10,20),6)
res4

#### Evaluación propuesta en negativos con a = -5, b = 0  y aumentar N = 15 ####
# a = -5
# b = 0
# N = 15
res5 = round(fibonacci(fx,-5,0,15),6)
res5





