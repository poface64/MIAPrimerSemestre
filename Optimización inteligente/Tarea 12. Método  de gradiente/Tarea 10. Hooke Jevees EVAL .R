rm(list=ls())
# Busqueda de patrones de Hooke-Jeeves
# Librería para hacer las tablas bonitas
library(flextable)
# Llamo el código
source("Tarea 10. Hooke-Jevees MAIN.R")
# Declaro la función que se desea minimizar
#### Evaluación para el ejercicio de clase ####
# x0 = (0,0)
# delta = (0.5,0.5)
# alfa = 2
# tol = 0.001
# Declaro la función que se desea minimizar
fx = function(X){(X[1]^2+X[2]-11)^2 + (X[1] + X[2]^2-7)^2}
#### Evaluación para el ejercicio de clase ####
x0 = c(0,0)
delta = c(0.5,0.5)
alfa = 2
tol = 0.001
patroneshj(fx,x0,delta,alfa,tol)

