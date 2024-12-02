rm(list=ls())
# Busqueda de direcciones conjugadas de Powell
# Librería para hacer las tablas bonitas
library(flextable)
# Llamo el código
source("Tarea 11. Conjugadas de Powell MAIN.R")
# Declaro la función que se desea minimizar
#### Evaluación para el ejercicio de clase ####
# fx = función multivariable
# X = punto inicial
# tol = tolerancia
# a = Limite inferior de busqueda
# b = Limite superior de busqueda
# Declaro la función que se desea minimizar
fx = function(X){(X[1]^2+X[2]-11)^2 + (X[1] + X[2]^2-7)^2}

#### Evaluación para el ejercicio de clase ####
x = c(0,4)
tol = 0.001
a = -1
b = 10

# Resultado de la corrida de clase forzandolo un poco
powell(fx,x,tol,a,b)

#### Evaluación con cambio de limites ####
x = c(0,4)
tol = 0.001
a = -10
b = 10

# Resultado de la corrida de clase forzandolo un poco
powell(fx,x,tol,a,b)




