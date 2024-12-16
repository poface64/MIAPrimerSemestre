rm(list=ls())
# Busqueda del gradiente conjugado
# Librería para hacer las tablas bonitas
# Llamo el código
source("Tarea 13. Conjugadas  MAIN.R")
# Declaro la función que se desea minimizar
#### Evaluación para el ejercicio de clase ####
# fx = función multivariable
# X = punto inicial
# tol = tolerancia
# a = Limite inferior de busqueda
# b = Limite superior de busqueda
# tol = Tolerancia permitida

# Parametros de inicio
# Función objetivo
fx = function(X){(X[1]^2+X[2]-11)^2 + (X[1] + X[2]^2-7)^2}
# Parametros de la función
X0 = c(0,0)
h = 0.001
a = 0.001
b = 1
tol = 0.001
# Resultado de la corrida de clase forzandolo un poco
conjugadas(fx,X0,tol,a,b)

