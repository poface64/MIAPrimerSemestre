rm(list=ls())

# Cargar la función principal
source("Tarea 14 Marquart MAIN.R")
#### Evaluación de clase ####
# Función objetivo
fx = function(X){(X[1]^2+X[2]-11)^2 + (X[1] + X[2]^2-7)^2}
# Parametros de la función
X0 = c(0,0)
a = 0
b = 5
tol = 0.001
NH(fx,X0,tol,a,b)
#### Evaluación del libro ####
X1 = c(2,1)
a = 0
b = 5
tol = 0.001
NH(fx,X1,tol,a,b)
