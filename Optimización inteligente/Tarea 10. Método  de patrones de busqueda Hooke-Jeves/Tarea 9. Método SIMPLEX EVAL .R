
### Método SIMPLEX ####

# Librería para hacer las tablas bonitas
library(flextable)
# Llamo el código
source("Tarea 9. SIMPLEX MAIN.R")
# Declaro la función que se desea minimizar
fx = function(X){(X[1]^2+X[2]-11)^2 + (X[1] + X[2]^2-7)^2}
#### Evaluación para el ejercicio de clase ####
# x1 = c(0,0)
# x2 = c(2,0)
# x3 = c(1,1)
# tol = 0.001
# gamma = 1.5
# beta = 0.5
x1 = c(0,0)
x2 = c(2,0)
x3 = c(1,1)
tol = 0.001
gamma = 1.5
beta = 0.5
# Evaluar la función con la corrida de clase
res1 = SIMPLEX(fx,x1,x2,x3,gamma,beta,tol)
res1
