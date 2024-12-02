rm(list=ls())
# Cargar la función y dar los parametros iniciales
source("Tarea 12. Busqueda Cauchy MAIN.R")
library(flextable)
fx = function(X){((X[1]^2+X[2]-11)^2 + (X[1] + X[2]^2-7)^2) |> as.numeric()}
x = c(0,0) # Punto inicial
tol1 = 0.001
tol2 = 0.001
# Paramertros para la derivada y el método de busqueda
a = 0
b = 1
# Reportar los resultados
resu = cauchy(fx,x,tol1,0,1)
autofit(theme_box(flextable(resu)))

