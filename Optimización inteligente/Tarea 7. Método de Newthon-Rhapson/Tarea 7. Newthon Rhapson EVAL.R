# Librería para hacer las tablas bonitas
library(flextable)
# Llamo el código
source("Tarea 7. Newthon Rhapson MAIN.R")

# Declaro la función que se desea minimizar
fx = function(x){(x^2) +  (54/x)}

#### Evaluación para el ejercicio de clase ####
# x = 1
# Delta = 0.001
# Tol= 0.001 
# Fijo = FALSE
res1 = NS(fx,1,0.001,0.001,F)

# Reportar los resultados bonitos
autofit(align(theme_box(flextable(res1)), align = "center", part = "all"))

#### Evaluación para el ejercicio de clase ####
# x = 1
# Delta = 0.001
# Tol= 0.001 
# Fijo = TRUE
res2 = NS(fx,1,0.001,0.001,T)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(res2)), align = "center", part = "all"))

#### Evaluación para el ejercicio de clase ####
# x = 50
# Delta = 0.001
# Tol= 0.001 
# Fijo = FALSE
res3 = NS(fx,50,0.001,0.001,F)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(res3)), align = "center", part = "all"))

#### Evaluación para el ejercicio de clase ####
# x = -10
# Delta = 0.001
# Tol= 0.001 
# Fijo = FALSE
res4 = NS(fx,-10,0.001,0.001,F)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(res4)), align = "center", part = "all"))

