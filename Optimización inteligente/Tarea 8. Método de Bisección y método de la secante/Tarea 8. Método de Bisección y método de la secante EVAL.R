
#### Método de bisección ####

# Librería para hacer las tablas bonitas
library(flextable)
# Llamo el código
source("Tarea 8. Método de Bisección y método de la secante MAIN.R")

# Declaro la función que se desea minimizar
fx = function(x){(x^2) +  (54/x)}
#### Evaluación para el ejercicio de clase ####
# a = 2
# b = 5
# Tol= 0.001 
# Delta = 0.001
res1 = biseccion(fx,2,5,0.001,0.001)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(res1)), align = "center", part = "all"))

#### Evaluación para a = 0 y b = 5####
# a = 0
# b = 5
# tol= 0.001 
# Delta = 0.001
res2 = biseccion(fx,0,5,0.001,0.001)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(res2)), align = "center", part = "all"))

#### Evaluación para a = 2 y b = 50####
# a = 2
# b = 5
# tol= 0.001 
# Delta = 0.001
res3 = biseccion(fx,2,50,0.001,0.001)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(res3)), align = "center", part = "all"))


#### Evaluación para a = -5 y b = 10####
# a = -5
# b = 10
# tol= 0.001 
# Delta = 0.001
res4 = biseccion(fx,-5,10,0.001,0.001)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(res4)), align = "center", part = "all"))


#### Método de la secante####

# Llamo el código
source("Tarea 8. Método de Bisección y método de la secante MAIN.R")
# Declaro la función que se desea minimizar
fx = function(x){(x^2) +  (54/x)}
#### Evaluación para el ejercicio de clase ####
# L = 2
# R = 5
# tol= 0.001 
# Delta = 0.001
res1 = secante(fx,2,5,0.001,0.001)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(res1)), align = "center", part = "all"))

# Crear el gráfico
library(ggplot2)
ggplot(res1) +
  # Puntos para (L, f(L))
  geom_point(aes(x = L, y = `f'(L)`), color = "blue", size = 3) +
  # Puntos para (R, f(R))
  geom_point(aes(x = R, y = `f'(R)`), color = "red", size = 3) +
  # Líneas que conectan cada par (L, f(L)) y (R, f(R)) en cada iteración
  geom_segment(aes(x = L, y = `f'(L)`, xend = R, yend = `f'(R)`), color = "black") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  labs(title = "Gráfico de puntos y líneas entre (L, f'(L)) y (R, f'(R)) por iteración",
       x = "X",
       y = "f'(X)") + 
  theme_bw()

#### Evaluación para L= 1 y R = 5####
# L = 1
# R = 5
# tol= 0.001 
# Delta = 0.001
res2 = secante(fx,1,5,0.001,0.001)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(round(res2,8))),align="center",part="all"))

#### Evaluación para a = 2 y b = 50####
# a = 2
# b = 50
# tol= 0.001 
# Delta = 0.001
res3 =secante(fx,2,50,0.001,0.001)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(round(res3,8))),align="center",part="all"))

#### Evaluación para a = -5 y b = 10####
# a = -5
# b = 10
# tol= 0.001 
# Delta = 0.001
res4 = secante(fx,-5,10,0.001,0.001)
# Reportar los resultados bonitos
autofit(align(theme_box(flextable(round(res4,8))),align="center",part="all"))

