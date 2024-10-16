##Cargar el script con el método de Estimaciones Cuadráticas Sucesivas ####
source("Tarea 6. Estimaciones cuadráticas sucesivas MAIN.R")
# Función a evaluar #
fx = function(x){(x^2)+(54/x)}
#### Evaluación para el ejercicio de clase ####
# x1 = 1
# Delta = 1
# Tol1=Tol2= 0.001 
res1 = round(MSC(fx,1,1,0.001,0.001),6)
res1
#### Evaluación para el ejercicio de clase disminuyendo cambiando el delta= 0.1 ####
# x1 = 1
# Delta = 0.1
# Tol1=Tol2= 0.00001 
res2 = round(MSC(fx,1,0.1,0.001,0.001),6)
res2
#### Evaluación para el ejercicio de clase cambiando el delta= 0.5 ####
# x1 = 1
# Delta = 0.5
# Tol1=Tol2= 0.001 
res3 = round(MSC(fx,1,0.5,0.001,0.001),6)
res3
#### Evaluación para el ejercicio de clase cambiando el punto de inicio en 5 ####
# x1 = 5
# Delta = 1
# Tol1=Tol2= 0.001 
res4 = round(MSC(fx,5,1,0.001,0.001),6)
res4
#### Evaluación para el ejercicio de clase cambiando el punto de inicio en 0.1 ####
# x1 = 0.1
# Delta = 1
# Tol1=Tol2= 0.001 
res5 = round(MSC(fx,0.1,1,0.001,0.001),6)
res5
#### Evaluación para el ejercicio de clase cambiando el delta a uno negativo ####
# x1 = 1
# Delta = -0.5
# Tol1=Tol2= 0.001 
res6 = round(MSC(fx,1,-0.5,0.001,0.001),6)
res6
#### Evaluación para el ejercicio de clase cambiando el punto de inicio en los negativos ####
# x1 = -1
# Delta = 0.5
# Tol1=Tol2= 0.001 
res7 = round(MSC(fx,-1,0.5,0.001,0.001),6)
res7
