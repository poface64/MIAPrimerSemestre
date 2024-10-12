rm(list=ls())

#### FASE DE ACOTAMIENTO ####

### Parámetros de entrada ###
## X_0 = punto inicial
## Delta = Tamaño del paso
## f(x) = Función

# Función completa
# Función completa

FA = function(FX,x_0,Delta){
  # Crear los contadores necesarios
  k = 0 # Para llegar el control de las iteraciones
  contador = 0 # Para llegar el control de las evaluaciones
  # Evaluar esta función y guardar en un objeto para ahorrarme 1 evaluación
  a1 = FX(x_0-abs(Delta)) 
  b1 = FX(x_0+ abs(Delta))
  contador = contador+2 # Actualizar el contador de evaluaciones de F(X)
  # Paso 2, determinar si el incremento es positivo o negativo
  if(a1 > b1){
    # SI se cumple,
    print("Delta es positivo")
  } else {
    if(a1 < b1){
      # Sí se cumple, delta es negativo
      print("Delta es negativo")
      Delta = -Delta 
    }else{
      # Decir que no se mueve
      print("No encuentra cambios, vuelve al paso 1")
      return()
    }
  }
  # Paso 3 Y 4 PARA QUE BUSQUE
  cond = T # Condición inicial que puede cambiar para detener la busqueda
  # Defino que k sea menor o igual a 20 iteraciones
  while (cond == T & k <=20) {
    # Revisar si k = 0 para ahorrar una evaluación
    # Aumentar el K y sacar el nuevo X
    #x_0 ES el punto actual
    x_m1 =  x_0-(2^(k-1)  * Delta)  # ES el punto k-1
    x_k1 = x_0 + 2^k  * Delta # Es el punto k+1
    # Evaluar el tema del paso 4
    cond = FX(x_k1)<FX(x_0) #Re hacer la condición de pare
    contador = contador+2 # Sumar 2 evaluaciones de la función
    # Ahora, actualizo las variables
    x_0 = x_k1 # Actualizo el punto actual con el punto k+1
    # Actualizo K para seguir moviendome y buscando
    k = k+1
  }
  # Reportar resultados
  if(k <20 ){
    # Reportar los resultados que convergieron en menos de 20 iteraciones
    print(paste0("El intervalo encontrado para: ",k, " iteraciones, con ",
                 contador," evaluaciones de la función esta en"))
  }else{
    print(paste0("El intervalo encontrado no convergio para: ",k, " iteraciones, con ",
                 contador," evaluaciones de la función esta en:" ))
  }
  # Devolver el objeto intervalo que contiene al intervalo por si se necesita.
  intervalo = c(x_m1,x_k1)
  #  print(paste0("[",intervalo[1],", ",intervalo[2],"]"))
  return(intervalo)
  #Termina aqui
}



#### GRÁFICA DE LA FUNCIÓN ####


# Declarar la función y evaluarla 
fx = function(x){(x^2) +  (54/x)}
# Definir la función de forma rapida
x = c(seq(-10,-0.1,by = 0.01),seq(0.1,10,by = 0.01))
y = fx(x)
# Gráficar
plot(x = x,y = y,type = "l",
     col = "blue",lwd = 3,
     main = expression("Gráfica para " ~ x^2 + frac(54, x)),
     xlim = c(-20, 20), ylim = c(-20, 60))
abline(h =0 )
abline(v =0 )
# Lineal del minimo
abline(h =27,col = "red",lwd = 3 )


# Llamo el codigo
source("Tarea 1. Fase de acotamiento MAIN.ro.r")


# Evaluación de clase
FA(FX = fx,x_0 = 0.6,Delta = 0.5)


# Evaluación para x_0 = 2  y delta = 0.5
FA(FX = fx,x_0 = 2,Delta = 0.5)

# Evaluación para x_0 = 2  y delta = 0.1
FA(FX = fx,x_0 = 2,Delta = 0.1)

# Evaluación para x_0 = 3  y delta = 0.5
FA(FX = fx,x_0 = 3,Delta = 0.5)

# Evaluación para x_0 = 3  y delta = .01
FA(FX = fx,x_0 = 3,Delta = 0.1)

# Evaluación para x_0 = 3  y delta = .01
FA(FX = fx,x_0 = 2.9,Delta = 0.1)

# Evaluación para x_0 = 2.9  y delta = 0.01
FA(FX = fx,x_0 = 2.9,Delta = 0.01)

# Evaluación para x_0 = 3  y delta = 0.01
FA(FX = fx,x_0 = 3,Delta = 0.01)

# Evaluación para x_0 = 3  y delta = 0.0001
FA(FX = fx,x_0 = 3,Delta = 0.0001)

# Evaluación para x_0 = -5  y delta = 0.5
FA(FX = fx,x_0 = -5,Delta = 0.5)

# Evaluación para x_0 = -5  y delta = 1
FA(FX = fx,x_0 = -5,Delta = 1)

# Evaluación para x_0 = -4  y delta = 0.5
FA(FX = fx,x_0 = -5,Delta = 1)

# Incluir el codigo
cat(readLines("Tarea 1. Fase de acotamiento.r"), sep = "\n")
