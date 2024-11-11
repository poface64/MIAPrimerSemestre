#### Método de la secante ####
# fx = función que se desea minímizar
# L = Punto que cumple que f'(x) <0
# R = Punto que cumple que f'(x) >0
# delta = Error definido para la derivada
# tol = Tolerancia para la convergencia
secante = function(fx,L,R,delta,tol){
  # Paso 1. Se definieron los parametros como entrada.
  # Definir el calculo numérico de la derivada.
  fp = function(fx,x,delta){
    # Crear los valores necesarios que se repiten
    a1 = fx(x + delta)
    a2 = fx(x - delta)
    ## Evaluar la primera y segunda derivada
    fp1 = (a1-a2)/(2*delta)
    ## Regresar los valores
    return(fp1)
  }
  # Definir una z provisional para el bucle
  # y los objetos donde se guardaran los resultados
  fz = 10 # Valor provisional de la derivada de fx en Z
  iter = 0 # Iniciar el contador de iteraciones
  eval = 0 # Evaluaciones de la función por la derivada analítica
  # Data frame con los resultados
  resultados = data.frame(iter = 0,L = 0, R = 0,
                          z = 0, fz = 0,fl = 0,fr = 0,eval = 0)
  names(resultados)[5] <- "f'(z)"
  names(resultados)[6:7] <- c("f'(L)","f'(R)")
  # Paso 3. IF |f'(z)| < epsilon THEN terminar 
  # Esto se va a convertir en la condición de paro
  while(abs(fz)>tol){
    # Paso 2. Calcular Z = R - (f'(R))*(R-L) /(f'(R) - f'(L))
    # Calcular FR
    FR = fp(fx,R,delta)
    FL = fp(fx,L,delta)
    z = R - (FR * (R-L))/(FR-FL)
    # Calcular f'(z)
    fz = fp(fx,z,delta)
    # Actualizar el contador de evaluaciones
    eval = eval + 6
    #se actualiza el contador de iteraciones
    iter = iter +1
    # Aqui actualizo la tabla de datos
    resultados[iter,] = c(iter,L,R,z,fz,FL,FR,eval)
    # Paso 4. IF f'(z) < 0 THEN a = Z e ir al paso 2 
    if(fz < 0){
      # Actualizar el valor de a
      L = z
    }else{
      # IF f'(z) > 0 THEN b= Z e ir al paso 2 
      R = z
    }
  }
  # Reportar los resultados
  return(resultados)
}
