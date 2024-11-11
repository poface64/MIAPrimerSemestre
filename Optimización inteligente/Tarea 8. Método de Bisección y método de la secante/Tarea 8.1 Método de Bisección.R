#### Método de Bisección ####
# fx = función que se desea minímizar
# a = Punto que cumple que f'(x) <0
# b = Punto que cumple que f'(x) >0
# delta = Error definido para la derivada
# tol = Tolerancia para la convergencia
biseccion = function(fx,a,b,delta,tol){
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
  fz = 10 # Valor provicional de la derivada de fx en Z
  iter = 0 # Iniciar el contador de iteraciones
  eval = 0 # Evaluaciones de la función por la derivada análitica
  # Data frame con los resultados
  resultados = data.frame(iter = 0,a = 0, b = 0,
                          z = 0, fz = 0, eval = 0)
  names(resultados)[5] <- "f'(z)"
  # Paso 3. IF |f'(z)| < epsilon THEN terminar 
  # Esto se va a convertir en la condición de paro
  while(abs(fz)>tol){
    # Paso 2. Calcular Z = (a+b)/2
    z = (a+b)/2
    # Calcular f'(z)
    fz = fp(fx,z,delta)
    # Actualizar el contador de evaluaciones
    eval = eval + 2
    #se actualiza el contador de iteraciones
    iter = iter +1
    # Aqui actualizo la tabla de datos
    resultados[iter,] = c(iter,a,b,z,fz,eval)
    # Paso 4. IF f'(z) < 0 THEN a = Z e ir al paso 2 
    if(fz < 0){
      # Actualizar el valor de a
      a = z
    }else{
      # IF f'(z) > 0 THEN b= Z e ir al paso 2 
      b = z
    }
  }
  # Reportar los resultados
  return(resultados)
}
