#### Función de exploración ####
# Mecanismo generador de la exploración
exploracion = function(fx,x0,delta){
  # Paso 1: Encontrar f = f(x), f+ = f(xi + deltai) y f- = f(xi - deltai)
  cont1 = 1 # Definir el contador i
  xc = x0
  # Calcular las dimensiones
  D = length(xc)
  # Acomodar los vectores y las evaluaciones
  soluciones = matrix(0,nrow = 3,ncol = D+1)
  
  # Paso 3. i coincide con las dimensiones? Si es el caso, ve al paso 4
  # si no, regresa al paso 1
  while(cont1<=D ){
    # El primer vector es la solución inicial
    soluciones[1:3,-(D+1)] = rbind(xc[-(D+1)],xc[-(D+1)],xc[-(D+1)])
    # Armar la matriz de exploraciones
    soluciones[,cont1] = rbind(soluciones[1,cont1], # Caso base
                               soluciones[2,cont1]+delta[cont1], # CASO DONDE SE AUMENTA EL DELTA
                               soluciones[3,cont1]-delta[cont1]) # Caso donde disminuye el delta
    # Calcular el valor de la solución y añadir a la matriz de puntos
    soluciones[,D+1] = apply(soluciones[,1:D],1,fx) # Aqui hubo 3 evaluaciones de la función
    # Paso 2: Encontrar fmin = min(f,f+,f-)
    # Reordeno en función de f(x) la matriz y me quedo el primero
    xc = soluciones[which.min(soluciones[,D+1]),]
    # Aumentar el contador
    cont1 = cont1 +1 
  }
  # Paso 4, si X encontrado es disinto de X inicial, reporta exito
  # SI no, reporta fracaso
  resultado = list()
  if(sum(x0==xc[-(D+1)])==0){
    # Si se cumple que son distintos
    resultado[1] = "Exito"
    resultado[2] =  list(xc)
    return(resultado) # Reporta un exito
  }else{
    # Reporta un fracaso
    resultado[1] = "Fracaso"
  }
  #Devolver la lista con los resultados
  return(resultado)
}

#### Función de Busqueda Hooke Jeves principal ####
patroneshj = function(fx,x0,delta,tol,alfa){
  # Asignar los objetos iniciales del paso 1
  k = 0
  xk = x0
  xb = x0
  #Resultado
  resultado = rbind(c(x0,fx(x0)))
  # Dimensión del vector de busqueda
  D = length(xb)
  norma <- sqrt(sum(delta^2))
  # Función que se itera desde el paso 2 hasta el paso 6
  # La condición de paro esta en el paso 3, por lo que se define un
  # while true para que busque hasta que caiga donde esta la función de paro
  while (T){
    # Hacer el movimiento exploratorio inicial del paso 2
    explora = exploracion(fx,xk[1:(D)],delta)
    # Del paso 2, revisa si se cumple la condición de siga, si no se cumple
    # Entra en la condición del paso 3
    if(explora[[1]]!="Exito"){
      if(norma<tol){
        break
      }else{ # SI no es el caso de que falla y la norma es menor a tol
        delta = delta/2
        norma <- sqrt(sum(delta^2))
      }
    }else{
      #Haz que la solución sea el resultado de la exploración
      xk  = explora[[2]]
    }
    # Verifica la condición del paso 6
    if(xk[D+1]<fx(xb)){
      # Hacer que k  sea k + 1
      k = k+1
      # Movimiento del patron
      xkp = 2*xk[1:(D-1)] - xb[1:D]
      # Guarda el xk anterior en xb
      xb = xk
      # Actualiza el valor
      resultado = rbind(resultado,xb)
      # Explorar un nuevo movimiento con xkp
      # Actualiza el xk
      explora = exploracion(fx,xkp,delta)
      if(explora[[1]]=="Exito"){
        xk = explora[[2]]
      }
    }
  }
  #Muestra la matriz de resultados
  resultado = rbind.data.frame(resultado)
  # Cambiar los nombres para más estetica
  names(resultado) = c(paste0("X",1:D),"f(X)")
  return(resultado)
} # Aqui termina la función

