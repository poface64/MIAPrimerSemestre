#### Búsqueda Exhaustiva ####
# fx = Función de la cual se quiere saber el mínimo
# a =  limite inferior del intervalo
# b = limite superior del intervalo
# n = tolerancia permitida

BE = function(fx,a,b,n){
  #### Paso 1 ####
  # Definir el punto x1 y el punto deltaX
  x1 = a
  Dx = (b-a)/n;Dx
  # Definir el punto x2 y el punto x3
  x2 = x1 + Dx
  x3 = x2 + Dx
  #### Paso 3 como condición de paro ####
  # Creo un objeto para guardaar el resultado de las iteraciones #
  contador = 0
  resultados = data.frame(a = 0,b = 0, n = 0)
  while(x3<=b){
    #### Paso 2 ####
    # Evaluar las funciones #
    fx1 = fx(x1)
    fx2 = fx(x2)
    fx3 = fx(x3)
    # Agregar los valores de x1 y x3
    contador = contador + 1
    resultados[contador,] = c(x1,x3,contador) 
    # Verificar si se cumple la condición 
    if(fx1 >= fx2 & fx2<=fx3){
      # EL minimo se encuentra en (x1,x3)
      return(resultados)
    } else{
      # Actualizar los valores de los 3 puntos para que avancen
      x1 = x2
      x2 = x3
      x3 = x2 + Dx
    }
  }
  return(paste0("No existe mínimo en (",a,",",b,") o un punto extremo es el mínimo."))
}


