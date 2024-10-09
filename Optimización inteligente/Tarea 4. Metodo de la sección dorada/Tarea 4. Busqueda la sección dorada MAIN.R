#### Busqueda de la sección dorada ####
# fx = Función de la cual se quiere saber el minimo
# a =  limite inferior del intervalo
# b 0= limite superior del intervalo
# epsilon = tolerancia permitida

Dorado = function(fx,a,b,epsilon){
  #### Paso 1 ####
  # Transformar los datos a un intervalo de 0 y 1
  tw = function(x){(x-a)/(b-a)}
  # Transformador de W a X
  tx = function(w){w*(b-a) + a }
  
  # Calcular el a y b remapeados en w
  aw = tw(a); bw = tw(b)
  # Definir Lw
  Lw = bw - aw
  # Definir k
  k = 1
  
  #### Paso 3 implicito por ser iterativo hasta que se cumpla la cond de paro
  # Crear una estructura para guardar los resultados de las iteraciones
  resultado = data.frame(a = a, b = b,Iter = k)
  while(epsilon<Lw){
    #### Paso 2 ####
    # Definir el w1 y el w2 como pasos intermedios
    w1 = aw + (0.618)*Lw
    w2 = bw - (0.618)*Lw
    # Evaluar los f(w1) y f(w2)
    fw1 = fx(tx(w1))
    fw2 = fx(tx(w2))
    # Evaluar que región eliminar
    if(fw1 < fw2){
      # Asignar el nuevo aw
      aw = w2
    }else{# EN caso de que fw1 sea mayor a fw2
      bw = w1
    }
    # Actualizo el valor de Lw y de K
    Lw = bw-aw
    k = k+1
    # Agregar los valores 
    resultado[k,] = data.frame(a = tx(aw) , b = tx(bw),Iter = k)
  }
  # Regresar el intervalo transformado
  return(resultado)
}
