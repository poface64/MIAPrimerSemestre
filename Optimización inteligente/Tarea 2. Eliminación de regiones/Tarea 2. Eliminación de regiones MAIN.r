#### Función del método de Eliminación de Regiones ####

### Parámetros de entrada ###
## a = LImite inferior del intervalo
## b = Limite superior del intervalo
## epsilon = Error permitido, fijado a 0.001
## f(x) = Función
ElimReg = function(a,b,epsilon,FX){
  ### Paso 1 ###
  xm = (a+b)/2 # Iniciar el valor medio x_m
  L = b-a # Definir la amplitud del intervalo que servira como tolerancia
  contador = 0 # Contador de iteraciones
  evaluador = 0 # Contador de evaluaciones de la función
  # Objeto donde se guardan los datos
  resultados = data.frame(Iter = 0, A = 0,B = 0,fx1 = 0, fx2 = 0,tolerancia = 0,
                          evaluaciones = 0)
  # Ciclo WHILE para los pasos del 2 al 5 #
  while(L>epsilon){
    #### Paso 2 ####
    x1 = a + (L/4); x2 = b - (L/4)
    fxm = FX(xm)
    evaluador = evaluador +1 # Se hizo 1 evaluación de la función
    # Calcular f(x1) y f(x2)
    fx1 = FX(x1);fx2 = FX(x2)
    evaluador = evaluador +2 # Se hicieron 2 evaluaciones de la función
    #### Paso 3 ####
    if(fx1<fxm){
      # Cambiar los limites
      b = xm; xm = x1
    }else{
      #### Paso 4 ####
      if(fx2<fxm){
        # Actualización de parametros
        a = xm; xm = x2
      }else{
        #### Paso 4.1 ####
        a = x1; b = x2
      }}
    #### PASO 5 ####
    L = abs(b-a) # Actualizar si ya se cumple la tolerancia
    contador = contador +1
    # Agregar los resultados
    resultados[contador,] = c(contador,a,b,fx1,fx2,L,evaluador)
  }
  # Reportar la tabla con los resultados
  names(resultados)[4:5] = c("f(x1)","f(x2)")
  return(resultados)
}
