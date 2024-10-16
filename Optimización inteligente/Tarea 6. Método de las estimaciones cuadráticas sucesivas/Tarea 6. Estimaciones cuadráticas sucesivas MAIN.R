#### Método de las Estimaciones Cuadráticas Sucesivas ####
#fx = función que se desea minímizar
#a = x1 = Punto de inicio
#Dx = Tamaño del paso
#T1 y T2 = Tolerancia para la convergencia

MSC = function(fx,a,Dx,T1,T2){
  #### Paso 1 ####
  # Contador para llevar el control de esto
  contador = 0
  # Calcula el punto x2
  x1 = a
  x2 = x1 + Dx
  
  #### Paso 2 ####
  # Calcula las evaluaciones de fx1 y fx2
  fx1 = fx(x1)
  fx2 = fx(x2)
  
  #### Paso 3 ####
  # Revisar hacia donde se esta moviendo para calcular el punto x3
  if(fx1>fx2){
    #### Si se cumple encontes: 
    x3 = x1 + 2*Dx
  }else{
    # SI no se cumple
    x3 = x1 - Dx
  }
  
  # Objeto para guardar los resultados
  resultados = data.frame(iteración = 0, Xmin = 0,Xestrella = 0 ,Fmin = 0,
                          Festrella = 0, Tol1 = 0, Tol2 = 0)
  ### Condición de paro para iterar del punto 4 al 7##
  while(contador<=50){
    #### Paso 4 ####
    contador = contador + 1
    fx1 = fx(x1)
    fx2 = fx(x2)
    fx3 = fx(x3)
    # Determinar Fmin = min(f1,f2,f3) y Xmin
    X = c(x1,x2,x3)
    Fi = c(fx1,fx2,fx3)
    ### Determiniar Fmin
    Fmin = min(Fi)
    ### DETERMINAR Xmin
    Xmin = X[which.min(Fi)]
    
    #### Paso 5 ####
    # Calcular X*
    a0 = fx1
    a1 = (fx2-fx1)/(x2-x1)
    a2 = (1/(x3-x2)) * ((fx3-fx1)/(x3-x1) - a1 )
    ## Calculo del X*
    xe = (x1+x2)/2 - (a1/(2*a2))
    fxe = fx(xe)
    
    ### Agregarlo al selector del optimo
    resultados[contador,]  = c(contador,Xmin,Xestrella = xe ,Fmin,fxe,
                               Tol1 = abs(Fmin-(fxe)), Tol2 = abs(Xmin-(xe)))
    
    #### Paso 6 ####
    if((abs(Fmin-(fxe))<= T1) & (abs(Xmin-(xe))<= T2)){
      # Devolver el optimo
      return(resultados)
    }
    
    #### Paso 7 ####
    # Asignar los nuevos puntos de x
    X = c(x1,x2,x3,xe)
    Fi = c(fx1,fx2,fx3,fxe)
    ### Determiniar Fmin
    Fmin = min(Fi)
    ### DETERMINAR Xmin
    Xmin = X[which.min(Fi)]
    ### Re ordenarlos
    XS = sort(X[-which.max(Fi)])
    
    x1 = XS[1]
    x2 = XS[2]
    x3 = XS[3]
  }
  #  Devolver en caso de no lograr la convergencia
  print("No se logro encontrar un buen optimo, este fue el recorrido en 50 iters:")
  return(resultados)
}



