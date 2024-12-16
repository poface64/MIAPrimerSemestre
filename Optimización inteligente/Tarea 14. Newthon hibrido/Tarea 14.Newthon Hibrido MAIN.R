
#### Método de las conjugadas del gradiente
conjugadas = function(fx,X0,tol,a,b){
  #### Paso 1 ####
  # Funciones auxiliares
  # Función que calcula la norma
  norma <- function(VEC){sqrt(sum(VEC^2))}
  # Función que saca el gradiente numerico
  gradienten <- function(fx, X, h) {
    # Parametros de dimensionalidad
    D <- length(X)  # Dimensiones de la entrada
    resu <- numeric(D)  # Inicializamos el vector del gradiente
    for (i in 1:D) {
      # Crear copias de X para aplicar las perturbaciones
      Xpos <- X
      Xneg <- X
      # Aplicar la perturbación en la dimensión i
      Xpos[i] <- X[i] + h
      Xneg[i] <- X[i] - h
      # Calcular la diferencia de las evaluaciones
      resu[i] <- (fx(Xpos) - fx(Xneg)) / (2 * h)
    }
    return(resu)
  }
  # Función para minimizar el lambda
  optilambda <- function(X, S, a, b, tol1) {
    fa <- function(lambda) fx(X + lambda * S)
    res <- optimize(fa, interval = c(a, b), tol = tol1)
    return(res$minimum)
  }
  # Definir las 3 tolerancias
  tol1 = tol2 = tol3 = tol
  # Definir el objeto que contenga los cambios en X
  D = length(X0)
  X = matrix(0,nrow = 1,ncol = D)
  # Definir el objeto que va a contener las direcciones 
  S = matrix(0,nrow = 1,ncol = D)
  # Tambien defino una matriz de gradientes
  grads = matrix(0,nrow = 1,ncol = D)
  #### Paso 2 ####
  # Definir el X0 como el primer elemento de la matriz X
  X[1,] = X0 
  # Encontrar el gradiente para f(X0)
  grads[1,] = gradienten(fx,X[1,],tol1)
  # Encontrar la dirección del gradiente S0
  #Actualizar la dirección en la matriz de direcciones
  S[1,] = -grads[1,]
  # Paso 3 Encontrar un lambda0 tal que f(x0 + lambda0*S1)
  # Sea minimo con tolerancia tol1
  lmin = optilambda(X[1,],S[1,],a,b,tol1)
  # Definir X1 como X0 + lambda0*S0
  X = rbind(X,X[1,] + lmin*S[1,]) 
  # Calcular el gradiente de X1
  grads = rbind(grads,gradienten(fx,X[2,],tol1)) 
  # Iniciar K en K + 1
  k = 1
  
  # Paso 6 implicito que devuelve al paso 4 de forma iterativa
  norma2 = 5 # norma auxiliar para la condición de paro
  norma3 = 5 # norma auxiliar para la condición de paro
  while(norma2>tol2 & norma3 >tol3){
    #### Paso 4 ####
    # Crear una nueva dirección
    # Considera reinicializar la cosa cada n + 1 iteraciones
    #### Paso 4: Calcular nueva dirección ####
    if(k %% (D+1) == 0){
      # Re-inicializar dirección al descenso empinado
      ns = -grads[k+1,]
    } else {
      # Actualizar dirección conjugada
      ns = -grads[k+1,] + S[k,] * (norma(grads[k+1,])/norma(grads[k,]))^2
    }
    # Añadir la nueva dirección K
    S = rbind(S,ns)
    #### Paso 5 ####
    #Encontrar un lambdaK tal que:
    # f(Xk + lambdaK*Sk) sea minima para la tol1
    lmin = optilambda(X[k+1,], S[k+1,], a, b, tol1);lmin
    # Hacer que el nuevo Xk sea Xk + lambdaK*sK
    X = rbind(X,X[k+1,] + lmin*S[k+1,] )
    #### Paso 6 ####
    # Verifica mediante las normas si el nuevo punto es 
    # parecido al punto anterior
    norma2 = norma(X[k+2,]-X[k+1,])/norma(X[k+1,])
    # Actualiza la matriz de gradientes con el nuevo punto
    grads = rbind(grads, gradienten(fx,X[k+2,],tol1))
    # Verifica si el gradiente ya cumple el criterio
    norma3 = norma(grads[k+2,])
    # Actualiza el K por K+1
    k = k+1
  }
  # Revuelve la matriz de X
  resu = as.data.frame(X)
  # Calcular los valores de Y y la tolerancia
  Y = c()
  for(i in 1:nrow(X)){
    Y[i] = fx(X[i,])
  }
  # Juntarlos
  resu = round(cbind.data.frame(resu,Y),6)
  names(resu)[-(D+1)] = paste0("X",1:D)
  return(resu)
}


