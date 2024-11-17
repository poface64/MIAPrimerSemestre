##### Método Simplex ####
# Introducir 3 puntos
#x1 = un punto de p dimensiones
#x2 = un punto de p dimensiones
#x3 = un punto de p dimensiones
#gamma = parametro de ajuste
#beta = parametro de ajuste
#tol =  tolerancia permitida
#fx = función de varias variables
SIMPLEX = function(fx,x1,x2,x3,gamma,beta,tol){
  #### PASO 1: Elegir Gamma > 1 y Beta en [0,1] y una tolerancia epsilon
  k = 1
  #### PASO 2: ####
  # xl el mejor punto (entiendase mejor por el que minimiza la función)
  # xg el segundo mejor punto
  # Encontrar Xh (el peor punto), Basados en su desempeño
  X = rbind(x1,x2,x3)
  Fi = apply(X,1,fx)
  # Agregarle sus valores
  X = cbind(X,Y = Fi)
  Q = 50
  
  resultados = data.frame(Iter = 0,Y = 0,Q = 0)
  # Pegarle un punto X
  resultados = cbind.data.frame(as.data.frame(t(x1)) ,resultados)
  # Cambiarle los nombres
  names(resultados)[1:(ncol(resultados)-3)]=paste0("X",1:(ncol(resultados)-3))
  # Re acomodar para poner al principio la iter
  it = (length(resultados)-2)
  resultados = resultados[,c(it,setdiff(1:ncol(resultados),it ))]
  resultados
  
  
  while(Q>tol){
    #### AQUI VA LA PARTE ITERATIVA
    # Ordenarlos de acuerdo a sus desempeños
    X = X[order(X[,ncol(X)]),]# Ordenados: 1 mejor, 2 segundo mejor, 3 peor
    # Obtener el centroide sin el peor punto (xh)
    xc = apply(X[-3,-ncol(X)],2,mean)
    fxc = fx(xc)
    #### Paso 3 ####
    xr =  2*xc-X[3,-ncol(X)]
    
    # Definir un x auxiliar
    xnuevo = xr
    # Calcular fx de xr
    fxr = fx(xr)
    # Verifiicar si f(xr) < f(xl) ENTONCES Xnew = (1+gamma)xc - gamma(xh) Expansión
    if(fxr<X[1,ncol(X)]){
      # Aplicar la expansión
      xnuevo = (1+gamma)*xc - gamma*(X[3,-ncol(X)])
    }else{
      if(fxr >= X[3,ncol(X)]){
        # Aplica el método de contracción
        xnuevo = (1-beta)*xc + beta*(X[3,-ncol(X)])
      }else{
        if((X[2,ncol(X)] < fxr) & (fxr< X[3,ncol(X)])){
          # Aplica el método de contracción
          xnuevo = (1+beta)*xc - beta*(X[3,-ncol(X)])
        }
      }
    }
    # Calcula f(xnuevo) y remplaza xh por xnuevo
    # Pegar como vector
    X[3,] = c(xnuevo,fx(xnuevo))
    # Calcular Q
    Q = 0
    for(i in 1:nrow(X)){
      # Calcular las diferencias y guardarlas
      Q = Q + ((X[i,ncol(X)]-fxc)^2/nrow(X))
    }
    Q = sqrt(Q)
    # Armar la matriz
    resultados[k,] = c(k,X[1,],Q)
    # Actualizar K para llevar el control de las iteraciones
    k = k+1
  }
  # Devolver los resultados
  return(resultados)
}



