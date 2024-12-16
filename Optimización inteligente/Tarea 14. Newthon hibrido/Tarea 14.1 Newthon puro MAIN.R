
# Función del método de Newthon #
NH = function(fx,X0,tol,a,b){
  #### Paso 1, elegir un punto inicial
  # Definir internamente las funciones que necesito
  ## Función que calcula la norma
  norma <- function(VEC){sqrt(sum(VEC^2))}
  ## Función del gradiente
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
  ## Función para calcular la Hessiana
  hess =  function(fx,X0){
    # Definir las dimensiones
    P <- length(X0) # Dimensión
    tol <- 0.001 # Perturbación numérica
    # Inicializar la matriz Hessiana
    H <- matrix(0, ncol = P, nrow = P)
    # Calcular la Hessiana
    for (i in 1:P) {
      for (j in 1:P) {
        if (i == j) {
          # Diagonal principal: segunda derivada respecto a la misma variable
          # Generar el vector de perturbaciones
          vec_perturb <- numeric(P)
          # Dar el valor del error de pertubación en la variable i-esima
          vec_perturb[i] <- tol
          H[i, j] <- (fx(X0 + vec_perturb) - 2 * fx(X0) + fx(X0 - vec_perturb)) / (tol^2)
        } else if (i > j) {
          # Derivadas cruzadas (fuera de la diagonal)
          vec_perturb_i <- numeric(P) # Un vector para perturbar la variable i
          vec_perturb_j <- numeric(P) # Un vector para perturbar la variable j
          vec_perturb_i[i] <- tol #Dar el valor de la pertubación para i
          vec_perturb_j[j] <- tol #Dar el valor de la pertubación para j
          # Fórmula para derivadas cruzadas
          H[i, j] <- (fx(X0 + vec_perturb_i + vec_perturb_j) - 
                        fx(X0 + vec_perturb_i - vec_perturb_j) - 
                        fx(X0 - vec_perturb_i + vec_perturb_j) + 
                        fx(X0 - vec_perturb_i - vec_perturb_j)) / (4 * tol^2)
          # Simetría: H[j, i] = H[i, j]
          H[j, i] <- H[i, j]
        }
      }
    }
    # Devolver la matriz Hessiana como salida
    return(H)
  }
  # Función para minimizar el lambda
  optilambda <- function(X, a, b, tol) {
    # Calcular el gradiente la Hessiana
    GH = solve(hess(fx,X))%*%gradienten(fx,X,0.001)
    # Actualizar la función lambda temporal
    fa <- function(lambda) fx(X-lambda*GH)
    # Optimizar el lambda
    res <- optimize(fa, interval = c(a, b), tol = tol)
    # EXtraer el Alfa
    lambd = res$minimum
    # Calcular el nuevo valor de Xk
    Xk = X-lambd*GH
    # Reportar el resultado
    return(Xk)
  }
  # Definir la matriz de valores de X
  # Definir el objeto que contenga los cambios en X
  D = length(X0)
  X = matrix(X0,nrow = 1,ncol = D)
  # Definir el K inicial
  k = 0
  # Calcular f(X) en el punto y guardarlo
  FX = c(fx(X0))
  # Condición auxiliar para el método
  cond1 = cond2 = 5
  # Guargar en un vector 
  cond1l = cond1
  cond2l = cond2
  #### Paso 3 implicito como condición de paro ####
  while(cond1>tol&cond2>tol ){
    #### Paso 2 Calcular el gradiente K-esimo
    gradiente = gradienten(fx,X[k+1,],0.001)
    # Paso 4, efectuar la busqueda unidireccional para encontrar lambda K
    X = rbind(X, t(optilambda(X[k+1,],a,b,0.001)))
    #Paso 5. es (f(xk+1)-f(xk) / f(xk))<0.001
    # Actualizar fx para xk+1
    FX = c(FX,fx(X[k+2,]))
    # Verificar los criterios de paro
    cond1 = norma(gradiente)
    cond2 = abs((FX[k+2]-FX[k+1])/FX[k+1])
    # Guardar los criterios
    cond1l = c(cond1l,cond1)
    cond2l = c(cond2l,cond2)
    # Verificar la condición
    # Actualizar el K = K+1
    k = k+1
    # Regresa al paso 2
  }
  # Acomodar la salida
  resu = round(cbind.data.frame(X,FX,cond1l,cond2l),6)
  names(resu) = c(paste0("X",1:D),"f(X)","NormaGrad","DiffX")
  # Reportar los resultados
  return(resu)
}
