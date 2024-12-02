#### Función de Cauchy ####

# fx = función multivariante
# x = punto inicial
# tol1 = tolerancia permitida
# a = limite inferior del intervalo de busqueda
# b = limite superior del intervalo de busqueda

cauchy = function(fx,x,tol1,a,b){
  # Parametros de inicio
  tol2 = tol1
  h = 0.0001
  k = 1 # Punto de inicio par K
  D = length(x) # Dimensiones de entrada
  # Crear el objeto que guarda los valores de X
  M = 100
  X <- matrix(0, nrow = 1, ncol = D)
  ### Funciones necesarias ###
  #Función para calcular el gradiente
  gradienten <- function(fx, X, h) {
    D <- length(X)  # Dimensiones de la entrada
    resu <- numeric(D)
    f0 <- fx(X)     # Valor de la función en X actual
    for (i in 1:D) {
      perturbado <- X
      perturbado[i] <- perturbado[i] + h
      resu[i] <- (fx(perturbado) - f0) / h
    }
    return(resu)
  }
  # Función en terminos de lambda que minimiza el valor de fx(x-lambda*grad)
  fa <- function(lambda) { fx(X[k,] - lambda * gradiente) }
  # Función par extraer la norma
  norma <- function(VEC){sqrt(sum(VEC^2))} 
  #### Paso 3 implicito como criterio de parada
  # Norma auxiliar
  normaG = 5
  normaX = 5
  tc = c(normaX)
  # Paso 3 que implica el bucle hasta la convergencia
  while(normaG >= tol1 & normaX >= tol1  & k < M){
    # PASO 2: Calcular gradiente numérico
    gradiente <- gradienten(fx, X[k, ], h)
    # PASO 3: Calcula la norma del gradiente
    normaG <- norma(gradiente)
    # Paso 4: Efectuar la busqueda unidireccional par encontrar lambda
    opt_result <- optimize(fa, interval = c(a, b), tol = 1e-5)
    lmin <- opt_result$minimum
    # Actualizar X
    X =  rbind(X,X[k, ] - lmin * gradiente) 
    k <- k + 1
    # Calcular las normas de X y el uevo X
    normaX = norma(X[k,]-X[k-1,])/norma(X[k-1,])
    tc = c(tc,normaX)
  }
  
  # Añadir al final la columna de resultados
  X = cbind.data.frame(1:k,X,apply(X, 1, fx),tc)
  # Ponerlo bonito para los resultados:
  resu = as.data.frame(X)
  names(resu) = c("Iter","X1","X2","Y","Tol")
  resu = resu |> round(digits = 4)
  # Devolver los resultados bonitos
  return(resu)
}
