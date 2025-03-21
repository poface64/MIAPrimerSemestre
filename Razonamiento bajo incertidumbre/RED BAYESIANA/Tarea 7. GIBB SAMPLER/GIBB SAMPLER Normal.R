# Algoritmo de Gibb sampler solo para el caso de la normal multivariada

condicional = function(i,x,media,var1){
  # Calcular las medias condicionales #
  # Supongamos que iniciamos en i = 1
  # Dimensión de la función
  D = length(media)
  # Conjunto de indices
  oindices = setdiff(1:D,i)
  # Partir la matriz de varianzas y covarianzas #
  sigma_ii = var1[i,i] #Varianza marginal de la variable i-esima
  sigma_ij = var1[i,oindices] # Covarianza de xi con las demás variables
  sigma_jj = var1[oindices,oindices] # Covarianza de las demás variables
  # Partir el vector de medias
  mui = media[i] # La media i-esima
  muj = media[oindices] # La media de las otras variables
  # Calcular la media condicional:
  mediac = mui + sigma_ij %*% solve(sigma_jj) %*% t(t((x[oindices]-muj)))
  # Calcular la varianza condicional
  varc = sigma_ii - sigma_ij %*% solve(sigma_jj)%*%t(t(sigma_ij))
  # Resultados
  resultados = list(media = mediac,var = varc)
  return(resultados)
}

# Implementación del Gibbs Sampler
gibbs <- function(inicial, iteraciones, media, var1){
  # Dimensión del vector
  D = length(media)
  # Generar la matriz de mezclas
  muestras <- matrix(0, ncol = (D+2), nrow = iteraciones)
  # Punto de inicio
  x = inicial
  # Para el numéro de iteraciones solicitado
  for(t in 1:iteraciones){
    # Para cada una de las dimensiones del problema
    for(i in 1:D){
      # Parametros de la distribución condicional
      parametros <- condicional(i, x, media, var1)
      # Nuevo valor de la normal dada la información condicional
      x[i] <- (rnorm(1, mean = as.numeric(parametros$media), 
                        sd = sqrt(as.numeric(parametros$var))))
      # Verificar el tema de los valores que se salen por debajo
      if(x[i]<0){
        # Redondealo a su limite inmediato
        x[i] = 0
      }
      # Verificar los valores que se salen por arriba
      if(x[i]>1){
        # Redondealo a su limite inmediato
        x[i] = 1
        }
    }
    # Resultado de la distribución condicional para el t-esimo momento
    muestras[t, ] =  c(x,parametros$media,parametros$var)
  }
  # Devolver la matriz de resultante
  return(muestras)
}






