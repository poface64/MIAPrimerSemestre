rm(list = ls())

#### Clustering K-medias elaborado por Angel ####

# Paramétros de la función #
#X = Matriz de datos numericos continuos
#K = Cantidad de grupos deseados
X = faithful
K = 3

kmedias = function(X,K){
  ## Convertir los datos en una matriz por facilidad en las operaciones
  datos =  as.matrix(X)
  
  ### Creo todos los objetos necesarios
  P = ncol(datos) # Numero de Variables de los datos
  contador = 0 # Contador para llevar el conteo de las iteraciones
  iter = 50 # Cantidad de iteraciones permitidas
  error = 0.0001 # error permitido
  etiqueta = c() # Vector que va a guardar las asignaciones del cluster
  Jv = c() # Vector que guardara las medidas de distorción por iteración
  # Creo una matriz de KxP donde K son los grupos y P las dimensiones
  medias = matrix(0,nrow = K,ncol = P)
  
  ### Asignar los centroides al azar con K sujetos random
  for(i in 1:K){
    # Extraer una cantidad de round(n/K) datos para las medias
    ns = sample(1:nrow(datos),1)
    # Asignar al sujeto como centroice
    medias[i,] = datos[ns,]
  }
  
  # Aqui inicia la parte iterativa del algoritmo #
  while(contador<iter){
    ### Matriz que contenga las distancias del dato i al centroide k-esimo
    distancia = matrix(0,nrow = nrow(datos),ncol = K )
    #### Calculo de las distancias al cluster
    for(i in 1:nrow(datos)){
      for(j in 1:K){
        ### Calcular la distancia hacia cada centroide
        dif = datos[i,] - medias[j,]
        ### Guardar dicha distancia para cada K
        distancia[i,j] = (dif%*%t(t(dif))) 
      }
      # Etiquetar al dato con el cluster de menor distancia
      etiqueta[i] = which.min(distancia[i,])
    }
    
    ## Evaluar la medida de distorción ##
    J = 0 # Medida de distorción a minimizar
    for(i in 1:nrow(distancia)){
      # Sumar la distancia minima del sujeto i-esimo
      J = J + distancia[i,etiqueta[i]]
    }
    
    ## Actualizar los valores de los centroides en base al etiquetado
    for(i in unique(etiqueta)){
      # Revisar y actualizar 
      medias[i,] = apply(datos[etiqueta==i ,] ,2,mean)
    }
    
    ## Actualizar el contador
    contador = contador + 1
    ## Agregar al vector el valor de J de la i-esima iteración
    Jv[contador] = J
    ## Evaluar la diferencia entre las medidas de disimilitud
    ## Como criterio de pare
    if(contador >=2){
      if(error>abs(Jv[contador]-Jv[contador-1])){
        break
      }
    }
  }
  ### Hacer una lista con las salidas
  resultados = list(Etiqueta = etiqueta,
                    J = Jv)
  return(resultados)
}









