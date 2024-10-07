rm(list=ls())

### Cargar la base de datos ####
datos = iris


#### Calcular las medias ####
k = 3
P = ncol(datos)-1
# Creo una matriz de KxP donde K son los grupos y P las dimensiones
medias = matrix(0,nrow = k,ncol = P)

for(i in 1:k){
  # Calcular la media para cada grupo
  medias[i,] = as.vector(apply(datos[datos$Species==unique(datos$Species)[i] ,1:4] 
                     ,2,mean ))
}

### Calcular las varianzas y covarianzas ####
Sigmas = list()
i = 1
for(i in 1:k){
  # Cargar la matriz de datos de la clase k-esima
  X = datos[datos$Species==unique(datos$Species)[i],1:4] |> as.matrix()
  n = nrow(X)
  # Recupero el vector de medias
  xb = as.matrix(medias[i,])
  # Aplico algebra lineal
  Sigmas[[i]] = as.matrix((t(X)%*%(X) - n*(xb%*%t(xb)))/(n-1))
  # Aplicar la función que calcula las covarianzas
}

#### Calcular los coeficientes de mezcla ####

mezcla = as.vector(table(datos$Species)/nrow(datos))


#### Definir la función de verosimilitud ####

# cargo la libreria para estimar la Normal Multivariante #

# Debo evaluar la suma de las probabilidades de que el dato i pertenezca al K-esimo grupo
# y luego aplicar eso para cada caso.

deto = datos[,-5]
i = 1
j = 1
for(i in 1:nrow(datos)){
  for(j in 1:k){
    #Aqui calculo la cosa esta toda extraña
    resu = mezcla[j]* (dmvnorm(deto[j,],medias[j,],Sigmas[[j]]))
  }
}

dmvnorm()




# Tomar 10 datos random

j = 1
medias[j,]

Sigmas[[j]]



