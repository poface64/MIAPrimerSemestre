# Mecanismo generador de fibonacci
FIBO = function(n){
  # Crear una lista inicial con los valores F_0=1 y F_1 = 1
  FV = c(1,1)
  if(n>=2){
    # Hacer el ciclo que la construya poco a poco
    for(i in 3:(n+1) ){
      # Desde el elemnto 3 hasta el elemento n, suma los 2 anteriores
      FV[i] = FV[i-1] + FV[i-2]
    }
    # Nombrar los datos en orden
    names(FV) = 0:(n)
    FV[paste0(n)] |> as.numeric()
  }else{
    names(FV) = 0:1
    FV[paste0(n-1)] |> as.numeric()
    
  }
  
  #Devolver el ultimo numero
  
}

#### Definir los parámetros de entrada para el método ####

# fx = función a evaluar
# a = limite inferior
# b = limite superior
# N = Cantidad de iteraciones -1

### Definir la función a evaluar

fibonacci = function(fx,a,b,N){
  #### Paso 1 ####
  L = b-a;L # ESTO VA 
  k = 2 # ESTO VA FIJO
  # Llevar el control interno
  contador = 0
  # Tabla bonita para los resultados
  resu = data.frame(iter = 0, a = 0, b = 0, fx1 = 0, fx2 = 0)
  #### Paso 2 ####
  while(k<=N){
    # Calcular la razon con numeros de fibonacci para la distancia
    Lke = (FIBO(N-k+1)/FIBO(N+1))*L;Lke
    x1 = a+Lke
    x2 = b-Lke
    #### Paso 3, eliminación de regiones ###
    
    # Evalua ambaos lados
    fx1 = fx(x1)
    fx2 = fx(x2)
    # Decidir que hacer si 
    if(fx1>fx2){ #Si fx1 es mayor a fx2, corta la izquierda
      a = x1
    } else{
      if(fx1<fx2){ #Si fx1 es menor a fx2, corta la derecha
        b = x2
      }else{ # Si no se cumple ninguna, corta ambos lados
        a = x1
        b = x2
      }
    }
    ## Evaluar si K == N
    k = k+1
    resu[k-2,] = data.frame(iter = k-2, a, b , fx1 , fx2)
    
  }
  
  # Devolver la función
  return(resu)
}








