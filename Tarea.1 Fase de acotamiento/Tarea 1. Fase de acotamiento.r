rm(list=ls())

#### FASE DE ACOTAMIENTO ####
## Cambio random ##
### Parametros de entrada ###
## X_0 = punto inicial
## Delta = Tamaño del paso
## f(x) = Función
## K = itreación

## Definir la función a evaluar
FU = function(x){x^2 + 54/x}
## Definir el punto de inicio
x_0 = 0.4
## Definir el tamaño del paso
Delta = 0.5
## Definir k 
k = 0

FA = function(FX,x_0,Delta){
  # Crear los contadores necesarios
  k = 0 # Para llegar el control de las iteraciones
  contador = 0 # Para llegar el control de las evaluaciones
  # Paso 2, determinar si el incremento es positivo o negativo
  if(FX(x_0-abs(Delta)) >= FX(x_0+ abs(Delta))){
    # SI se cumple,
    contador = contador+2 # y Delta es positivo
    print("Delta es positivo")
  } else {
    if(FX(x_0 - abs(Delta))<=  FX(x_0 + abs(Delta))){
      # Sí se cumple, delta es negativo
      contador = contador+2
      print("Delta es negativo")
    }else{
      # Decir que no se mueve
      print("Valio chetos")
      break
    }
  }
  # Paso 3 Y 4 PARA QUE BUSQUE
  cond = T
  while (cond == T) {
    # Aumentar el K y sacar el nuevo X
    #x_0 ES el punto actual
    x_km1 =  x_0-(2^(k-1)  * Delta)  # ES el punto anterior
    x_k1 = x_0 + 2^k  * Delta
    # Evaluar el tema del paso 4
    cond = FX(x_k1)<FX(x_0)
    contador = contador+2
    # Ahora, actualizo las variables
    x_0 = x_k1 # Actualizo X_0 para tomar el nuevo valor K
    # Actualizo K
    k = k+1
  }
  # Aqui termina, reporta los resultados
  # Intervalo encontrado
  print(paste0("El intervalo es: ", "[", x_km1 ,", ",  x_k1,"]" ))
  # Numero de iteraciones para K
  print(paste("Se hicieron ", k, " iteraciones"))
  # Numero evaluaciones
  print(paste("Se hicieron ", contador, " evaluaciones de la función"))
}

CU = function(x){x*0}
FA(FX = CU,x_0 = -0.4,Delta = -0.5)


#### Parametros dados en clase ####
FA(FX = FU,x_0 = 0.6,Delta = 0.5)
#INtervalo = (2.1,8.1)
# iteraciones = 4
# Evaluaciones = 10

#### Propuesta intermedia ####
FA(FX = FU,x_0 = 2.1,Delta = 0.5)

# Intervalo = (2.1,3.6)
# iteraciones = 2
# Evaluaciones = 6



#### Mejores resultados ####
FA(FX = FU,x_0 = 3,Delta = 0.05)
# Intervalo = (2.375,3.05)
# iteraciones = 1
# Evaluaciones = 4

### Se recomienda una busqueda de rejilla para calibrar los parametros ####

# se requieren el numero de parametros

# Para cada parametro, determinar los valores adecuados

# Realizar ejecuciones con cada combinación de valores de los parámetros

# Evaluar el desempeño de cada combinación

# PREGUNTA DE EXAMEN

# ¿ESTE ALGORITMO ES DETERMINISTA?
# Sí, es determinista porque si yo le doy los mismos parametros, siempre 
# me va a devolver el mismo resultado.

# La función es unimodal




X = seq(2.95,3.05,by = 0.001)
Y = FU(X)
plot(X,Y,type = "l")



# Codigo y documentar la calibración

# Recordar que hay que optimizar esto #

# Debo ver que onda con esto

