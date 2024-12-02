
### Funciones de busqueda unidireccional
source("/home/angel/Escritorio/MIAPrimerSemestre/Optimización inteligente/Tarea 2. Eliminación de regiones/Tarea 2. Eliminación de regiones MAIN.r")
source("/home/angel/Escritorio/MIAPrimerSemestre/Optimización inteligente/Tarea 4. Metodo de la sección dorada/Tarea 4. Busqueda la sección dorada MAIN.R")
# Función principal de Powell
powell = function(fx,x,tol,a,b){
  # Definir las funciones auxiliares internas
  # Función que calcula la norma de un vector
  norma = function(X){
    # Entra un vector y se obtiene su norma
    return(sqrt(sum(X^2)))
  }
  # Función para convertir el polinomio en terminos de alpha
  fa = function(alpha) {
    fx(x + alpha * di)  # f(x + alfa * d)
  }
  # Paso 1. Definir los objetos iniciales
  # Definir un K para llevar el conteo de cuantas vueltas a dado
  k = 0
  # Crear una matriz de resultados
  resultados = data.frame(Iter = 0,x1 = 0,x2 = 0,fx = 0, tol = 0 )
  # Mecanismo generador de las direcciones linealmente independientes
  # Crea una matriz de tamaño 
  D = length(x)
  direcciones = matrix(0,nrow = D+1,ncol = D)
  # Agregar la diagonal para que sean los vectores unitarios
  diag(direcciones) = 1
  # Valores de X para irlos almacenando
  X = matrix(0,nrow = D+2,ncol = D)
  # Definir que el primer punto, es el punto inicial anterior
  X[1,] = x
  normaD = 5 #Punto auxiliar para inicializar la norma que es condición de paro
  while(normaD>tol){
    # Paso 2. Minimizar a lo largo de las N + 1 direcciones, usando el minimo previo
    # para iniciar la siguiente busqueda y haicendo que s(N) sea 
    # la primera y la ultima dirección
    for(i in 1:nrow(direcciones)){
      # Seleccionar la dirección i-esima
      if(i == (D+1)){
        di = direcciones[1,]
      }else{
        di = direcciones[i,]
      }
      # Buscar el intervalo donde se encuentra el minimo
      elimina = ElimReg(a,b,0.001,fa)
      ultimo = nrow(elimina)
      # Buscar una buena aproximación del minimo
      xmin = Dorado(fa,elimina[ultimo,2],elimina[ultimo,3],0.001 )
      # Obtener la fila donde esta el ultimo
      ultimo2 = nrow(xmin)
      # Este es el alfa que minimiza la cosa
      amin = xmin[ultimo2,2]
      # Actualizar el punto en la matriz de puntos X
      X[i+1,] = X[i,] + amin * di
      x = X[i+1,]
    }
    # Paso 3 Forma la nueva dirección del subespacio paralelo
    Dv = X[4,]- X[2,]
    normaD =  norma(Dv)
    X[1,] = x
    ### Aqui actualiza la cosa
    k = k + 1
    resultados[k,] = data.frame(Iter = k,x1 = x[1],
                                x2 = x[2],fx = fx(x), tol = normaD)
    # La dirección s1 ahora se convierte en la s2
    direcciones[3,] = Dv/normaD
    for( i in 1:(D)){
      # Que la dirección n-1 sea igual a la dirección n
      direcciones[i,] = direcciones[i+1,]
    }
  }
  # Reportar los resultados
  return(resultados)
}













