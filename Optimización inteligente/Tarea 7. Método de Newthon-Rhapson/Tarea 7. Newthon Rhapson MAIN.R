#### Método de Newthon-Rhapson ####
# fx = función que se desea minímizar
# x = Punto de inicio
# delta = Tamaño del paso
# tolerancia = Tolerancia para la convergencia
# fijo = Valor para decir si el delta es dinamico o no
NS = function(fx,x,delta,tolerancia,fijo){
  # Definir algunas funciones auxiliares como la de las derivadas
  fp = function(fx,x,delta){
    # Crear los valores necesarios que se repiten
    a1 = fx(x + delta)
    a2 = fx(x - delta)
    ## Evaluar la primera y segunda derivada
    fp1 = (a1-a2)/(2*delta)
    fp2 = (a1-2*fx(x)+ a2)/(delta^2)
    ## Regresar los valores
    resu = c(fp1,fp2)
    return(resu)
  }
  # Defino un valaor provicional que se actualizara solo en el while
  fp1 = 10
  k = 1
  evalu = 0
  resultados = data.frame(Iter = 0,X = 0,derivada1 = 0,
                          derivada2 = 0,delta = 0,Evals =0)
  # Paso 4 que queda implicito como criterio de paro
  while(abs(fp1)>tolerancia){
    # Paso 2
    # Calcular las primeras y segundas derivadas numericamente
    # Calcular las derivadas numericas
    fpe = fp(fx,x,delta)
    evalu = evalu + 3
    # Asignar la primera derivada
    fp1 = fpe[1]
    # Asignar la segunda derivada
    fp2 = fpe[2]
    # Asignar los resultados al objeto
    resultados[k,] = c(Iter = k,X = x,derivada1 = fp1,
                       derivada2 = fp2,delta,evalu)
    # Paso 3, actualizar el valor de X
    # Calcular la nueva X 
    x = x - (fp1/fp2)
    # Paso 4, Actualizar el valor de K y volver al paso 2
    if(fijo==F){
      # Actualizar el valor de delta dinamico
      delta = ifelse(abs(x)>0.01,0.01*abs(x),0.01)
    }
    k = k+1
  }
  return(resultados)
}
