# K medias Angel #

#### Librerias necesarias para el procesamiento ###
using CSV # Para cargar archivos CSV
using DataFrames  # Para cargar los datos en un DataFrame
using Distributions # Para las distribuciones de probabilidad
using Statistics # Para calcular el vector de medias y la covarianza
using RDatasets # Para hacer pruebas con Iris

#### Funciones auxiliares para el camino ####
#datos = dataset("datasets", "iris") # Iris para hacer pruebas
#X = Matrix(datos[:,1:4 ])  # Iris para mis pruebas 
function kmedias(X,K)
    # Creo los objettos necesarios
    K = Int(K)
    datos = Matrix(X) # Convertir los datos en Matrix
    N = Int(size(datos)[1]) # Numero de filas
    P = Int(size(datos)[2]) # Numero de columnas
    contador = 0 # Contador para llevar el conteo de las iteraciones
    iter = 50 # Cantidad de iteraciones permitidas
    error = 0.0001 # error permitido
    etiqueta = zeros(Int,N) # Vector que va a guardar las asignaciones del cluster
    Jv = [] # Vector que guardara las medidas de distorción por iteración
    # Creo una matriz de KxP donde K son los grupos y P las dimensiones
    medias = zeros(K,P)
    #### Asignar los centroides al azar con K sujetos ramdon ###
    for i in 1:K
        medias[i,:] = datos[rand(1:N) ,:]
    end
    #### Parte iterativa ####
    ## Matriz para guardar las distancias ##
    distancia = zeros(N,K)
    while (contador<=iter)
        ## Calculo de las distancias al cluser ##
        for i in  1:N
            for j in 1:K
                ### Calcular la diferencia del dato respecto al kesimo cluster
                dif = datos[i,:]-medias[j,:]
                ### Calcular la distancia para cada K
                distancia[i,j] = (dif')*dif
            end   
            # Etiquetar al dato con el cluster con el que tiene menor distancia
            etiqueta[i] = argmin(distancia[i,:])    
        end



        ## Evaluar la medida de distorción ##
        J = 0 # Meida de distorción a minimizar
        for i in 1:N
            # Sumar la distancia minima de los clusters seleccionados
            J = J + distancia[i,etiqueta[i]]   
        end

        ## Actualizar los valores de los centroides ##
        for i in unique(etiqueta)
            # Revisar y Actualizar los centroides
            medias[i,:] = mean.(eachcol(datos[etiqueta .== i, :]))
        end

        ## Actualizar el contador ##
        contador = contador + 1
        ## Agregar al vector Jv el valor de J de la J-esima iteración
        push!(Jv,J)

        ## Evaluar la diferencia entre las medidas de disimilitud para convergencia
        if contador >=2
            if (error>abs(Jv[contador]-Jv[contador-1]))
                break
            end
        end                
    end
    # Creo una lista con las salidas
    etiquetas = DataFrame(etiquetas=etiqueta)
    return etiquetas
end

