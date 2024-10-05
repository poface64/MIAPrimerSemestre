
#### Librerias necesarias para el procesamiento ###
using CSV # Para cargar archivos CSV
using DataFrames  # Para cargar los datos en un DataFrame
using Distributions # Para las distribuciones de probabilidad
using Statistics # Para calcular el vector de medias y la covarianza

#### Funciones auxiliares para el camino ####

#### Función para calcular la densidad para un punto nuevo ###
function dmvn(x, dist)
  pdf(dist, x)
end

#### Función para calcular la distribución normal multivariada ####
### y la clásificación en base al criterio bayesiano ###
function Bayesiano(X,prueba,cate,lambdaSI)
  ### Extraer las matrices de cada distribución ###
  # Para Piel
  SImed = mean.(eachcol((X[X[:, 4] .== cate, 1:3])))
  SIvar = cov(Matrix((X[X[:, 4] .== cate, 1:3])))
  # Para fondo
  NOmed = mean.(eachcol((X[X[:, 4] .!= cate, 1:3])))
  NOvar = cov(Matrix((X[X[:, 4] .!= cate, 1:3])))
  
  ### Distribuciones pre armadas ###
  # Distribución multivariada normal para SÍ y para NO
  dist1 = MvNormal(SImed,SIvar,)
  dist2 = MvNormal(NOmed,NOvar,)
  # Definir la apriori
  lambdaNO = 1-lambdaSI
  ### Hacer la implementación Bayesiana
  # Dondde guardar los resultados
  resultados = DataFrame(si = Float64[], no = Float64[],Etiqueta = String[])
    for i in 1:size(prueba)[1] # Comienzo un for que va desde 1 hasta nrow de prueba
    punto = Array(prueba[i,1:3])
    # Evaluo su distribución normal para PIEL
    nSI = dmvn(punto,dist1)
    nNO = dmvn(punto,dist2)
    # Calcular la norma (suma de probabilidades ponderadas)
    norma = (nSI * lambdaSI + nNO * lambdaNO)
    # Aplicar la regla de decisión de Bayes
    SIrgb = (nSI * lambdaSI) / norma
    NOrgb = (nNO * lambdaNO) / norma
    # Almacenar los resultados
    push!(resultados, (si = SIrgb, no = NOrgb,Etiqueta = map((x, y) -> x > y ? "Piel" : "Fondo", SIrgb, NOrgb)))
  end
  nprueba = hcat(prueba,resultados )
return (nprueba)
end



