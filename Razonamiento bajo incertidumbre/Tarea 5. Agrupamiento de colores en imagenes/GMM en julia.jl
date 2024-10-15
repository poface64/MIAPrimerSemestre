
# Cargar las paqueterias para la mezcla de Gaussianas

# Ajustar el modelo GMM con 3 componentes (ya que sabemos que hay 3 especies en iris)
using DataFrames
using GaussianMixtures
using RDatasets
using Statistics
function gausiano(K,X)
    ### indicarle que K es un entero
    K = Int(K)
    ### Indicarle que X es una Matrix
    X = Matrix(X)
    ### Calcular el modelo de mezcla de Gaussiana
    gmm = GMM(K, X)
    ### Calcular los posteriores
    A = (gmmposterior(gmm,X))
    ### Extraer las etiquetas
    Z = Matrix(A[1])
    etiqueta = [argmax(row) for row in eachrow(Z)]
    return(DataFrame(Etiqueta = etiqueta))    
end






