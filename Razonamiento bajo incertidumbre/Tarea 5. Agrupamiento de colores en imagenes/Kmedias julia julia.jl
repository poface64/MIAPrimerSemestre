
### Exportar el Kmedias a R ####
using DataFrames
using Clustering

# Cargar conjunto de datos Iris
#iris = dataset("datasets", "iris")
# Seleccionar características (variables independientes)
#X = collect(Matrix(iris[:, 1:4])')
# Seleccionar etiquetas (variable dependiente)
#y = iris[:,5]

# Definir modelo k-medias con k=3
function kmedias(X,K)
    # Modificar la matriz de entrada
    X = collect(Matrix(X)')
    # Calcular el kmedias
    resultados = kmeans(X, Int(K))
    return DataFrame(Cluster = resultados.assignments)
end
