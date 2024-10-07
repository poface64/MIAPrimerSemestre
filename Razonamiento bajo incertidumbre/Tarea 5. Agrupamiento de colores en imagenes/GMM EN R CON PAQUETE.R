# Librerías necesarias
library(MASS)  # Para la función mvrnorm

# Función GMM para distribución normal multivariante
gmm_multivariante <- function(X, K, max_iter = 100, tol = 1e-6) {
  N <- nrow(X)
  d <- ncol(X)
  
  # Inicialización
  pi_k <- rep(1/K, K)  # Pesos iniciales
  mu_k <- matrix(rnorm(K * d), nrow = K)  # Medias iniciales aleatorias
  Sigma_k <- array(0, dim = c(d, d, K))  # Matrices de covarianza iniciales
  
  for (k in 1:K) {
    Sigma_k[, , k] <- diag(d)  # Inicializar con matrices de identidad
  }
  
  log_likelihood <- numeric(max_iter)
  
  for (iter in 1:max_iter) {
    # E-step: calcular responsabilidades
    r_ik <- matrix(0, N, K)
    for (k in 1:K) {
      r_ik[, k] <- pi_k[k] * dmvnorm(X, mean = mu_k[k, ], sigma = Sigma_k[, , k])
    }
    r_ik <- r_ik / rowSums(r_ik)
    
    # M-step: actualizar los parámetros
    N_k <- colSums(r_ik)
    pi_k <- N_k / N
    mu_k <- t(r_ik) %*% X / N_k
    
    for (k in 1:K) {
      diff <- X - mu_k[k, ]
      Sigma_k[, , k] <- (t(diff) %*% (diff * r_ik[, k])) / N_k[k]
    }
    
    # Calcular log-verosimilitud
    log_likelihood[iter] <- sum(log(rowSums(r_ik)))
    
    # Comprobar convergencia
    if (iter > 1 && abs(log_likelihood[iter] - log_likelihood[iter - 1]) < tol) {
      break
    }
  }
  
  return(list(pi = pi_k, mu = mu_k, Sigma = Sigma_k, logLik = log_likelihood[1:iter]))
}

# Ejemplo de uso
set.seed(123)

# Generar datos simulados de 2 distribuciones normales multivariantes
X1 <- mvrnorm(100, mu = c(5, 5), Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2))
X2 <- mvrnorm(100, mu = c(10, 10), Sigma = matrix(c(1, -0.7, -0.7, 2), nrow = 2))
X <- rbind(X1, X2)  # Conjunto de datos combinado

# Aplicar el algoritmo GMM
K <- 2
resultado <- gmm_multivariante(X, K)

# Resultados finales
cat("Pesos de mezcla (pi):", resultado$pi, "\n")
cat("Medias (mu):\n", resultado$mu, "\n")
cat("Matrices de covarianza (Sigma):\n", resultado$Sigma, "\n")
