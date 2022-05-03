

step2fun <- function(X, w_j, k_j, Lambda=0.1){
  
  # Store number of demographic profiles for convenience
  N <- length(w_j)
  
  # Turn jth col vector into diagonal NxN matrix
  Omega_j <- diag(w_j, nrow = N, ncol = N)
  
  Lambda <- diag(0.1, nrow = 2, ncol = 2)
  
  # Applying the definition of V_beta
  V_beta <- solve(Lambda + (t(X) %*% Omega_j %*% X))
  
  # Applying the definition of m_beta
  m_beta <- V_beta %*% (t(X) %*% k_j)
  
  # Attempt at sampling beta from multivariate normal
  beta_tilde_j <- MASS::mvrnorm(1, mu = m_beta, Sigma = V_beta)
  
  return(beta_tilde_j)
}

