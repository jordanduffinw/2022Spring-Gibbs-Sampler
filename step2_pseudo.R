

step2fun <- function(X, omega, kappa, lambda){
  
  # Applying the definition of v_beta
  v_beta <- solve(lambda + (t(X) %*% omega %*% X))
  
  # Applying the definition of m_beta
  m_beta <- v_beta %*% (t(X) %*% kappa)
  
  # Attempt at sampling beta from multivariate normal
  # What is n? 
  beta_sample <- MASS::mvrnorm(n, mu = m_beta, Sigma = v_beta)
  
  return(beta_sample)
  
}