step4fun <- function(Z, rho, theta, sigma2_theta=1){
  
  K_rho <- calculateKfun(Z, rho)
  
  #storing n
  n <- nrow(K_rho)
  
  #calculating Sigma_theta for the calculation of m_f
  Sigma_theta <- sigma2_theta*diag(n)
  
  #Applying the definition of V_f
  V_f <- K_rho - K_rho %*%  solve(K_rho + solve(Sigma_theta)) %*% K_rho
  
  # Applying the definition of m_f
  m_f <- K_rho %*% solve(K_rho + solve(Sigma_theta))%*% theta
  
  # Attempt at sampling theta from multivariate normal distribution
  #Note the Sigma here is not the same as the sigma argument
  f_t <- MASS::mvrnorm(1, mu = m_f, Sigma = V_f)
  
  # Remove uninformative names
  names(f_t) <- NULL
  
  return(f_t)
  
}