
step3fun <- function(sigma, beta, f_prior, y){
  
  # Applying the definition of v_theta
  #betas are current time, t, betas
  v_theta <- solve( solve(sigma) + (t(beta) %*% beta))
  
  # Applying the definition of m_theta
  #beta is current time,t, beta
  #f_prior is the prior time's f, f^(t-1)
  m_theta <- v_theta %*% ((f_prior %/% sigma) + (t(beta) %*% y))
  
  # Attempt at sampling theta from multivariate normal distribution
  #Note the Sigma here is not the same as the sigma argument
  theta_sample <- MASS::mvrnorm(n, mu = m_theta, Sigma = v_theta)
  
  return(theta_sample)
  
}