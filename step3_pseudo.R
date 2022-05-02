
step3fun <- function(sigma2_theta=1, beta_tilde, f_prior, y_tilde){
  
  # Applying the definition of V_theta
  #betas are current time, t, betas
  V_theta <- solve(solve(sigma2_theta) + (t(beta_tilde) %*% beta_tilde))
  
  # Applying the definition of m_theta
  #beta is current time,t, beta
  #f_prior is the prior time's f, f^(t-1)
  m_theta <- V_theta %*% ((f_prior %/% sigma2_theta) + (t(beta_tilde) %*% y_tilde))
  
  # Attempt at sampling theta from multivariate normal distribution
  #Note the Sigma here is not the same as the sigma argument
  theta_sample <- MASS::mvrnorm(n, mu = m_theta, Sigma = V_theta)
  
  return(theta_sample)
  
}