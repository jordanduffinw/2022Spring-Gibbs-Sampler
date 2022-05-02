
step3fun <- function(beta_tilde, f_prior_i, y_tilde_i, sigma2_theta=1){
  
  
  # Applying the definition of V_theta
  #betas are current time, t, betas
  V_theta <- solve((1/sigma2_theta) + (t(as.matrix(beta_tilde)) %*% as.matrix(beta_tilde)))
  
  # Applying the definition of m_theta
  #beta is current time,t, beta
  #f_prior is the prior time's f, f^(t-1)
  m_theta <- V_theta %*% ((f_prior_i / sigma2_theta) + (t(as.matrix(beta_tilde)) %*% matrix(y_tilde_i, byrow=FALSE)))
  
  # Attempt at sampling theta from multivariate normal distribution
  #Note the Sigma here is not the same as the sigma argument
  theta_i <- MASS::mvrnorm(1, mu = m_theta, Sigma = V_theta)
  
  names(theta_i) <- NULL
  
  return(theta_i)
  
}