#' step4fun
#'
#' Runs the fourth step of the Gibbs Sampler, in which you sample \eqn{f^{(t)}}.
#'
#' @param Z An NxD matrix where rows represent each unique demographic profile, 
#' and columns represent the distinct demographic features that make up each 
#' profile.
#' @param rho A vector of length D, drawn from the multivariate normal.
#' distribution in the previous step.#' @param theta A vector of length N containing the ideal point of each demographic profile group.
#' @param theta A vector of length N containing the ideal point of each demographic profile group.
#' @param sigma2_theta A scalar, default is set to 1.
#'
#' @return A vector of length N:
#'  \item{\eqn{f^{(t)}}{The sampled \eqn{f^{(t)}} from the from the
#'  multivariate normal distribution.}
#'
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @examples
#'
#'
#' @seealso step1fun, step2fun, step3fun, GibbsSampler-method
#' @aliases step4
#' @rdname step4fun
#' @import MASS
#' @keywords internal


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
