#' step4fun
#'
#' Runs the fourth step of the gibbs sampler, in which you sample \eqn{f^{(t)}}.
#'
#' @param K_rho An N x N covariance matrix generated using a kernel of demographic features. NOTE: We need to either generate that here, or create a function for that
#' @param \eqn{\theta^{(t)}} An N x 2 matrix, which is the output of the previous step for all i.
#'
#' @return A 1x? vector \eqn{f^{(t)}}:
#'  \item{\eqn{f^{(t)}}{The sampled \eqn{f^{(t)}} from the from the
#'  multivariate normal distribution.}
#'
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @note I think Alma's question about the output on theta is correct; if it ouptuts a vector of length 2, we get a vector that is
#' @examples
#'
#'
#' @seealso step1fun, step2fun, step3fun, GibbsSampler-method
#' @aliases step4
#' @rdname step4fun
#' @import
#' @keywords internal


step4fun <- function(K_rho, theta, sigma2_theta=1){

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
