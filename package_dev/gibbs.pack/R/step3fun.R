#' step3fun
#'
#' Runs the third step of the Gibbs sampler, which samples \theta ideal point distribution given the function's inputs.
#'  The returned value is a number.
#'
#' @param sigma2_theta A scalar, default is set to 1.
#' @param beta_tilde A Jx2 matrix, or data.frame of Step 2 outputs where each row indexes a response item j.
#' @param f_prior_i A single scalar element of the f_prior vector of length N.
#' @param y_tilde A single row vector of length J, from the matrix given by
#'  \eqn{[\{\kappa_{ij} / \omega_{ij}^{(t)} + \alpha_{j}^{(t)}\}_{j=1}^{J}]^T}.
#'
#' @return A vector of length 2, containing \eqn{\theta_i^{(t)}} for a unique demographic profile:
#'  \item{\code{theta_i}}{The sampled \eqn{\theta_i^{(t)}} from the from the
#'  multivariate normal distribution.}
#'
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @note Need some clarity on the other values
#' @examples
#'
#'
#' @seealso step1fun, step2fun
#' @aliases step3
#' @rdname step3fun
#' @import
#' @keywords internal


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

  # Remove uninformative names
  names(theta_i) <- NULL

  return(theta_i)

}
