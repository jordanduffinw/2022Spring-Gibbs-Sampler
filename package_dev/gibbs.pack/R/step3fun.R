#' step3fun
#'
#' Runs the third step of the Gibbs sampler, which samples \theta ideal point distribution given the function's inputs.
#'  The returned value is a number.
#'
#' @param sigma2_theta A scalar, default is set to 1.
#' @param beta_tilde A Jx2 matrix, the matrix of Step 2 outputs where each row indexes a response item j
#' @param f_prior A vector of length N, the number of unique demographic profiles.
#' @param y_tilde A vector of length J, with each element given by
#'  \eqn{[\{\kappa_{ij} / \omega_{ij}^{(t)} + \alpha_{j}^{(t)}\}_{j=1}^{J}]^T}.
#'
#' @return A vector of length N, containing \eqn{\theta_i^{(t)}} for each unique demographic profile:
#'  \item{\code{theta_sample}}{The vector of sampled \eqn{\theta_i^{(t)}} from the from the
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
