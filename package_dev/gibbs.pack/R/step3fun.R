#' Sample \eqn{\theta_i} in the third step of the MCMC sampler.
#'
#' For internal use only. Runs the third step of the Gibbs sampler, which samples \theta_i ideal point
#' for a unique demographic profile in 1...N.
#'
#' @param sigma2_theta A scalar, default is set to 1.
#' @param beta_j A Jx1 matrix, or data.frame corresponding to the first column of
#' \eqn{\tilde{\beta} = [\beta_j, \alpha_j]^T} created in Step 2, where each row indexes a response item j.
#' @param f_prior_i A single scalar element of the f_prior vector of length N.
#' @param y_tilde A single row vector of length J, from the matrix given by
#'  \eqn{[\{\kappa_{ij} / \omega_{ij}^{(t)} + \alpha_{j}^{(t)}\}_{j=1}^{J}]^T}.
#'
#' @return A single scalar, corresponding to \eqn{\theta_i^{(t)}} for a unique demographic profile:
#'  \item{\code{theta_i}}{The sampled \eqn{\theta_i^{(t)}} from the from the
#'  univariate normal distribution.}
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


step3fun <- function(beta_j, f_prior_i, y_tilde_i, sigma2_theta=1){

  # Applying the definition of V_theta
  #betas are current time, t, betas
  V_theta <- as.numeric(solve((1/sigma2_theta) + (t(as.matrix(beta_j)) %*% as.matrix(beta_j))))

  # Applying the definition of m_theta
  #beta is current time,t, beta
  #f_prior is the prior time's f, f^(t-1)
  m_theta <- as.numeric(V_theta %*% ((f_prior_i / sigma2_theta) + (t(as.matrix(beta_j)) %*% matrix(y_tilde_i, byrow=FALSE))))

  # Sample theta from univariate normal distribution
  theta_i <- rnorm(1, mean = m_theta, sd = sqrt(V_theta))

  names(theta_i) <- NULL

  return(theta_i)

}
