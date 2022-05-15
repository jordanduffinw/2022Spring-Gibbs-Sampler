#' Step 5: Sample the log hyperparameter \eqn{\rho_t}}
#'
#' For internal use only. Runs the first part of the fifth step of the Gibbs sampler, which samples \eqn{l\rho^*}}.
#'
#' @param Z An NxD matrix where rows represent each unique demographic profile,
#' and columns represent the distinct demographic features that make up each
#' profile.
#' @param f A vector of length N, sampled in step 4.
#' @param rho_lag A vector of length D that denotes the scale-length hyperparameter of each feature/covariate in period t-1.
#' @param Sigma_rho A matrix of d*d that denotes the covariance of the proposal (or jumping) density.
#' @param a,b The parameters that define the inverse-gamma distribution of the hyperpriors \eqn{\rho}.
#'
#' @return  A vector of length D \eqn{\rho_t}}, with each element denoting the hyperparameter of each feature/covariate in period t.
#'
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @note Let's see what we need here.
#' @examples
#'
#'
#' @seealso step1fun, step2fun, step3fun, step4fun, calculateKfun, calculatePartialpifun GibbsSampler-method
#' @aliases step5_1
#' @rdname step5fun
#' @import MASS, kernlab
#' @keywords internal

#' @export

library(kernlab)
step5fun <- function(Z, f, rho_lag, Sigma_rho, a, b){

  #5.1 Get lrho
  D <- length(rho_lag)
  lrho <- MASS::mvrnorm(1, mu = rho_lag, Sigma = Sigma_rho)

  #5.2 Get the kernel
  K_rho_lag <- calculateKfun(Z, rho_lag)
  K_lrho <- calculateKfun(Z, lrho)

  pi_exp_lrho <- calculateParitalpifun(Z, f, lrho, a, b)
  pi_exp_rho_lag <- calculateParitalpifun(Z, f, rho_lag, a, b)

  r <- pi_exp_lrho / pi_exp_rho_lag
  rho <- r^(exp(lrho)) * (1-r)^(rho_lag)
  return(rho)
}
