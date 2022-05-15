#' Calculate the joint posterior distribution density \eqn{\pi}} with respect to data and log hyperparameters \eqn{\rho}.
#'
#' For internal use only. Calculate \eqn{\pi}} in Equation (5) only with respect to components that contain data and \eqn{\rho}. 
#' That means only the third row and the fifth row of the right hand side would be calculated.
#'
#'
#' @param Z An NxD matrix where rows represent each unique demographic profile, 
#' and columns represent the distinct demographic features that make up each 
#' profile.
#' @param rho A vector of length D, drawn from the multivariate normal 
#' distribution in the previous step.
#' @param a,b The parameters that define the inverse-gamma distribution  of the hyperpriors \eqn{\rho}.
#'
#' @return A value \eqn{\pi} that the joint posterior distribution density.
#'
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @note 
#' @examples
#'
#'
#' @seealso step5fun, calculateKfun
#' @aliases calculateK
#' @rdname calculateParitalpifun
#' @import kernlab, invgamma, MASS
#' @keywords internal
#' @export
calculateParitalpifun <- function(Z, rho, a, b){
  
  # Calculate squared exponential kernel  
  K_rho <- calculateKfun(Z, rho)
  
  # Get the third row
  ## Get f
  N <- nrow(Z)
  f <- MASS::mvrnorm(1, rep(0, N), K_rho) ## This needs to be changed. Probably use results from step 4
  thirdrow_part1 <- det(K_rho)^(-0.5)
  thirdrow_part2 <- exp(-0.5*(t(f) %*% solve(K_rho) %*% f))[1,1]
  
  # Get the fifth row, which is the density of an inverse gamma distribution
  fifthrow <- exp(sum(log(invgamma::dinvgamma(rho, shape = a, rate = b))))
  
  # Get the density
  Partialpi <- thirdrow_part1 * thirdrow_part2 * fifthrow
  
  return(Partialpi)
}



