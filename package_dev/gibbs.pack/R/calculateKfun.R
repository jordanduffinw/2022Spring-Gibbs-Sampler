#' Calculate the kernel function \eqn{K}} in the fifth step of the MCMC sampler.
#'
#' For internal use only. Creates the squared exponential function
#' \eqn{K(z,z'| \rho) = exp[-0.5*||z_d/\rho_d - z'_d/\rho_d||]}
#'
#'
#' @param Z An NxD matrix where rows represent each unique demographic profile, 
#' and columns represent the distinct demographic features that make up each 
#' profile.
#' @param rho_t A vector of length D, drawn from the multivariate normal 
#' distribution in the previous step.
#'
#' @return  \eqn{K} An NxN matrix 
#'  \item{K}{The squared-exponential kernel function}
#'
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @note Nested for loops need to be replaced for efficiency
#' @examples
#'
#'
#' @seealso step3fun, step4fun
#' @aliases calculateK
#' @rdname calculateK
#' @import
#' @keywords internal
#' @export
calculateKfun <- function(Z, rho_t){
  
  # Divide each column of Z by its corresponding rho
  Z <- do.call(cbind, lapply(1:ncol(Z), function(i){Z[,i]/rho_t[i]}))
  
  # Calculate squared exponential kernel  
  rbf <- rbfdot(sigma=1)
  K <-kernelMatrix(kernel=rbf, as.matrix(Z))
  
  return(K)
}
