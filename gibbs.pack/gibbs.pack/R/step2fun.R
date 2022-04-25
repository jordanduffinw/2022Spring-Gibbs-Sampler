#' step2fun
#'
#' Runs the second step of the Gibbs sampler, which samples the beta distribution created through the defined m and v once, given the input values
#'
#' @param X An nx2 matrix with each column x_i = [\theta_i^{t-1},-1]
#' @param omega An nxn matrix defined as $diag({\omega_{ij}^{(t)}}_{i=1}^N)
#' @param kappa An nx1 matrix defined as the transpose of the jth row of the \Kappa NxN covariance matrix
#' @param lambda A 2x2 matrix defined as ???
#'
#' @return A 2x1 matrix \hat{\beta_j}:
#'  \item{\beta_j}{The sampled \beta_j from the distribution calculated from the above information}
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @note Currently still need to verify what exactly lambda is in this, add checks, and run a test
#' @examples
#'
#'
#' @seealso
#' @aliases step2
#' @rdname step2fun
#' @import
#' @export

#set the generic for the function
setGeneric(name = "step2fun",
           def = function(X, omega, kappa, lambda)
             {standardGeneric("step2fun")}
)

#Define the method
setMethod(f = "step2fun",
          definition = function(X, omega, kappa, lambda){
            # Applying the definition of v_beta
            v_beta <- solve(lambda + (t(X) %*% omega %*% X))
            # Applying the definition of m_beta
            m_beta <- v_beta %*% (t(X) %*% kappa)

            # Attempt at sampling beta from multivariate normal
            beta_sample <- MASS::mvrnorm(1, mu = m_beta, Sigma = v_beta)

            return(beta_sample)
          }
)
