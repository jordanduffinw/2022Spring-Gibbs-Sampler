#' Sample \tilde{\beta_j} in the second step of the MCMC sampler.
#'
#' For internal use only. Runs the second step of the Gibbs sampler, which samples the beta distribution
#' created through the defined m and v once, given the input values
#'
#' @param X An nx2 matrix with each column x_i = [\theta_i^{t-1},-1]
#' @param w An nx1 matrix defined as {\omega_{ij}^{(t)}}_{i=1}^N
#' @param k An nx1 matrix defined as the transpose of the jth row of the \Kappa NxN covariance matrix
#' @param Lambda A 2x2 matrix defined as a covariate matrix with diag(0.1)
#'
#' @return A 2x1 matrix \tilde{\beta_j}:
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
#' @keywords internal



step2fun <- function(X, w, k, Lambda){

            # Store length of w for convenience, equal to number of respondent groups
            N <- length(w)

            # Turn 1xN jth col vector into diagonal NxN matrix
            Omega <- diag(w, nrow = N, ncol = N)

            # Applying the definition of V_beta
            V_beta <- solve(Lambda + (t(X) %*% Omega %*% X))
            # Applying the definition of m_beta
            m_beta <- V_beta %*% (t(X) %*% k)

            # Attempt at sampling beta from multivariate normal
            beta_tilde <- MASS::mvrnorm(1, mu = m_beta, Sigma = V_beta)

            return(beta_tilde)
          }
)
