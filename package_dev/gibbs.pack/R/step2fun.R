#' Sample \eqn{\tilde{\beta_j}} in the second step of the MCMC sampler.
#'
#' For internal use only. Runs the second step of the Gibbs sampler, which samples the beta distribution
#' created through the defined m and v once, given the input values
#'
#' @param X An Nx2 matrix with each column x_i = [\theta_i^{t-1},-1]
#' @param w_j An Nx1 matrix defined as \eqn{\omega_{ij}^{(t)}}_{i=1}^N
#' @param y_j A vector of length N containing the count of respondents in group \eqn{i \in 1...N} who
#' responded to item j affirmatively.
#' @param n_j A scalar or a vector of length N containing the number of respondents in each unique demographic
#' profile who answered item j.
#' @param Lambda A 2x2 matrix defined as a covariate matrix with diag(0.1)
#'
#' @return A 2x1 matrix \tilde{\beta_j}:
#'  \item {\tilde{\beta_j}}{The sampled \beta_j from the distribution calculated from the above information}
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



step2fun <- function(X, w_j, y_j, n_j, Lambda=0.1){

  # Store length of w for convenience, equal to number of respondent groups
  N <- length(w_j)

  # Calculate kappa j
  k_j <- data.matrix((y_j-n_j)/2)

  # Turn 1xN jth col vector into diagonal NxN matrix
  Omega_j <- diag(w_j, nrow = N, ncol = N)

  Lambda <- diag(0.1, nrow = 2, ncol = 2)

  # Applying the definition of V_beta
  V_beta <- solve(Lambda + (t(X) %*% Omega_j %*% X))

  # Applying the definition of m_beta
  m_beta <- V_beta %*% (t(X) %*% k_j)

  # Attempt at sampling beta from multivariate normal
  beta_tilde_j <- MASS::mvrnorm(1, mu = m_beta, Sigma = V_beta)

  return(beta_tilde_j)
}



