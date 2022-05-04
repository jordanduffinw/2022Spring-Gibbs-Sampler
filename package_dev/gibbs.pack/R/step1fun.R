#' Sample \eqn{\omega_{ij}} in the first step of the MCMC sampler.
#'
#' For internal use only. Runs the first step of the Gibbs sampler, which samples
#' \eqn{\omega_{ij} \sim PG(n_{ij},\mu_{ij}^{(t-1)})} for each unique demographic
#' profile/response item combination.
#'
#'
#' @param y A matrix y with J columns corresponding to affirmative responses to each item,
#  and N rows corresponding to each unique demographic profile.
#' @param n A vector of length N containing total respondents per unique demographic profile.
#' @param J_items A scalar representing the total number of response items.
#'
#' @return  \eqn{\omega} An NxJ matrix with elements \eqn{\omega_{ij} \sim PG(n_{ij},\mu_{ij}^{(t-1)})}
#' corresponding to demographic profiles i and response items j.
#'  \item{w}{The matrix \eqn{\omega} of Polya-Gamma-sampled values}
#'
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @note Nested for loops need to be replaced for efficiency
#' @examples
#'
#'
#' @seealso step2fun, step3fun
#' @aliases step1
#' @rdname step1fun
#' @import
#' @keywords internal

step1fun <- function(y, n, J_items){

  # Store number of unique demographic profiles and response items for convenience.
  N_groups <- nrow(y)

  # Define a matrix pi in which each column of y will be divided by the n_i
  # corresponding to the appropriate demographic profile--this is the observed probability of y, pi.
  pi <- data.matrix(do.call(cbind, apply(y, 2, function(x){x/n})))


  # Later on, apply qlogis() (logit) to each pi_{ij} given that pi is defined as the inverse logit of mu.
  # Cannot do it in this step because inputs are columns.

  # Rename columns and rows appropriately
  colnames(pi) <- paste0("item_",1:J_items)
  rownames(pi) <- paste0("group_",1:N_groups)

  # Initialize empty matrix omega with appropriate dimensions
  w <- matrix(NA, nrow = N_groups, ncol = J_items)

  # Matrix values imputed elementwise in a loop for now for transparency, this must be optimized.

  # For each unique demographic profile i in 1...N
  for(i in 1:N_groups){

    # For each response item j in 1...J
    for(j in 1:J_items){

      ### mu_{ij}} calculated here
      # Sample omega_{ij} from the Polya-Gamma distribution using the appropriate n_{ij} and mu_{ij}=qlogis(pi_{ij})
      w[i,j] <- BayesLogit::rpg(1, as.numeric(n[i,]), qlogis(pi[i,j]))

    }
  }
}
