#' Sample \eqn{\omega_{ij} in the first step of the MCMC sampler.
#'
#' For internal use only. Runs the first step of the Gibbs sampler, which samples 
#' \eqn{\omega_{ij} \sim PG(n_{ij},\mu_{ij}^{(t-1)})} for each unique demographic 
#' profile/response item combination.
#' 
#'
#' @param df A data frame where each row represents a unique demographic profile. 
#' Let \code{J_items} represent the total number of response items. Columns must be organized
#'  as follows:
#' \describe{
#' \item{\code{df[, ncol(df)]}}{The last column must be a vector containing the total number of 
#' individuals in demographic profile i who responsed to item j.}
#' \item{\code{df[ , (ncol(df)-J_items):(ncol(df)-1)]}}{Columns containing counts 
#' of affirmative responses per demographic profile/response item must appear between 
#' those containing demographic characteristics and the column of totals described above.}
#' }
#' @param J_items A scalar representing the total number of response items.
#'
#' @return  \eqn{\omega}: An NxJ matrix with elements \eqn{\omega_{ij} \sim PG(n_{ij},\mu_{ij}^{(t-1)})} 
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



step1fun <- function(df, J_items){
  
  # Define a matrix y with columns corresponding to affirmative responses to each item,
  # rows corresponding to each unique demographic profile.
  y <- df[ , (ncol(df)-J_items):(ncol(df)-1)]
  
  # Define a vector n corresponding to total respondents per unique demographic profile
  n <- df[ , ncol(df)]
  
  # Define a matrix mu in which each column of y will be divided by the n_i 
  # corresponding to the appropriate demographic profile--this is the observed probability of y, pi.
  
  # Next, apply qlogis() (logit) to each pi_{ij} given that pi is defined as the inverse logit of mu.
  
  mu <- data.matrix(do.call(cbind, apply(y, 2, function(x){qlogis(x/n)})))
  
  # Rename columns and rows appropriately
  colnames(mu) <- paste0("item_",1:N_items)
  rownames(mu) <- paste0("group_",1:nrow(mu))
  
  # Initialize empty matrix omega with appropriate dimensions
  w <- matrix(nrow = nrow(mu), ncol = ncol(mu))
  
  
  # Matrix values imputed elementwise in a loop for now for transparency, this must be optimized.
  
  
  # For each unique demographic profile i in 1...N
  for(i in 1:nrow(mu)){
    
    # For each response item j in 1...J
    for(j in 1:ncol(mu)){
      
      # Sample omega_{ij} from the Polya-Gamma distribution using the appropriate n_{ij} and mu_{ij}
      w[i,j] <- BayesLogit::rpg(1, as.numeric(n[i,]), mu[i,j])
      
    }
    
  }
  
  # Rename columns and rows appropriately
  colnames(w) <- paste0("item_",1:N_items)
  rownames(w) <- paste0("group_",1:nrow(mu))
  
  # Return the matrix omega of PG-sampled values
  return(w)
}