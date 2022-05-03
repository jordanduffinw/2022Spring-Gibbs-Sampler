#' Completes an iteration of the MCMC sampler in 4 steps.
#'
#'
#' @param df A data frame where each row represents a unique demographic profile.
#' Let \code{J} represent the total number of response items. Columns must be organized
#'  as follows:
#' \describe{
#' \item{\code{df[, ncol(df)]}}{The last column must be a vector containing the total number of
#' individuals in demographic profile i who responsed to item j.}
#' \item{\code{df[ , (ncol(df)-J):(ncol(df)-1)]}}{Columns containing counts
#' of affirmative responses per demographic profile/response item must appear between
#' those containing demographic characteristics and the column of totals described above.}
#' }
#' @param J A scalar representing the total number of response items.
#'
#' @return  Describe final output mathematically
#'  \item{\code{final_output_placeholder}}{Practical description}
#'
#' @author Jacob Montgomery, Bryant Moy, Noa Dasanaike, Santiago Olivella
#' @note Need steps 4 and 5, need to replace for loops, need to verify parameters
#' @examples
#'
#'
#' @seealso step1fun, step2fun, step3fun
#' @aliases GibbsSampler, GibbsSampler-method
#' @rdname step1fun
#' @import
#' @export
setGeneric(name="GibbsSampler",
           def=function(df, J)
           {standardGeneric("GibbsSampler")}
)
#' @rdname GibbsSampler
#'
#' @export
setMethod(f="GibbsSampler",
          definition=function(df, J){

            y <- df[ , (ncol(df)-4):(ncol(df)-1)]
            # and total respondents per demographic profile
            n <- as.data.frame(df[ , ncol(df)])

            # Store number of demographic groups and response items for convenience
            groups <- nrow(y)
            items <- J

            #### TODO: Replace with true definition
            # Create a fake X matrix; revisit when we figure out how to do this
            X <- matrix(c(c(1,2,3,4,5,6,7,8), rep(-1, 8)), byrow = FALSE,nrow = 8,ncol = 2)

            # Use step 1 to output omega matrix
            omega <- step1fun(y, n, items)

            # Calculate kappa for steps 2 and 3
            kappa <- data.matrix(do.call(cbind,apply(y,2,function(x){(x-n)/2})))

            # Initiate empty beta tilde matrix with 2 columns and rows the length number of response items
            beta_tilde <- matrix(nrow = items, ncol = 2)

            #### TODO: Replace for loop
            # Use step 2 to sample a beta_tilde for each response item
            for(j in 1:items){
              beta_tilde[j,] <- step2fun(X, omega[,j], kappa[,j])
            }

            # Save as dataframe for informative names
            beta_tilde <- data.frame(beta_tilde)

            # Assign names to components
            colnames(beta_tilde) <- c("beta", "alpha")
            rownames(beta_tilde) <- paste0("item_",1:items)

            # Use kappa, omega, and beta_tilde to calculate y_tilde
            # This divides, elementwise, kappa_{ij}/omega_{ij}. Then, to each item in the jth column, adds the jth item in the alpha component of beta_tilde
            y_tilde <- do.call(cbind, lapply(1:4,function(j){(kappa * (1/omega))[,j] + beta_tilde$alpha[j]}))

            #### TODO: Replace with true definition
            # Create a fake f_prior vector; revisit when we figure out how to do this
            f_prior <- sample(1:80, groups)

            #### TODO: Sampled thetas currently output as length 2 vectors for each demographic profile, is this correct?
            # Initiate empty theta matrix with 2 columns and rows the length number of demographic profiles
            theta <- matrix(nrow = groups, ncol = 2)

            #### TODO: Replace for loop
            # Use step 3 to sample a theta_i for each demographic profile
            for(i in 1:groups){
              theta[i,] <- step3fun(beta_tilde$beta, f_prior[i], y_tilde[i,])
            }

            colnames(theta) <- "theta"

            #### TODO: Add steps 4 and 5, adjust output accordingly

            return(theta)
          }
)
