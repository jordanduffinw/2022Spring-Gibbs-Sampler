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
#' @param X An Nx2 matrix with rows \eqn{x_i= [\theta_{i}^{t-1}, -1]}
#' @param rho_prior A vector of length D, the number of demographic features
#'  making up each profile.
#' @param f_prior A vector of length N, the number of demographic profiles.
#' @param Sigma_rho A matrix of d*d that denotes the covariance of the proposal (or jumping) density.
#' @param a,b The parameters that define the inverse-gamma distribution of the hyperpriors \eqn{\rho}.
#' Defaults are set at 2 and 1, respectively, for now.
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
          definition=function(df, J, X, rho_prior, f_prior, Sigma_rho, a=2, b=1){

            ## Important numbers:
            # store number of demographic groups,
            groups <- nrow(y)
            # number of demographic features,
            dem_features <- (ncol(df)-1)-J
            # and response items for convenience
            items <- J

            ## Important datasets:
            # store affirmative responses to each survey item,
            y <- df[ , (ncol(df)-J):(ncol(df)-1)]
            # total respondents per demographic profile,
            n <- as.data.frame(df[ , ncol(df)])
            # and matrix of demographic features and groups
            Z <- df[ , 1:N_dem_features]


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


            theta <- matrix(nrow = groups, ncol = 1)

            #### TODO: Replace for loop
            # Use step 3 to sample a theta_i for each demographic profile
            for(i in 1:groups){
              theta[i,] <- step3fun(beta_tilde$beta, f_prior[i], y_tilde[i,])
            }

            colnames(theta) <- "theta"

            K_rho <- calculateKfun(Z, rho_prior)

            f_t <- step4fun(K_rho, theta)

            rho_t <- step5fun(Z, f_t, rho_prior, Sigma_rho, a, b)

            #### TODO: Figure out what to output
            outputs <- list(...)

            return(outputs)
          }
)
